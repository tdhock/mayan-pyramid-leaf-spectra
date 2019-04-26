source("packages.R")

reg.dir <- "registry-tuneGrid"
if(FALSE){# run this to delete registry if you want to restart from scratch.
  unlink(reg.dir, recursive=TRUE)
}
## Before creating your registry make sure to define cluster functions
if(FALSE){#put this in your ~/.batchtools.conf.R:
  ##TODO maybe change to your local copy of this template (especially if you want to execute a custom sbatch script)
  cluster.functions = batchtools:makeClusterFunctionsSlurm("~th798/R/PeakSegPipeline/inst/templates/slurm-afterok.tmpl")
}
reg <- if(file.exists(reg.dir)){
  batchtools::loadRegistry(reg.dir)
}else{
  batchtools::makeExperimentRegistry(reg.dir)
}
## Some code that initializes all the variables in that next function,
## for interactive testing.
getProbList <- function(job, data, test.fold, ...){
  source("packages.R")
  mayan <- data.table::fread("Mayan_folds.csv")
  all.X.mat <- as.matrix(mayan[, -(1:2), with=FALSE])
  all.y.vec <- mayan$pyramid
  dt <- data.table(
    location=factor(
      ifelse(all.y.vec==0, "other", "pyramid"),
      c("pyramid", "other")),
    all.X.mat)
  is.train <- if(test.fold==0){
    some.i <- as.integer(sapply(c(0,1), function(y)which(all.y.vec==y)[1:10]))
    seq_along(all.y.vec) %in% some.i
  }else{
    mayan$fold != test.fold
  }
  L <- list(
    full.dt=mayan,
    fold.vec=mayan$fold,
    test.fold=test.fold,
    is.train=is.train,
    train.dt=dt[is.train],
    train.X.mat=all.X.mat[is.train,],
    train.y.vec=all.y.vec[is.train],
    test.dt=dt[!is.train],
    test.X.mat=all.X.mat[!is.train,],
    test.y.vec=all.y.vec[!is.train])
  L
}
small.instance <- getProbList(test.fold=0)

addProblem("cv", reg=reg, fun=getProbList)
makeFun <- function(expr){
  e <- substitute(expr)
  function(instance, ...){
    eval(e, instance)
  }
}
pred.fun.list <- list(
  ## ADD NEW ML ALGOS HERE
  glmnet=makeFun({
    fit <- glmnet::cv.glmnet(
      train.X.mat, factor(train.y.vec), family="binomial")
    pred.prob.vec <- predict(fit, test.X.mat, type="response", s="lambda.min")
    list(fit=fit, pred.prob.vec=pred.prob.vec)
  }),
  xgboost=makeFun({
    xg.param <- list(
      objective="binary:logistic",
      eval_metric="auc"
    )
    xg.mat <- xgboost::xgb.DMatrix(
      train.X.mat, label=train.y.vec,
      missing=NA)
    fit <- xgboost::xgb.train(
      params=xg.param,
      data=xg.mat,
      nrounds=50)
    list(fit=fit, pred.prob.vec=predict(fit, test.X.mat))
  }),
  ## https://cloud.r-project.org/web/packages/caret/vignettes/caret.html
  ## https://topepo.github.io/caret/train-models-by-tag.html
  pls=makeFun({
    fit <- caret::train(
      location ~ .,
      data=train.dt,
      method="pls",
      preProc=c("center", "scale"),
      tuneGrid=expand.grid(ncomp=seq(2, 150, by=2)),
      trControl=caret::trainControl(
        method="cv",
        classProbs=TRUE,
        summaryFunction=caret::twoClassSummary),
      metric="ROC")
    pred.mat <- predict(fit, newdata=test.dt, type="prob")
    list(fit=fit, pred.prob.vec=pred.mat[, "pyramid"])
  }),
  knn=makeFun({
    fit <- caret::train(
      location ~ .,
      data=train.dt,
      method="knn",
      preProc=c("center", "scale"),
      tuneGrid=expand.grid(k=1:20),
      trControl=caret::trainControl(
        method="cv",
        classProbs=TRUE,
        summaryFunction=caret::twoClassSummary),
      metric="ROC")
    pred.mat <- predict(fit, newdata=test.dt, type="prob")
    list(fit=fit, pred.prob.vec=pred.mat[, "pyramid"])
  }),
  major.class=makeFun({
    response.tab <- table(train.y.vec)
    response.dec <- sort(response.tab, decreasing=TRUE)
    major.class <- as.integer(names(response.dec)[1])
    list(fit=response.dec, pred.prob.vec=rep(major.class, nrow(test.X.mat)))
  }))
## Interactively run each algorthm on small.instance to make sure it
## works here before calling addAlgorithm to indicate that it should
## be used with batchtools.
algo.list <- list()
funs.to.launch <- names(pred.fun.list)
##funs.to.launch <- "earth"
for(fun.name in funs.to.launch){
  pred.fun <- pred.fun.list[[fun.name]]
  small.result <- pred.fun(instance=small.instance)
  if(!all(
    is.numeric(small.result$pred.prob.vec),
    length(small.result$pred.prob.vec)==nrow(small.instance$test.y.vec)
  )){
    stop("pred.prob.vec not numeric vector of correct size")
  }
  is.prob <- with(small.result, 0 <= pred.prob.vec & pred.prob.vec <= 1)
  if(!all(is.prob)){
    stop("some predictions not in[0,1]")
  }
  batchtools::addAlgorithm(fun.name, reg=reg, fun=pred.fun)
  algo.list[[fun.name]] <- data.table()
}

## bind values to arguments of "cv" function...
test.fold.vec <- sort(unique(small.instance$fold.vec))
batchtools::addExperiments(
  list(cv=CJ(
    test.fold=test.fold.vec)),
  algo.list,
  reg=reg)

summarizeExperiments(reg=reg)
unwrap(getJobPars(reg=reg))

batchtools::submitJobs(reg=reg)

batchtools::getJobStatus()

(job.table <- batchtools::getJobTable(reg=reg))
done <- job.table[!is.na(done)]
for(algorithm in unique(done$algorithm)){
  select.dt <- data.table(algorithm)
  algo.jobs <- done[select.dt, on=list(algorithm)]
  pred.dt <- data.table(pred.prob=rep(NA_real_, nrow(small.instance$full.dt)))
  interpretation.list <- list()
  for(job.i in 1:nrow(algo.jobs)){
    job.row <- algo.jobs[job.i]
    job.row$prob.pars[[1]]$test.fold
    res.list <- batchtools::loadResult(job.row)
    test.fold <- job.row$prob.pars[[1]]$test.fold
    if(algorithm=="glmnet"){
      plot(res.list$fit)
      w.vec <- coef(res.list$fit, s="lambda.min")[-1,]
      not.zero <- w.vec[w.vec != 0]
      interpretation.list[[test.fold]] <- data.table(
        test.fold,
        weight=not.zero,
        nanometers=as.numeric(sub("nm", "", names(not.zero))))
    }
    if(algorithm=="pls"){
      print(res.list$fit$finalModel$tuneValue)
      plot(ROC ~ ncomp, res.list$fit$results)
    }
    if(algorithm=="knn"){
      plot(ROC ~ k, res.list$fit$results)
      print(res.list$fit$finalModel$tuneValue)
    }
    is.test <- small.instance$fold.vec == test.fold
    pred.dt[is.test, pred.prob := if(is.data.frame(res.list$pred.prob.vec)){
      res.list$pred.prob.vec[, "pyramid"]
    }else{
      res.list$pred.prob.vec
    }]
  }
  stopifnot(!is.na(pred.dt$pred.prob))
  out.list <- list(predictions=pred.dt)
  if(length(interpretation.list)){
    out.list$interpretation <- do.call(rbind, interpretation.list)
  }
  for(out.dir in names(out.list)){
    dir.create(out.dir, showWarnings=FALSE, recursive=TRUE)
    out.csv <- file.path(out.dir, paste0(algorithm, ".csv"))
    print(out.csv)
    fwrite(out.list[[out.dir]], out.csv)
  }
}

if(FALSE){#for compute cluster:
  ## in batchtools we can do array jobs if we assign the same chunk
  ## number to a bunch of different rows/jobs in the job table. Below we
  ## assign each job the chunk=1 so that there will be one call to
  ## sbatch and all of the rows become tasks of that one job.
  (job.table <- getJobTable(reg=reg))
  chunks <- data.table(job.table, chunk=1)
  submitJobs(chunks, reg=reg, resources=list(
    ##walltime = 24*60,#minutes
    walltime = 24*60*2,#minutes
    memory = 4000,#megabytes per cpu
    ncpus=2,
    ntasks=1,
    chunks.as.arrayjobs=TRUE))#means to use job arrays instead of separate jobs, for every chunk.
}
