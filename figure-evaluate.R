source("packages.R")

mayan <- fread("Mayan_folds.csv")

pred.csv.vec <- Sys.glob("predictions/*.csv")

evaluation.list <- list()
for(pred.csv in pred.csv.vec){
  algorithm <- sub(".csv", "", basename(pred.csv))
  pred.dt <- fread(pred.csv)
  label.pred <- data.table(pred.dt, mayan)
  evaluation.list[[pred.csv]] <- label.pred[, {
    roc.dt <- WeightedROC::WeightedROC(pred.prob, pyramid)
    is.correct <- ifelse(0.5 < pred.prob, 1, 0) == pyramid
    data.table(
      algorithm,
      accuracy.percent=100*mean(is.correct),
      auc=WeightedROC::WeightedAUC(roc.dt))
  }, by=list(fold)]
}
evaluation <- do.call(rbind, evaluation.list)
eval.tall <- melt(evaluation, measure.vars=c("accuracy.percent", "auc"))
eval.stats <- eval.tall[, list(
  mean=mean(value),
  median=median(value)
), by=list(algorithm, variable)]
eval.ord <- eval.stats[variable=="accuracy.percent"][order(median)]
eval.tall[, Algorithm := factor(algorithm, eval.ord$algorithm)]

eval.tall[, var.fac := factor(variable, c("auc", "accuracy.percent"))]
gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(. ~ var.fac, scales="free")+
  geom_point(aes(
    value, Algorithm),
    data=eval.tall)+
  xlab("Prediction accuracy on test sets in 10-fold cross-validation")
png("figure-evaluate.png", 6, 2, units="in", res=100)
print(gg)
dev.off()
