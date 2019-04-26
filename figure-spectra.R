source("packages.R")

folds.dt <- fread("Mayan_folds.csv")
folds.dt[, leaf := 1:.N]
folds.tall <- melt(folds.dt, id.vars=c("fold", "pyramid", "leaf"))
folds.tall[, nanometers := as.numeric(sub("nm", "", variable))]

folds.tall[, location := factor(
  ifelse(pyramid==1, "pyramid", "other"),
  c("pyramid", "other"))]
some.leaves <- folds.tall[, {
  .SD[leaf %in% unique(leaf)[1:5] ]
}, by=list(pyramid)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_line(aes(
    nanometers, value, group=leaf, color=location),
    data=some.leaves)

stats.dt <- folds.tall[, list(
  median=median(value),
  q25=quantile(value, 0.25),
  q75=quantile(value, 0.75)
), by=list(location, nanometers)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_ribbon(aes(
    nanometers, ymin=q25, ymax=q75, fill=location),
    alpha=0.5,
    data=stats.dt)+
  geom_line(aes(
    nanometers, median, color=location),
    data=stats.dt)

diff.dt <- folds.tall[, {
  is.pyramid <- location=="pyramid"
  with(t.test(value[is.pyramid], value[!is.pyramid]), list(
    mean.pyramid=estimate[1],
    mean.other=estimate[2],
    pyramid.minus.other=estimate[1]-estimate[2],
    ci95.lo=conf.int[1],
    ci95.hi=conf.int[2]))
}, by=list(nanometers)]
ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  geom_hline(
    yintercept=0,
    color="grey")+
  geom_ribbon(aes(
    nanometers, ymin=ci95.lo, ymax=ci95.hi),
    alpha=0.5,
    data=diff.dt)+
  geom_line(aes(
    nanometers, pyramid.minus.other),
    data=diff.dt)

glmnet.dt <- fread("interpretation/glmnet.csv")

importance.dt <- glmnet.dt[, list(
  mean.weight=mean(weight),
  n.nonzero=.N
  ), by=list(nanometers)][order(n.nonzero)]
diff.name <- "mean, 95% CI
of pyramid-other"
spec.name <- "median and
quartiles of spectra"
weight.name <- "linear model
weight"
yfac <- function(...){
  factor(c(...), c(weight.name, diff.name, spec.name))
}
stats.dt[, y := yfac(spec.name)]
diff.dt[, y := yfac(diff.name)]
stats.tall <- melt(stats.dt, measure.vars=c("median", "q25", "q75"))
stats.tall[, Variable := ifelse(variable=="median", "median", "quartile")]
gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y ~ ., scales="free")+
  geom_hline(aes(
    yintercept=yint),
    data=data.table(yint=0, y=yfac(diff.name, weight.name)),
    color="grey50")+
  ## geom_text(aes(
  ##   nanometers, mean.weight, label=n.nonzero),
  ##   data=data.table(importance.dt, y="weight"))+
  geom_point(aes(
    nanometers, mean.weight, fill=n.nonzero),
    shape=21,
    data=data.table(importance.dt, y=yfac(weight.name)))+
  ## geom_vline(aes(
  ##   xintercept=nanometers, alpha=n.nonzero),
  ##   data=importance.dt)+
  geom_line(aes(
    nanometers, pyramid.minus.other),
    data=diff.dt)+
  geom_ribbon(aes(
    nanometers, ymin=ci95.lo, ymax=ci95.hi),
    alpha=0.5,
    data=diff.dt)+
  geom_line(aes(
    nanometers, value, color=location, linetype=Variable,
    group=paste(variable, location)),
    data=stats.tall)+
  ylab("")+
  scale_fill_gradient(low="white", high="red", breaks=c(1, 5, 10))+
  scale_x_continuous(
    limits=c(NA, 2600),
    breaks=seq(400, 2500, by=100))+
  directlabels::geom_dl(aes(
    nanometers, value, color=location, label=location),
    method="last.polygons",
    data=stats.tall[variable=="median"])+
  guides(color="none")
png("figure-spectra-importance.png", 12, 4, units="in", res=100)
print(gg)
dev.off()

gg <- ggplot()+
  theme_bw()+
  theme(panel.margin=grid::unit(0, "lines"))+
  facet_grid(y ~ ., scales="free")+
  geom_hline(aes(
    yintercept=yint),
    data=data.table(yint=0, y=yfac(diff.name)),
    color="grey50")+
  geom_line(aes(
    nanometers, pyramid.minus.other),
    data=diff.dt)+
  geom_ribbon(aes(
    nanometers, ymin=ci95.lo, ymax=ci95.hi),
    alpha=0.5,
    data=diff.dt)+
  geom_ribbon(aes(
    nanometers, ymin=q25, ymax=q75, fill=location),
    alpha=0.5,
    data=stats.dt)+
  geom_line(aes(
    nanometers, median, color=location),
    data=stats.dt)+
  ylab("")+
  scale_x_continuous(limits=c(NA, 2600))
(dl <- directlabels::direct.label(gg, "last.polygons"))
png("figure-spectra.png", 6, 6, units="in", res=100)
print(dl)
dev.off()
