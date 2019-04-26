source("packages.R")

mayan <- fread("Mayan_data.csv", header=TRUE)

## 0=off pyramid
## 1=on pyramid

## The column headings give the spectral nanometers.
setnames(mayan, c("pyramid", paste0(names(mayan)[-1], "nm")))

set.seed(1)
n.folds <- 10
Mayan_folds <- data.table(fold=sample(rep(1:n.folds, l=nrow(mayan))), mayan)

fwrite(Mayan_folds, "Mayan_folds.csv")
