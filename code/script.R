#' ---
#' title: "Factor Analysis of FSS-BR"
#' author: Geiser Chalco Challco <geiser@alumni.usp.br>
#' output:
#'   github_document:
#'     toc: true
#'   html_document: 
#'     toc: true
#'     toc_depth: 3
#'     theme: default
#'     highlight: default
#' fontsize: 10pt
#' ---
#' 
#' # Prerequisites
#' 
#'  - DataSet: [../data/fss.csv](../data/fss.csv)
#'  - R-script file: [../code/ancova.R](../script.R)
#'  - Files related to the presented results: [../results/](../results/)
#' 
#' ## Loading libs (packages)
#' 
## ---- message=FALSE-----------------------------------------------------------------------------------------------------------------------
wants <- c('mirt','semPlot','semTools','psych','parameters','latticeExtra','performance','olsrr','lavaan','knitr','matrixStats','stats','dplyr','readr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

#' 
#' ## Loading internal functions
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
#' Calculate factors from cfa model
#'
#' This functions calculate composite reliability values of factors
#'
#' @param fit a lavaan object expected to contain a CFA model
#' @param return.total logical indicating whether to return a final column containing the reliability of a composite of all items.
#' @return Reliability values of each factor in each group.
getFactors <- function(fit, secondFactor = NULL) {
  sl <- standardizedSolution(fit)
  sl <- sl[sl$op == '=~',]
  lfactors <- lapply(unique(sl$lhs), FUN = function(lhs) {
    sl$rhs[sl$lhs == lhs] 
  })
  names(lfactors) <- unique(sl$lhs)
  if (!is.null(secondFactor))
    lfactors[[secondFactor]] <- NULL
  return(lfactors)
}

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
#' Calculate composite reliability of factors
#'
#' This functions calculate composite reliability values of factors
#'
#' @param fit a lavaan object expected to contain a CFA model
#' @param return.total logical indicating whether to return a final column containing the reliability of a composite of all items.
#' @return Reliability values of each factor in each group.
compReliability <- function(fit, return.total = F) {
  toReturn <- sapply(getFactors(fit), FUN = function(x) {
    sl <- standardizedSolution(fit)
    sl <- sl$est.std[sl$op == "=~" & sl$rhs %in% x]
    names(sl) <- x
    
    re <- 1 - sl^2
    sum(sl)^2 / (sum(sl)^2 + sum(re))
  })
  if (return.total) {
    sl <- standardizedSolution(fit)
    sl <- sl$est.std[sl$op == "=~"]
    re <- 1 - sl^2
    toReturn <- c(toReturn, total=sum(sl)^2 / (sum(sl)^2 + sum(re)))
  }
  toReturn
}

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
#' Asessing convergent and discriminant validity
#'
#' This functions assess the discriminant and convergent validity of factors 
#' based on Heterotrait-Monotrait Ratio (semTools::htmt).
#'
#' @param fit a lavaan object expected to contain a CFA model
#' @param lnv string with the lavaan syntax of the fit CFA model.
#' @param dat data.frame with the data used in the CFA model
#' @param secondFactor second factor to be used in the CFA model
#' @return Composite Reliability, AVE and matrix of Heterotrait-Monotrait Ratio
convergentDiscriminantValidity <- function(fit, lvn, dat, secondFactor = NULL) {
  library(olsrr)
  library(semTools)
  
  CR <- compReliability(fit)
  AVE <- reliability(fit)[c("avevar"),]
  if (!is.null(secondFactor)) {
    AVE[secondFactor] <- reliability(fit, return.total = TRUE)["avevar","total"]
  }
  
  factors <- getFactors(fit, secondFactor)
  
  for (f1 in names(factors)) dat[[f1]] <- rowSums(dat[,factors[[f1]]])
  if (is.null(secondFactor)) secondFactor <- 'F0'
  dat[[secondFactor]] <- rowSums(dat[,names(factors)])
  
  mdl <- lm(as.formula(paste0(secondFactor,' ~ ', paste0(names(factors), collapse = '+'))), data = dat)
  VIF <- ols_vif_tol(mdl)$VIF
  VIF.i <- sapply(names(factors), FUN = function(f1) {
    mdl <- lm(as.formula(paste0(f1,' ~ ',paste0(factors[[f1]], collapse = '+'))), data = dat)
    max(ols_vif_tol(mdl)$VIF)
  })
  
  
  lvn2 <- stringr::str_replace(lvn, paste0("\n\\s*",secondFactor,"\\s*.*=~\\s*.*\n","\n"),"\n\n")
  corr.df <- as.table(inspect(fit, "cor.lv"))
  corr.df[upper.tri(corr.df)] <- NA
  htmt.df <- as.table(semTools::htmt(lvn2, dat))
  if (is.null(secondFactor) || secondFactor != 'F0') {
    cnames <- colnames(htmt.df)
    htmt.df <- cbind(htmt.df, rep(NA, nrow(htmt.df)))
    htmt.df <- rbind(htmt.df, rep(NA, ncol(htmt.df)))
    rownames(htmt.df) <- c(cnames, secondFactor)
    colnames(htmt.df) <- c(cnames, secondFactor)  
  }
  htmt.df[lower.tri(htmt.df)] <- NA
  
  df <- corr.df
  df[upper.tri(corr.df)] <- htmt.df[upper.tri(df)]
  for (cname in names(AVE)) df[cname,cname] <-  sqrt(AVE[[cname]])
  
  as.data.frame(cbind(CR,AVE, VIF, VIF.i, df))
}

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
#' Summarize fit indexes of CFA models
#'
#' This functions summarize fit indexes from CFA models
#'
#' @param fits a list of lavaan objects with CFA models
#' @return fit indexes results of CFA models
summariseFits <- function(fits, indexes = c(
  "chisq","df","gfi","agfi","cfi","tli","srmr",
  "rmsea","rmsea.ci.lower","rmsea.ci.upper")) {
  df_fit <- do.call(rbind, lapply(fits, FUN = function(fit) {
    dat <- as.list(round(fitMeasures(fit,indexes), 3))
    rbind(c(dat[c("chisq","df")],"chisq/df"=round(dat$chisq/dat$df,3)
            , dat[c("gfi","agfi","cfi","tli","srmr","rmsea")]
            , "rmsea.ci" = paste0("[",dat$rmsea.ci.lower,"; ",dat$rmsea.ci.upper,"]")))
  }))
  rownames(df_fit) <- names(fits)
  return(df_fit) 
}

#' 
#' ## Loading data
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(dplyr)

fss <- read.csv("data/fss.csv")
fss <- select(fss, starts_with("Q"))

#' 
#' # Checking assumptions
#' 
#' ### Performing Bartlett's test of sphericity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
(performance::check_sphericity_bartlett(fss))

#' 
#' -   Obs: We didnt use *bartlett.test()* because it performs homogeneity of variances
#' 
#' ### Performing Kaiser, Meyer, Olkin (KMO) based on Measure of Sampling Adequacy (MSA)
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(psych)
(kmo_mod <- psych::KMO(fss)) 

#' 
#' ### Summarizing assumptions
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
df <- data.frame(item=colnames(fss))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(mean(fss[[x]]), digits = 3) })))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(median(fss[[x]]), digits = 3) })))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(sd(fss[[x]]), digits = 3) })))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(skew(fss[[x]])[[1]], digits = 3) })))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(kurtosi(fss[[x]]), digits = 3) })))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) { round(shapiro.test(fss[[x]])$statistic, digits = 3)})))
df <- cbind(df, as.vector(sapply(colnames(fss), function(x) {
    p.val <- shapiro.test(fss[[x]])$p
    if (p.val < 0.001)
      return("< 0.001")
    else
      return(round(p.val, digits = 3))
  })))
df <- cbind(df, as.vector(round(kmo_mod$MSAi, digits =3)))
colnames(df) <- c("Item","M","Mdn","SD","Skew","Kurtosis","statistic","p.val","MSAi")

knitr::kable(df, digits = 3)

#' 
#' # Exploratory Factorial Analysis (EFA)
#' 
#' ## Performing parallel factorial analysis
#' 
## ----parallel-analisis--------------------------------------------------------------------------------------------------------------------
library(psych)
(pfa_mod <- fa.parallel(fss, fm = 'wls', fa = 'fa', cor='poly', n.iter = 1, main = "", ylabel = "", plot = F))
plot(pfa_mod)

#' 
#' ## Running EFA with 9 factors
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
efa_mod <- fa(fss, nfactors = 9, cor = 'poly', fm = 'wls')
print(loadings(fa.sort(efa_mod)), cutoff = 0.3)

#' 
#' # Construct validity of the long version of the FSS-BR
#' 
#' We decided to use the mean and variance-adjusted weighted least squares (WLSMV) estimator. WLSMV adequately performs on non-normal variables, and WLSMV is a robust estimator developed to model categorical or ordered data (5-likert scale).
#' 
#' -   Brown, T. (2006). Confirmatory factor analysis for applied research. New York: Guildford.
#' -   Proitsi, P., et al. (2009). A multiple indicators multiple causes (MIMIC) model of behavioural and psychological symptoms in dementia (BPSD). Neurobiology Aging. <doi:10.1016/j.neurobiolaging.2009.03.005>
#' 
#' ## Multicorrelated nine factor model (original model)
#' 
#' ### Structure validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(lavaan)
library(semTools)

mdl4multi <- '
CSB =~ Q1 + Q10 + Q19 + Q28
MAA =~ Q2 + Q11 + Q20 + Q29
CG  =~ Q3 + Q12 + Q21 + Q30
UF  =~ Q4 + Q13 + Q22 + Q31
CTH =~ Q5 + Q14 + Q23 + Q32
SC  =~ Q6 + Q15 + Q24 + Q33
LSC =~ Q7 + Q16 + Q25 + Q34
TT  =~ Q8 + Q17 + Q26 + Q35
AE  =~ Q9 + Q18 + Q27 + Q36

CSB ~~ MAA
CSB ~~ CG
CSB ~~ UF
CSB ~~ CTH
CSB ~~ SC
CSB ~~ LSC
CSB ~~ TT
CSB ~~ AE

MAA ~~ CG
MAA ~~ UF
MAA ~~ CTH
MAA ~~ SC
MAA ~~ LSC
MAA ~~ TT
MAA ~~ AE

CG ~~ UF
CG ~~ CTH
CG ~~ SC
CG ~~ LSC
CG ~~ TT
CG ~~ AE

UF ~~ CTH
UF ~~ SC
UF ~~ LSC
UF ~~ TT
UF ~~ AE

CTH ~~ SC
CTH ~~ LSC
CTH ~~ TT
CTH ~~ AE

SC ~~ LSC
SC ~~ TT
SC ~~ AE

LSC ~~ TT
LSC ~~ AE

TT ~~ AE
'

fit4multi <- cfa(mdl4multi, data=fss, estimator="WLSMV", std.lv=T)
standardizedsolution(fit4multi)

fitMeasures(fit4multi, fit.measures = "all")

#' 
## ----structure4longFSS, fig.width=8, fig.height=8, dpi=300--------------------------------------------------------------------------------
library(semPlot)
semPlot::semPaths(
  fit4multi, "std", curvePivot = T, layout = "circle", rotation = 3,
  fade = T, intercepts = T, residuals = T, sizeLat = 4, sizeLat2 = 3, sizeMan = 4,
  esize = 1.5, asize = 1.5, edge.label.position = 0.55, levels = c(9.75,10,10,10))

#' 
#' ### Internal consistency
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(semTools)
(df <- semTools::reliability(fit4multi))
knitr::kable(df, digits = 3)

#' 
#' ### Convergent validity and discriminant validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
(df <- convergentDiscriminantValidity(fit4multi, mdl4multi, fss))
knitr::kable(df, digits = 3)

#' 
#' ## Second order model (36 itens and 9 factors)
#' 
#' ### Structure validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(lavaan)

mdl4second <- '
CSB =~ Q1 + Q10 + Q19 + Q28
MAA =~ Q2 + Q11 + Q20 + Q29
CG  =~ Q3 + Q12 + Q21 + Q30
UF  =~ Q4 + Q13 + Q22 + Q31
CTH =~ Q5 + Q14 + Q23 + Q32
SC  =~ Q6 + Q15 + Q24 + Q33
LSC =~ Q7 + Q16 + Q25 + Q34
TT  =~ Q8 + Q17 + Q26 + Q35
AE  =~ Q9 + Q18 + Q27 + Q36

FSS  =~ CSB + MAA + CG + UF + CTH + SC + LSC + TT + AE

CSB ~~ 0*MAA
CSB ~~ 0*CG
CSB ~~ 0*UF
CSB ~~ 0*CTH
CSB ~~ 0*SC
CSB ~~ 0*LSC
CSB ~~ 0*TT
CSB ~~ 0*AE

MAA ~~ 0*CG
MAA ~~ 0*UF
MAA ~~ 0*CTH
MAA ~~ 0*SC
MAA ~~ 0*LSC
MAA ~~ 0*TT
MAA ~~ 0*AE

CG ~~ 0*UF
CG ~~ 0*CTH
CG ~~ 0*SC
CG ~~ 0*LSC
CG ~~ 0*TT
CG ~~ 0*AE

UF ~~ 0*CTH
UF ~~ 0*SC
UF ~~ 0*LSC
UF ~~ 0*TT
UF ~~ 0*AE

CTH ~~ 0*SC
CTH ~~ 0*LSC
CTH ~~ 0*TT
CTH ~~ 0*AE

SC ~~ 0*LSC
SC ~~ 0*TT
SC ~~ 0*AE

LSC ~~ 0*TT
LSC ~~ 0*AE

TT ~~ 0*AE
'

fit4second <-cfa(mdl4second, data=fss, estimator="WLSMV", std.lv=T)
standardizedSolution(fit4second)

fitMeasures(fit4second, fit.measures = "all")

#' 
## ----structure4secondLongFSS, fig.width=8, fig.height=6, dpi=300--------------------------------------------------------------------------
library(semPlot)
semPlot::semPaths(
  fit4second, "std", curvePivot = T, layout = "tree", rotation = 1,
  fade = T, intercepts = T, residuals = T, esize = 1.5, asize = 1.5,
  sizeLat = 4, sizeMan = 2, sizeMan2 = 4, sizeInt = 6, sizeInt2 = 12)

#' 
#' ### Internal consistency
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(semTools)
semTools::reliabilityL2(fit4second, "FSS")
(df <- semTools::reliability(fit4second, return.total = T))
knitr::kable(df, digits = 3)

#' 
#' ### Convergent validity and discriminant validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
(df <- convergentDiscriminantValidity(fit4second, mdl4second, fss, "FSS"))
knitr::kable(df, digits = 3)

#' 
#' ## Summarizing assessment model fits
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
(df <- summariseFits(list('9-multi model(36 items)'=fit4multi,
                          '2nd-order model (36 items)'=fit4second)))
knitr::kable(df, digits = 3)

#' 
#' ## IRT on the FSS-BR (36 itens and 9 factors - original version)
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(mirt)
mirt4mult <- mirt(fss, 1, itemtype='graded')
(param4mult <- coef(mirt4mult, simplify=T, IRTpars=T))

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(matrixStats)
df <- data.frame(param4mult$items)
df <- cbind(df, bx = matrixStats::rowMeans2(as.matrix(df), cols = c(2,3,4,5)))
df <- rbind(df, M = colMeans(df))
df <- rbind(df, SD = matrixStats::colSds(as.matrix(df)))
knitr::kable(df, digits = 3)

#' 
## ----irt4longFSS2infotrace, dpi=600-------------------------------------------------------------------------------------------------------
plot(mirt4mult, type='infotrace')

#' 
## ----irt4longFSS2info, dpi=600------------------------------------------------------------------------------------------------------------
plot(mirt4mult, type='info')

#' 
## ----irt4longFSS2infoSE, dpi=600----------------------------------------------------------------------------------------------------------
plot(mirt4mult, type='infoSE')

#' 
#' 
#' ## Unidimensional short model of FSS (original)
#' 
#' ### Structure validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(lavaan)
mdl4uni <- 'FSS =~ Q19 + Q29 + Q12 + Q22 + Q32 + Q6 + Q7 + Q17 + Q36'
fss.df <- fss[,c("Q19","Q29","Q12","Q22","Q32","Q6","Q7","Q17","Q36")]

fitMeasures(cfa(mdl4uni, fss.df), fit.measures = "all")

fit4uni <- cfa(mdl4uni, data=fss.df, estimator="WLSMV", std.lv=T)
standardizedSolution(fit4uni)

fitMeasures(fit4uni, fit.measures = "all")
moreFitIndices(fit4uni)

#' 
## ----structure4originalShortFSS, fig.width=12, fig.height=12, dpi=600---------------------------------------------------------------------
library(semPlot)
semPlot::semPaths(
  fit4uni, "std", curvePivot = T, layout = "circle",
  rotation = 3, fade = T, intercepts = T, residuals = T,
  sizeLat = 4, sizeLat2 = 3, sizeMan = 4, esize = 1.5, asize = 1.5,
  edge.label.position = 0.55, levels = c(9.75,10,10,10))

#' 
#' ### Internal consistency
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(semTools)
(df <- semTools::reliability(fit4uni))
knitr::kable(df, digits = 3)

#' 
#' ## Unidimensional short alternative model (FSS-short)
#' 
#' ### Structure validity
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(lavaan)
mdl4alt <- 'FSS =~ Q10 + Q11 + Q30 + Q4 + Q23 + Q24 + Q7 + Q26 + Q27'
fss.df <- fss[,c("Q10","Q11","Q30","Q4","Q23","Q24","Q7","Q26","Q27")]

fitMeasures(cfa(mdl4alt, fss.df), fit.measures = "all")

fit4alt <-cfa(mdl4alt, data=fss.df, estimator="WLSMV", std.lv=T)
standardizedSolution(fit4alt)

fitMeasures(fit4alt, fit.measures = "all")
moreFitIndices(fit4alt)

#' 
## ----structure4altShortFSS, fig.width=12, fig.height=12, dpi=300--------------------------------------------------------------------------
library(semPlot)
semPlot::semPaths(
  fit4alt, "std", curvePivot = T, layout = "circle",
  rotation = 3, fade = T, intercepts = T, residuals = T,
  sizeLat = 4, sizeLat2 = 3, sizeMan = 4, esize = 1.5, asize = 1.5,
  edge.label.position = 0.55, levels = c(9.75,10,10,10))

#' 
#' ### Internal consistency
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(semTools)
(df <- semTools::reliability(fit4alt))
knitr::kable(df, digits = 3)

#' 
#' ## Summarizing assessment model fits in short-versions
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
df <- summariseFits(list('uni model'=fit4uni, 'alt model'=fit4alt))
knitr::kable(df, digits = 3)

#' 
#' ## IRT on the FSS-short version (9 itens - alternative version)
#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(mirt)
mirt4alt <- mirt(fss[,c("Q10","Q11","Q30","Q4","Q23","Q24","Q7","Q26","Q27")],
                 1, itemtype='graded')
param4alt <- coef(mirt4alt, simplify=T, IRTpars=T)

#' 
## -----------------------------------------------------------------------------------------------------------------------------------------
library(matrixStats)
df <- data.frame(param4alt$items)
df <- cbind(df, bx = matrixStats::rowMeans2(as.matrix(df), cols = c(2,3,4,5)))
df <- rbind(df, M = colMeans(df))
df <- rbind(df, SD = matrixStats::colSds(as.matrix(df)))
knitr::kable(df, digits = 3)

#' 
## ----irt4shortFSS2infotrace, dpi=300------------------------------------------------------------------------------------------------------
plot(mirt4alt, type='infotrace')

#' 
## ----irt4shortFSS2info, dpi=300-----------------------------------------------------------------------------------------------------------
plot(mirt4alt, type='info')

#' 
## ----irt4shortFSS2infoSE, dpi=300---------------------------------------------------------------------------------------------------------
plot(mirt4alt, type='infoSE')

