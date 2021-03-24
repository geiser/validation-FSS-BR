## ----setup, include=FALSE, echo=FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
defaultW <- getOption("warn")
options(warn = -1)


## ---- message=FALSE-------------------------------------------------------------------------------------------------------------
wants <- c('psych', 'dplyr','readxl','psy','olsrr','MVN','parameters')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options("mc.cores"=parallel::detectCores())

if (!'OpenMx' %in% rownames(installed.packages())) {
#  source('https://vipbg.vcu.edu/vipbg/OpenMx2/software/getOpenMx.R')
}

library(readr)
library(dplyr)
library(psych)
library(lavaan)
library(olsrr)
library(semTools)
library(mirt)


## -------------------------------------------------------------------------------------------------------------------------------
if (!file.exists("data-fss.csv")) {
  library(readxl)
  fss <- read_excel("fss2-data.xlsx", sheet = "raw-data")
  write_csv(fss[!fss$is_invalid,], "data-fss.csv")
}
raw_data <- read.csv("data-fss.csv")
fss <- as.data.frame(sapply(select(raw_data, starts_with("Q")), as.integer))


## -------------------------------------------------------------------------------------------------------------------------------
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

#' Summarize fit indexes of CFA models
#'
#' This functions summarize fit indexes from CFA models
#'
#' @param fits a list of lavaan objects with CFA models
#' @return fit indexes results of CFA models
summariseFits <- function(fits) {
 df_fit <- do.call(rbind, lapply(fits, FUN = function(fit) {
  dat <- as.list(round(fitMeasures(fit, c("chisq","df","cfi","tli","srmr","rmsea","rmsea.ci.lower","rmsea.ci.upper")), 3))
  rbind(c(dat[c("chisq","df")],"chisq/df"=round(dat$chisq/dat$df,3)
          , dat[c("cfi","tli","srmr","rmsea")]
          , "rmsea.ci" = paste0("[",dat$rmsea.ci.lower,"; ",dat$rmsea.ci.upper,"]")))
  }))
 rownames(df_fit) <- names(fits)
 return(df_fit) 
}


## -------------------------------------------------------------------------------------------------------------------------------
(parameters::check_sphericity(fss)) 


## -------------------------------------------------------------------------------------------------------------------------------
(kmo_mod <- KMO(fss)) 


## -------------------------------------------------------------------------------------------------------------------------------
(mvn_mod <- MVN::mvn(fss))


## -------------------------------------------------------------------------------------------------------------------------------
df <- cbind(mvn_mod$Descriptives[,c("Mean","Median","Std.Dev")]
            , "MSAi"=as.numeric(kmo_mod$MSAi)
            , "Statistic"=as.numeric(mvn_mod$univariateNormality$Statistic)
            , mvn_mod$Descriptives[,c("Skew","Kurtosis")]
            , mvn_mod$univariateNormality[,c("p value","Normality")])
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
if (!file.exists("pfa_mod.RData")) {
  pfa_mod <- fa.parallel(fss, fm = 'wls', fa = 'fa', cor='poly', plot = F)
  save(pfa_mod, file = "pfa_mod.RData")
}
load("pfa_mod.RData")
(pfa_mod)


## -------------------------------------------------------------------------------------------------------------------------------
plot(pfa_mod)


## -------------------------------------------------------------------------------------------------------------------------------
efa_mod <- fa(fss, nfactors = pfa_mod$nfact, cor = 'poly', fm = 'wls')
print(loadings(fa.sort(efa_mod)), cutoff = 0.3)


## -------------------------------------------------------------------------------------------------------------------------------
efa_mod <- fa(fss, nfactors = 9, cor = 'poly', fm = 'wls')
print(loadings(fa.sort(efa_mod)), cutoff = 0.3)


## -------------------------------------------------------------------------------------------------------------------------------
Modelo1a <- '
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

fit1a <-cfa(Modelo1a, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit1a, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit1a, return.total = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit1a, Modelo1a, fss)


## -------------------------------------------------------------------------------------------------------------------------------
Modelo1b <- '
CSB =~ Q1 + Q10 + Q19
MAA =~ Q2 + Q11 + Q20 + Q29
CG  =~ Q3 + Q12 + Q21 
UF  =~ Q4 + Q13 + Q22 + Q31
CTH =~ Q5       + Q23 + Q32
SC  =~      Q15 + Q24 + Q33
LSC =~ Q7 + Q16 + Q25
TT  =~ Q8 + Q17 + Q26
AE  =~ Q9 + Q18 + Q27 

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

fit1b <-cfa(Modelo1b, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit1b, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit1b, return.total = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit1b, Modelo1b, fss)


## -------------------------------------------------------------------------------------------------------------------------------
Modelo1c <- '
CSB =~    Q1 + Q10 + Q19
MAA =~    Q2 + Q11 + Q20 + Q29
CG_UF  =~ Q3 + Q12 + Q21       + Q4 + Q13 + Q22 + Q31
CTH =~    Q5       + Q23 + Q32
SC  =~         Q15 + Q24 + Q33
LSC =~    Q7 + Q16 + Q25
TT  =~    Q8 + Q17 + Q26
AE  =~    Q9 + Q18 + Q27

CSB ~~ MAA
CSB ~~ CG_UF
CSB ~~ CTH
CSB ~~ SC
CSB ~~ LSC
CSB ~~ TT
CSB ~~ AE

MAA ~~ CG_UF
MAA ~~ CTH
MAA ~~ SC
MAA ~~ LSC
MAA ~~ TT
MAA ~~ AE

CG_UF ~~ CTH
CG_UF ~~ SC
CG_UF ~~ LSC
CG_UF ~~ TT
CG_UF ~~ AE

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

fit1c <-cfa(Modelo1c, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit1c, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit1c, return.total = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit1c, Modelo1c, fss)


## -------------------------------------------------------------------------------------------------------------------------------
Modelo2a <- '
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

fit2a <-cfa(Modelo2a, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit2a, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit2a, return.total = TRUE)
reliabilityL2(fit2a, "FSS")


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit2a, Modelo2a, fss, "FSS")


## -------------------------------------------------------------------------------------------------------------------------------
Modelo2b <- '
CSB =~ Q1 + Q10 + Q19
MAA =~ Q2 + Q11 + Q20 + Q29
CG  =~ Q3 + Q12 + Q21 
UF  =~ Q4 + Q13 + Q22 + Q31
CTH =~ Q5       + Q23 + Q32
SC  =~      Q15 + Q24 + Q33
LSC =~ Q7 + Q16 + Q25
TT  =~ Q8 + Q17 + Q26
AE  =~ Q9 + Q18 + Q27 

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

fit2b <-cfa(Modelo2b, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit2b, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit2b, return.total = TRUE)
reliabilityL2(fit2b, "FSS")


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit2b, Modelo2b, fss, "FSS")


## -------------------------------------------------------------------------------------------------------------------------------
Modelo2c <- '
CSB =~    Q1 + Q10 + Q19
MAA =~    Q2 + Q11 + Q20 + Q29
CG_UF  =~ Q3 + Q12 + Q21       + Q4 + Q13 + Q22 + Q31
CTH =~    Q5       + Q23 + Q32
SC  =~         Q15 + Q24 + Q33
LSC =~    Q7 + Q16 + Q25
TT  =~    Q8 + Q17 + Q26
AE  =~    Q9 + Q18 + Q27

FSS  =~ CSB + MAA + CG_UF + CTH + SC + LSC + TT + AE

CSB ~~ 0*MAA
CSB ~~ 0*CG_UF
CSB ~~ 0*CTH
CSB ~~ 0*SC
CSB ~~ 0*LSC
CSB ~~ 0*TT
CSB ~~ 0*AE

MAA ~~ 0*CG_UF
MAA ~~ 0*CTH
MAA ~~ 0*SC
MAA ~~ 0*LSC
MAA ~~ 0*TT
MAA ~~ 0*AE

CG_UF ~~ 0*CTH
CG_UF ~~ 0*SC
CG_UF ~~ 0*LSC
CG_UF ~~ 0*TT
CG_UF ~~ 0*AE

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

fit2c <-cfa(Modelo2c, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit2c, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit2c, return.total = TRUE)
reliabilityL2(fit2c, "FSS")


## -------------------------------------------------------------------------------------------------------------------------------
convergentDiscriminantValidity(fit2c, Modelo2c, fss, 'FSS')


## -------------------------------------------------------------------------------------------------------------------------------
Modelo3a <- 'FSS =~ Q19 + Q29 + Q12 + Q22 + Q32 + Q6 + Q7 + Q17 + Q36'

fit3a <-cfa(Modelo3a, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit3a, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit3a, return.total = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
Modelo3b <- 'FSS =~ Q10 + Q11 + Q21 + Q4 + Q32 + Q24 + Q7 + Q17 + Q27'

fit3b <-cfa(Modelo3b, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit3b, fit.measures=TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
reliability(fit3b, return.total = TRUE)


## -------------------------------------------------------------------------------------------------------------------------------
df <- summariseFits(list('nine-multicorrelated model'=fit1a
                         ,'nine-multicorrelated model (with removed items)'=fit1b
                         , 'eight-multicorrelated model (with removed items)'=fit1c
                         , '2nd-order nine-factor model'=fit2a
                         , '2nd-order nine-factor model (with removed items)'=fit2b
                         , '2nd-order eight-factor model (with removed items)'=fit2c
                         , 'unidimensional model (short)'=fit3a
                         , 'unidimensional model (alt short)'=fit3b))
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
lavTestLRT(fit1b, fit1c, model.names=c('nine-multicorr','eight-multicorr'))


## -------------------------------------------------------------------------------------------------------------------------------
df <- rbind(reliability(fit1b, return.total = T)
            , "CR"=compReliability(fit1b, return.total = T))
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
df <- rbind(reliability(fit1c, return.total = T)
            , "CR"=compReliability(fit1c, return.total = T))
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
df <- rbind(reliability(fit3b), "CR"=compReliability(fit3b))
knitr::kable(t(df), digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
df <- convergentDiscriminantValidity(fit1b, Modelo1b, fss)
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
df <- convergentDiscriminantValidity(fit1c, Modelo1c, fss)
knitr::kable(df, digits = 3)


## -------------------------------------------------------------------------------------------------------------------------------
ls <- as.data.frame(standardizedSolution(fit1b))
ls <- ls[ls$op == '=~',]

fss2 <- fss[,ls$rhs]


## -------------------------------------------------------------------------------------------------------------------------------
dados_tri<-mirt(fss2, 1, itemtype='graded')


## -------------------------------------------------------------------------------------------------------------------------------
params <- coef(dados_tri, simplify=TRUE, IRTpars=TRUE)
knitr::kable(params$items, digits = 3)


## ---- fig.width=12--------------------------------------------------------------------------------------------------------------
plot(dados_tri, type='infotrace')


## ---- fig.width=12--------------------------------------------------------------------------------------------------------------
plot(dados_tri, type='info')


## ---- fig.width=12--------------------------------------------------------------------------------------------------------------
plot(dados_tri, type='infoSE')


## -------------------------------------------------------------------------------------------------------------------------------
ls <- as.data.frame(standardizedSolution(fit3b))
ls <- ls[ls$op == '=~',]

fss3 <- fss[,ls$rhs]


## -------------------------------------------------------------------------------------------------------------------------------
dados_tri3 <-mirt(fss3, 1, itemtype='graded')


## -------------------------------------------------------------------------------------------------------------------------------
params3 <- coef(dados_tri3, simplify=TRUE, IRTpars=TRUE)
knitr::kable(params3$items, digits=3)


## ----fig.width=12---------------------------------------------------------------------------------------------------------------
plot(dados_tri3,type='infotrace')


## ----fig.width=12---------------------------------------------------------------------------------------------------------------
plot(dados_tri3,type='info')


## ---- fig.width=12--------------------------------------------------------------------------------------------------------------
plot(dados_tri3,type='infoSE')


## ---- echo=FALSE----------------------------------------------------------------------------------------------------------------


