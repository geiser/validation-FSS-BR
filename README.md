Factor Analysis of FSS-BR
================
Geiser Chalco Challco <geiser@alumni.usp.br>

-   [Prerequisites](#prerequisites)
    -   [Loading libs, functions and
        data](#loading-libs-functions-and-data)
    -   [Loading functions](#loading-functions)
-   [Checking assumptions](#checking-assumptions)
    -   [Performing Bartlett’s test of
        sphericity](#performing-bartletts-test-of-sphericity)
    -   [Performing Kaiser, Meyer, Olkin (KMO) based on Measure of
        Sampling Adequacy
        (MSA)](#performing-kaiser-meyer-olkin-kmo-based-on-measure-of-sampling-adequacy-msa)
    -   [Evaluating normality](#evaluating-normality)
    -   [Summarizing assumptions to perform
        analysis](#summarizing-assumptions-to-perform-analysis)
-   [Exploratory Factorial Analysis
    (EFA)](#exploratory-factorial-analysis-efa)
-   [Construct validity](#construct-validity)
    -   [Multicorrelated models](#multicorrelated-models)
        -   [Multicorrelated nine factor original
            model](#multicorrelated-nine-factor-original-model)
        -   [Multicorrelated nine factor model - fixed convergence and
            discriminant](#multicorrelated-nine-factor-model---fixed-convergence-and-discriminant)
        -   [Multicorrelated eight factor model - merging
            CG\~UF](#multicorrelated-eight-factor-model---merging-cguf)
    -   [2nd-order factor models](#nd-order-factor-models)
        -   [2nd-order nine factor original
            model](#nd-order-nine-factor-original-model)
        -   [2nd-order eight factor model - fixed convergence and
            discriminant](#nd-order-eight-factor-model---fixed-convergence-and-discriminant)
        -   [2nd-order eight factor model - merging
            CG\~UF](#nd-order-eight-factor-model---merging-cguf)
    -   [Unidimensional models (Short version of
        FSS-BR)](#unidimensional-models-short-version-of-fss-br)
        -   [Original short version of FSS-BR
            (FSS-short)](#original-short-version-of-fss-br-fss-short)
        -   [Alternative short version of FSS-BR (alt
            FSS-short)](#alternative-short-version-of-fss-br-alt-fss-short)
    -   [Summary of construct validity (structural, reliability and
        convergent/discriminant)](#summary-of-construct-validity-structural-reliability-and-convergentdiscriminant)
        -   [Structural validity - CFAs](#structural-validity---cfas)
        -   [Draw diagrams using semTool](#draw-diagrams-using-semtool)
        -   [Reliability validity - Cronbach / Omega /
            CR](#reliability-validity---cronbach-omega-cr)
        -   [Convergent and discriminant
            validity](#convergent-and-discriminant-validity)
-   [Item quality analysis based on IRT (Item Response
    Theory)](#item-quality-analysis-based-on-irt-item-response-theory)
    -   [IRT analysis in the long version of the
        DFS-BR](#irt-analysis-in-the-long-version-of-the-dfs-br)
        -   [Estimate parameters](#estimate-parameters)
        -   [Graphs of test information
            curve](#graphs-of-test-information-curve)
    -   [IRT analysis in the short version of the alt
        FSS-BR](#irt-analysis-in-the-short-version-of-the-alt-fss-br)
        -   [Estimate parameters](#estimate-parameters-1)
        -   [Graphs of test information
            curve](#graphs-of-test-information-curve-1)

# Prerequisites

-   R-script: [script.R](script.R)
-   DataSet: [data-fss.csv](data-fss.csv) OR
    [fss2-data.xlsx](fss2-data.xlsx)

## Loading libs, functions and data

Loading needed libs (packages)

``` r
wants <- c('psych', 'dplyr','readxl','psy','olsrr','MVN','parameters')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])
options("mc.cores"=parallel::detectCores())

if (!'OpenMx' %in% rownames(installed.packages())) {
  source('https://vipbg.vcu.edu/vipbg/OpenMx2/software/getOpenMx.R')
}

library(readr)
library(dplyr)
library(psych)
library(lavaan)
library(olsrr)
library(semTools)
library(mirt)
```

Loading data

``` r
if (!file.exists("data-fss.csv")) {
  library(readxl)
  fss <- read_excel("fss2-data.xlsx", sheet = "raw-data")
  write_csv(fss[!fss$is_invalid,], "data-fss.csv")
}
raw_data <- read.csv("data-fss.csv")
fss <- as.data.frame(sapply(select(raw_data, starts_with("Q")), as.integer))
```

## Loading functions

``` r
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
```

# Checking assumptions

### Performing Bartlett’s test of sphericity

``` r
(parameters::check_sphericity(fss)) 
```

    ## # Test of Sphericity
    ## 
    ## Bartlett's test of sphericity suggests that there is sufficient significant correlation in the data for factor analysis (Chisq(630) = 6543.72, p < .001).

-   Obs: We didn’t use *bartlett.test()* because it performs homogeneity
    of variances

### Performing Kaiser, Meyer, Olkin (KMO) based on Measure of Sampling Adequacy (MSA)

``` r
(kmo_mod <- KMO(fss)) 
```

    ## Kaiser-Meyer-Olkin factor adequacy
    ## Call: KMO(r = fss)
    ## Overall MSA =  0.86
    ## MSA for each item = 
    ##   Q1   Q2   Q3   Q4   Q5   Q6   Q7   Q8   Q9  Q10  Q11  Q12  Q13  Q14  Q15  Q16 
    ## 0.90 0.89 0.88 0.90 0.90 0.83 0.90 0.68 0.78 0.88 0.91 0.83 0.84 0.91 0.90 0.79 
    ##  Q17  Q18  Q19  Q20  Q21  Q22  Q23  Q24  Q25  Q26  Q27  Q28  Q29  Q30  Q31  Q32 
    ## 0.84 0.79 0.83 0.81 0.88 0.84 0.88 0.91 0.88 0.88 0.91 0.81 0.64 0.92 0.87 0.86 
    ##  Q33  Q34  Q35  Q36 
    ## 0.94 0.80 0.77 0.86

### Evaluating normality

``` r
(mvn_mod <- MVN::mvn(fss))
```

    ## Registered S3 method overwritten by 'GGally':
    ##   method from   
    ##   +.gg   ggplot2

    ## sROC 0.1-2 loaded

    ## $multivariateNormality
    ##              Test       Statistic               p value Result
    ## 1 Mardia Skewness 14306.451580769 6.06332246876875e-310     NO
    ## 2 Mardia Kurtosis 40.226562392401                     0     NO
    ## 3             MVN            <NA>                  <NA>     NO
    ## 
    ## $univariateNormality
    ##            Test  Variable Statistic   p value Normality
    ## 1  Shapiro-Wilk    Q1        0.8104  <0.001      NO    
    ## 2  Shapiro-Wilk    Q2        0.9051  <0.001      NO    
    ## 3  Shapiro-Wilk    Q3        0.8950  <0.001      NO    
    ## 4  Shapiro-Wilk    Q4        0.8596  <0.001      NO    
    ## 5  Shapiro-Wilk    Q5        0.8861  <0.001      NO    
    ## 6  Shapiro-Wilk    Q6        0.8949  <0.001      NO    
    ## 7  Shapiro-Wilk    Q7        0.8842  <0.001      NO    
    ## 8  Shapiro-Wilk    Q8        0.9131  <0.001      NO    
    ## 9  Shapiro-Wilk    Q9        0.8873  <0.001      NO    
    ## 10 Shapiro-Wilk    Q10       0.8709  <0.001      NO    
    ## 11 Shapiro-Wilk    Q11       0.8955  <0.001      NO    
    ## 12 Shapiro-Wilk    Q12       0.9059  <0.001      NO    
    ## 13 Shapiro-Wilk    Q13       0.8969  <0.001      NO    
    ## 14 Shapiro-Wilk    Q14       0.8642  <0.001      NO    
    ## 15 Shapiro-Wilk    Q15       0.8723  <0.001      NO    
    ## 16 Shapiro-Wilk    Q16       0.8974  <0.001      NO    
    ## 17 Shapiro-Wilk    Q17       0.9025  <0.001      NO    
    ## 18 Shapiro-Wilk    Q18       0.9143  <0.001      NO    
    ## 19 Shapiro-Wilk    Q19       0.8907  <0.001      NO    
    ## 20 Shapiro-Wilk    Q20       0.8933  <0.001      NO    
    ## 21 Shapiro-Wilk    Q21       0.8991  <0.001      NO    
    ## 22 Shapiro-Wilk    Q22       0.8792  <0.001      NO    
    ## 23 Shapiro-Wilk    Q23       0.8916  <0.001      NO    
    ## 24 Shapiro-Wilk    Q24       0.8702  <0.001      NO    
    ## 25 Shapiro-Wilk    Q25       0.8867  <0.001      NO    
    ## 26 Shapiro-Wilk    Q26       0.8998  <0.001      NO    
    ## 27 Shapiro-Wilk    Q27       0.8960  <0.001      NO    
    ## 28 Shapiro-Wilk    Q28       0.9122  <0.001      NO    
    ## 29 Shapiro-Wilk    Q29       0.9093  <0.001      NO    
    ## 30 Shapiro-Wilk    Q30       0.8660  <0.001      NO    
    ## 31 Shapiro-Wilk    Q31       0.8741  <0.001      NO    
    ## 32 Shapiro-Wilk    Q32       0.8692  <0.001      NO    
    ## 33 Shapiro-Wilk    Q33       0.8518  <0.001      NO    
    ## 34 Shapiro-Wilk    Q34       0.8931  <0.001      NO    
    ## 35 Shapiro-Wilk    Q35       0.9096  <0.001      NO    
    ## 36 Shapiro-Wilk    Q36       0.8597  <0.001      NO    
    ## 
    ## $Descriptives
    ##       n     Mean   Std.Dev Median Min Max 25th 75th         Skew    Kurtosis
    ## Q1  386 3.981865 0.8231600      4   1   5    4    4 -0.969842243  1.49902651
    ## Q2  386 3.178756 1.1629182      3   1   5    2    4 -0.221429494 -0.90797752
    ## Q3  386 3.316062 0.9769292      3   1   5    3    4 -0.162795183 -0.70192506
    ## Q4  386 3.722798 0.8817624      4   1   5    3    4 -0.611587654  0.22811532
    ## Q5  386 3.549223 0.9823967      4   1   5    3    4 -0.399386729 -0.48143364
    ## Q6  386 3.132124 1.2234310      3   1   5    2    4 -0.226788477 -1.06280655
    ## Q7  386 3.510363 1.0300152      4   1   5    3    4 -0.497138482 -0.41167692
    ## Q8  386 3.072539 1.2016968      3   1   5    2    4 -0.147998301 -0.87662668
    ## Q9  386 3.510363 1.0325338      4   1   5    3    4 -0.543077568 -0.20464608
    ## Q10 386 3.808290 0.9984579      4   1   5    3    5 -0.610430732 -0.15088868
    ## Q11 386 3.417098 1.0392894      4   1   5    3    4 -0.367768223 -0.56820771
    ## Q12 386 3.072539 1.0493893      3   1   5    2    4 -0.077141382 -0.78978922
    ## Q13 386 3.170984 0.9865525      3   1   5    2    4 -0.119094912 -0.72972884
    ## Q14 386 3.608808 1.1094986      4   1   5    3    4 -0.712645236 -0.24247765
    ## Q15 386 3.639896 0.9015796      4   1   5    3    4 -0.462731650 -0.19069571
    ## Q16 386 3.321244 1.1533867      4   1   5    2    4 -0.220150638 -0.97643312
    ## Q17 386 3.282383 1.0195749      3   1   5    3    4 -0.231569369 -0.62162702
    ## Q18 386 3.033679 1.0721530      3   1   5    2    4 -0.054084770 -0.69542352
    ## Q19 386 3.559585 1.0156305      4   1   5    3    4 -0.450860854 -0.37496631
    ## Q20 386 3.551813 1.1182822      4   1   5    3    4 -0.424023772 -0.65960338
    ## Q21 386 3.375648 0.9757196      3   1   5    3    4 -0.302656785 -0.31875307
    ## Q22 386 3.246114 1.0162564      3   1   5    2    4 -0.267402012 -0.84871179
    ## Q23 386 3.507772 1.0019159      4   1   5    3    4 -0.399928218 -0.44499552
    ## Q24 386 3.686528 0.8843076      4   1   5    3    4 -0.359567964 -0.45213541
    ## Q25 386 3.391192 1.0268222      4   1   5    3    4 -0.360126498 -0.66322150
    ## Q26 386 3.326425 1.0725264      3   1   5    3    4 -0.321524120 -0.67326964
    ## Q27 386 3.443005 0.9793267      4   1   5    3    4 -0.345586965 -0.35997124
    ## Q28 386 3.238342 1.1532816      3   1   5    2    4 -0.096895085 -0.86643754
    ## Q29 386 3.054404 1.1047213      3   1   5    2    4 -0.003360323 -0.87960252
    ## Q30 386 3.847150 0.9561583      4   1   5    3    5 -0.545960821 -0.37527951
    ## Q31 386 3.580311 0.9230249      4   1   5    3    4 -0.423255283 -0.38139194
    ## Q32 386 3.668394 1.0587353      4   1   5    3    4 -0.568899492 -0.51146027
    ## Q33 386 3.699482 0.8696019      4   1   5    3    4 -0.587178209  0.03500656
    ## Q34 386 3.398964 1.2322080      4   1   5    2    4 -0.374104088 -0.91755268
    ## Q35 386 3.266839 1.1436051      3   1   5    2    4 -0.262259461 -0.75298082
    ## Q36 386 3.860104 1.0475024      4   1   5    3    5 -0.666379448 -0.31718022

### Summarizing assumptions to perform analysis

``` r
df <- cbind(mvn_mod$Descriptives[,c("Mean","Median","Std.Dev")]
            , "MSAi"=as.numeric(kmo_mod$MSAi)
            , "Statistic"=as.numeric(mvn_mod$univariateNormality$Statistic)
            , mvn_mod$Descriptives[,c("Skew","Kurtosis")]
            , mvn_mod$univariateNormality[,c("p value","Normality")])
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
Mean
</th>
<th style="text-align:right;">
Median
</th>
<th style="text-align:right;">
Std.Dev
</th>
<th style="text-align:right;">
MSAi
</th>
<th style="text-align:right;">
Statistic
</th>
<th style="text-align:right;">
Skew
</th>
<th style="text-align:right;">
Kurtosis
</th>
<th style="text-align:left;">
p value
</th>
<th style="text-align:left;">
Normality
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Q1
</td>
<td style="text-align:right;">
3.982
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.823
</td>
<td style="text-align:right;">
0.896
</td>
<td style="text-align:right;">
0.810
</td>
<td style="text-align:right;">
-0.970
</td>
<td style="text-align:right;">
1.499
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q2
</td>
<td style="text-align:right;">
3.179
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.163
</td>
<td style="text-align:right;">
0.887
</td>
<td style="text-align:right;">
0.905
</td>
<td style="text-align:right;">
-0.221
</td>
<td style="text-align:right;">
-0.908
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q3
</td>
<td style="text-align:right;">
3.316
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.977
</td>
<td style="text-align:right;">
0.880
</td>
<td style="text-align:right;">
0.895
</td>
<td style="text-align:right;">
-0.163
</td>
<td style="text-align:right;">
-0.702
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q4
</td>
<td style="text-align:right;">
3.723
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.882
</td>
<td style="text-align:right;">
0.904
</td>
<td style="text-align:right;">
0.860
</td>
<td style="text-align:right;">
-0.612
</td>
<td style="text-align:right;">
0.228
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q5
</td>
<td style="text-align:right;">
3.549
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.982
</td>
<td style="text-align:right;">
0.897
</td>
<td style="text-align:right;">
0.886
</td>
<td style="text-align:right;">
-0.399
</td>
<td style="text-align:right;">
-0.481
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q6
</td>
<td style="text-align:right;">
3.132
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.223
</td>
<td style="text-align:right;">
0.832
</td>
<td style="text-align:right;">
0.895
</td>
<td style="text-align:right;">
-0.227
</td>
<td style="text-align:right;">
-1.063
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q7
</td>
<td style="text-align:right;">
3.510
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.030
</td>
<td style="text-align:right;">
0.901
</td>
<td style="text-align:right;">
0.884
</td>
<td style="text-align:right;">
-0.497
</td>
<td style="text-align:right;">
-0.412
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q8
</td>
<td style="text-align:right;">
3.073
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.202
</td>
<td style="text-align:right;">
0.677
</td>
<td style="text-align:right;">
0.913
</td>
<td style="text-align:right;">
-0.148
</td>
<td style="text-align:right;">
-0.877
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q9
</td>
<td style="text-align:right;">
3.510
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.033
</td>
<td style="text-align:right;">
0.780
</td>
<td style="text-align:right;">
0.887
</td>
<td style="text-align:right;">
-0.543
</td>
<td style="text-align:right;">
-0.205
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q10
</td>
<td style="text-align:right;">
3.808
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.998
</td>
<td style="text-align:right;">
0.884
</td>
<td style="text-align:right;">
0.871
</td>
<td style="text-align:right;">
-0.610
</td>
<td style="text-align:right;">
-0.151
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q11
</td>
<td style="text-align:right;">
3.417
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.039
</td>
<td style="text-align:right;">
0.909
</td>
<td style="text-align:right;">
0.895
</td>
<td style="text-align:right;">
-0.368
</td>
<td style="text-align:right;">
-0.568
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q12
</td>
<td style="text-align:right;">
3.073
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.049
</td>
<td style="text-align:right;">
0.831
</td>
<td style="text-align:right;">
0.906
</td>
<td style="text-align:right;">
-0.077
</td>
<td style="text-align:right;">
-0.790
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q13
</td>
<td style="text-align:right;">
3.171
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.987
</td>
<td style="text-align:right;">
0.839
</td>
<td style="text-align:right;">
0.897
</td>
<td style="text-align:right;">
-0.119
</td>
<td style="text-align:right;">
-0.730
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q14
</td>
<td style="text-align:right;">
3.609
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.109
</td>
<td style="text-align:right;">
0.914
</td>
<td style="text-align:right;">
0.864
</td>
<td style="text-align:right;">
-0.713
</td>
<td style="text-align:right;">
-0.242
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q15
</td>
<td style="text-align:right;">
3.640
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.902
</td>
<td style="text-align:right;">
0.902
</td>
<td style="text-align:right;">
0.872
</td>
<td style="text-align:right;">
-0.463
</td>
<td style="text-align:right;">
-0.191
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q16
</td>
<td style="text-align:right;">
3.321
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.153
</td>
<td style="text-align:right;">
0.791
</td>
<td style="text-align:right;">
0.897
</td>
<td style="text-align:right;">
-0.220
</td>
<td style="text-align:right;">
-0.976
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q17
</td>
<td style="text-align:right;">
3.282
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.020
</td>
<td style="text-align:right;">
0.841
</td>
<td style="text-align:right;">
0.902
</td>
<td style="text-align:right;">
-0.232
</td>
<td style="text-align:right;">
-0.622
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q18
</td>
<td style="text-align:right;">
3.034
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.072
</td>
<td style="text-align:right;">
0.795
</td>
<td style="text-align:right;">
0.914
</td>
<td style="text-align:right;">
-0.054
</td>
<td style="text-align:right;">
-0.695
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q19
</td>
<td style="text-align:right;">
3.560
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.016
</td>
<td style="text-align:right;">
0.831
</td>
<td style="text-align:right;">
0.891
</td>
<td style="text-align:right;">
-0.451
</td>
<td style="text-align:right;">
-0.375
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q20
</td>
<td style="text-align:right;">
3.552
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.118
</td>
<td style="text-align:right;">
0.813
</td>
<td style="text-align:right;">
0.893
</td>
<td style="text-align:right;">
-0.424
</td>
<td style="text-align:right;">
-0.660
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q21
</td>
<td style="text-align:right;">
3.376
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
0.976
</td>
<td style="text-align:right;">
0.881
</td>
<td style="text-align:right;">
0.899
</td>
<td style="text-align:right;">
-0.303
</td>
<td style="text-align:right;">
-0.319
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q22
</td>
<td style="text-align:right;">
3.246
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.016
</td>
<td style="text-align:right;">
0.843
</td>
<td style="text-align:right;">
0.879
</td>
<td style="text-align:right;">
-0.267
</td>
<td style="text-align:right;">
-0.849
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q23
</td>
<td style="text-align:right;">
3.508
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.002
</td>
<td style="text-align:right;">
0.881
</td>
<td style="text-align:right;">
0.892
</td>
<td style="text-align:right;">
-0.400
</td>
<td style="text-align:right;">
-0.445
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q24
</td>
<td style="text-align:right;">
3.687
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.884
</td>
<td style="text-align:right;">
0.914
</td>
<td style="text-align:right;">
0.870
</td>
<td style="text-align:right;">
-0.360
</td>
<td style="text-align:right;">
-0.452
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q25
</td>
<td style="text-align:right;">
3.391
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.027
</td>
<td style="text-align:right;">
0.885
</td>
<td style="text-align:right;">
0.887
</td>
<td style="text-align:right;">
-0.360
</td>
<td style="text-align:right;">
-0.663
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q26
</td>
<td style="text-align:right;">
3.326
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.073
</td>
<td style="text-align:right;">
0.879
</td>
<td style="text-align:right;">
0.900
</td>
<td style="text-align:right;">
-0.322
</td>
<td style="text-align:right;">
-0.673
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q27
</td>
<td style="text-align:right;">
3.443
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.979
</td>
<td style="text-align:right;">
0.914
</td>
<td style="text-align:right;">
0.896
</td>
<td style="text-align:right;">
-0.346
</td>
<td style="text-align:right;">
-0.360
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q28
</td>
<td style="text-align:right;">
3.238
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.153
</td>
<td style="text-align:right;">
0.806
</td>
<td style="text-align:right;">
0.912
</td>
<td style="text-align:right;">
-0.097
</td>
<td style="text-align:right;">
-0.866
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q29
</td>
<td style="text-align:right;">
3.054
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.105
</td>
<td style="text-align:right;">
0.643
</td>
<td style="text-align:right;">
0.909
</td>
<td style="text-align:right;">
-0.003
</td>
<td style="text-align:right;">
-0.880
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q30
</td>
<td style="text-align:right;">
3.847
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.956
</td>
<td style="text-align:right;">
0.921
</td>
<td style="text-align:right;">
0.866
</td>
<td style="text-align:right;">
-0.546
</td>
<td style="text-align:right;">
-0.375
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q31
</td>
<td style="text-align:right;">
3.580
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.923
</td>
<td style="text-align:right;">
0.872
</td>
<td style="text-align:right;">
0.874
</td>
<td style="text-align:right;">
-0.423
</td>
<td style="text-align:right;">
-0.381
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q32
</td>
<td style="text-align:right;">
3.668
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.059
</td>
<td style="text-align:right;">
0.865
</td>
<td style="text-align:right;">
0.869
</td>
<td style="text-align:right;">
-0.569
</td>
<td style="text-align:right;">
-0.511
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q33
</td>
<td style="text-align:right;">
3.699
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
0.870
</td>
<td style="text-align:right;">
0.938
</td>
<td style="text-align:right;">
0.852
</td>
<td style="text-align:right;">
-0.587
</td>
<td style="text-align:right;">
0.035
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q34
</td>
<td style="text-align:right;">
3.399
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.232
</td>
<td style="text-align:right;">
0.803
</td>
<td style="text-align:right;">
0.893
</td>
<td style="text-align:right;">
-0.374
</td>
<td style="text-align:right;">
-0.918
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q35
</td>
<td style="text-align:right;">
3.267
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
1.144
</td>
<td style="text-align:right;">
0.774
</td>
<td style="text-align:right;">
0.910
</td>
<td style="text-align:right;">
-0.262
</td>
<td style="text-align:right;">
-0.753
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
<tr>
<td style="text-align:left;">
Q36
</td>
<td style="text-align:right;">
3.860
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
1.048
</td>
<td style="text-align:right;">
0.860
</td>
<td style="text-align:right;">
0.860
</td>
<td style="text-align:right;">
-0.666
</td>
<td style="text-align:right;">
-0.317
</td>
<td style="text-align:left;">
&lt;0.001
</td>
<td style="text-align:left;">
NO
</td>
</tr>
</tbody>
</table>

# Exploratory Factorial Analysis (EFA)

Performing parallel factorial analysis

``` r
if (!file.exists("pfa_mod.RData")) {
  pfa_mod <- fa.parallel(fss, fm = 'wls', fa = 'fa', cor='poly', plot = F)
  save(pfa_mod, file = "pfa_mod.RData")
}
load("pfa_mod.RData")
(pfa_mod)
```

    ## Call: fa.parallel(x = fss, fm = "wls", fa = "fa", cor = "poly", plot = F)
    ## Parallel analysis suggests that the number of factors =  8  and the number of components =  NA 
    ## 
    ##  Eigen Values of 
    ## 
    ##  eigen values of factors
    ##  [1]  9.76  3.39  1.91  1.10  0.91  0.73  0.46  0.34  0.30  0.20  0.11  0.05
    ## [13] -0.03 -0.09 -0.10 -0.14 -0.17 -0.22 -0.24 -0.28 -0.30 -0.32 -0.36 -0.38
    ## [25] -0.40 -0.44 -0.47 -0.51 -0.55 -0.56 -0.58 -0.62 -0.64 -0.66 -0.69 -0.73
    ## 
    ##  eigen values of simulated factors
    ##  [1]  0.70  0.57  0.51  0.47  0.42  0.37  0.34  0.30  0.27  0.23  0.20  0.17
    ## [13]  0.14  0.10  0.08  0.05  0.02  0.00 -0.03 -0.05 -0.07 -0.10 -0.12 -0.15
    ## [25] -0.17 -0.20 -0.22 -0.24 -0.27 -0.29 -0.32 -0.34 -0.37 -0.40 -0.43 -0.47
    ## 
    ##  eigen values of components 
    ##  [1] 10.42  4.22  2.72  1.89  1.63  1.45  1.15  1.03  0.96  0.89  0.85  0.80
    ## [13]  0.67  0.62  0.60  0.57  0.52  0.49  0.44  0.41  0.39  0.38  0.33  0.32
    ## [25]  0.29  0.28  0.25  0.24  0.22  0.19  0.18  0.16  0.14  0.12  0.11  0.09
    ## 
    ##  eigen values of simulated components
    ## [1] NA

``` r
plot(pfa_mod)
```

![](script20210331_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Running EFA with the factors sugested by the parallel factorial
analysis.

``` r
efa_mod <- fa(fss, nfactors = pfa_mod$nfact, cor = 'poly', fm = 'wls')
print(loadings(fa.sort(efa_mod)), cutoff = 0.3)
```

    ## 
    ## Loadings:
    ##     WLS2   WLS1   WLS8   WLS4   WLS3   WLS5   WLS6   WLS7  
    ## Q22  0.829                                                 
    ## Q13  0.743                                                 
    ## Q12  0.723                                                 
    ## Q21  0.609                                                 
    ## Q3   0.549                                                 
    ## Q6   0.467                                                 
    ## Q31  0.349         0.319                                   
    ## Q23         0.911                                          
    ## Q5          0.651                                          
    ## Q24         0.596                                          
    ## Q33         0.555                                          
    ## Q32         0.532                                          
    ## Q4          0.505                                          
    ## Q15  0.313  0.364                                          
    ## Q10                0.674                                   
    ## Q36                0.665                                   
    ## Q30                0.665                                   
    ## Q1   0.312         0.332                                   
    ## Q28                       0.715                            
    ## Q18                       0.694                            
    ## Q8                        0.688                            
    ## Q34                       0.678         0.444              
    ## Q9                               0.715                     
    ## Q19                              0.654                     
    ## Q35                              0.640                     
    ## Q16                                     0.930              
    ## Q7                                      0.508              
    ## Q25                                     0.476              
    ## Q20                0.350                       0.661       
    ## Q29                              0.550         0.612       
    ## Q2                                             0.574       
    ## Q11                                            0.455  0.365
    ## Q14                                            0.326       
    ## Q17                                                   0.807
    ## Q26                                                   0.450
    ## Q27                                                   0.372
    ## 
    ##                 WLS2  WLS1  WLS8  WLS4  WLS3  WLS5  WLS6  WLS7
    ## SS loadings    3.423 3.135 2.225 2.136 1.933 1.853 1.731 1.557
    ## Proportion Var 0.095 0.087 0.062 0.059 0.054 0.051 0.048 0.043
    ## Cumulative Var 0.095 0.182 0.244 0.303 0.357 0.408 0.457 0.500

Running EFA with nine (09) factors sugested by the parallel factorial
analysis.

``` r
efa_mod <- fa(fss, nfactors = 9, cor = 'poly', fm = 'wls')
print(loadings(fa.sort(efa_mod)), cutoff = 0.3)
```

    ## 
    ## Loadings:
    ##     WLS2   WLS1   WLS4   WLS8   WLS3   WLS5   WLS6   WLS7   WLS9  
    ## Q22  0.868                                                        
    ## Q13  0.711                                                        
    ## Q12  0.695                                                        
    ## Q21  0.592                                                        
    ## Q3   0.582                                                        
    ## Q6   0.477                                                        
    ## Q1   0.351                0.322                                   
    ## Q23         0.885                                                 
    ## Q32         0.685                                                 
    ## Q5          0.592                                                 
    ## Q33         0.413                                                 
    ## Q24         0.411                                            0.402
    ## Q4          0.353                                            0.305
    ## Q28                0.707                                          
    ## Q18                0.693                                          
    ## Q34                0.685                0.412                     
    ## Q8                 0.685                                          
    ## Q36                       0.656                                   
    ## Q10                       0.635                                   
    ## Q30                       0.632                                   
    ## Q31  0.314                0.317                                   
    ## Q19                              0.715                            
    ## Q9                               0.705                            
    ## Q35                              0.628                            
    ## Q16                                     0.932                     
    ## Q7                                      0.462                     
    ## Q25                                     0.431                0.350
    ## Q20                       0.329                0.692              
    ## Q29                              0.470         0.674              
    ## Q2                                             0.571              
    ## Q11                                            0.467  0.342       
    ## Q17                                                   0.840       
    ## Q26                                                   0.441       
    ## Q27                                                   0.357       
    ## Q14                                                          0.512
    ## Q15                                                          0.443
    ## 
    ##                 WLS2  WLS1  WLS4  WLS8  WLS3  WLS5  WLS6  WLS7  WLS9
    ## SS loadings    3.345 2.436 2.134 2.061 1.910 1.729 1.726 1.604 1.293
    ## Proportion Var 0.093 0.068 0.059 0.057 0.053 0.048 0.048 0.045 0.036
    ## Cumulative Var 0.093 0.161 0.220 0.277 0.330 0.378 0.426 0.471 0.507

# Construct validity

As there is not normality in the results, we decided to use the mean-
and variance-adjusted weighted least squares (WLSMV) estimator, based on
that WLSMV does not assume normal variables and it is a robust estimator
developed to model categorical or ordered data (our case 5-likert
scale).

-   Brown, T. (2006). Confirmatory factor analysis for applied research.
    New York: Guildford.
-   Proitsi, P., et al. (2009). A multiple indicators multiple causes
    (MIMIC) model of behavioural and psychological symptoms in dementia
    (BPSD). Neurobiology Aging.
    <doi:10.1016/j.neurobiolaging.2009.03.005>

## Multicorrelated models

### Multicorrelated nine factor original model

``` r
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
```

    ## lavaan 0.6-7 ended normally after 47 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                        108
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              2354.280    2051.033
    ##   Degrees of freedom                               558         558
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.371
    ##   Shift parameter                                          333.788
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             13760.031    3821.873
    ##   Degrees of freedom                               630         630
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  4.114
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.863       0.532
    ##   Tucker-Lewis Index (TLI)                       0.846       0.472
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.091       0.083
    ##   90 Percent confidence interval - lower         0.088       0.080
    ##   90 Percent confidence interval - upper         0.095       0.087
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.104       0.104
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.427    0.046    9.284    0.000
    ##     Q10               0.632    0.051   12.355    0.000
    ##     Q19               0.455    0.051    8.977    0.000
    ##     Q28               0.521    0.059    8.833    0.000
    ##   MAA =~                                              
    ##     Q2                0.807    0.058   13.881    0.000
    ##     Q11               0.816    0.062   13.215    0.000
    ##     Q20               0.706    0.060   11.764    0.000
    ##     Q29               0.379    0.077    4.903    0.000
    ##   CG =~                                               
    ##     Q3                0.425    0.058    7.390    0.000
    ##     Q12               0.214    0.065    3.309    0.001
    ##     Q21               0.566    0.053   10.650    0.000
    ##     Q30               0.632    0.053   11.977    0.000
    ##   UF =~                                               
    ##     Q4                0.720    0.052   13.842    0.000
    ##     Q13               0.373    0.061    6.095    0.000
    ##     Q22               0.438    0.062    7.106    0.000
    ##     Q31               0.594    0.052   11.493    0.000
    ##   CTH =~                                              
    ##     Q5                0.590    0.051   11.679    0.000
    ##     Q14               0.602    0.054   11.112    0.000
    ##     Q23               0.667    0.048   13.985    0.000
    ##     Q32               0.680    0.053   12.875    0.000
    ##   SC =~                                               
    ##     Q6                0.246    0.069    3.582    0.000
    ##     Q15               0.594    0.045   13.193    0.000
    ##     Q24               0.668    0.040   16.708    0.000
    ##     Q33               0.593    0.042   14.058    0.000
    ##   LSC =~                                              
    ##     Q7                0.805    0.057   14.118    0.000
    ##     Q16               0.797    0.063   12.693    0.000
    ##     Q25               0.736    0.053   13.894    0.000
    ##     Q34               0.678    0.069    9.754    0.000
    ##   TT =~                                               
    ##     Q8                0.444    0.076    5.863    0.000
    ##     Q17               0.712    0.059   12.138    0.000
    ##     Q26               0.819    0.064   12.889    0.000
    ##     Q35               0.496    0.069    7.196    0.000
    ##   AE =~                                               
    ##     Q9                0.389    0.062    6.266    0.000
    ##     Q18               0.466    0.065    7.223    0.000
    ##     Q27               0.797    0.048   16.619    0.000
    ##     Q36               0.691    0.054   12.688    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB ~~                                              
    ##     MAA               0.509    0.088    5.777    0.000
    ##     CG                0.909    0.069   13.249    0.000
    ##     UF                0.651    0.070    9.316    0.000
    ##     CTH               0.744    0.069   10.751    0.000
    ##     SC                0.786    0.060   13.109    0.000
    ##     LSC               0.619    0.073    8.518    0.000
    ##     TT                0.606    0.075    8.073    0.000
    ##     AE                0.879    0.064   13.707    0.000
    ##   MAA ~~                                              
    ##     CG                0.463    0.088    5.284    0.000
    ##     UF                0.241    0.080    3.016    0.003
    ##     CTH               0.562    0.074    7.644    0.000
    ##     SC                0.423    0.078    5.427    0.000
    ##     LSC               0.413    0.070    5.930    0.000
    ##     TT                0.644    0.060   10.796    0.000
    ##     AE                0.645    0.066    9.843    0.000
    ##   CG ~~                                               
    ##     UF                0.931    0.055   17.006    0.000
    ##     CTH               0.728    0.081    9.043    0.000
    ##     SC                0.948    0.055   17.225    0.000
    ##     LSC               0.584    0.081    7.224    0.000
    ##     TT                0.470    0.097    4.844    0.000
    ##     AE                0.582    0.073    7.984    0.000
    ##   UF ~~                                               
    ##     CTH               0.600    0.078    7.695    0.000
    ##     SC                0.832    0.046   17.923    0.000
    ##     LSC               0.451    0.075    5.971    0.000
    ##     TT                0.217    0.082    2.632    0.008
    ##     AE                0.358    0.079    4.552    0.000
    ##   CTH ~~                                              
    ##     SC                0.951    0.048   19.841    0.000
    ##     LSC               0.694    0.062   11.215    0.000
    ##     TT                0.658    0.071    9.228    0.000
    ##     AE                0.677    0.064   10.513    0.000
    ##   SC ~~                                               
    ##     LSC               0.586    0.066    8.901    0.000
    ##     TT                0.513    0.080    6.446    0.000
    ##     AE                0.577    0.061    9.485    0.000
    ##   LSC ~~                                              
    ##     TT                0.447    0.080    5.617    0.000
    ##     AE                0.545    0.070    7.775    0.000
    ##   TT ~~                                               
    ##     AE                0.860    0.054   16.033    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.496    0.056    8.853    0.000
    ##    .Q10               0.597    0.064    9.357    0.000
    ##    .Q19               0.824    0.065   12.611    0.000
    ##    .Q28               1.058    0.073   14.403    0.000
    ##    .Q2                0.702    0.085    8.276    0.000
    ##    .Q11               0.414    0.081    5.117    0.000
    ##    .Q20               0.752    0.088    8.582    0.000
    ##    .Q29               1.077    0.074   14.533    0.000
    ##    .Q3                0.773    0.048   16.082    0.000
    ##    .Q12               1.056    0.061   17.187    0.000
    ##    .Q21               0.632    0.052   12.193    0.000
    ##    .Q30               0.515    0.068    7.548    0.000
    ##    .Q4                0.258    0.057    4.539    0.000
    ##    .Q13               0.834    0.057   14.626    0.000
    ##    .Q22               0.841    0.061   13.735    0.000
    ##    .Q31               0.499    0.056    8.966    0.000
    ##    .Q5                0.617    0.054   11.497    0.000
    ##    .Q14               0.869    0.080   10.913    0.000
    ##    .Q23               0.559    0.051   10.932    0.000
    ##    .Q32               0.659    0.062   10.584    0.000
    ##    .Q6                1.436    0.078   18.503    0.000
    ##    .Q15               0.460    0.043   10.673    0.000
    ##    .Q24               0.336    0.040    8.333    0.000
    ##    .Q33               0.405    0.037   10.803    0.000
    ##    .Q7                0.413    0.074    5.575    0.000
    ##    .Q16               0.696    0.085    8.210    0.000
    ##    .Q25               0.512    0.061    8.442    0.000
    ##    .Q34               1.059    0.086   12.352    0.000
    ##    .Q8                1.247    0.076   16.311    0.000
    ##    .Q17               0.533    0.066    8.077    0.000
    ##    .Q26               0.480    0.088    5.428    0.000
    ##    .Q35               1.062    0.082   12.882    0.000
    ##    .Q9                0.915    0.075   12.228    0.000
    ##    .Q18               0.932    0.066   14.117    0.000
    ##    .Q27               0.325    0.050    6.436    0.000
    ##    .Q36               0.620    0.062    9.926    0.000
    ##     CSB               1.000                           
    ##     MAA               1.000                           
    ##     CG                1.000                           
    ##     UF                1.000                           
    ##     CTH               1.000                           
    ##     SC                1.000                           
    ##     LSC               1.000                           
    ##     TT                1.000                           
    ##     AE                1.000

*Observations (**fit indexes**)*:

-   CFI and TLI do not achieve 0.90s values.

``` r
reliability(fit1a, return.total = TRUE)
```

    ##              CSB       MAA        CG        UF       CTH        SC       LSC
    ## alpha  0.5803449 0.7191396 0.5892662 0.6989406 0.6944780 0.6032188 0.7758457
    ## omega  0.5821001 0.7134549 0.5312373 0.6502134 0.7044913 0.6258340 0.7723475
    ## omega2 0.5821001 0.7134549 0.5312373 0.6502134 0.7044913 0.6258340 0.7723475
    ## omega3 0.5798357 0.6887733 0.4799315 0.5916500 0.7146978 0.6277596 0.7659038
    ## avevar 0.2628942 0.3994678 0.2411247 0.3310513 0.3743179 0.3145961 0.4600699
    ##               TT        AE     total
    ## alpha  0.6512167 0.6610580 0.9032067
    ## omega  0.6476554 0.6626880 0.9196391
    ## omega2 0.6476554 0.6626880 0.9196391
    ## omega3 0.6320262 0.6473995 0.9143113
    ## avevar 0.3278914 0.3464392 0.3444874

*Observations (**reliability**)*:

-   less than 0.60 in CSB and CG.

``` r
convergentDiscriminantValidity(fit1a, Modelo1a, fss)
```

    ##            CR       AVE      VIF    VIF.i       CSB       MAA        CG
    ## CSB 0.5901002 0.2628942 2.051490 1.338167 0.5127321 0.5491145 0.8490849
    ## MAA 0.7161025 0.3994678 1.403274 1.672072 0.5091070 0.6320347 0.4901649
    ## CG  0.5410786 0.2411247 2.178275 1.430092 0.9090157 0.4631680 0.4910445
    ## UF  0.6656885 0.3310513 1.989352 1.766786 0.6506752 0.2411204 0.9307093
    ## CTH 0.7069982 0.3743179 2.156430 1.767734 0.7437842 0.5623354 0.7283452
    ## SC  0.6792292 0.3145961 2.462381 1.609705 0.7856544 0.4228389 0.9483835
    ## LSC 0.7816508 0.4600699 1.506810 1.664968 0.6192135 0.4127502 0.5836254
    ## TT  0.6632146 0.3278914 1.743574 1.480520 0.6060000 0.6442573 0.4704864
    ## AE  0.6696859 0.3464392 2.101808 1.562181 0.8787177 0.6454494 0.5820791
    ##            UF       CTH        SC       LSC        TT        AE
    ## CSB 0.6601975 0.7457629 0.8317423 0.6424276 0.6524711 0.9560090
    ## MAA 0.3343935 0.5689242 0.4697885 0.4060196 0.6660926 0.6195077
    ## CG  1.0053516 0.6796351 0.9925814 0.5158094 0.4289835 0.5760505
    ## UF  0.5753706 0.5695896 0.8835910 0.4080997 0.2591158 0.4273592
    ## CTH 0.6002346 0.6118152 1.0042116 0.7059522 0.6335204 0.6476611
    ## SC  0.8321718 0.9513076 0.5608887 0.5937076 0.5387951 0.6500179
    ## LSC 0.4508071 0.6943127 0.5859139 0.6782845 0.4469299 0.5472759
    ## TT  0.2171178 0.6584487 0.5130844 0.4467818 0.5726180 0.9011199
    ## AE  0.3584276 0.6767690 0.5774101 0.5447653 0.8602279 0.5885909

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability less than 0.60 in CSB and CG
-   Based on the HTMT0.85 rule dicriminant are inadequate for CSB\~AE
    CG\~UF CG\~SC UF\~SC CTH\~SC TT\~AE.

### Multicorrelated nine factor model - fixed convergence and discriminant

Based on the EFA, the following values should be observed:

-   In the `CSB`, the items `Q1+Q10` or `Q19` or `Q28`
-   In the `TT`, the items `Q26+Q17` or `Q35` or `Q8`
-   In the `AE`, the items `Q27`. or `Q36` or `Q18` or `Q9`

Based on the HTMT0.85, the convergent and discriminant validity suggest
combine:

-   (challenge-skill balance) CSB \~ AE (autotelic experience)
-   (clear goal) CG \~ UF (unambiguous feedback)
-   (transformation of time) TT \~ AE (autotelic experience)

``` r
Modelo1b <- '
CSB =~ Q1 + Q10 + Q19
MAA =~ Q2 + Q11 + Q20
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
```

    ## lavaan 0.6-7 ended normally after 46 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         92
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              1076.323    1221.604
    ##   Degrees of freedom                               314         314
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.013
    ##   Shift parameter                                          159.391
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              8973.411    2707.611
    ##   Degrees of freedom                               378         378
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  3.690
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.911       0.610
    ##   Tucker-Lewis Index (TLI)                       0.893       0.531
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.079       0.087
    ##   90 Percent confidence interval - lower         0.074       0.082
    ##   90 Percent confidence interval - upper         0.085       0.092
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.091       0.091
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.488    0.050    9.666    0.000
    ##     Q10               0.655    0.058   11.218    0.000
    ##     Q19               0.478    0.057    8.351    0.000
    ##   MAA =~                                              
    ##     Q2                0.783    0.058   13.441    0.000
    ##     Q11               0.837    0.063   13.385    0.000
    ##     Q20               0.643    0.064   10.002    0.000
    ##   CG =~                                               
    ##     Q3                0.608    0.060   10.100    0.000
    ##     Q12               0.407    0.068    6.034    0.000
    ##     Q21               0.753    0.056   13.333    0.000
    ##   UF =~                                               
    ##     Q4                0.689    0.050   13.767    0.000
    ##     Q13               0.439    0.057    7.729    0.000
    ##     Q22               0.500    0.057    8.800    0.000
    ##     Q31               0.567    0.050   11.415    0.000
    ##   CTH =~                                              
    ##     Q5                0.692    0.051   13.500    0.000
    ##     Q23               0.755    0.048   15.620    0.000
    ##     Q32               0.751    0.055   13.759    0.000
    ##   SC =~                                               
    ##     Q15               0.597    0.045   13.188    0.000
    ##     Q24               0.665    0.041   16.351    0.000
    ##     Q33               0.603    0.042   14.300    0.000
    ##   LSC =~                                              
    ##     Q7                0.794    0.057   13.971    0.000
    ##     Q16               0.773    0.062   12.432    0.000
    ##     Q25               0.710    0.054   13.151    0.000
    ##   TT =~                                               
    ##     Q8                0.398    0.077    5.193    0.000
    ##     Q17               0.732    0.058   12.597    0.000
    ##     Q26               0.809    0.063   12.846    0.000
    ##   AE =~                                               
    ##     Q9                0.379    0.066    5.765    0.000
    ##     Q18               0.480    0.066    7.291    0.000
    ##     Q27               0.885    0.063   13.957    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB ~~                                              
    ##     MAA               0.473    0.082    5.758    0.000
    ##     CG                0.620    0.069    9.046    0.000
    ##     UF                0.627    0.082    7.681    0.000
    ##     CTH               0.613    0.074    8.308    0.000
    ##     SC                0.738    0.075    9.860    0.000
    ##     LSC               0.584    0.074    7.914    0.000
    ##     TT                0.484    0.079    6.121    0.000
    ##     AE                0.667    0.074    9.050    0.000
    ##   MAA ~~                                              
    ##     CG                0.160    0.090    1.782    0.075
    ##     UF                0.243    0.080    3.059    0.002
    ##     CTH               0.463    0.072    6.403    0.000
    ##     SC                0.502    0.072    6.957    0.000
    ##     LSC               0.459    0.068    6.785    0.000
    ##     TT                0.663    0.060   11.057    0.000
    ##     AE                0.505    0.071    7.163    0.000
    ##   CG ~~                                               
    ##     UF                0.912    0.043   21.149    0.000
    ##     CTH               0.471    0.073    6.444    0.000
    ##     SC                0.666    0.055   12.051    0.000
    ##     LSC               0.383    0.076    5.010    0.000
    ##     TT                0.174    0.090    1.927    0.054
    ##     AE                0.356    0.076    4.707    0.000
    ##   UF ~~                                               
    ##     CTH               0.580    0.072    8.027    0.000
    ##     SC                0.771    0.048   16.220    0.000
    ##     LSC               0.467    0.076    6.162    0.000
    ##     TT                0.217    0.084    2.594    0.009
    ##     AE                0.390    0.077    5.032    0.000
    ##   CTH ~~                                              
    ##     SC                0.840    0.051   16.563    0.000
    ##     LSC               0.604    0.069    8.759    0.000
    ##     TT                0.548    0.074    7.425    0.000
    ##     AE                0.564    0.066    8.564    0.000
    ##   SC ~~                                               
    ##     LSC               0.617    0.065    9.555    0.000
    ##     TT                0.592    0.075    7.855    0.000
    ##     AE                0.569    0.063    9.006    0.000
    ##   LSC ~~                                              
    ##     TT                0.495    0.079    6.238    0.000
    ##     AE                0.519    0.072    7.248    0.000
    ##   TT ~~                                               
    ##     AE                0.757    0.066   11.543    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.439    0.055    7.990    0.000
    ##    .Q10               0.568    0.066    8.642    0.000
    ##    .Q19               0.803    0.069   11.597    0.000
    ##    .Q2                0.740    0.087    8.513    0.000
    ##    .Q11               0.380    0.084    4.505    0.000
    ##    .Q20               0.838    0.092    9.074    0.000
    ##    .Q3                0.585    0.063    9.272    0.000
    ##    .Q12               0.935    0.068   13.679    0.000
    ##    .Q21               0.385    0.079    4.904    0.000
    ##    .Q4                0.302    0.050    6.048    0.000
    ##    .Q13               0.780    0.057   13.723    0.000
    ##    .Q22               0.783    0.061   12.930    0.000
    ##    .Q31               0.530    0.052   10.122    0.000
    ##    .Q5                0.487    0.055    8.920    0.000
    ##    .Q23               0.434    0.054    7.963    0.000
    ##    .Q32               0.556    0.065    8.579    0.000
    ##    .Q15               0.456    0.043   10.551    0.000
    ##    .Q24               0.340    0.039    8.659    0.000
    ##    .Q33               0.393    0.036   10.920    0.000
    ##    .Q7                0.430    0.074    5.769    0.000
    ##    .Q16               0.733    0.081    9.033    0.000
    ##    .Q25               0.550    0.063    8.733    0.000
    ##    .Q8                1.286    0.076   16.942    0.000
    ##    .Q17               0.503    0.068    7.389    0.000
    ##    .Q26               0.496    0.086    5.799    0.000
    ##    .Q9                0.922    0.077   11.907    0.000
    ##    .Q18               0.919    0.067   13.635    0.000
    ##    .Q27               0.177    0.098    1.808    0.071
    ##     CSB               1.000                           
    ##     MAA               1.000                           
    ##     CG                1.000                           
    ##     UF                1.000                           
    ##     CTH               1.000                           
    ##     SC                1.000                           
    ##     LSC               1.000                           
    ##     TT                1.000                           
    ##     AE                1.000

*Observations (**fit indexes**)*:

-   TLI does not achieve 0.90s value but its close with 0.885

``` r
reliability(fit1b, return.total = TRUE)
```

    ##              CSB       MAA        CG        UF       CTH        SC       LSC
    ## alpha  0.5934344 0.7306380 0.6339465 0.6989406 0.7647409 0.7439148 0.7521790
    ## omega  0.5921464 0.7234211 0.6214540 0.6682003 0.7659077 0.7450985 0.7517971
    ## omega2 0.5921464 0.7234211 0.6214540 0.6682003 0.7659077 0.7450985 0.7517971
    ## omega3 0.5869910 0.7127863 0.6003448 0.6312779 0.7664951 0.7453787 0.7505713
    ## avevar 0.3310418 0.4686906 0.3666314 0.3411745 0.5220627 0.4941100 0.5029596
    ##               TT        AE     total
    ## alpha  0.6174260 0.5963732 0.8897113
    ## omega  0.6221922 0.6011071 0.9175248
    ## omega2 0.6221922 0.6011071 0.9175248
    ## omega3 0.6091411 0.5770272 0.9214102
    ## avevar 0.3713566 0.3643717 0.4169921

*Observations (**reliability**)*:

-   less than 0.60 in CSB and AE (but close to 0.60 with 0.59)

``` r
convergentDiscriminantValidity(fit1b, Modelo1b, fss)
```

    ##            CR       AVE      VIF    VIF.i       CSB       MAA        CG
    ## CSB 0.5969688 0.3310418 1.635697 1.333860 0.5753623 0.4946320 0.6493433
    ## MAA 0.7288512 0.4686906 1.407987 1.630312 0.4727944 0.6846098 0.3436731
    ## CG  0.6299260 0.3666314 1.854826 1.281195 0.6200875 0.1600494 0.6055009
    ## UF  0.6794267 0.3411745 2.031633 1.766786 0.6269556 0.2432576 0.9124344
    ## CTH 0.7662908 0.5220627 1.854397 1.676972 0.6127303 0.4634876 0.4714079
    ## SC  0.7454672 0.4941100 2.302430 1.605942 0.7380838 0.5021404 0.6660019
    ## LSC 0.7548531 0.5029596 1.467206 1.587952 0.5838918 0.4591864 0.3826359
    ## TT  0.6432182 0.3713566 1.607296 1.458711 0.4844892 0.6627197 0.1741785
    ## AE  0.6149333 0.3643717 1.601983 1.304209 0.6670422 0.5054062 0.3556584
    ##            UF       CTH        SC       LSC        TT        AE
    ## CSB 0.6374709 0.6123117 0.7368030 0.5910707 0.4631082 0.7804386
    ## MAA 0.3639031 0.4504706 0.4936054 0.4734595 0.6683290 0.4397118
    ## CG  0.9778317 0.4711559 0.6482011 0.3658158 0.2717332 0.4346909
    ## UF  0.5841015 0.5378309 0.7320650 0.4279098 0.2995317 0.4253087
    ## CTH 0.5795225 0.7225390 0.8448819 0.6054889 0.5308638 0.5362922
    ## SC  0.7711158 0.8401051 0.7029296 0.6193045 0.5719674 0.5401687
    ## LSC 0.4667968 0.6035038 0.6173299 0.7091965 0.4811568 0.5048797
    ## TT  0.2170643 0.5482942 0.5918070 0.4948569 0.6093903 0.8042737
    ## AE  0.3896863 0.5641950 0.5694506 0.5186075 0.7567360 0.6036321

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability of CSB is 0.597 (close to 0.60s)
-   Based on the HTMT0.85 rule dicriminant are inadequate only for
    CG\~UF

### Multicorrelated eight factor model - merging CG\~UF

``` r
Modelo1c <- '
CSB =~    Q1 + Q10 + Q19
MAA =~    Q2 + Q11 + Q20
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
```

    ## lavaan 0.6-7 ended normally after 44 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         84
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              1082.616    1209.290
    ##   Degrees of freedom                               322         322
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.036
    ##   Shift parameter                                          164.495
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              8973.411    2707.611
    ##   Degrees of freedom                               378         378
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  3.690
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.912       0.619
    ##   Tucker-Lewis Index (TLI)                       0.896       0.553
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.078       0.085
    ##   90 Percent confidence interval - lower         0.073       0.080
    ##   90 Percent confidence interval - upper         0.083       0.090
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.091       0.091
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.488    0.050    9.662    0.000
    ##     Q10               0.655    0.058   11.224    0.000
    ##     Q19               0.478    0.057    8.348    0.000
    ##   MAA =~                                              
    ##     Q2                0.783    0.058   13.428    0.000
    ##     Q11               0.838    0.063   13.401    0.000
    ##     Q20               0.641    0.064    9.969    0.000
    ##   CG_UF =~                                            
    ##     Q3                0.562    0.057    9.809    0.000
    ##     Q12               0.370    0.066    5.566    0.000
    ##     Q21               0.695    0.054   12.896    0.000
    ##     Q4                0.693    0.049   14.219    0.000
    ##     Q13               0.452    0.057    7.978    0.000
    ##     Q22               0.514    0.056    9.201    0.000
    ##     Q31               0.571    0.051   11.136    0.000
    ##   CTH =~                                              
    ##     Q5                0.692    0.051   13.488    0.000
    ##     Q23               0.755    0.048   15.610    0.000
    ##     Q32               0.751    0.055   13.748    0.000
    ##   SC =~                                               
    ##     Q15               0.597    0.045   13.189    0.000
    ##     Q24               0.665    0.041   16.344    0.000
    ##     Q33               0.603    0.042   14.298    0.000
    ##   LSC =~                                              
    ##     Q7                0.795    0.057   13.962    0.000
    ##     Q16               0.773    0.062   12.440    0.000
    ##     Q25               0.710    0.054   13.135    0.000
    ##   TT =~                                               
    ##     Q8                0.398    0.077    5.195    0.000
    ##     Q17               0.733    0.058   12.601    0.000
    ##     Q26               0.809    0.063   12.847    0.000
    ##   AE =~                                               
    ##     Q9                0.379    0.066    5.769    0.000
    ##     Q18               0.480    0.066    7.295    0.000
    ##     Q27               0.884    0.063   13.959    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB ~~                                              
    ##     MAA               0.473    0.082    5.755    0.000
    ##     CG_UF             0.640    0.065    9.923    0.000
    ##     CTH               0.613    0.074    8.308    0.000
    ##     SC                0.738    0.075    9.861    0.000
    ##     LSC               0.584    0.074    7.914    0.000
    ##     TT                0.485    0.079    6.121    0.000
    ##     AE                0.667    0.074    9.053    0.000
    ##   MAA ~~                                              
    ##     CG_UF             0.215    0.078    2.752    0.006
    ##     CTH               0.464    0.072    6.405    0.000
    ##     SC                0.502    0.072    6.958    0.000
    ##     LSC               0.459    0.068    6.782    0.000
    ##     TT                0.663    0.060   11.056    0.000
    ##     AE                0.506    0.071    7.164    0.000
    ##   CG_UF ~~                                            
    ##     CTH               0.549    0.066    8.353    0.000
    ##     SC                0.745    0.041   18.023    0.000
    ##     LSC               0.444    0.072    6.187    0.000
    ##     TT                0.204    0.081    2.507    0.012
    ##     AE                0.385    0.069    5.576    0.000
    ##   CTH ~~                                              
    ##     SC                0.840    0.051   16.562    0.000
    ##     LSC               0.603    0.069    8.759    0.000
    ##     TT                0.548    0.074    7.425    0.000
    ##     AE                0.564    0.066    8.565    0.000
    ##   SC ~~                                               
    ##     LSC               0.617    0.065    9.555    0.000
    ##     TT                0.592    0.075    7.855    0.000
    ##     AE                0.570    0.063    9.007    0.000
    ##   LSC ~~                                              
    ##     TT                0.495    0.079    6.238    0.000
    ##     AE                0.519    0.072    7.248    0.000
    ##   TT ~~                                               
    ##     AE                0.757    0.066   11.546    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.440    0.055    7.985    0.000
    ##    .Q10               0.568    0.066    8.641    0.000
    ##    .Q19               0.803    0.069   11.590    0.000
    ##    .Q2                0.740    0.087    8.506    0.000
    ##    .Q11               0.377    0.085    4.466    0.000
    ##    .Q20               0.840    0.092    9.093    0.000
    ##    .Q3                0.638    0.053   11.944    0.000
    ##    .Q12               0.964    0.066   14.552    0.000
    ##    .Q21               0.469    0.067    7.011    0.000
    ##    .Q4                0.297    0.049    6.125    0.000
    ##    .Q13               0.769    0.057   13.430    0.000
    ##    .Q22               0.769    0.060   12.894    0.000
    ##    .Q31               0.526    0.055    9.615    0.000
    ##    .Q5                0.487    0.055    8.912    0.000
    ##    .Q23               0.433    0.055    7.948    0.000
    ##    .Q32               0.556    0.065    8.578    0.000
    ##    .Q15               0.456    0.043   10.558    0.000
    ##    .Q24               0.340    0.039    8.648    0.000
    ##    .Q33               0.393    0.036   10.920    0.000
    ##    .Q7                0.429    0.075    5.755    0.000
    ##    .Q16               0.732    0.081    9.029    0.000
    ##    .Q25               0.551    0.063    8.736    0.000
    ##    .Q8                1.285    0.076   16.941    0.000
    ##    .Q17               0.503    0.068    7.386    0.000
    ##    .Q26               0.496    0.085    5.805    0.000
    ##    .Q9                0.922    0.077   11.906    0.000
    ##    .Q18               0.919    0.067   13.632    0.000
    ##    .Q27               0.177    0.098    1.816    0.069
    ##     CSB               1.000                           
    ##     MAA               1.000                           
    ##     CG_UF             1.000                           
    ##     CTH               1.000                           
    ##     SC                1.000                           
    ##     LSC               1.000                           
    ##     TT                1.000                           
    ##     AE                1.000

*Observations (**fit indexes**)*:

-   TLI does not achieve 0.90s value but its close with 0.896

``` r
reliability(fit1c, return.total = TRUE)
```

    ##              CSB       MAA     CG_UF       CTH        SC       LSC        TT
    ## alpha  0.5934344 0.7306380 0.7995720 0.7647409 0.7439148 0.7521790 0.6174260
    ## omega  0.5922181 0.7233552 0.7704575 0.7659088 0.7451006 0.7518178 0.6221968
    ## omega2 0.5922181 0.7233552 0.7704575 0.7659088 0.7451006 0.7518178 0.6221968
    ## omega3 0.5870938 0.7125473 0.7046656 0.7664958 0.7453810 0.7506008 0.6091662
    ## avevar 0.3311229 0.4686928 0.3327851 0.5220651 0.4941137 0.5029953 0.3713442
    ##               AE     total
    ## alpha  0.5963732 0.8897113
    ## omega  0.6010737 0.9169198
    ## omega2 0.6010737 0.9169198
    ## omega3 0.5770334 0.9213168
    ## avevar 0.3642763 0.4123875

\*Observations (\*\*reliability*)*:

-   less than 0.60 in CSB and AE (but close to 0.60s with 0.59)

``` r
convergentDiscriminantValidity(fit1c, Modelo1c, fss)
```

    ##              CR       AVE      VIF    VIF.i       CSB       MAA     CG_UF
    ## CSB   0.5969937 0.3311229 1.634652 1.333860 0.5754328 0.4946320 0.6466831
    ## MAA   0.7288411 0.4686928 1.406340 1.630312 0.4725084 0.6846114 0.3574851
    ## CG_UF 0.7785514 0.3327851 1.566147 2.095230 0.6400511 0.2151651 0.5768753
    ## CTH   0.7662937 0.5220651 1.850176 1.676972 0.6126672 0.4635582 0.5487325
    ## SC    0.7454694 0.4941137 2.299694 1.605942 0.7380367 0.5021506 0.7448042
    ## LSC   0.7548652 0.5029953 1.466861 1.587952 0.5838713 0.4589477 0.4435783
    ## TT    0.6432206 0.3713442 1.605198 1.458711 0.4845096 0.6627758 0.2039261
    ## AE    0.6148827 0.3642763 1.601970 1.304209 0.6673170 0.5055073 0.3852753
    ##             CTH        SC       LSC        TT        AE
    ## CSB   0.6123117 0.7368030 0.5910707 0.4631082 0.7804386
    ## MAA   0.4504706 0.4936054 0.4734595 0.6683290 0.4397118
    ## CG_UF 0.5124450 0.7004903 0.4038012 0.2894295 0.4320867
    ## CTH   0.7225408 0.8448819 0.6054889 0.5308638 0.5362922
    ## SC    0.8400995 0.7029322 0.6193045 0.5719674 0.5401687
    ## LSC   0.6034979 0.6172944 0.7092216 0.4811568 0.5048797
    ## TT    0.5482780 0.5918103 0.4948407 0.6093802 0.8042737
    ## AE    0.5642657 0.5695130 0.5186542 0.7568580 0.6035530

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability of CSB is 0.597 (close to 0.60s)

## 2nd-order factor models

### 2nd-order nine factor original model

``` r
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
```

    ## lavaan 0.6-7 ended normally after 112 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         81
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              2732.684    2087.269
    ##   Degrees of freedom                               585         585
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.586
    ##   Shift parameter                                          364.465
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                             13760.031    3821.873
    ##   Degrees of freedom                               630         630
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  4.114
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.836       0.529
    ##   Tucker-Lewis Index (TLI)                       0.824       0.493
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.098       0.082
    ##   90 Percent confidence interval - lower         0.094       0.078
    ##   90 Percent confidence interval - upper         0.101       0.085
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.112       0.112
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.176    0.044    3.967    0.000
    ##     Q10               0.261    0.066    3.953    0.000
    ##     Q19               0.182    0.047    3.912    0.000
    ##     Q28               0.213    0.054    3.958    0.000
    ##   MAA =~                                              
    ##     Q2                0.641    0.064   10.053    0.000
    ##     Q11               0.653    0.072    9.020    0.000
    ##     Q20               0.560    0.059    9.453    0.000
    ##     Q29               0.279    0.061    4.590    0.000
    ##   CG =~                                               
    ##     Q3                0.173    0.050    3.478    0.001
    ##     Q12               0.071    0.032    2.217    0.027
    ##     Q21               0.238    0.063    3.765    0.000
    ##     Q30               0.288    0.083    3.471    0.001
    ##   UF =~                                               
    ##     Q4                0.556    0.061    9.179    0.000
    ##     Q13               0.242    0.047    5.194    0.000
    ##     Q22               0.290    0.048    6.047    0.000
    ##     Q31               0.452    0.052    8.748    0.000
    ##   CTH =~                                              
    ##     Q5                0.223    0.062    3.629    0.000
    ##     Q14               0.229    0.064    3.560    0.000
    ##     Q23               0.254    0.070    3.624    0.000
    ##     Q32               0.259    0.072    3.609    0.000
    ##   SC =~                                               
    ##     Q6                0.093    0.032    2.879    0.004
    ##     Q15               0.261    0.039    6.754    0.000
    ##     Q24               0.301    0.047    6.416    0.000
    ##     Q33               0.263    0.041    6.428    0.000
    ##   LSC =~                                              
    ##     Q7                0.588    0.060    9.787    0.000
    ##     Q16               0.585    0.060    9.690    0.000
    ##     Q25               0.542    0.053   10.243    0.000
    ##     Q34               0.495    0.059    8.391    0.000
    ##   TT =~                                               
    ##     Q8                0.313    0.062    5.062    0.000
    ##     Q17               0.516    0.068    7.564    0.000
    ##     Q26               0.598    0.083    7.218    0.000
    ##     Q35               0.333    0.059    5.667    0.000
    ##   AE =~                                               
    ##     Q9                0.227    0.043    5.212    0.000
    ##     Q18               0.275    0.045    6.149    0.000
    ##     Q27               0.486    0.060    8.105    0.000
    ##     Q36               0.406    0.054    7.499    0.000
    ##   FSS =~                                              
    ##     CSB               2.226    0.588    3.784    0.000
    ##     MAA               0.769    0.126    6.090    0.000
    ##     CG                2.077    0.601    3.453    0.001
    ##     UF                0.889    0.124    7.166    0.000
    ##     CTH               2.434    0.708    3.438    0.001
    ##     SC                2.031    0.330    6.146    0.000
    ##     LSC               0.928    0.125    7.446    0.000
    ##     TT                0.973    0.167    5.824    0.000
    ##     AE                1.346    0.184    7.317    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .CSB ~~                                              
    ##    .MAA               0.000                           
    ##    .CG                0.000                           
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .MAA ~~                                              
    ##    .CG                0.000                           
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CG ~~                                               
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .UF ~~                                               
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CTH ~~                                              
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .SC ~~                                               
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .LSC ~~                                              
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .TT ~~                                               
    ##    .AE                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.492    0.056    8.803    0.000
    ##    .Q10               0.591    0.065    9.124    0.000
    ##    .Q19               0.834    0.066   12.691    0.000
    ##    .Q28               1.059    0.074   14.291    0.000
    ##    .Q2                0.698    0.090    7.793    0.000
    ##    .Q11               0.401    0.086    4.650    0.000
    ##    .Q20               0.751    0.092    8.187    0.000
    ##    .Q29               1.096    0.073   14.941    0.000
    ##    .Q3                0.795    0.048   16.521    0.000
    ##    .Q12               1.075    0.061   17.568    0.000
    ##    .Q21               0.651    0.053   12.270    0.000
    ##    .Q30               0.473    0.073    6.510    0.000
    ##    .Q4                0.225    0.067    3.382    0.001
    ##    .Q13               0.868    0.058   15.024    0.000
    ##    .Q22               0.882    0.063   14.096    0.000
    ##    .Q31               0.486    0.060    8.040    0.000
    ##    .Q5                0.620    0.054   11.485    0.000
    ##    .Q14               0.869    0.080   10.900    0.000
    ##    .Q23               0.557    0.051   10.827    0.000
    ##    .Q32               0.657    0.062   10.531    0.000
    ##    .Q6                1.453    0.077   18.818    0.000
    ##    .Q15               0.465    0.044   10.636    0.000
    ##    .Q24               0.318    0.042    7.546    0.000
    ##    .Q33               0.401    0.038   10.502    0.000
    ##    .Q7                0.418    0.074    5.654    0.000
    ##    .Q16               0.693    0.085    8.170    0.000
    ##    .Q25               0.507    0.061    8.306    0.000
    ##    .Q34               1.063    0.086   12.378    0.000
    ##    .Q8                1.253    0.079   15.915    0.000
    ##    .Q17               0.520    0.070    7.408    0.000
    ##    .Q26               0.453    0.097    4.686    0.000
    ##    .Q35               1.092    0.083   13.202    0.000
    ##    .Q9                0.922    0.076   12.129    0.000
    ##    .Q18               0.936    0.067   13.922    0.000
    ##    .Q27               0.294    0.057    5.185    0.000
    ##    .Q36               0.633    0.065    9.815    0.000
    ##    .CSB               1.000                           
    ##    .MAA               1.000                           
    ##    .CG                1.000                           
    ##    .UF                1.000                           
    ##    .CTH               1.000                           
    ##    .SC                1.000                           
    ##    .LSC               1.000                           
    ##    .TT                1.000                           
    ##    .AE                1.000                           
    ##     FSS               1.000

*Observations (**fit indexes**)*:

-   CFI and TLI do not achieve 0.90s values.

``` r
reliability(fit2a, return.total = TRUE)
```

    ## Higher-order factors were ignored.

    ##              CSB       MAA        CG        UF       CTH        SC       LSC
    ## alpha  0.5803449 0.7191396 0.5892662 0.6989406 0.6944780 0.6032188 0.7758457
    ## omega  0.5813764 0.7107983 0.5126569 0.6332147 0.7046375 0.6208137 0.7722128
    ## omega2 0.5813764 0.7107983 0.5126569 0.6332147 0.7046375 0.6208137 0.7722128
    ## omega3 0.5782897 0.6805285 0.4481260 0.5559578 0.7149287 0.6141959 0.7655611
    ## avevar 0.2626710 0.3989181 0.2366293 0.3231641 0.3745556 0.3149113 0.4598983
    ##               TT        AE     total
    ## alpha  0.6512167 0.6610580 0.9032067
    ## omega  0.6452029 0.6623996 0.9181711
    ## omega2 0.6452029 0.6623996 0.9181711
    ## omega3 0.6248071 0.6451415 0.8978793
    ## avevar 0.3284020 0.3478765 0.3434615

``` r
reliabilityL2(fit2a, "FSS")
```

    ##        omegaL1        omegaL2 partialOmegaL1 
    ##      0.8542808      0.9540754      0.9125862

*Observations (**reliability**)*:

-   less than 0.60 in CSB and CG.

``` r
convergentDiscriminantValidity(fit2a, Modelo2a, fss, "FSS")
```

    ## Higher-order factors were ignored.
    ## 
    ## Higher-order factors were ignored.

    ##            CR       AVE      VIF    VIF.i       CSB       MAA        CG
    ## CSB 0.5898286 0.2626710 2.051490 1.338167 0.5125144 0.5491145 0.8490849
    ## MAA 0.7136180 0.3989181 1.403274 1.672072 0.5559508 0.6315996 0.4901649
    ## CG  0.5240410 0.2366293 2.178275 1.430092 0.8219122 0.5491296 0.4864456
    ## UF  0.6520276 0.3231641 1.989352 1.766786 0.6060646 0.4049192 0.5986285
    ## CTH 0.7071092 0.3745556 2.156430 1.767734 0.8437726 0.5637348 0.8334200
    ## SC  0.6775048 0.3149113 2.462381 1.609705 0.8184073 0.5467879 0.8083659
    ## LSC 0.7815957 0.4598983 1.506810 1.664968 0.6205962 0.4146279 0.6129818
    ## TT  0.6618600 0.3284020 1.743574 1.480520 0.6362526 0.4250882 0.6284462
    ## AE  0.6699938 0.3478765 2.101808 1.562181 0.7321997 0.4891916 0.7232161
    ## FSS 0.9385826 0.3434615 2.051490 1.338167 0.9122071 0.6094568 0.9010149
    ##            UF       CTH        SC       LSC        TT        AE       FSS
    ## CSB 0.6601975 0.7457629 0.8317423 0.6424276 0.6524711 0.9560090        NA
    ## MAA 0.3343935 0.5689242 0.4697885 0.4060196 0.6660926 0.6195077        NA
    ## CG  1.0053516 0.6796351 0.9925814 0.5158094 0.4289835 0.5760505        NA
    ## UF  0.5684752 0.5695896 0.8835910 0.4080997 0.2591158 0.4273592        NA
    ## CTH 0.6145503 0.6120094 1.0042116 0.7059522 0.6335204 0.6476611        NA
    ## SC  0.5960758 0.8298660 0.5611696 0.5937076 0.5387951 0.6500179        NA
    ## LSC 0.4520028 0.6292853 0.6103679 0.6781580 0.4469299 0.5472759        NA
    ## TT  0.4634059 0.6451610 0.6257663 0.4745170 0.5730637 0.9011199        NA
    ## AE  0.5332877 0.7424515 0.7201321 0.5460743 0.5598507 0.5898105        NA
    ## FSS 0.6643936 0.9249792 0.8971726 0.6803238 0.6974870 0.8026683 0.5860559

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability less than 0.60 in CSB and CG
-   Based on the HTMT0.85 rule dicriminant are inadequate for CSB\~AE
    CG\~UF CG\~SC UF\~SC CTH\~SC TT\~AE.

### 2nd-order eight factor model - fixed convergence and discriminant

Based on the EFA, the following values should be observed:

-   In the `CSB`, the items `Q1+Q10` or `Q19` or `Q28`
-   In the `TT`, the items `Q26+Q17` or `Q35` or `Q8`
-   In the `AE`, the items `Q27`. or `Q36` or `Q18` or `Q9`

Based on the HTMT0.85, the convergent and discriminant validity suggest
combine:

-   (challenge-skill balance) CSB \~ AE (autotelic experience)
-   (clear goal) CG \~ UF (unambiguous feedback)
-   (transformation of time) TT \~ AE (autotelic experience)

``` r
Modelo2b <- '
CSB =~ Q1 + Q10 + Q19
MAA =~ Q2 + Q11 + Q20
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
```

    ## lavaan 0.6-7 ended normally after 93 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         65
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              1389.972    1317.423
    ##   Degrees of freedom                               341         341
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.227
    ##   Shift parameter                                          184.691
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              8973.411    2707.611
    ##   Degrees of freedom                               378         378
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  3.690
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.878       0.581
    ##   Tucker-Lewis Index (TLI)                       0.865       0.535
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.089       0.086
    ##   90 Percent confidence interval - lower         0.085       0.081
    ##   90 Percent confidence interval - upper         0.094       0.091
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.103       0.103
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.284    0.054    5.296    0.000
    ##     Q10               0.386    0.072    5.323    0.000
    ##     Q19               0.269    0.054    4.988    0.000
    ##   MAA =~                                              
    ##     Q2                0.634    0.060   10.636    0.000
    ##     Q11               0.698    0.075    9.252    0.000
    ##     Q20               0.504    0.059    8.542    0.000
    ##   CG =~                                               
    ##     Q3                0.451    0.054    8.386    0.000
    ##     Q12               0.251    0.055    4.600    0.000
    ##     Q21               0.616    0.072    8.605    0.000
    ##   UF =~                                               
    ##     Q4                0.507    0.058    8.791    0.000
    ##     Q13               0.254    0.042    6.104    0.000
    ##     Q22               0.295    0.044    6.785    0.000
    ##     Q31               0.411    0.050    8.278    0.000
    ##   CTH =~                                              
    ##     Q5                0.390    0.052    7.446    0.000
    ##     Q23               0.427    0.057    7.486    0.000
    ##     Q32               0.424    0.058    7.292    0.000
    ##   SC =~                                               
    ##     Q15               0.196    0.045    4.378    0.000
    ##     Q24               0.221    0.052    4.266    0.000
    ##     Q33               0.199    0.047    4.271    0.000
    ##   LSC =~                                              
    ##     Q7                0.570    0.063    9.048    0.000
    ##     Q16               0.552    0.060    9.180    0.000
    ##     Q25               0.510    0.052    9.751    0.000
    ##   TT =~                                               
    ##     Q8                0.292    0.062    4.680    0.000
    ##     Q17               0.556    0.069    8.053    0.000
    ##     Q26               0.605    0.080    7.599    0.000
    ##   AE =~                                               
    ##     Q9                0.267    0.047    5.625    0.000
    ##     Q18               0.335    0.047    7.150    0.000
    ##     Q27               0.618    0.084    7.352    0.000
    ##   FSS =~                                              
    ##     CSB               1.411    0.292    4.837    0.000
    ##     MAA               0.714    0.120    5.958    0.000
    ##     CG                0.845    0.123    6.875    0.000
    ##     UF                1.039    0.146    7.097    0.000
    ##     CTH               1.460    0.223    6.555    0.000
    ##     SC                2.858    0.677    4.219    0.000
    ##     LSC               0.973    0.138    7.076    0.000
    ##     TT                0.880    0.154    5.708    0.000
    ##     AE                1.021    0.154    6.628    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .CSB ~~                                              
    ##    .MAA               0.000                           
    ##    .CG                0.000                           
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .MAA ~~                                              
    ##    .CG                0.000                           
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CG ~~                                               
    ##    .UF                0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .UF ~~                                               
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CTH ~~                                              
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .SC ~~                                               
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .LSC ~~                                              
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .TT ~~                                               
    ##    .AE                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.436    0.055    7.934    0.000
    ##    .Q10               0.552    0.067    8.245    0.000
    ##    .Q19               0.814    0.069   11.734    0.000
    ##    .Q2                0.745    0.094    7.932    0.000
    ##    .Q11               0.345    0.094    3.659    0.000
    ##    .Q20               0.867    0.098    8.867    0.000
    ##    .Q3                0.605    0.066    9.148    0.000
    ##    .Q12               0.993    0.068   14.597    0.000
    ##    .Q21               0.302    0.103    2.932    0.003
    ##    .Q4                0.244    0.059    4.146    0.000
    ##    .Q13               0.839    0.058   14.528    0.000
    ##    .Q22               0.851    0.062   13.712    0.000
    ##    .Q31               0.502    0.057    8.869    0.000
    ##    .Q5                0.488    0.055    8.912    0.000
    ##    .Q23               0.432    0.054    7.927    0.000
    ##    .Q32               0.557    0.065    8.586    0.000
    ##    .Q15               0.460    0.043   10.641    0.000
    ##    .Q24               0.335    0.040    8.429    0.000
    ##    .Q33               0.393    0.036   10.906    0.000
    ##    .Q7                0.428    0.075    5.714    0.000
    ##    .Q16               0.736    0.081    9.044    0.000
    ##    .Q25               0.549    0.064    8.613    0.000
    ##    .Q8                1.293    0.079   16.375    0.000
    ##    .Q17               0.492    0.074    6.616    0.000
    ##    .Q26               0.500    0.094    5.337    0.000
    ##    .Q9                0.921    0.078   11.785    0.000
    ##    .Q18               0.920    0.068   13.469    0.000
    ##    .Q27               0.178    0.105    1.691    0.091
    ##    .CSB               1.000                           
    ##    .MAA               1.000                           
    ##    .CG                1.000                           
    ##    .UF                1.000                           
    ##    .CTH               1.000                           
    ##    .SC                1.000                           
    ##    .LSC               1.000                           
    ##    .TT                1.000                           
    ##    .AE                1.000                           
    ##     FSS               1.000

*Observations (**fit indexes**)*:

-   CFI and TLI does not achieve 0.90s values

``` r
reliability(fit2b, return.total = TRUE)
```

    ## Higher-order factors were ignored.

    ##              CSB       MAA        CG        UF       CTH        SC       LSC
    ## alpha  0.5934344 0.7306380 0.6339465 0.6989406 0.7647409 0.7439148 0.7521790
    ## omega  0.5939861 0.7223174 0.6105636 0.6473088 0.7659304 0.7453056 0.7516534
    ## omega2 0.5939861 0.7223174 0.6105636 0.6473088 0.7659304 0.7453056 0.7516534
    ## omega3 0.5891025 0.7088994 0.5719309 0.5851457 0.7665171 0.7456305 0.7503071
    ## avevar 0.3337347 0.4686687 0.3681769 0.3299182 0.5221098 0.4944904 0.5027520
    ##               TT        AE     total
    ## alpha  0.6174260 0.5963732 0.8897113
    ## omega  0.6209969 0.6009761 0.9165474
    ## omega2 0.6209969 0.6009761 0.9165474
    ## omega3 0.6061942 0.5770324 0.9112995
    ## avevar 0.3712108 0.3640185 0.4159341

``` r
reliabilityL2(fit2b, "FSS")
```

    ##        omegaL1        omegaL2 partialOmegaL1 
    ##      0.8353441      0.9330689      0.9091718

*Observations (**reliability**)*:

-   less than 0.60 in CSB and AE (but close to 0.60 with 0.59)

``` r
convergentDiscriminantValidity(fit2b, Modelo2b, fss, "FSS")
```

    ## Higher-order factors were ignored.
    ## 
    ## Higher-order factors were ignored.

    ##            CR       AVE      VIF    VIF.i       CSB       MAA        CG
    ## CSB 0.5991536 0.3337347 1.635697 1.333860 0.5776978 0.4946320 0.6493433
    ## MAA 0.7286496 0.4686687 1.407987 1.630312 0.4740695 0.6845938 0.3436731
    ## CG  0.6215577 0.3681769 1.854826 1.281195 0.5267132 0.3751520 0.6067759
    ## UF  0.6636071 0.3299182 2.031633 1.766786 0.5878078 0.4186667 0.4651580
    ## CTH 0.7663204 0.5221098 1.854397 1.676972 0.6730912 0.4794099 0.5326466
    ## SC  0.7457072 0.4944904 2.302430 1.605942 0.7700684 0.5484819 0.6093888
    ## LSC 0.7548313 0.5027520 1.467206 1.587952 0.5689527 0.4052371 0.4502372
    ## TT  0.6427012 0.3712108 1.607296 1.458711 0.5388857 0.3838218 0.4264438
    ## AE  0.6147859 0.3640185 1.601983 1.304209 0.5828371 0.4151263 0.4612245
    ## FSS 0.9149154 0.4159341 1.635697 1.333860 0.8158391 0.5810821 0.6456092
    ##            UF       CTH        SC       LSC        TT        AE       FSS
    ## CSB 0.6374709 0.6123117 0.7368030 0.5910707 0.4631082 0.7804386        NA
    ## MAA 0.3639031 0.4504706 0.4936054 0.4734595 0.6683290 0.4397118        NA
    ## CG  0.9778317 0.4711559 0.6482011 0.3658158 0.2717332 0.4346909        NA
    ## UF  0.5743851 0.5378309 0.7320650 0.4279098 0.2995317 0.4253087        NA
    ## CTH 0.5944294 0.7225717 0.8448819 0.6054889 0.5308638 0.5362922        NA
    ## SC  0.6800732 0.7787431 0.7032001 0.6193045 0.5719674 0.5401687        NA
    ## LSC 0.5024612 0.5753619 0.6582585 0.7090501 0.4811568 0.5048797        NA
    ## TT  0.4759080 0.5449562 0.6234720 0.4606423 0.6092707 0.8042737        NA
    ## AE  0.5147230 0.5894027 0.6743223 0.4982122 0.4718836 0.6033394        NA
    ## FSS 0.7204948 0.8250294 0.9438974 0.6973835 0.6605294 0.7144021 0.6449295

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability of CSB is 0.597 (close to 0.60s)
-   Based on the HTMT0.85 rule dicriminant are inadequate only for
    CG\~UF

### 2nd-order eight factor model - merging CG\~UF

``` r
Modelo2c <- '
CSB =~    Q1 + Q10 + Q19
MAA =~    Q2 + Q11 + Q20
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
```

    ## lavaan 0.6-7 ended normally after 89 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         64
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                              1290.797    1233.494
    ##   Degrees of freedom                               342         342
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  1.231
    ##   Shift parameter                                          184.553
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              8973.411    2707.611
    ##   Degrees of freedom                               378         378
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  3.690
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.890       0.617
    ##   Tucker-Lewis Index (TLI)                       0.878       0.577
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.085       0.082
    ##   90 Percent confidence interval - lower         0.080       0.077
    ##   90 Percent confidence interval - upper         0.090       0.087
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.100       0.100
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   CSB =~                                              
    ##     Q1                0.278    0.054    5.125    0.000
    ##     Q10               0.382    0.074    5.150    0.000
    ##     Q19               0.266    0.055    4.852    0.000
    ##   MAA =~                                              
    ##     Q2                0.631    0.060   10.505    0.000
    ##     Q11               0.691    0.075    9.152    0.000
    ##     Q20               0.503    0.059    8.550    0.000
    ##   CG_UF =~                                            
    ##     Q3                0.435    0.048    9.011    0.000
    ##     Q12               0.268    0.052    5.114    0.000
    ##     Q21               0.547    0.048   11.488    0.000
    ##     Q4                0.549    0.046   11.882    0.000
    ##     Q13               0.334    0.043    7.781    0.000
    ##     Q22               0.384    0.042    9.076    0.000
    ##     Q31               0.447    0.048    9.300    0.000
    ##   CTH =~                                              
    ##     Q5                0.380    0.053    7.187    0.000
    ##     Q23               0.419    0.058    7.216    0.000
    ##     Q32               0.416    0.059    7.042    0.000
    ##   SC =~                                               
    ##     Q15               0.190    0.047    4.031    0.000
    ##     Q24               0.215    0.055    3.922    0.000
    ##     Q33               0.193    0.049    3.925    0.000
    ##   LSC =~                                              
    ##     Q7                0.562    0.063    8.869    0.000
    ##     Q16               0.546    0.061    9.023    0.000
    ##     Q25               0.502    0.053    9.552    0.000
    ##   TT =~                                               
    ##     Q8                0.287    0.061    4.669    0.000
    ##     Q17               0.546    0.069    7.881    0.000
    ##     Q26               0.597    0.080    7.445    0.000
    ##   AE =~                                               
    ##     Q9                0.262    0.047    5.614    0.000
    ##     Q18               0.329    0.046    7.082    0.000
    ##     Q27               0.605    0.085    7.150    0.000
    ##   FSS =~                                              
    ##     CSB               1.444    0.309    4.678    0.000
    ##     MAA               0.728    0.123    5.927    0.000
    ##     CG_UF             0.808    0.100    8.074    0.000
    ##     CTH               1.507    0.238    6.341    0.000
    ##     SC                2.959    0.766    3.864    0.000
    ##     LSC               1.000    0.143    6.970    0.000
    ##     TT                0.911    0.161    5.651    0.000
    ##     AE                1.061    0.164    6.485    0.000
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##  .CSB ~~                                              
    ##    .MAA               0.000                           
    ##    .CG_UF             0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .MAA ~~                                              
    ##    .CG_UF             0.000                           
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CG_UF ~~                                            
    ##    .CTH               0.000                           
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .CTH ~~                                              
    ##    .SC                0.000                           
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .SC ~~                                               
    ##    .LSC               0.000                           
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .LSC ~~                                              
    ##    .TT                0.000                           
    ##    .AE                0.000                           
    ##  .TT ~~                                               
    ##    .AE                0.000                           
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q1                0.439    0.055    7.989    0.000
    ##    .Q10               0.547    0.067    8.148    0.000
    ##    .Q19               0.814    0.069   11.725    0.000
    ##    .Q2                0.744    0.094    7.951    0.000
    ##    .Q11               0.350    0.094    3.740    0.000
    ##    .Q20               0.863    0.097    8.859    0.000
    ##    .Q3                0.642    0.056   11.551    0.000
    ##    .Q12               0.983    0.066   14.844    0.000
    ##    .Q21               0.458    0.069    6.686    0.000
    ##    .Q4                0.280    0.052    5.381    0.000
    ##    .Q13               0.789    0.058   13.615    0.000
    ##    .Q22               0.789    0.061   13.004    0.000
    ##    .Q31               0.521    0.057    9.170    0.000
    ##    .Q5                0.492    0.055    8.982    0.000
    ##    .Q23               0.430    0.055    7.885    0.000
    ##    .Q32               0.554    0.065    8.519    0.000
    ##    .Q15               0.461    0.043   10.634    0.000
    ##    .Q24               0.333    0.040    8.316    0.000
    ##    .Q33               0.394    0.036   10.891    0.000
    ##    .Q7                0.429    0.075    5.729    0.000
    ##    .Q16               0.734    0.081    9.040    0.000
    ##    .Q25               0.550    0.063    8.667    0.000
    ##    .Q8                1.294    0.079   16.440    0.000
    ##    .Q17               0.494    0.074    6.698    0.000
    ##    .Q26               0.497    0.093    5.352    0.000
    ##    .Q9                0.921    0.078   11.811    0.000
    ##    .Q18               0.919    0.068   13.495    0.000
    ##    .Q27               0.180    0.104    1.732    0.083
    ##    .CSB               1.000                           
    ##    .MAA               1.000                           
    ##    .CG_UF             1.000                           
    ##    .CTH               1.000                           
    ##    .SC                1.000                           
    ##    .LSC               1.000                           
    ##    .TT                1.000                           
    ##    .AE                1.000                           
    ##     FSS               1.000

*Observations (**fit indexes**)*:

-   CFI and TLI does not achieve 0.90s value but its close with 0.888

``` r
reliability(fit2c, return.total = TRUE)
```

    ## Higher-order factors were ignored.

    ##              CSB       MAA     CG_UF       CTH        SC       LSC        TT
    ## alpha  0.5934344 0.7306380 0.7995720 0.7647409 0.7439148 0.7521790 0.6174260
    ## omega  0.5949395 0.7224711 0.7647658 0.7661011 0.7454101 0.7517601 0.6209382
    ## omega2 0.5949395 0.7224711 0.7647658 0.7661011 0.7454101 0.7517601 0.6209382
    ## omega3 0.5903921 0.7094485 0.6871789 0.7667709 0.7457437 0.7504968 0.6059994
    ## avevar 0.3349108 0.4686635 0.3282455 0.5224061 0.4946921 0.5029104 0.3712562
    ##               AE     total
    ## alpha  0.5963732 0.8897113
    ## omega  0.6008710 0.9165888
    ## omega2 0.6008710 0.9165888
    ## omega3 0.5770377 0.9184129
    ## avevar 0.3637346 0.4116936

``` r
reliabilityL2(fit2c, "FSS")
```

    ##        omegaL1        omegaL2 partialOmegaL1 
    ##      0.8135831      0.9313381      0.9070103

\*Observations (\*\*reliability*)*:

-   less than 0.60 in CSB and AE (but close to 0.60s with 0.59)

``` r
convergentDiscriminantValidity(fit2c, Modelo2c, fss, 'FSS')
```

    ## Higher-order factors were ignored.
    ## 
    ## Higher-order factors were ignored.

    ##              CR       AVE      VIF    VIF.i       CSB       MAA     CG_UF
    ## CSB   0.5997040 0.3349108 1.634652 1.333860 0.5787148 0.4946320 0.6466831
    ## MAA   0.7286793 0.4686635 1.406340 1.630312 0.4838708 0.6845900 0.3574851
    ## CG_UF 0.7737922 0.3282455 1.566147 2.095230 0.5165091 0.3698501 0.5729271
    ## CTH   0.7664036 0.5224061 1.850176 1.676972 0.6849769 0.4904827 0.5235670
    ## SC    0.7458229 0.4946921 2.299694 1.605942 0.7787559 0.5576338 0.5952477
    ## LSC   0.7548546 0.5029104 1.466861 1.587952 0.5812850 0.4162334 0.4443094
    ## TT    0.6426282 0.3712562 1.605198 1.458711 0.5534585 0.3963080 0.4230400
    ## AE    0.6146289 0.3637346 1.601970 1.304209 0.5982364 0.4283715 0.4572663
    ## FSS   0.9092231 0.4116936 1.634652 1.333860 0.8220360 0.5886248 0.6283291
    ##             CTH        SC       LSC        TT        AE       FSS
    ## CSB   0.6123117 0.7368030 0.5910707 0.4631082 0.7804386        NA
    ## MAA   0.4504706 0.4936054 0.4734595 0.6683290 0.4397118        NA
    ## CG_UF 0.5124450 0.7004903 0.4038012 0.2894295 0.4320867        NA
    ## CTH   0.7227767 0.8448819 0.6054889 0.5308638 0.5362922        NA
    ## SC    0.7893973 0.7033435 0.6193045 0.5719674 0.5401687        NA
    ## LSC   0.5892280 0.6698983 0.7091618 0.4811568 0.5048797        NA
    ## TT    0.5610213 0.6378297 0.4760938 0.6093079 0.8042737        NA
    ## AE    0.6064110 0.6894337 0.5146125 0.4899776 0.6031042        NA
    ## FSS   0.8332687 0.9473501 0.7071285 0.6732777 0.7277496 0.6416335

*Observations (**convergent and discriminant validity**)*:

-   Composite Reliability of CSB is 0.599 (close to 0.60s)

## Unidimensional models (Short version of FSS-BR)

### Original short version of FSS-BR (FSS-short)

``` r
Modelo3a <- 'FSS =~ Q19 + Q29 + Q12 + Q22 + Q32 + Q6 + Q7 + Q17 + Q36'

fit3a <-cfa(Modelo3a, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit3a, fit.measures=TRUE)
```

    ## lavaan 0.6-7 ended normally after 25 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         18
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                               296.506     327.033
    ##   Degrees of freedom                                27          27
    ##   P-value (Chi-square)                           0.000       0.000
    ##   Scaling correction factor                                  0.923
    ##   Shift parameter                                            5.689
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               599.675     437.360
    ##   Degrees of freedom                                36          36
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.404
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.522       0.252
    ##   Tucker-Lewis Index (TLI)                       0.363       0.003
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.161       0.170
    ##   90 Percent confidence interval - lower         0.145       0.154
    ##   90 Percent confidence interval - upper         0.178       0.187
    ##   P-value RMSEA <= 0.05                          0.000       0.000
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.137       0.137
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   FSS =~                                              
    ##     Q19               0.465    0.058    8.024    0.000
    ##     Q29               0.135    0.076    1.767    0.077
    ##     Q12               0.670    0.061   10.990    0.000
    ##     Q22               0.723    0.059   12.298    0.000
    ##     Q32               0.140    0.061    2.300    0.021
    ##     Q6                0.662    0.066   10.078    0.000
    ##     Q7                0.306    0.066    4.627    0.000
    ##     Q17               0.027    0.073    0.377    0.706
    ##     Q36              -0.041    0.061   -0.672    0.502
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q19               0.815    0.063   12.967    0.000
    ##    .Q29               1.202    0.067   17.863    0.000
    ##    .Q12               0.652    0.073    8.919    0.000
    ##    .Q22               0.510    0.074    6.911    0.000
    ##    .Q32               1.101    0.070   15.670    0.000
    ##    .Q6                1.058    0.092   11.550    0.000
    ##    .Q7                0.967    0.074   13.115    0.000
    ##    .Q17               1.039    0.063   16.607    0.000
    ##    .Q36               1.096    0.073   14.997    0.000
    ##     FSS               1.000

*Observations (**fit indexes**)*:

-   CFI and TLI does not achieve 0.90s value

``` r
reliability(fit3a, return.total = TRUE)
```

    ##              FSS
    ## alpha  0.5897855
    ## omega  0.5305905
    ## omega2 0.5305905
    ## omega3 0.4449016
    ## avevar 0.1726656

\*Observations (\*\*reliability*)*:

-   less than 0.60

### Alternative short version of FSS-BR (alt FSS-short)

To elaborate this version, we replaced the items from the original short
version that doesn’t have item loadings close or high to the value of
0.7 in the long version of the FSS-BR. Thus, we performed the following
changes:

-   In the CSB, the item Q19 was replaced for Q10.
-   In the MAA, the item Q29 was replaced for Q11.
-   In the CG, the item Q12 was replaced for Q21.
-   In the UF, the item Q22 was replaced for Q4.
-   In the SC, the item Q6 was replaced for Q24.
-   In the AE, the item Q36 was replaced for Q27.
-   For the rest of factors CTH, LSC and TT the items Q32, Q7 and Q17
    were maintained.

``` r
Modelo3b <- 'FSS =~ Q10 + Q11 + Q21 + Q4 + Q32 + Q24 + Q7 + Q17 + Q27'

fit3b <-cfa(Modelo3b, data=fss, estimator="WLSMV", std.lv=TRUE)
summary(fit3b, fit.measures=TRUE)
```

    ## lavaan 0.6-7 ended normally after 21 iterations
    ## 
    ##   Estimator                                       DWLS
    ##   Optimization method                           NLMINB
    ##   Number of free parameters                         18
    ##                                                       
    ##   Number of observations                           386
    ##                                                       
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                39.799      74.788
    ##   Degrees of freedom                                27          27
    ##   P-value (Chi-square)                           0.053       0.000
    ##   Scaling correction factor                                  0.570
    ##   Shift parameter                                            4.980
    ##        simple second-order correction                             
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                              1350.095     705.005
    ##   Degrees of freedom                                36          36
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.964
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.990       0.929
    ##   Tucker-Lewis Index (TLI)                       0.987       0.905
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                            NA
    ##   Robust Tucker-Lewis Index (TLI)                               NA
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.035       0.068
    ##   90 Percent confidence interval - lower         0.000       0.050
    ##   90 Percent confidence interval - upper         0.057       0.086
    ##   P-value RMSEA <= 0.05                          0.857       0.052
    ##                                                                   
    ##   Robust RMSEA                                                  NA
    ##   90 Percent confidence interval - lower                        NA
    ##   90 Percent confidence interval - upper                        NA
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.054       0.054
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                           Robust.sem
    ##   Information                                 Expected
    ##   Information saturated (h1) model        Unstructured
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##   FSS =~                                              
    ##     Q10               0.513    0.052    9.782    0.000
    ##     Q11               0.568    0.056   10.182    0.000
    ##     Q21               0.490    0.054    9.131    0.000
    ##     Q4                0.538    0.046   11.603    0.000
    ##     Q32               0.605    0.055   11.003    0.000
    ##     Q24               0.648    0.040   16.315    0.000
    ##     Q7                0.478    0.055    8.711    0.000
    ##     Q17               0.588    0.054   10.947    0.000
    ##     Q27               0.680    0.043   15.653    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)
    ##    .Q10               0.733    0.062   11.754    0.000
    ##    .Q11               0.758    0.064   11.912    0.000
    ##    .Q21               0.712    0.053   13.493    0.000
    ##    .Q4                0.488    0.044   11.132    0.000
    ##    .Q32               0.755    0.069   10.914    0.000
    ##    .Q24               0.362    0.038    9.497    0.000
    ##    .Q7                0.833    0.071   11.737    0.000
    ##    .Q17               0.694    0.068   10.263    0.000
    ##    .Q27               0.497    0.052    9.499    0.000
    ##     FSS               1.000

``` r
reliability(fit3b, return.total = TRUE)
```

    ##              FSS
    ## alpha  0.8166178
    ## omega  0.8172900
    ## omega2 0.8172900
    ## omega3 0.8154458
    ## avevar 0.3349650

## Summary of construct validity (structural, reliability and convergent/discriminant)

### Structural validity - CFAs

``` r
df <- summariseFits(list('nine-multicorrelated model'=fit1a
                         ,'nine-multicorrelated model (with removed items)'=fit1b
                         , 'eight-multicorrelated model (with removed items)'=fit1c
                         , '2nd-order nine-factor model'=fit2a
                         , '2nd-order nine-factor model (with removed items)'=fit2b
                         , '2nd-order eight-factor model (with removed items)'=fit2c
                         , 'unidimensional model (short)'=fit3a
                         , 'unidimensional model (alt short)'=fit3b))
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:left;">
chisq
</th>
<th style="text-align:left;">
df
</th>
<th style="text-align:left;">
chisq/df
</th>
<th style="text-align:left;">
cfi
</th>
<th style="text-align:left;">
tli
</th>
<th style="text-align:left;">
srmr
</th>
<th style="text-align:left;">
rmsea
</th>
<th style="text-align:left;">
rmsea.ci
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
nine-multicorrelated model
</td>
<td style="text-align:left;">
2354.28
</td>
<td style="text-align:left;">
558
</td>
<td style="text-align:left;">
4.219
</td>
<td style="text-align:left;">
0.863
</td>
<td style="text-align:left;">
0.846
</td>
<td style="text-align:left;">
0.104
</td>
<td style="text-align:left;">
0.091
</td>
<td style="text-align:left;">
\[0.088; 0.095\]
</td>
</tr>
<tr>
<td style="text-align:left;">
nine-multicorrelated model (with removed items)
</td>
<td style="text-align:left;">
1076.323
</td>
<td style="text-align:left;">
314
</td>
<td style="text-align:left;">
3.428
</td>
<td style="text-align:left;">
0.911
</td>
<td style="text-align:left;">
0.893
</td>
<td style="text-align:left;">
0.091
</td>
<td style="text-align:left;">
0.079
</td>
<td style="text-align:left;">
\[0.074; 0.085\]
</td>
</tr>
<tr>
<td style="text-align:left;">
eight-multicorrelated model (with removed items)
</td>
<td style="text-align:left;">
1082.616
</td>
<td style="text-align:left;">
322
</td>
<td style="text-align:left;">
3.362
</td>
<td style="text-align:left;">
0.912
</td>
<td style="text-align:left;">
0.896
</td>
<td style="text-align:left;">
0.091
</td>
<td style="text-align:left;">
0.078
</td>
<td style="text-align:left;">
\[0.073; 0.083\]
</td>
</tr>
<tr>
<td style="text-align:left;">
2nd-order nine-factor model
</td>
<td style="text-align:left;">
2732.684
</td>
<td style="text-align:left;">
585
</td>
<td style="text-align:left;">
4.671
</td>
<td style="text-align:left;">
0.836
</td>
<td style="text-align:left;">
0.824
</td>
<td style="text-align:left;">
0.112
</td>
<td style="text-align:left;">
0.098
</td>
<td style="text-align:left;">
\[0.094; 0.101\]
</td>
</tr>
<tr>
<td style="text-align:left;">
2nd-order nine-factor model (with removed items)
</td>
<td style="text-align:left;">
1389.972
</td>
<td style="text-align:left;">
341
</td>
<td style="text-align:left;">
4.076
</td>
<td style="text-align:left;">
0.878
</td>
<td style="text-align:left;">
0.865
</td>
<td style="text-align:left;">
0.103
</td>
<td style="text-align:left;">
0.089
</td>
<td style="text-align:left;">
\[0.085; 0.094\]
</td>
</tr>
<tr>
<td style="text-align:left;">
2nd-order eight-factor model (with removed items)
</td>
<td style="text-align:left;">
1290.797
</td>
<td style="text-align:left;">
342
</td>
<td style="text-align:left;">
3.774
</td>
<td style="text-align:left;">
0.89
</td>
<td style="text-align:left;">
0.878
</td>
<td style="text-align:left;">
0.1
</td>
<td style="text-align:left;">
0.085
</td>
<td style="text-align:left;">
\[0.08; 0.09\]
</td>
</tr>
<tr>
<td style="text-align:left;">
unidimensional model (short)
</td>
<td style="text-align:left;">
296.506
</td>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
10.982
</td>
<td style="text-align:left;">
0.522
</td>
<td style="text-align:left;">
0.363
</td>
<td style="text-align:left;">
0.137
</td>
<td style="text-align:left;">
0.161
</td>
<td style="text-align:left;">
\[0.145; 0.178\]
</td>
</tr>
<tr>
<td style="text-align:left;">
unidimensional model (alt short)
</td>
<td style="text-align:left;">
39.799
</td>
<td style="text-align:left;">
27
</td>
<td style="text-align:left;">
1.474
</td>
<td style="text-align:left;">
0.99
</td>
<td style="text-align:left;">
0.987
</td>
<td style="text-align:left;">
0.054
</td>
<td style="text-align:left;">
0.035
</td>
<td style="text-align:left;">
\[0; 0.057\]
</td>
</tr>
</tbody>
</table>

*Observations (**fit indexes**)*:

-   Original nine-multicorrelated model doesn’t have adequate fit
    indexes
-   None of 2nd-order models have adequate `srmr` (all values are
    greater than `0.10`)
-   The nine-multicorrelated model and eight-multicorrelated model with
    some removed items, both models have good fit indexes
-   Only the alternative version of FSS-BR have good fit

Running anova test to compare fit indexes in the nine-multicorrelated
and eight-multicorrelated CFA models with some removed items

``` r
lavTestLRT(fit1b, fit1c, model.names=c('nine-multicorr','eight-multicorr'))
```

    ## Scaled Chi-Squared Difference Test (method = "satorra.2000")
    ## 
    ## lavaan NOTE:
    ##     The "Chisq" column contains standard test statistics, not the
    ##     robust test that should be reported per model. A robust difference
    ##     test is a function of two standard (not robust) statistics.
    ##  
    ##                  Df AIC BIC  Chisq Chisq diff Df diff Pr(>Chisq)
    ## nine-multicorr  314         1076.3                              
    ## eight-multicorr 322         1082.6     7.6714       8     0.4662

*Observations (**comparing models**)*:

-   Pr(&gt;Chisq) is not significat =&gt; there is not difference
    between both models
-   Both models give proper results (by flow theory - we sugest use the
    nine-multicorrelated model)

### Draw diagrams using semTool

nine-multicorrelated model with some removed items

``` r
semPlot::semPaths(
  fit1b, "std", curvePivot = T, layout = "circle",
  rotation = 3, fade = T, intercepts = T, residuals = T
  , sizeLat = 4, sizeLat2 = 3, sizeMan = 4
  , esize = 1.5, asize = 1.5
  , edge.label.position = 0.55
  , levels = c(9.75,10,10,10))
```

    ## Registered S3 methods overwritten by 'lme4':
    ##   method                          from
    ##   cooks.distance.influence.merMod car 
    ##   influence.merMod                car 
    ##   dfbeta.influence.merMod         car 
    ##   dfbetas.influence.merMod        car

![](script20210331_files/figure-gfm/unnamed-chunk-36-1.png)<!-- -->

alternative unidimension model for the short version

``` r
semPlot::semPaths(
  fit3b, "std", curvePivot = T, layout = "circle"
  , rotation = 3, fade = T, intercepts = T, residuals = T
  , esize = 1.5, asize = 1.5)
```

![](script20210331_files/figure-gfm/unnamed-chunk-37-1.png)<!-- -->

### Reliability validity - Cronbach / Omega / CR

nine-multicorrelated model with some removed items

``` r
df <- rbind(reliability(fit1b, return.total = T)
            , "CR"=compReliability(fit1b, return.total = T))
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
CSB
</th>
<th style="text-align:right;">
MAA
</th>
<th style="text-align:right;">
CG
</th>
<th style="text-align:right;">
UF
</th>
<th style="text-align:right;">
CTH
</th>
<th style="text-align:right;">
SC
</th>
<th style="text-align:right;">
LSC
</th>
<th style="text-align:right;">
TT
</th>
<th style="text-align:right;">
AE
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
alpha
</td>
<td style="text-align:right;">
0.593
</td>
<td style="text-align:right;">
0.731
</td>
<td style="text-align:right;">
0.634
</td>
<td style="text-align:right;">
0.699
</td>
<td style="text-align:right;">
0.765
</td>
<td style="text-align:right;">
0.744
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.617
</td>
<td style="text-align:right;">
0.596
</td>
<td style="text-align:right;">
0.890
</td>
</tr>
<tr>
<td style="text-align:left;">
omega
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.621
</td>
<td style="text-align:right;">
0.668
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.622
</td>
<td style="text-align:right;">
0.601
</td>
<td style="text-align:right;">
0.918
</td>
</tr>
<tr>
<td style="text-align:left;">
omega2
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.621
</td>
<td style="text-align:right;">
0.668
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.622
</td>
<td style="text-align:right;">
0.601
</td>
<td style="text-align:right;">
0.918
</td>
</tr>
<tr>
<td style="text-align:left;">
omega3
</td>
<td style="text-align:right;">
0.587
</td>
<td style="text-align:right;">
0.713
</td>
<td style="text-align:right;">
0.600
</td>
<td style="text-align:right;">
0.631
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.751
</td>
<td style="text-align:right;">
0.609
</td>
<td style="text-align:right;">
0.577
</td>
<td style="text-align:right;">
0.921
</td>
</tr>
<tr>
<td style="text-align:left;">
avevar
</td>
<td style="text-align:right;">
0.331
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
0.367
</td>
<td style="text-align:right;">
0.341
</td>
<td style="text-align:right;">
0.522
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
0.503
</td>
<td style="text-align:right;">
0.371
</td>
<td style="text-align:right;">
0.364
</td>
<td style="text-align:right;">
0.417
</td>
</tr>
<tr>
<td style="text-align:left;">
CR
</td>
<td style="text-align:right;">
0.597
</td>
<td style="text-align:right;">
0.729
</td>
<td style="text-align:right;">
0.630
</td>
<td style="text-align:right;">
0.679
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.755
</td>
<td style="text-align:right;">
0.643
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.952
</td>
</tr>
</tbody>
</table>

Eight-multicorrelated model with some removed items

``` r
df <- rbind(reliability(fit1c, return.total = T)
            , "CR"=compReliability(fit1c, return.total = T))
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
CSB
</th>
<th style="text-align:right;">
MAA
</th>
<th style="text-align:right;">
CG\_UF
</th>
<th style="text-align:right;">
CTH
</th>
<th style="text-align:right;">
SC
</th>
<th style="text-align:right;">
LSC
</th>
<th style="text-align:right;">
TT
</th>
<th style="text-align:right;">
AE
</th>
<th style="text-align:right;">
total
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
alpha
</td>
<td style="text-align:right;">
0.593
</td>
<td style="text-align:right;">
0.731
</td>
<td style="text-align:right;">
0.800
</td>
<td style="text-align:right;">
0.765
</td>
<td style="text-align:right;">
0.744
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.617
</td>
<td style="text-align:right;">
0.596
</td>
<td style="text-align:right;">
0.890
</td>
</tr>
<tr>
<td style="text-align:left;">
omega
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.770
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.622
</td>
<td style="text-align:right;">
0.601
</td>
<td style="text-align:right;">
0.917
</td>
</tr>
<tr>
<td style="text-align:left;">
omega2
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.770
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.752
</td>
<td style="text-align:right;">
0.622
</td>
<td style="text-align:right;">
0.601
</td>
<td style="text-align:right;">
0.917
</td>
</tr>
<tr>
<td style="text-align:left;">
omega3
</td>
<td style="text-align:right;">
0.587
</td>
<td style="text-align:right;">
0.713
</td>
<td style="text-align:right;">
0.705
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.751
</td>
<td style="text-align:right;">
0.609
</td>
<td style="text-align:right;">
0.577
</td>
<td style="text-align:right;">
0.921
</td>
</tr>
<tr>
<td style="text-align:left;">
avevar
</td>
<td style="text-align:right;">
0.331
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
0.333
</td>
<td style="text-align:right;">
0.522
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
0.503
</td>
<td style="text-align:right;">
0.371
</td>
<td style="text-align:right;">
0.364
</td>
<td style="text-align:right;">
0.412
</td>
</tr>
<tr>
<td style="text-align:left;">
CR
</td>
<td style="text-align:right;">
0.597
</td>
<td style="text-align:right;">
0.729
</td>
<td style="text-align:right;">
0.779
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.755
</td>
<td style="text-align:right;">
0.643
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.951
</td>
</tr>
</tbody>
</table>

Alternative short version of FSS-BR

``` r
df <- rbind(reliability(fit3b), "CR"=compReliability(fit3b))
knitr::kable(t(df), digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
alpha
</th>
<th style="text-align:right;">
omega
</th>
<th style="text-align:right;">
omega2
</th>
<th style="text-align:right;">
omega3
</th>
<th style="text-align:right;">
avevar
</th>
<th style="text-align:right;">
CR
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
FSS
</td>
<td style="text-align:right;">
0.817
</td>
<td style="text-align:right;">
0.817
</td>
<td style="text-align:right;">
0.817
</td>
<td style="text-align:right;">
0.815
</td>
<td style="text-align:right;">
0.335
</td>
<td style="text-align:right;">
0.821
</td>
</tr>
</tbody>
</table>

### Convergent and discriminant validity

nine-multicorrelated model with some removed items

``` r
df <- convergentDiscriminantValidity(fit1b, Modelo1b, fss)
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
CR
</th>
<th style="text-align:right;">
AVE
</th>
<th style="text-align:right;">
VIF
</th>
<th style="text-align:right;">
VIF.i
</th>
<th style="text-align:right;">
CSB
</th>
<th style="text-align:right;">
MAA
</th>
<th style="text-align:right;">
CG
</th>
<th style="text-align:right;">
UF
</th>
<th style="text-align:right;">
CTH
</th>
<th style="text-align:right;">
SC
</th>
<th style="text-align:right;">
LSC
</th>
<th style="text-align:right;">
TT
</th>
<th style="text-align:right;">
AE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CSB
</td>
<td style="text-align:right;">
0.597
</td>
<td style="text-align:right;">
0.331
</td>
<td style="text-align:right;">
1.636
</td>
<td style="text-align:right;">
1.334
</td>
<td style="text-align:right;">
0.575
</td>
<td style="text-align:right;">
0.495
</td>
<td style="text-align:right;">
0.649
</td>
<td style="text-align:right;">
0.637
</td>
<td style="text-align:right;">
0.612
</td>
<td style="text-align:right;">
0.737
</td>
<td style="text-align:right;">
0.591
</td>
<td style="text-align:right;">
0.463
</td>
<td style="text-align:right;">
0.780
</td>
</tr>
<tr>
<td style="text-align:left;">
MAA
</td>
<td style="text-align:right;">
0.729
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
1.408
</td>
<td style="text-align:right;">
1.630
</td>
<td style="text-align:right;">
0.473
</td>
<td style="text-align:right;">
0.685
</td>
<td style="text-align:right;">
0.344
</td>
<td style="text-align:right;">
0.364
</td>
<td style="text-align:right;">
0.450
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
0.473
</td>
<td style="text-align:right;">
0.668
</td>
<td style="text-align:right;">
0.440
</td>
</tr>
<tr>
<td style="text-align:left;">
CG
</td>
<td style="text-align:right;">
0.630
</td>
<td style="text-align:right;">
0.367
</td>
<td style="text-align:right;">
1.855
</td>
<td style="text-align:right;">
1.281
</td>
<td style="text-align:right;">
0.620
</td>
<td style="text-align:right;">
0.160
</td>
<td style="text-align:right;">
0.606
</td>
<td style="text-align:right;">
0.978
</td>
<td style="text-align:right;">
0.471
</td>
<td style="text-align:right;">
0.648
</td>
<td style="text-align:right;">
0.366
</td>
<td style="text-align:right;">
0.272
</td>
<td style="text-align:right;">
0.435
</td>
</tr>
<tr>
<td style="text-align:left;">
UF
</td>
<td style="text-align:right;">
0.679
</td>
<td style="text-align:right;">
0.341
</td>
<td style="text-align:right;">
2.032
</td>
<td style="text-align:right;">
1.767
</td>
<td style="text-align:right;">
0.627
</td>
<td style="text-align:right;">
0.243
</td>
<td style="text-align:right;">
0.912
</td>
<td style="text-align:right;">
0.584
</td>
<td style="text-align:right;">
0.538
</td>
<td style="text-align:right;">
0.732
</td>
<td style="text-align:right;">
0.428
</td>
<td style="text-align:right;">
0.300
</td>
<td style="text-align:right;">
0.425
</td>
</tr>
<tr>
<td style="text-align:left;">
CTH
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.522
</td>
<td style="text-align:right;">
1.854
</td>
<td style="text-align:right;">
1.677
</td>
<td style="text-align:right;">
0.613
</td>
<td style="text-align:right;">
0.463
</td>
<td style="text-align:right;">
0.471
</td>
<td style="text-align:right;">
0.580
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.845
</td>
<td style="text-align:right;">
0.605
</td>
<td style="text-align:right;">
0.531
</td>
<td style="text-align:right;">
0.536
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
2.302
</td>
<td style="text-align:right;">
1.606
</td>
<td style="text-align:right;">
0.738
</td>
<td style="text-align:right;">
0.502
</td>
<td style="text-align:right;">
0.666
</td>
<td style="text-align:right;">
0.771
</td>
<td style="text-align:right;">
0.840
</td>
<td style="text-align:right;">
0.703
</td>
<td style="text-align:right;">
0.619
</td>
<td style="text-align:right;">
0.572
</td>
<td style="text-align:right;">
0.540
</td>
</tr>
<tr>
<td style="text-align:left;">
LSC
</td>
<td style="text-align:right;">
0.755
</td>
<td style="text-align:right;">
0.503
</td>
<td style="text-align:right;">
1.467
</td>
<td style="text-align:right;">
1.588
</td>
<td style="text-align:right;">
0.584
</td>
<td style="text-align:right;">
0.459
</td>
<td style="text-align:right;">
0.383
</td>
<td style="text-align:right;">
0.467
</td>
<td style="text-align:right;">
0.604
</td>
<td style="text-align:right;">
0.617
</td>
<td style="text-align:right;">
0.709
</td>
<td style="text-align:right;">
0.481
</td>
<td style="text-align:right;">
0.505
</td>
</tr>
<tr>
<td style="text-align:left;">
TT
</td>
<td style="text-align:right;">
0.643
</td>
<td style="text-align:right;">
0.371
</td>
<td style="text-align:right;">
1.607
</td>
<td style="text-align:right;">
1.459
</td>
<td style="text-align:right;">
0.484
</td>
<td style="text-align:right;">
0.663
</td>
<td style="text-align:right;">
0.174
</td>
<td style="text-align:right;">
0.217
</td>
<td style="text-align:right;">
0.548
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.495
</td>
<td style="text-align:right;">
0.609
</td>
<td style="text-align:right;">
0.804
</td>
</tr>
<tr>
<td style="text-align:left;">
AE
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.364
</td>
<td style="text-align:right;">
1.602
</td>
<td style="text-align:right;">
1.304
</td>
<td style="text-align:right;">
0.667
</td>
<td style="text-align:right;">
0.505
</td>
<td style="text-align:right;">
0.356
</td>
<td style="text-align:right;">
0.390
</td>
<td style="text-align:right;">
0.564
</td>
<td style="text-align:right;">
0.569
</td>
<td style="text-align:right;">
0.519
</td>
<td style="text-align:right;">
0.757
</td>
<td style="text-align:right;">
0.604
</td>
</tr>
</tbody>
</table>

Eight-multicorrelated model with some removed items

``` r
df <- convergentDiscriminantValidity(fit1c, Modelo1c, fss)
knitr::kable(df, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
CR
</th>
<th style="text-align:right;">
AVE
</th>
<th style="text-align:right;">
VIF
</th>
<th style="text-align:right;">
VIF.i
</th>
<th style="text-align:right;">
CSB
</th>
<th style="text-align:right;">
MAA
</th>
<th style="text-align:right;">
CG\_UF
</th>
<th style="text-align:right;">
CTH
</th>
<th style="text-align:right;">
SC
</th>
<th style="text-align:right;">
LSC
</th>
<th style="text-align:right;">
TT
</th>
<th style="text-align:right;">
AE
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
CSB
</td>
<td style="text-align:right;">
0.597
</td>
<td style="text-align:right;">
0.331
</td>
<td style="text-align:right;">
1.635
</td>
<td style="text-align:right;">
1.334
</td>
<td style="text-align:right;">
0.575
</td>
<td style="text-align:right;">
0.495
</td>
<td style="text-align:right;">
0.647
</td>
<td style="text-align:right;">
0.612
</td>
<td style="text-align:right;">
0.737
</td>
<td style="text-align:right;">
0.591
</td>
<td style="text-align:right;">
0.463
</td>
<td style="text-align:right;">
0.780
</td>
</tr>
<tr>
<td style="text-align:left;">
MAA
</td>
<td style="text-align:right;">
0.729
</td>
<td style="text-align:right;">
0.469
</td>
<td style="text-align:right;">
1.406
</td>
<td style="text-align:right;">
1.630
</td>
<td style="text-align:right;">
0.473
</td>
<td style="text-align:right;">
0.685
</td>
<td style="text-align:right;">
0.357
</td>
<td style="text-align:right;">
0.450
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
0.473
</td>
<td style="text-align:right;">
0.668
</td>
<td style="text-align:right;">
0.440
</td>
</tr>
<tr>
<td style="text-align:left;">
CG\_UF
</td>
<td style="text-align:right;">
0.779
</td>
<td style="text-align:right;">
0.333
</td>
<td style="text-align:right;">
1.566
</td>
<td style="text-align:right;">
2.095
</td>
<td style="text-align:right;">
0.640
</td>
<td style="text-align:right;">
0.215
</td>
<td style="text-align:right;">
0.577
</td>
<td style="text-align:right;">
0.512
</td>
<td style="text-align:right;">
0.700
</td>
<td style="text-align:right;">
0.404
</td>
<td style="text-align:right;">
0.289
</td>
<td style="text-align:right;">
0.432
</td>
</tr>
<tr>
<td style="text-align:left;">
CTH
</td>
<td style="text-align:right;">
0.766
</td>
<td style="text-align:right;">
0.522
</td>
<td style="text-align:right;">
1.850
</td>
<td style="text-align:right;">
1.677
</td>
<td style="text-align:right;">
0.613
</td>
<td style="text-align:right;">
0.464
</td>
<td style="text-align:right;">
0.549
</td>
<td style="text-align:right;">
0.723
</td>
<td style="text-align:right;">
0.845
</td>
<td style="text-align:right;">
0.605
</td>
<td style="text-align:right;">
0.531
</td>
<td style="text-align:right;">
0.536
</td>
</tr>
<tr>
<td style="text-align:left;">
SC
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.494
</td>
<td style="text-align:right;">
2.300
</td>
<td style="text-align:right;">
1.606
</td>
<td style="text-align:right;">
0.738
</td>
<td style="text-align:right;">
0.502
</td>
<td style="text-align:right;">
0.745
</td>
<td style="text-align:right;">
0.840
</td>
<td style="text-align:right;">
0.703
</td>
<td style="text-align:right;">
0.619
</td>
<td style="text-align:right;">
0.572
</td>
<td style="text-align:right;">
0.540
</td>
</tr>
<tr>
<td style="text-align:left;">
LSC
</td>
<td style="text-align:right;">
0.755
</td>
<td style="text-align:right;">
0.503
</td>
<td style="text-align:right;">
1.467
</td>
<td style="text-align:right;">
1.588
</td>
<td style="text-align:right;">
0.584
</td>
<td style="text-align:right;">
0.459
</td>
<td style="text-align:right;">
0.444
</td>
<td style="text-align:right;">
0.603
</td>
<td style="text-align:right;">
0.617
</td>
<td style="text-align:right;">
0.709
</td>
<td style="text-align:right;">
0.481
</td>
<td style="text-align:right;">
0.505
</td>
</tr>
<tr>
<td style="text-align:left;">
TT
</td>
<td style="text-align:right;">
0.643
</td>
<td style="text-align:right;">
0.371
</td>
<td style="text-align:right;">
1.605
</td>
<td style="text-align:right;">
1.459
</td>
<td style="text-align:right;">
0.485
</td>
<td style="text-align:right;">
0.663
</td>
<td style="text-align:right;">
0.204
</td>
<td style="text-align:right;">
0.548
</td>
<td style="text-align:right;">
0.592
</td>
<td style="text-align:right;">
0.495
</td>
<td style="text-align:right;">
0.609
</td>
<td style="text-align:right;">
0.804
</td>
</tr>
<tr>
<td style="text-align:left;">
AE
</td>
<td style="text-align:right;">
0.615
</td>
<td style="text-align:right;">
0.364
</td>
<td style="text-align:right;">
1.602
</td>
<td style="text-align:right;">
1.304
</td>
<td style="text-align:right;">
0.667
</td>
<td style="text-align:right;">
0.506
</td>
<td style="text-align:right;">
0.385
</td>
<td style="text-align:right;">
0.564
</td>
<td style="text-align:right;">
0.570
</td>
<td style="text-align:right;">
0.519
</td>
<td style="text-align:right;">
0.757
</td>
<td style="text-align:right;">
0.604
</td>
</tr>
</tbody>
</table>

*Observations (**convergent and discriminant validity**)*:

-   If AVE is less than 0.5, but CR is higher than 0.6 in all the
    factors, the convergent validity of the construct is still adequate
    (Fornell & Larcker, 1981).
-   If the VIF &gt; 5, there is mulcollinearity issues in the dataset.
    The possible solutions are: Remove items with maximal VIF in the
    items for factors (VFI.i); or combine constructs with more than .80
    or .85 correlation values.
-   The lower triangualar part of the table corresponds with
    correlations between factors. The square root of AVE is located in
    the diagonally of the matrix. For discriminate validity, the
    diagonal elements should be larger than the off-diagonal elements
    (correlations for factors).
-   The upper triangular part of the table displays the
    heterotrait-monotrait ratio (HTMT) of the correlations (Henseler,
    Ringlet & Sarstedt, 2015). Lower values indicate higher levels of
    discriminant validity. HTMT values greater than 0.85 (&gt; 0.85)
    indicate discriminant validity issues.

# Item quality analysis based on IRT (Item Response Theory)

Loadings fss data for performing IRT analysis

``` r
ls <- as.data.frame(standardizedSolution(fit1b))
ls <- ls[ls$op == '=~',]

fss2 <- fss[,ls$rhs]
```

## IRT analysis in the long version of the DFS-BR

``` r
dados_tri<-mirt(fss2, 1, itemtype='graded')
```

    ## Iteration: 1, Log-Lik: -13760.713, Max-Change: 2.18866Iteration: 2, Log-Lik: -13585.246, Max-Change: 0.45650Iteration: 3, Log-Lik: -13561.102, Max-Change: 0.18810Iteration: 4, Log-Lik: -13554.394, Max-Change: 0.10376Iteration: 5, Log-Lik: -13551.471, Max-Change: 0.07220Iteration: 6, Log-Lik: -13549.739, Max-Change: 0.04903Iteration: 7, Log-Lik: -13547.647, Max-Change: 0.02691Iteration: 8, Log-Lik: -13547.158, Max-Change: 0.02167Iteration: 9, Log-Lik: -13546.829, Max-Change: 0.01827Iteration: 10, Log-Lik: -13546.224, Max-Change: 0.00709Iteration: 11, Log-Lik: -13546.173, Max-Change: 0.00670Iteration: 12, Log-Lik: -13546.138, Max-Change: 0.00600Iteration: 13, Log-Lik: -13546.057, Max-Change: 0.00332Iteration: 14, Log-Lik: -13546.054, Max-Change: 0.00134Iteration: 15, Log-Lik: -13546.052, Max-Change: 0.00106Iteration: 16, Log-Lik: -13546.052, Max-Change: 0.00093Iteration: 17, Log-Lik: -13546.051, Max-Change: 0.00078Iteration: 18, Log-Lik: -13546.051, Max-Change: 0.00044Iteration: 19, Log-Lik: -13546.051, Max-Change: 0.00056Iteration: 20, Log-Lik: -13546.050, Max-Change: 0.00045Iteration: 21, Log-Lik: -13546.050, Max-Change: 0.00035Iteration: 22, Log-Lik: -13546.050, Max-Change: 0.00032Iteration: 23, Log-Lik: -13546.050, Max-Change: 0.00029Iteration: 24, Log-Lik: -13546.050, Max-Change: 0.00027Iteration: 25, Log-Lik: -13546.050, Max-Change: 0.00026Iteration: 26, Log-Lik: -13546.050, Max-Change: 0.00022Iteration: 27, Log-Lik: -13546.050, Max-Change: 0.00020Iteration: 28, Log-Lik: -13546.050, Max-Change: 0.00019Iteration: 29, Log-Lik: -13546.050, Max-Change: 0.00017Iteration: 30, Log-Lik: -13546.050, Max-Change: 0.00016Iteration: 31, Log-Lik: -13546.050, Max-Change: 0.00014Iteration: 32, Log-Lik: -13546.050, Max-Change: 0.00013Iteration: 33, Log-Lik: -13546.050, Max-Change: 0.00012Iteration: 34, Log-Lik: -13546.050, Max-Change: 0.00011Iteration: 35, Log-Lik: -13546.050, Max-Change: 0.00011Iteration: 36, Log-Lik: -13546.050, Max-Change: 0.00010

### Estimate parameters

``` r
params <- coef(dados_tri, simplify=TRUE, IRTpars=TRUE)
knitr::kable(params$items, digits = 3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
a
</th>
<th style="text-align:right;">
b1
</th>
<th style="text-align:right;">
b2
</th>
<th style="text-align:right;">
b3
</th>
<th style="text-align:right;">
b4
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Q1
</td>
<td style="text-align:right;">
1.327
</td>
<td style="text-align:right;">
-3.829
</td>
<td style="text-align:right;">
-2.629
</td>
<td style="text-align:right;">
-1.340
</td>
<td style="text-align:right;">
1.044
</td>
</tr>
<tr>
<td style="text-align:left;">
Q10
</td>
<td style="text-align:right;">
1.399
</td>
<td style="text-align:right;">
-3.319
</td>
<td style="text-align:right;">
-1.957
</td>
<td style="text-align:right;">
-0.628
</td>
<td style="text-align:right;">
0.889
</td>
</tr>
<tr>
<td style="text-align:left;">
Q19
</td>
<td style="text-align:right;">
0.851
</td>
<td style="text-align:right;">
-4.502
</td>
<td style="text-align:right;">
-2.207
</td>
<td style="text-align:right;">
-0.460
</td>
<td style="text-align:right;">
2.038
</td>
</tr>
<tr>
<td style="text-align:left;">
Q2
</td>
<td style="text-align:right;">
1.025
</td>
<td style="text-align:right;">
-2.714
</td>
<td style="text-align:right;">
-1.047
</td>
<td style="text-align:right;">
0.063
</td>
<td style="text-align:right;">
2.230
</td>
</tr>
<tr>
<td style="text-align:left;">
Q11
</td>
<td style="text-align:right;">
1.321
</td>
<td style="text-align:right;">
-2.962
</td>
<td style="text-align:right;">
-1.302
</td>
<td style="text-align:right;">
-0.187
</td>
<td style="text-align:right;">
1.753
</td>
</tr>
<tr>
<td style="text-align:left;">
Q20
</td>
<td style="text-align:right;">
0.872
</td>
<td style="text-align:right;">
-3.974
</td>
<td style="text-align:right;">
-1.929
</td>
<td style="text-align:right;">
-0.455
</td>
<td style="text-align:right;">
1.564
</td>
</tr>
<tr>
<td style="text-align:left;">
Q3
</td>
<td style="text-align:right;">
0.919
</td>
<td style="text-align:right;">
-4.546
</td>
<td style="text-align:right;">
-1.505
</td>
<td style="text-align:right;">
0.114
</td>
<td style="text-align:right;">
2.701
</td>
</tr>
<tr>
<td style="text-align:left;">
Q12
</td>
<td style="text-align:right;">
0.439
</td>
<td style="text-align:right;">
-6.482
</td>
<td style="text-align:right;">
-1.821
</td>
<td style="text-align:right;">
1.007
</td>
<td style="text-align:right;">
5.916
</td>
</tr>
<tr>
<td style="text-align:left;">
Q21
</td>
<td style="text-align:right;">
1.413
</td>
<td style="text-align:right;">
-2.898
</td>
<td style="text-align:right;">
-1.394
</td>
<td style="text-align:right;">
0.041
</td>
<td style="text-align:right;">
1.902
</td>
</tr>
<tr>
<td style="text-align:left;">
Q4
</td>
<td style="text-align:right;">
1.923
</td>
<td style="text-align:right;">
-2.984
</td>
<td style="text-align:right;">
-1.683
</td>
<td style="text-align:right;">
-0.580
</td>
<td style="text-align:right;">
1.252
</td>
</tr>
<tr>
<td style="text-align:left;">
Q13
</td>
<td style="text-align:right;">
0.574
</td>
<td style="text-align:right;">
-6.085
</td>
<td style="text-align:right;">
-1.804
</td>
<td style="text-align:right;">
0.590
</td>
<td style="text-align:right;">
4.706
</td>
</tr>
<tr>
<td style="text-align:left;">
Q22
</td>
<td style="text-align:right;">
0.716
</td>
<td style="text-align:right;">
-4.978
</td>
<td style="text-align:right;">
-1.494
</td>
<td style="text-align:right;">
0.004
</td>
<td style="text-align:right;">
3.756
</td>
</tr>
<tr>
<td style="text-align:left;">
Q31
</td>
<td style="text-align:right;">
1.107
</td>
<td style="text-align:right;">
-4.584
</td>
<td style="text-align:right;">
-1.923
</td>
<td style="text-align:right;">
-0.455
</td>
<td style="text-align:right;">
2.001
</td>
</tr>
<tr>
<td style="text-align:left;">
Q5
</td>
<td style="text-align:right;">
1.587
</td>
<td style="text-align:right;">
-3.133
</td>
<td style="text-align:right;">
-1.381
</td>
<td style="text-align:right;">
-0.283
</td>
<td style="text-align:right;">
1.488
</td>
</tr>
<tr>
<td style="text-align:left;">
Q23
</td>
<td style="text-align:right;">
1.763
</td>
<td style="text-align:right;">
-2.704
</td>
<td style="text-align:right;">
-1.311
</td>
<td style="text-align:right;">
-0.253
</td>
<td style="text-align:right;">
1.397
</td>
</tr>
<tr>
<td style="text-align:left;">
Q32
</td>
<td style="text-align:right;">
1.598
</td>
<td style="text-align:right;">
-2.948
</td>
<td style="text-align:right;">
-1.327
</td>
<td style="text-align:right;">
-0.550
</td>
<td style="text-align:right;">
1.070
</td>
</tr>
<tr>
<td style="text-align:left;">
Q15
</td>
<td style="text-align:right;">
1.764
</td>
<td style="text-align:right;">
-3.268
</td>
<td style="text-align:right;">
-1.598
</td>
<td style="text-align:right;">
-0.440
</td>
<td style="text-align:right;">
1.396
</td>
</tr>
<tr>
<td style="text-align:left;">
Q24
</td>
<td style="text-align:right;">
2.393
</td>
<td style="text-align:right;">
-3.393
</td>
<td style="text-align:right;">
-1.463
</td>
<td style="text-align:right;">
-0.432
</td>
<td style="text-align:right;">
1.114
</td>
</tr>
<tr>
<td style="text-align:left;">
Q33
</td>
<td style="text-align:right;">
1.941
</td>
<td style="text-align:right;">
-3.284
</td>
<td style="text-align:right;">
-1.599
</td>
<td style="text-align:right;">
-0.578
</td>
<td style="text-align:right;">
1.358
</td>
</tr>
<tr>
<td style="text-align:left;">
Q7
</td>
<td style="text-align:right;">
1.418
</td>
<td style="text-align:right;">
-2.930
</td>
<td style="text-align:right;">
-1.396
</td>
<td style="text-align:right;">
-0.346
</td>
<td style="text-align:right;">
1.593
</td>
</tr>
<tr>
<td style="text-align:left;">
Q16
</td>
<td style="text-align:right;">
1.127
</td>
<td style="text-align:right;">
-2.996
</td>
<td style="text-align:right;">
-0.994
</td>
<td style="text-align:right;">
-0.055
</td>
<td style="text-align:right;">
1.754
</td>
</tr>
<tr>
<td style="text-align:left;">
Q25
</td>
<td style="text-align:right;">
1.286
</td>
<td style="text-align:right;">
-3.170
</td>
<td style="text-align:right;">
-1.236
</td>
<td style="text-align:right;">
-0.191
</td>
<td style="text-align:right;">
1.967
</td>
</tr>
<tr>
<td style="text-align:left;">
Q8
</td>
<td style="text-align:right;">
0.418
</td>
<td style="text-align:right;">
-4.794
</td>
<td style="text-align:right;">
-1.907
</td>
<td style="text-align:right;">
1.052
</td>
<td style="text-align:right;">
4.825
</td>
</tr>
<tr>
<td style="text-align:left;">
Q17
</td>
<td style="text-align:right;">
1.243
</td>
<td style="text-align:right;">
-3.056
</td>
<td style="text-align:right;">
-1.206
</td>
<td style="text-align:right;">
0.101
</td>
<td style="text-align:right;">
2.164
</td>
</tr>
<tr>
<td style="text-align:left;">
Q26
</td>
<td style="text-align:right;">
1.248
</td>
<td style="text-align:right;">
-2.835
</td>
<td style="text-align:right;">
-1.178
</td>
<td style="text-align:right;">
-0.041
</td>
<td style="text-align:right;">
1.944
</td>
</tr>
<tr>
<td style="text-align:left;">
Q9
</td>
<td style="text-align:right;">
0.634
</td>
<td style="text-align:right;">
-5.107
</td>
<td style="text-align:right;">
-2.746
</td>
<td style="text-align:right;">
-0.518
</td>
<td style="text-align:right;">
2.864
</td>
</tr>
<tr>
<td style="text-align:left;">
Q18
</td>
<td style="text-align:right;">
0.684
</td>
<td style="text-align:right;">
-3.856
</td>
<td style="text-align:right;">
-1.204
</td>
<td style="text-align:right;">
0.913
</td>
<td style="text-align:right;">
3.811
</td>
</tr>
<tr>
<td style="text-align:left;">
Q27
</td>
<td style="text-align:right;">
1.807
</td>
<td style="text-align:right;">
-2.601
</td>
<td style="text-align:right;">
-1.308
</td>
<td style="text-align:right;">
-0.117
</td>
<td style="text-align:right;">
1.539
</td>
</tr>
</tbody>
</table>

### Graphs of test information curve

``` r
plot(dados_tri, type='infotrace')
```

![](script20210331_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

``` r
plot(dados_tri, type='info')
```

![](script20210331_files/figure-gfm/unnamed-chunk-47-1.png)<!-- -->

Graph of Test Information and Standard Errors

``` r
plot(dados_tri, type='infoSE')
```

![](script20210331_files/figure-gfm/unnamed-chunk-48-1.png)<!-- -->

## IRT analysis in the short version of the alt FSS-BR

Loadings fss data for performing IRT analysis

``` r
ls <- as.data.frame(standardizedSolution(fit3b))
ls <- ls[ls$op == '=~',]

fss3 <- fss[,ls$rhs]
```

``` r
dados_tri3 <-mirt(fss3, 1, itemtype='graded')
```

    ## Iteration: 1, Log-Lik: -4353.295, Max-Change: 2.53219Iteration: 2, Log-Lik: -4279.915, Max-Change: 0.54013Iteration: 3, Log-Lik: -4264.097, Max-Change: 0.27634Iteration: 4, Log-Lik: -4258.700, Max-Change: 0.14424Iteration: 5, Log-Lik: -4256.320, Max-Change: 0.09331Iteration: 6, Log-Lik: -4255.187, Max-Change: 0.06534Iteration: 7, Log-Lik: -4254.492, Max-Change: 0.03686Iteration: 8, Log-Lik: -4254.280, Max-Change: 0.02584Iteration: 9, Log-Lik: -4254.170, Max-Change: 0.01817Iteration: 10, Log-Lik: -4254.061, Max-Change: 0.00766Iteration: 11, Log-Lik: -4254.051, Max-Change: 0.00503Iteration: 12, Log-Lik: -4254.046, Max-Change: 0.00284Iteration: 13, Log-Lik: -4254.043, Max-Change: 0.00288Iteration: 14, Log-Lik: -4254.042, Max-Change: 0.00176Iteration: 15, Log-Lik: -4254.041, Max-Change: 0.00161Iteration: 16, Log-Lik: -4254.040, Max-Change: 0.00030Iteration: 17, Log-Lik: -4254.040, Max-Change: 0.00016Iteration: 18, Log-Lik: -4254.040, Max-Change: 0.00016Iteration: 19, Log-Lik: -4254.040, Max-Change: 0.00054Iteration: 20, Log-Lik: -4254.040, Max-Change: 0.00053Iteration: 21, Log-Lik: -4254.040, Max-Change: 0.00014Iteration: 22, Log-Lik: -4254.040, Max-Change: 0.00012Iteration: 23, Log-Lik: -4254.040, Max-Change: 0.00029Iteration: 24, Log-Lik: -4254.040, Max-Change: 0.00008

### Estimate parameters

``` r
params3 <- coef(dados_tri3, simplify=TRUE, IRTpars=TRUE)
knitr::kable(params3$items, digits=3)
```

<table>
<thead>
<tr>
<th style="text-align:left;">
</th>
<th style="text-align:right;">
a
</th>
<th style="text-align:right;">
b1
</th>
<th style="text-align:right;">
b2
</th>
<th style="text-align:right;">
b3
</th>
<th style="text-align:right;">
b4
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
Q10
</td>
<td style="text-align:right;">
1.249
</td>
<td style="text-align:right;">
-3.610
</td>
<td style="text-align:right;">
-2.108
</td>
<td style="text-align:right;">
-0.660
</td>
<td style="text-align:right;">
0.988
</td>
</tr>
<tr>
<td style="text-align:left;">
Q11
</td>
<td style="text-align:right;">
1.336
</td>
<td style="text-align:right;">
-2.963
</td>
<td style="text-align:right;">
-1.297
</td>
<td style="text-align:right;">
-0.166
</td>
<td style="text-align:right;">
1.775
</td>
</tr>
<tr>
<td style="text-align:left;">
Q21
</td>
<td style="text-align:right;">
1.210
</td>
<td style="text-align:right;">
-3.258
</td>
<td style="text-align:right;">
-1.542
</td>
<td style="text-align:right;">
0.082
</td>
<td style="text-align:right;">
2.117
</td>
</tr>
<tr>
<td style="text-align:left;">
Q4
</td>
<td style="text-align:right;">
1.793
</td>
<td style="text-align:right;">
-3.113
</td>
<td style="text-align:right;">
-1.764
</td>
<td style="text-align:right;">
-0.603
</td>
<td style="text-align:right;">
1.330
</td>
</tr>
<tr>
<td style="text-align:left;">
Q32
</td>
<td style="text-align:right;">
1.406
</td>
<td style="text-align:right;">
-3.232
</td>
<td style="text-align:right;">
-1.430
</td>
<td style="text-align:right;">
-0.583
</td>
<td style="text-align:right;">
1.190
</td>
</tr>
<tr>
<td style="text-align:left;">
Q24
</td>
<td style="text-align:right;">
2.480
</td>
<td style="text-align:right;">
-3.418
</td>
<td style="text-align:right;">
-1.476
</td>
<td style="text-align:right;">
-0.423
</td>
<td style="text-align:right;">
1.145
</td>
</tr>
<tr>
<td style="text-align:left;">
Q7
</td>
<td style="text-align:right;">
1.119
</td>
<td style="text-align:right;">
-3.470
</td>
<td style="text-align:right;">
-1.619
</td>
<td style="text-align:right;">
-0.375
</td>
<td style="text-align:right;">
1.893
</td>
</tr>
<tr>
<td style="text-align:left;">
Q17
</td>
<td style="text-align:right;">
1.497
</td>
<td style="text-align:right;">
-2.720
</td>
<td style="text-align:right;">
-1.091
</td>
<td style="text-align:right;">
0.097
</td>
<td style="text-align:right;">
1.958
</td>
</tr>
<tr>
<td style="text-align:left;">
Q27
</td>
<td style="text-align:right;">
1.956
</td>
<td style="text-align:right;">
-2.525
</td>
<td style="text-align:right;">
-1.276
</td>
<td style="text-align:right;">
-0.102
</td>
<td style="text-align:right;">
1.529
</td>
</tr>
</tbody>
</table>

### Graphs of test information curve

``` r
plot(dados_tri3,type='infotrace')
```

![](script20210331_files/figure-gfm/unnamed-chunk-52-1.png)<!-- -->

``` r
plot(dados_tri3,type='info')
```

![](script20210331_files/figure-gfm/unnamed-chunk-53-1.png)<!-- -->

Graph of Test Information and Standard Errors

``` r
plot(dados_tri3,type='infoSE')
```

![](script20210331_files/figure-gfm/unnamed-chunk-54-1.png)<!-- -->

------------------------------------------------------------------------
