## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE, message=FALSE---------------------------------------
library(dySEM)
library(dplyr)
library(lavaan)
DRES <- as_tibble(DRES) 

## ----previewtib---------------------------------------------------------------
DRES

## ----scrape-------------------------------------------------------------------
dvn <- scrapeVarCross(DRES, x_order = "sip", x_stem = "PRQC", x_delim1="_",x_delim2=".",  distinguish_1="1", distinguish_2="2")


## ----configscript-------------------------------------------------------------

qual.indist.script <- scriptCFA(dvn, lvname = "Quality")


## ----scriptsequence-----------------------------------------------------------

qual.res.script <- scriptCFA(dvn, lvname = "Quality", constr_dy_meas = c("loadings", "intercepts", "residuals"), constr_dy_struct = c("none"))

qual.int.script <- scriptCFA(dvn, lvname = "Quality", constr_dy_meas = c("loadings", "intercepts"), constr_dy_struct = c("none"))

qual.load.script <- scriptCFA(dvn, lvname = "Quality", constr_dy_meas = c("loadings"), constr_dy_struct = c("none"))

qual.config.script <- scriptCFA(dvn, lvname = "Quality", constr_dy_meas = c("none"), constr_dy_struct = c("none"))


## ----modelfit, warning= FALSE-------------------------------------------------

#Fit fully indistinguishable model
qual.ind.fit <- lavaan::cfa(qual.indist.script, data = DRES, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

#Fit residual invariance model
qual.res.fit <- lavaan::cfa(qual.res.script, data = DRES, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

#Fit intercept invariance model
qual.int.fit <- lavaan::cfa(qual.int.script, data = DRES, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

#Fit loading invariance model
qual.load.fit <- lavaan::cfa(qual.load.script, data = DRES, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)

#Fit configural invariance model
qual.config.fit <- lavaan::cfa(qual.config.script, data = DRES, std.lv = FALSE, auto.fix.first= FALSE, meanstructure = TRUE)


## ----summary, eval = FALSE----------------------------------------------------
# summary(qual.config.fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

## ----anova--------------------------------------------------------------------
anova(qual.config.fit, qual.load.fit, qual.int.fit, qual.res.fit, qual.ind.fit)

## ----dyoutput, eval = FALSE---------------------------------------------------
# outputParamTab(dvn, model = "cfa", fit = qual.indist.fit,
#                tabletype = "measurement", writeTo = tempdir(),
#                fileName = "cfa_indist")
# 
# outputParamFig(fit = qual.indist.fit, figtype = "standardized",
#                writeTo = tempdir(),
#                fileName = "cfa_indist")

