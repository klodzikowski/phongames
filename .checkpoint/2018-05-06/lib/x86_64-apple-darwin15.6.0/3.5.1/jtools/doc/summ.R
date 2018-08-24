## ----echo=FALSE----------------------------------------------------------
required <- c("survey", "huxtable", "broom")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5)
library(jtools)

## ------------------------------------------------------------------------
# Fit model
fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
summ(fit)

## ------------------------------------------------------------------------
summ(fit, robust = TRUE, robust.type = "HC3")

## ------------------------------------------------------------------------
summ(fit, scale = TRUE)

## ------------------------------------------------------------------------
summ(fit, scale = TRUE, n.sd = 2)

## ------------------------------------------------------------------------
summ(fit, center = TRUE)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, digits = 2)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, ci.width = .5, digits = 2)

## ------------------------------------------------------------------------
summ(fit, confint = TRUE, pvals = FALSE, digits = 2)

## ------------------------------------------------------------------------
fitg <- glm(vs ~ drat + mpg, data = mtcars, family = binomial)

summ(fitg)

## ------------------------------------------------------------------------
summ(fitg, odds.ratio = TRUE)

## ----message = FALSE, warning = FALSE------------------------------------
library(lme4)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

summ(fm1)

## ------------------------------------------------------------------------
plot_summs(fit)

## ------------------------------------------------------------------------
plot_summs(fit, scale = TRUE)

## ------------------------------------------------------------------------
fit2 <- lm(Income ~ Frost + Illiteracy + Murder + `HS Grad`,
           data = as.data.frame(state.x77))
plot_summs(fit, fit2, scale = TRUE)

## ------------------------------------------------------------------------
s1 <- summ(fit, scale = TRUE)
s2 <- summ(fit, scale = TRUE, robust = TRUE, robust.type = "HC0")
s3 <- summ(fit, scale = TRUE, robust = TRUE, robust.type = "HC3")

## ------------------------------------------------------------------------
plot_coefs(s1, s2, s3, model.names = c("OLS","HC0","HC3"))

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy)

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy, interval = TRUE)

## ------------------------------------------------------------------------
effect_plot(fit, pred = Illiteracy, interval = TRUE, plot.points = TRUE)

## ------------------------------------------------------------------------
effect_plot(fitg, pred = mpg)

## ------------------------------------------------------------------------
effect_plot(fitg, pred = mpg, plot.points = TRUE, jitter = 0)

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE)

## ----echo = FALSE, results = 'asis'--------------------------------------
export_summs(fit, fit2, scale = TRUE)

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE,
#               error_format = "[{conf.low}, {conf.high}]")

## ----echo = FALSE, results = 'asis'--------------------------------------
export_summs(fit, fit2, scale = TRUE,
             error_format = "[{conf.low}, {conf.high}]")

## ----eval = FALSE--------------------------------------------------------
#  export_summs(fit, fit2, scale = TRUE, to.word = TRUE, word.file = "test.docx")

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, model.fit = FALSE)

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 5)

## ------------------------------------------------------------------------
summ(fit, model.info = FALSE, digits = 1)

## ------------------------------------------------------------------------
options("jtools-digits" = 2)
summ(fit, model.info = FALSE)

## ----echo = F------------------------------------------------------------
options("jtools-digits" = NULL)

## ------------------------------------------------------------------------
j <- summ(fit, digits = 3)

j$coeftable

## ------------------------------------------------------------------------
summ(fit, vifs = TRUE)

