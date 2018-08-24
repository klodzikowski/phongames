## ----echo=FALSE----------------------------------------------------------
required <- c("survey")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5)
library(jtools)

## ------------------------------------------------------------------------
fiti <- lm(Income ~ Illiteracy * Murder, data = as.data.frame(state.x77))
summ(fiti)

## ------------------------------------------------------------------------
summ(fiti, scale = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder")

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", modxvals = "plus-minus")

## ------------------------------------------------------------------------
fitiris <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species")

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species", plot.points = TRUE)

## ------------------------------------------------------------------------
fiti <- lm(Income ~ Illiteracy * Murder, data = as.data.frame(state.x77),
           weights = Population)
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", interval = TRUE, int.width = 0.8)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder",
              x.label = "Custom X Label", y.label = "Custom Y Label",
              main.title = "Sample Plot",  legend.main = "Custom Legend Title",
              color.class = "Oranges")

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species") + theme_apa()

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = FALSE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, modxvals = c(0, 5, 10),
           johnson_neyman = FALSE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE)

## ------------------------------------------------------------------------
johnson_neyman(fiti, pred = Illiteracy, modx = Murder, alpha = 0.01)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE,
           control.fdr = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, cond.int = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, robust = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, scale = TRUE, 
           centered = "all")

## ------------------------------------------------------------------------
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc = ~fpc)
regmodel <- svyglm(api00 ~ avg.ed * growth, design = dstrat)

probe_interaction(regmodel, pred = growth, modx = avg.ed, cond.int = TRUE,
                  interval = TRUE,  jnplot = TRUE)

## ------------------------------------------------------------------------
out <- probe_interaction(regmodel, pred = growth, modx = avg.ed,
                         cond.int = TRUE, interval = TRUE, jnplot = TRUE)
names(out)

## ------------------------------------------------------------------------
fita3 <- lm(rating ~ privileges * critical * learning, data = attitude)
probe_interaction(fita3, pred = critical, modx = learning, mod2 = privileges)

## ------------------------------------------------------------------------
mtcars$cyl <- factor(mtcars$cyl,
                     labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
fitc3 <- lm(mpg ~ hp * wt * cyl, data = mtcars)
interact_plot(fitc3, pred = hp, modx = wt, mod2 = cyl) + 
  theme_apa(legend.pos = "bottomright")

## ----fig.height = 8------------------------------------------------------
regmodel3 <- svyglm(api00 ~ avg.ed * growth * enroll, design = dstrat)
sim_slopes(regmodel3, pred = growth, modx = avg.ed, mod2 = enroll,
          jnplot = TRUE)

## ------------------------------------------------------------------------
set.seed(5)
x <- rnorm(100)
m <- rnorm(100)
prob <- boot::inv.logit(.25 + .3*x + .3*m + -.5*(x*m) + rnorm(100))
y <- rep(0, 100)
y[prob >= .5] <- 1
logit_fit <- glm(y ~ x * m, family = binomial)

## ------------------------------------------------------------------------
summ(logit_fit)

## ------------------------------------------------------------------------
interact_plot(logit_fit, pred = x, modx = m)

