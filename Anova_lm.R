rm(list = ls())

library(broom)
library(ggplot2)

load("C:/Users/MFARR/Documents/R_files/Spotfire.data/karnes.RData")
write.csv(output, "karnes_output.csv")

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


mod <- lm(CUM12MOLiquid ~ LBS_FT_CLEAN, data = output)
summary(mod)
augment(mod)

ggplotRegression(mod)

X <- model.matrix(mod)
sigma2 <- sum((output$CUM12MOLiquid - fitted(mod))^2) / (nrow(X) - ncol(X))

sqrt(sigma2)
sqrt(diag(solve(crossprod(X))) * sigma2)


###-----------------------------------------------------------------------
#need to understand the difference between lm, aov, and anova summary (p-value)

summary(aov(CUM12MOLiquid ~ OperatorName, data = output))
  

anova(mod)
