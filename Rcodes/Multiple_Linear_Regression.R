################
###*-AOJI-*#####
################

################
##4.Regression2
##############

### Multiple Linear Regression
rm(list=ls())

dat <- read.csv(file='LM_multi.csv', sep=',', header=T)

# regression analysis using the function "glm()"
fit <- glm (y ~ x1+x2+x3, data = dat) # linear (regression) model fit
summary(fit)      # summary of the regression results

#prediction with model
input = with(dat, data.frame(x1=0.32, x2=0.477, x3=-0.981))
input$y = predict.glm(fit, newdata = input)
input