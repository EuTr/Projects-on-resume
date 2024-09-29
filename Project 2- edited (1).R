##########

#Chau Tran
#
#Project 2
#Fall 2023
##########

library(tidyverse)
library(magrittr)
library(GGally)
library(car)
library(forecast)
library(glmnet)
library(olsrr)
library(lmtest)
library(lmvar)
library(MASS)

dat = read.csv("http://www.jpstats.org/data/car_price.csv")

####################  Model Selection  ########################
full = lm(price~., data = dat, y=T, x=T)

fit.none = lm(price~1, data = dat)

fit.fsel = step(fit.none, scope =formula(full), direction = "forward", trace=T)

fit.fs = step(fit.none, scope =formula(full), direction = "both", trace=T)

fit.belim = step(full, direction = "backward",  trace=T)

fit.bs = step(full, direction = "both", trace=T)

summary(full)
summary(fit.fsel)
summary(fit.fs)
summary(fit.belim)
summary(fit.bs)

# check lasso
Y = dat$price %>% as.matrix()
form = reformulate(names(dat[,1:23]))
X = model.matrix(formula(form), data = dat)

fit.lasso = cv.glmnet(X,Y,alpha = 1)
plot(fit.lasso)
lasso.coef = predict(fit.lasso, s = "lambda.min", type = "coefficients")

fit.lasso = lm(price~ car_ID + I(carCompany=="audi") + I(carCompany=="bmw") + 
                 I(carCompany=="buick") + I(carCompany=="dodge") + 
                 I(carCompany=="honda") + I(carCompany=="jaguar") +
                 I(carCompany=="mercury")+I(carCompany=="mitsubishi") +
                 I(carCompany=="nissan")+I(carCompany=="peugeot") +
                 I(carCompany=="plymouth") + I(carCompany=="porsche") +
                 I(carCompany=="renault")+ I(carCompany=="saab")+
                 I(carCompany=="subaru")+I(carCompany=="toyota")+ 
                 I(carCompany=="volvo")+ I(aspiration =="turbo") + 
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(carbody == "wagon")+ I(drivewheel =="fwd")+ 
                 I(enginelocation == "rear")+  carwidth + carheight+ curbweight + 
                 I(enginetype == "hcv")+ I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="three")+
                 I(cylindernumber =="twelve")+ I(cylindernumber =="two")+ 
                 enginesize + I(fuelsystem == "fi")+ I(fuelsystem == "pdi")+
                 I(fuelsystem == "pfi") + boreratio + stroke + horsepower + 
                 peakrpm, data = dat)
                 

summary(fit.lasso)

# 3 candidate models are given... fsel = fs and belim = bs 
####################  CHECK FOR MULTICOLLINEARITY ###################

fit.fsel
#removed 2 variables with N/A because of aliases
fit.fsel = lm(price~ enginesize + I(carCompany=="audi") + I(carCompany=="bmw") + 
                 I(carCompany=="buick") + I(carCompany=="chevrolet")+I(carCompany=="dodge") + 
                 I(carCompany=="honda") +I(carCompany=="isuzu") + 
                 I(carCompany=="mercury")+I(carCompany=="mitsubishi") +
                 I(carCompany=="nissan")+I(carCompany=="peugeot") +
                 I(carCompany=="plymouth") + I(carCompany=="porsche") +
                 I(carCompany=="renault")+ I(carCompany=="saab")+
                 I(carCompany=="subaru")+I(carCompany=="toyota")+ 
                 I(carCompany=="volkswagon")+ I(carCompany=="volvo")+ 
                 I(enginetype == "dohcv")+I(enginetype == "l")+
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(aspiration == "turbo")+ peakrpm +
                 I(enginetype == "hcv")+ I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                 I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                stroke + car_ID + highwaympg+ carlength + wheelbase, data = dat)

fit.fsel
#removed several variables with N/A because of aliases
fit.fsel = lm(price~ enginesize + I(enginetype == "dohcv")+I(enginetype == "l")+
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                 I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                 stroke + car_ID + highwaympg+ carlength + wheelbase, data = dat)

fit.fsel
vif(fit.fsel)


# curbweight has the highest so we remove it
fit.fsel = lm(price~ enginesize + I(enginetype == "dohcv")+I(enginetype == "l")+
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                 I(cylindernumber =="six")+ I(cylindernumber =="twelve")+
                 stroke + car_ID + highwaympg+ carlength + wheelbase, data = dat)

vif(fit.fsel)

# cylindernumberfour has the highest so we remove it
fit.fsel = lm(price~ enginesize + I(enginetype == "dohcv")+I(enginetype == "l")+
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ 
                 I(cylindernumber =="six")+ I(cylindernumber =="twelve")+
                 stroke + car_ID + highwaympg+ carlength + wheelbase, data = dat)

vif(fit.fsel)
# no variables have a VIF above 10 so we can stop 


fit.belim 
fit.belim = lm(price~ enginesize + I(carCompany=="audi") + I(carCompany=="bmw") + 
                I(carCompany=="buick") + I(carCompany=="chevrolet")+I(carCompany=="dodge") + 
                I(carCompany=="honda") +I(carCompany=="isuzu") + I(carCompany=="jaguar") +
                I(carCompany=="mercury")+I(carCompany=="mitsubishi") +
                 I(carCompany=="mazda") +I(carCompany=="nissan")+I(carCompany=="peugeot") +
                I(carCompany=="plymouth") + I(carCompany=="porsche") +
                I(carCompany=="renault")+ I(carCompany=="saab")+
                I(carCompany=="subaru")+I(carCompany=="toyota")+ 
                I(carCompany=="volkswagon")+ I(carCompany=="volvo")+ 
                I(carbody == "hardtop") + I(carbody == "hatchback") + I(carbody == "sedan") +
                 I(carbody == "wagon") + I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(enginetype == "ohcv")+I(enginetype == "l")+I(aspiration == "turbo")+ 
                 peakrpm + I(enginetype == "dohcf")+
                I(enginetype == "hcv")+ I(enginetype == "rotor")+ 
                I(cylindernumber =="five")+ I(cylindernumber =="four")+ I(cylindernumber =="two")+
                I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                 boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                 I(fuelsystem == "idi")+ I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                 I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                 , data = dat)

fit.belim
#removed several variables with N/A because of aliases
fit.belim = lm(price~ enginesize + I(carbody == "hardtop") + 
                 I(carbody == "hatchback") + I(carbody == "sedan") +
                 I(carbody == "wagon") + I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                 I(enginetype == "ohcv")+I(enginetype == "l")+I(aspiration == "turbo")+ 
                 peakrpm + I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                 I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                 stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                 boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                 I(fuelsystem == "idi")+ I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                 I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
               , data = dat, y=T, x=T)

vif(fit.belim)

# fuelsystemidi has the highest so we remove it
fit.belim = lm(price~ enginesize + I(carbody == "hardtop") + 
                  I(carbody == "hatchback") + I(carbody == "sedan") +
                  I(carbody == "wagon") + I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+I(enginetype == "l")+I(aspiration == "turbo")+ 
                  peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                  I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                  stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)

# enginesize has the highest so we remove it
fit.belim = lm(price~ I(carbody == "hardtop") + 
                  I(carbody == "hatchback") + I(carbody == "sedan") +
                  I(carbody == "wagon") + I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+I(enginetype == "l")+I(aspiration == "turbo")+ 
                  peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                  I(cylindernumber =="six")+ I(cylindernumber =="twelve")+ curbweight+
                  stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)

# curbweight has the highest so we remove it
fit.belim = lm(price~ I(carbody == "hardtop") + 
                  I(carbody == "hatchback") +I(carbody == "wagon") + I(carbody == "sedan")+
                  I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+ I(enginetype == "l")+
                  I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                  I(cylindernumber =="six")+ I(cylindernumber =="twelve")+
                  stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)


# carbodysedan has the highest so we remove it
fit.belim = lm(price~ I(carbody == "hardtop") + 
                  I(carbody == "hatchback") +I(carbody == "wagon") +
                  I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+ I(enginetype == "l")+
                  I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="four")+ 
                  I(cylindernumber =="six")+ I(cylindernumber =="twelve")+
                  stroke + car_ID + highwaympg+ carlength + wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)


# cylindernumber4 has the highest so we remove it
fit.belim = lm(price~ I(carbody == "hardtop") + 
                  I(carbody == "hatchback") +I(carbody == "wagon") +
                  I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+ I(enginetype == "l")+
                  I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="six")+ 
                  I(cylindernumber =="twelve")+ stroke + car_ID + 
                  highwaympg+ carlength + wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)

# carlength has the highest so we remove it
fit.belim = lm(price~ I(carbody == "hardtop") + 
                  I(carbody == "hatchback") +I(carbody == "wagon") +
                  I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                  I(enginetype == "ohcv")+ I(enginetype == "l")+
                  I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="six")+ 
                  I(cylindernumber =="twelve")+ stroke + car_ID + 
                  highwaympg+ wheelbase+ carwidth+carheight+
                  boreratio+compressionratio+ I(fuelsystem == "2bbl")+ I(fuelsystem == "4bbl")+
                  I(fuelsystem == "mfi")+ I(fuelsystem == "mpfi")+
                  I(fuelsystem == "spdi")+ I(fuelsystem == "spfi")
                , data = dat)
vif(fit.belim)
# no variables have a VIF above 10 so we can stop 


fit.lasso
#removed several variables with N/A because of aliases
fit.lasso = lm(price~ car_ID + I(aspiration =="turbo") + 
                 I(carbody == "hardtop") + I(carbody == "hatchback") + 
                 I(carbody == "wagon")+ I(drivewheel =="fwd")+ 
                 I(enginelocation == "rear")+  carwidth + carheight+ curbweight + 
                 I(enginetype == "rotor")+ 
                 I(cylindernumber =="five")+ I(cylindernumber =="three")+
                 I(cylindernumber =="twelve")+
                 enginesize + boreratio + stroke + horsepower + 
                 peakrpm, data = dat)

vif(fit.lasso)

#curbweight has the highest vif so we remove it
fit.lasso = lm(price~ car_ID + I(aspiration =="turbo") + 
                  I(carbody == "hardtop") + I(carbody == "hatchback") + 
                  I(carbody == "wagon")+ I(drivewheel =="fwd")+ 
                  I(enginelocation == "rear")+  carwidth + carheight+ 
                  I(enginetype == "rotor")+ 
                  I(cylindernumber =="five")+ I(cylindernumber =="three")+
                  I(cylindernumber =="twelve")+
                  enginesize + boreratio + stroke + horsepower + 
                  peakrpm, data = dat)
vif(fit.lasso)

#enginesize has the highest vif so we remove it
fit.lasso = lm(price~ car_ID + I(aspiration =="turbo") + 
                  I(carbody == "hardtop") + I(carbody == "hatchback") + 
                  I(carbody == "wagon")+ I(drivewheel =="fwd")+ 
                  I(enginelocation == "rear")+  carwidth + carheight+ 
                  I(enginetype == "rotor")+ I(cylindernumber =="five")+ 
                  I(cylindernumber =="three")+ I(cylindernumber =="twelve")+
                  boreratio + stroke + horsepower + peakrpm, data = dat)
vif(fit.lasso)
# no variables have a VIF above 10 so we can stop 


###################
#the 3 final candidate models are:
summary(fit.fsel)
summary(fit.belim)
summary(fit.lasso)
#Select fit.fsel based on the highest R-squared value


#################### CHECKING FOR OUTLIERS  ########################

ols_plot_resid_lev(fit.fsel)
#look at 75 and 129 because they are outliers wrt y and leverage points

ols_plot_dffits(fit.fsel)
# both obs 75 & 129 are outliers to their own fitted value

ols_plot_cooksd_chart(fit.fsel)
# obs 129 is very influential to all the fitted values
# obs 75 is also influential to all fitted values, but not as bad at 129

ols_plot_dfbetas(fit.fsel)
#Each of these observations are influential on each of these fitted values:
#75 on cylindernumbersix, cylinernumbertwelve,carbodyhardtop, enginetypeohcf, &
#enginesize

#129 on wheelbase, highwaympg, stroke,car_ID, cylindernumbersix, cylinernumbertwelve,
#cylinernumberfive, enginetyperotor,peakrpm, carbodyhardtop, carbodyhatchback,
#enginetypeohc, enginetypeohcf, enginesize, enginetypedohcv, & enginetypel


#75 & 129 are influential outliers.


####################  CHECKING ASSUMPTIONS  ########################

#Testing Linearity
ols_plot_resid_fit(fit.fsel)
#The residuals are linear

# We tested for multicollinearity above and removed all predictor variables
# with a VIF > 10

#Testing Normality
dat$e = resid(fit.fsel)
ggplot(dat, aes(sample = e))+ geom_qq()+ geom_qq_line()
shapiro.test(dat$e)
# The shapiro-wilk test indicates that the residuals are not normal because
# the p-value is less than 0.05 indicating there is significant evidence to 
# reject the null hypothesis.
# qq-plot shows that the residuals follow a s-shape which is approximately normal.
# The lower p-value could be from the large sample set, so the qqplot does indicate
# that the model is approx normal

#try boxcox transformation to see if we can fix the slight non-normality
bc = boxcox(fit.fsel,lambda = seq(-2,4,by=0.1))
lam = with(bc, x[which.max(y)])

dat$Y.prime = log(dat$price)
fit.fsel2 = lm(Y.prime~ enginesize + I(enginetype == "dohcv")+I(enginetype == "l")+
                I(carbody == "hardtop") + I(carbody == "hatchback") + 
                I(enginetype == "ohc")+I(enginetype == "ohcf")+ 
                I(aspiration == "turbo")+ peakrpm + I(enginetype == "rotor")+ 
                I(cylindernumber =="five")+ 
                I(cylindernumber =="six")+ I(cylindernumber =="twelve")+
                stroke + car_ID + highwaympg+ carlength + wheelbase, data = dat)
dat$e2 = resid(fit.fsel2)
ggplot(dat, aes(sample = e2))+ geom_qq()+ geom_qq_line()
shapiro.test(dat$e2)

# The shaprio-wilk test's p-value got smaller with the box-cox transformation
# so the original y value is better to use


#Testing Independence
ggAcf(dat$e)
bgtest(fit.fsel)
# The ACF plot shows evidence for autocorrelation
# The p-value is less than 0.05, so there is enough evidence to say that there 
# is autocorrelation an reject the null hypothesis

#Testing Constant Variance
ols_plot_resid_fit(fit.fsel)
bptest(fit.fsel)
# the plot shows the residuals are pretty scattered, not a lot of evidence of 
# a cone shape
#The p-value is smaller then 0.05, so there is enough evidence to reject the 
# null hypothesis, therefore there is heteroscasticity and the variance is non-constant


#use robust regression to help with nonconstant variance
library(robustbase)
fit.irls = lmrob(formula(fit.fsel), data=dat,k.max = 1000)
fit.final = lm(formula(fit.fsel), data = dat, weights = fit.irls$rweights,x = T, y=T)
fit.final %>% summary()


####################  Model Validation  ########################

cv.fit = cv.lm(fit.final)
cv.fit
cv.lm(full)

