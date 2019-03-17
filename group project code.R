setwd("/Users/yaxincai/Desktop/depaul/DSC 423/group project/student")

math=read.csv("student-mat.csv",sep=";",header=T)

school=math[,1]

sex = math[,2]

age = math[,3]

address = math[,4]

famsize = math[,5]

Pstatus = math[,6]

Medu = math[,7]

Fedu = math[,8]

Mjob = math[,9]

Fjob = math[,10]

reason = math[,11]

guardian = math[,12]

traveltime = math[,13]

studytime = math[,14]

failures= math[,15]

schoolsup = math[,16]

famsup = math[,17]

paid= math[,18]

activities = math[,19]

nursery = math[,20]

higher = math[,21]

internet = math[,22]

romantic = math[,23]

famrel=math[,24]

freetime = math[,25]

goout = math[,26]

Dalc= math[,27]

Walc = math[,28]

health = math[,29]

absences = math[,30]

G1 = math[,31]

G2 = math[,32]

G3 = math[,33]
plot(math,  main="Simple Scatterplot Matrix")

M1=lm(G3~as.factor(school)+as.factor(sex)+age+as.factor(address)+as.factor(famsize)+as.factor(Pstatus)+Medu+Fedu+as.factor(Mjob)
      
      +as.factor(Fjob)+as.factor(reason)+as.factor(guardian)+traveltime+studytime+failures+as.factor(schoolsup)+as.factor(famsup)
      
      +as.factor(paid)+as.factor(activities)+as.factor(nursery)+as.factor(higher)+as.factor(internet)+as.factor(romantic)+famrel
      
      +freetime+goout+Dalc+Walc+health+absences+G1+G2)

plot(fitted(M1), rstandard(M1), main = "residual plot")


summary(M1)


# Validate model variable selection - backward
step(M1, direction = "backward")
# this added as.factor(school), as.factor(romantic), Walc, as.factor(activities),a along with age, G1, famrel, absences, G2

M2=lm(G3 ~ as.factor(school) + age + as.factor(activities) + 
     as.factor(romantic) + famrel + Walc + absences + G1 + G2)
summary(M2)
# eliminating not significant variables
M3=lm(G3 ~  age + as.factor(activities) + 
        as.factor(romantic) + famrel + Walc + absences + G1 + G2)
summary(M3)

M4=lm(G3 ~  age + as.factor(activities) + 
       famrel + Walc + absences + G1 + G2)
summary(M4)

M5=lm(G3 ~  age + as.factor(activities) + 
        famrel + absences + G1 + G2)
summary(M5)

M6=lm(G3 ~  age + 
        famrel + absences + G1 + G2)
summary(M6)  # final model

#forward selection
Base=lm(G3~1)
step(Base,scope=list(upper=M1,lower=~1),direction="forward")
#Stepwise selection
step(Base,scope=list(upper=M1,lower=~1),direction="both",trace=FALSE)
#check model
fit=lm(G3~G2+famrel+absences+G1+age+as.factor(activities)+Walc+as.factor(romantic)+as.factor(school))
summary(fit)

library(car)
vif(M6)      # check multicollinearity
# no multicollinearity across age, famrel, absences, G1, and G2
library(QuantPsyc)
lm.beta(M6)  #standardized coefficients,show the most important predictor
## Interaction model

MInteraction1=lm(G3~age+famrel+absences+G1+G2+famrel*absences+famrel*G1+famrel*G2)
summary(MInteraction1)
lm.beta(MInteraction1)
# There is an interaction between famely relationship and absences
# perhaps because absences inconsistent is why G1 and G2 does not reflect interaction

MInteraction2=lm(G3~age+famrel+absences+G1+G2+G2*absences+G2*famrel+G1*G2+absences*G1+famrel*G1)
summary(MInteraction2)
# adds an interaction between family relationship and G1 and G2

#correlationship
cor(age,G3)
cor(famrel,G3)
cor(absences,G3)
cor(G1,G3)
cor(G2,G3)
cor(Walc,G3)
#check MSE, F-value
anova(M6)
anova(fit)

library(leaps)


#INfluencers and Outliers
summary(influence.measures(M6))
influence.measures(M6)
# There are influences and outliers.
# The influencers will need to be investigated

plot(rstudent(M6)~hatvalues(M6))
# rstudent vs. hat validates there are outliers at the lower range

#residual plots
#residuals vs predicted values plot

plot( fitted(M6), rstandard(M6), main="Predicted vs residuals plot")
abline(a=0, b=0, col='red')
# the ages are discrete, hence the shape, but they are also randomly distributed

#normal probability plot of residuals
qqnorm(rstandard(M6))
qqline(rstandard(M6), col = 2)
# QQ plot is evidence of lin

# adjusted R2 indicates that 83.15% of the variation of the final grade can be explained by the model containing family relations, absences, and grades in the first two periods

# train and fit
set.seed(34567)
#create the training set and testing set
select.math <- sample(1:nrow(math), 0.75*nrow(math))
train.math <- math[select.math,]
# remaining data for test
test.math <-math[-select.math]
fit.train <- lm(G3~age+famrel+absences+G1+G2)
summary(fit.train)


# Confirms the model

# test prediction
