
library(utils)
library(readxl)
library(car)
library(base)
library(graphics)
library(stats)
library(olsrr)
library(MASS)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(rstatix)

# Q1

score<-c(44, 46, 59, 48, 49, 60, 51, 39,
         43, 45, 57, 51, 51, 49, 44, 61,
         51, 34, 53, 45, 41, 46, 41, 43,
         52, 47, 59, 56, 49, 61, 57, 54)
type<-rep(c("T1","T2","T3","T4"), each = 8)
tapply(score, type, mean)
type.ANOVA<-aov(score~factor(type))
summary(type.ANOVA)

boxplot(score~factor(type),
        main = "Distribution of people by type",
        xlab = "Type",
        ylab = "Test Score")

leveneTest(score~factor(type), center = "mean")

plot(x = type.ANOVA$fitted.values, y = type.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on type effects data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)

qqnorm(type.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on type effects data")
qqline(type.ANOVA$residuals)

#Q2
zinc<-c( 6340, 4280, 5170, 2880, 4330, 3050,
          3690, 4750, 5100, 2360, 1990, 2140,
          250,  470,  330,  400,  310,  430,
          2850, 2380, 3130, 1070, 960, 1300)
pH<-rep(c("pH 5.5(acid)","pH7(neutral)"), each = 3, time=4)
plant<-rep(c("Alpine pennycress","Bladder campion","Lettuce","Martin red fescue"), each = 6)

cbind(pH, plant, zinc)
####### raw data  ###############
zinc.ANOVA<-aov(zinc~factor(pH)*factor(plant))
summary(zinc.ANOVA)

#boxplot(zinc~factor(plant):factor(pH), main = "Boxplot of zinc")

#leveneTest(zinc~factor(plant)*factor(pH), center = "mean")

plot(x = zinc.ANOVA$fitted.values, y = zinc.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on uptake of zinc data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)

qqnorm(zinc.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on uptake of zinc data")
qqline(zinc.ANOVA$residuals)
interaction.plot(x.factor = pH, 
                 trace.factor = plant, 
                 response = zinc, 
                 fun = mean,
                 xlab = "pH", 
                 ylab = "Mean uptake of zinc",
                 main = "Interaction graph for four species of plant",
                 legend = TRUE, xpd=TRUE,
                 trace.label="plant name")
########log -transformed data  ########
zinc.log.ANOVA<-aov(log(zinc)~factor(pH)*factor(plant))
summary(zinc.log.ANOVA)

#boxplot(log(zinc)~factor(plant):factor(pH), main = "Boxplot of logged zinc data")

#leveneTest(log(zinc)~factor(plant)*factor(pH), center = "mean")

plot(x = zinc.log.ANOVA$fitted.values, y = zinc.log.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on uptake of logged zinc data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)

qqnorm(zinc.log.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on uptake of logged zinc data")
qqline(zinc.log.ANOVA$residuals)
interaction.plot(x.factor = pH, 
                 trace.factor = plant, 
                 response = log(zinc), 
                 fun = mean,
                 xlab = "pH", 
                 ylab = "Mean uptake of logged zinc",
                 main = "Interaction graph for logged zinc data",
                 legend = TRUE, xpd=TRUE,
                 trace.label="plant name")


Marks <- c(67, 66, 75, 76, 71, 70, 72,  63, 72, 62, 61, 69, 64, 71, 68, 56,
           69, 57, 55, 63, 65, 55,  59, 47, 49,
           30, 47,  39, 33)
Ethnity <- rep(c("E1","E2","E3"), c(16,9,4)) 
Sex <- rep(c(rep(c("F","M"),c(8,8)),
             rep(c("F","M"),c(6,3)),
             rep(c("F","M"),c(2,2)))) 
cbind(Ethnity, Sex, Marks)

#Q3
mark_data<- read_xlsx("question3.xlsx", sheet = "Data")
head(mark_data)
mark<-mark_data$mark
sex<-as.factor(mark_data$sex)
ethnic<-as.factor(mark_data$ethnic)

#mark.ANOVA<-aov(mark~sex*ethnic, data =mark_data)

boxplot(mark~ethnic*sex, main = "Boxplot of test marks")
mark.ANOVA<-aov(mark~ethnic*sex, data =mark_data)
summary(mark.ANOVA)

TukeyHSD(mark.ANOVA, which = "sex")

leveneTest(mark~ethnic*sex, center = "mean", data =mark_data)

plot(x = mark.ANOVA$fitted.values, y = mark.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on test mark data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)

qqnorm(mark.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on test mark data")
qqline(mark.ANOVA$residuals)
interaction.plot(x.factor = sex, 
                 trace.factor = ethnic, 
                 response = mark, 
                 fun = mean,
                 xlab = "sex", 
                 ylab = "Mean test mark",
                 main = "Interaction graph for test mark data",
                 legend = TRUE, xpd=TRUE,
                 trace.label="Ethnic group")


######Q4
area<-c(516,469.06,462.25,938.6,1357.15,1773.66,1686.01,1786.29,3090.07,3980.12,4424.84,4451.68,4982.89,
        4450.86,5490.74,7476.21,7138.82,9149.94,10133.07,9287.69,13729.13,20300.77,24712.72,27144.03,26117.81)
species<-c(3,7,6,8,10,9,10,11,16,9,13,14,12,
           14,20,22,15,20,22,21,15,24,25,25,24)
head(cbind(area,species))


####### raw data  ###############
# x = Area, y = number of different species
macro.lm<-lm(species~area)
anova(macro.lm)
summary(macro.lm)
# Produce a scatterplot of area vs. species 
plot(x=area, y=species, 
     main="Scatterplot of the mussel clumps area vs. species of\n macroinvertebrates found with fitted regression line",
     xlab = "Average Area(dm2)", ylab = "Number of Species")
abline(macro.lm)
# Produce a scatterplot of studentized residuals versus fitted values
plot(x=macro.lm$fitted.values, y = rstudent(macro.lm),
     main="Studentized residuals vs. fitted values\n regression for macroinvertebrates data",
     xlab = "Predicted value", ylab = "Studentized residual")
abline(h = 0)
abline(h = c(-2, 2), lty = 2)
# Produce a normal Q-Q plot of residuals
qqnorm(macro.lm$residuals,
main = "Normal Q-Q plot of residuals\n regression for macroinvertebrates data")
qqline(macro.lm$residuals)
ols_plot_cooksd_chart(macro.lm)


####################################
####### logged data  ###############
macro.lm2<-lm(species~log(area))
anova(macro.lm2)
summary(macro.lm2)
# Produce a scatterplot of  area vs. species 
plot(x=log(area), y=species, 
     main="Scatterplot of the mussel clumps area vs. species of\n 
     macroinvertebrates found with fitted regression line",
    xlab = "Average logged Area(dm2)", ylab = "Number of Species")
abline(macro.lm2)
# Produce a scatterplot of studentized residuals versus fitted values
plot(x=macro.lm2$fitted.values, y = rstudent(macro.lm2),
     main="Studentized residuals vs. fitted values\n regression for 
     logged macroinvertebrates data",
     xlab = "Predicted value", ylab = "Studentized residual")
abline(h = 0)
abline(h = c(-2, 2), lty = 2)
# Produce a normal Q-Q plot of residuals
qqnorm(macro.lm2$residuals,
main = "Normal Q-Q plot of residuals\n regression for logged macroinvertebrates data")
qqline(macro.lm2$residuals)

# Plot Cook's distances
ols_plot_cooksd_chart(macro.lm2)

# Obtain 95% confidence intervals for beta_0 and beta_1.
confint.default(macro.lm2)




































