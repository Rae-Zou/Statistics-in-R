
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

# unbalanced data entry
Development_time <- c(78,91,97,82,85,77,
                      75,93,78,71,63,76,69,
                      64,72,68,77,56,95,71,
                      55,66,49,64,70,68)
Temperature <- rep(c(12,16,20,24), c(6,7,7,6))
result <- c(67, 66, 75, 76, 71, 70, 72, 63, 72, 62, 61, 69, 64, 71, 68, 56,
            69, 57, 55, 63, 65, 55, 59, 47, 49,
            30, 47, 39, 33)
ethnic <- rep(c("E1","E2","E3"), c(16,9,4))
sex<-rep(c(rep(c("femal","male"),c(7,9)),
           rep(c("femal","male"),c(6,3)),
           rep(c("femal","male"),c(2,2))))


## ONE-WAY ANOVA and Tukeytest:categorical variables (factors)

# csv
areas1 <- read.csv("areasLess3.csv")
#xlsx
areas.less6<-read_xlsx("areasLess6.xlsx", sheet = "Data")
tapply(areas.less6$Size, areas.less6$Shape, mean)

boxplot(Size ~ factor(Shape), main = "Distribution of Size: Areas data without 6 'outliers'", xlab = "Shape", data = areas.less6)
size.less6.ANOVA <- aov(Size ~ factor(Shape), data = areas.less6)
summary(size.less6.ANOVA)
leveneTest(Size ~ factor(Shape), center = "mean", data = areas.less6)
plot(x = size.less6.ANOVA$fitted.values, y = size.less6.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on Areas data without 6 'outliers'",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty=2)
qqnorm(size.less6.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on Areas data without 6 'outliers'")
qqline(size.less6.ANOVA$residuals)

# Non-parametric Kruskal-Wallis test
kruskal.test(Size ~ factor(Shape), data = areas.less6)
# Tukey's HSD
TukeyHSD(size.less6.ANOVA)

club <- rep(c("A","B","C","D"), each = 10)
times <- c(52.59, 55.55, 57.17, 50.31, 55.86, 56.01, 53.85, 53.91, 53.87, 53.22,
55.05, 54.00, 54.45, 56.13, 57.92, 55.78, 54.98, 54.18, 54.33, 60.83,
54.27, 53.02, 53.12, 54.92, 52.61, 51.10, 55.15, 51.95, 53.97, 52.13,
60.20, 57.05, 56.58, 57.00, 54.74, 55.66, 53.64, 55.32, 57.41, 57.07)
tapply(times, club, mean)
mean(times)
boxplot(times ~ factor(club), 
        main = "Distribution of Albatross Wingspan by Colony", 
        xlab = "Colony", ylab = "Wingspan (cm)")
times.ANOVA <- aov(times ~ factor(club))
summary(times.ANOVA)
leveneTest(times ~ factor(club), center = "mean")
TukeyHSD(times.ANOVA)

#enter data
wingspan <- c(160, 183, 192, 168, 177, 160, 169, 185,
              173, 157, 171, 166, 154, 172, 165, 166,
              147, 135, 156, 153, 148, 156, 150, 145,
              173, 159, 162, 169, 173, 178, 156, 163)
colony<-rep(c("A","B","C","D"), each = 8)
cbind(colony, wingspan)
# Display the mean, useful for Tukey underlining diagram
tapply(wingspan, colony, mean)
#boxplot
boxplot(wingspan ~ factor(colony), 
        main = "Distribution of Albatross Wingspan by Colony", 
        xlab = "Colony", ylab = "Wingspan (cm)")
# Levene's test
leveneTest(wingspan ~ factor(colony), center = "mean")
# Carry out a one-way ANOVA
wingspan.ANOVA <- aov(wingspan ~ factor(colony))
summary(wingspan.ANOVA)
# Scatterplot of residuals vs. fitted values
plot(x = wingspan.ANOVA$fitted.values, y = wingspan.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on albatross data", 
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)
# Normal Q-Q plot
qqnorm(wingspan.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on albatross data")
qqline(wingspan.ANOVA$residuals)

# Carry out Tukey's HSD test
TukeyHSD(wingspan.ANOVA)


## random effects ONE-WAY ANOVA: same code as means model (categorical variables (factors))

score <- c(69, 77, 59, 71, 63, 69, 62, 52, 71, 79,
           52, 58, 47, 38, 63, 58, 47, 49, 58, 49,
           72, 46, 56, 73, 53, 53, 60, 54, 71, 52,
           53, 84, 76, 81, 61, 74, 59, 53, 74, 70,
           83, 76, 81, 59, 80, 72, 69, 67, 78, 78)
teacher <- rep(c(1,2,3,4,5), each = 10)
# Display the mean, useful for Tukey underlining diagram
tapply(score, teacher, mean)
#boxplot
boxplot(score ~ factor(teacher), 
        main = "Distribution of Student Score by Teacher", 
        xlab = "Teacher", ylab = "Test Score (%)")
# Levene's test
leveneTest(score ~ factor(teacher), center = "mean")
# Carry out a one-way ANOVA
teacher.ANOVA <- aov(score ~ factor(teacher))
summary(teacher.ANOVA)
# Scatterplot of residuals vs. fitted values
plot(x = teacher.ANOVA$fitted.values, y = teacher.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA on teacher effects data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)
# Normal Q-Q plot
qqnorm(teacher.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA on teacher effects data")
qqline(teacher.ANOVA$residuals)


# two-way ANOVA:interaction test (categorical variables (factors))

conc <- c(12, 14, 19, 17, 21, 24, 11, 14, 25, 23, 32, 37)
dose <- rep(rep(c("1.Low", "2.Medium","3.High"),each =2), times = 2) # times: 2 rows 
delivery<- rep(c("Patch", "Capsule"), each=6) #each row has 6 value
# Fit a two-way ANOVA
drug.ANOVA <- aov(conc ~ factor(dose) * factor(delivery))
summary(drug.ANOVA)
# Levene's test
leveneTest(conc ~ factor(dose) * factor(delivery), center = "mean")
# Scatterplot of residuals vs. fitted values
plot(x = drug.ANOVA$fitted.values, y = drug.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA for drug concentration data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)
# Normal Q-Q plot
qqnorm(drug.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA for drug concentration data")
qqline(drug.ANOVA$residuals)
# Interaction graph
interaction.plot(x.factor = dose, 
                 trace.factor = delivery, 
                 response = conc, 
                 fun = mean,
                 xlab = "Dose", 
                 ylab = "Mean drug concentration",
                 main = "Interaction graph for drug delivery",
                 legend = TRUE, xpd=TRUE,
                 trace.label="Delivery")
#TukeyHSD(rat.logmistakes.ANOVA, which = "factor(strain)")


# three way ANOVA: interaction test (categorical variables (factors))

Recovery <- c(6, 3, 7, 4, 5, 4, 7, 7, 8, 5, 6, 7, 4, 6, 8,
              8, 5, 6, 9, 5, 6, 6, 7, 2, 4, 6, 7, 3, 9, 5, 8, 12, 3, 6, 8, 7, 5, 6, 6, 6, 8, 9, 6,
              7, 9, 9, 11, 6, 7, 8, 6, 10, 11, 12, 6, 13, 7, 9, 8)
Age <- factor(rep(c("Y","M","O"), c(15,28,16)), levels = c("Y","M","O")) # each ega group(row) it has 15,28, 16 inputs
Severity <- factor(c(rep(c("L","H"),c(6,9)),
                     rep(c("L","H"),c(13,15)),
                     rep(c("L","H"),c(8,8))),
                   levels = c("L","H")) # three rows with each Low/High
SelfMed <- factor(c(rep(c("Y","N"),c(2,4)),rep(c("Y","N"),c(3,6)),
                    rep(c("Y","N"),c(7,6)),rep(c("Y","N"),c(11,4)),
                    rep(c("Y","N"),c(5,3)),rep(c("Y","N"),c(6,2))),
                  levels = c("Y","N"))
head(cbind(Age, Severity, SelfMed))
# Fit a three-way ANOVA
recover.ANOVA <- aov(Recovery ~ Age * Severity + Age * SelfMed + Severity * SelfMed)
summary(recover.ANOVA)
# Scatterplot of residuals vs. fitted values
plot(x = recover.ANOVA$fitted.values, y = recover.ANOVA$residuals, 
     main = "Residuals vs. fitted values\n ANOVA for flu recovery data",
     xlab = "Predicted values", ylab = "Residuals")
abline(h = 0, lty = 2)
# Normal Q-Q plot
qqnorm(recover.ANOVA$residuals, 
       main = "Normal Q-Q plot of residuals\n ANOVA for flu recovery data")
qqline(recover.ANOVA$residuals)
# Interaction graph for flu recovery data.
interaction.plot(x.factor = Age,
                 trace.factor = Severity,
                 response = Recovery,
                 fun = mean,
                 ylab = "Mean recovery (days)",
                 main = "Interaction graph for flu recovery",
                 legend = TRUE, xpd=TRUE)
#multiple comparisons test
# Fit a one-way ANOVA
# including only the Age factor. Age has more than one comparison
recover2.ANOVA <- aov(Recovery ~ Age)
summary(recover2.ANOVA)
TukeyHSD(recover2.ANOVA) # Since there is no significant interaction
TukeyHSD(recover.ANOVA)

diversity <- c(2.8, 1.9, 2.6, 2.7, 2.4, 3.4, 3.2, 3.3, 2.1, 2.1,
               3.4, 2.6, 0.4, 3.3, 3.2, 2.0, 3.9, 2.8, 3.3, 2.6,
               0.9, 4.6, 4.0, 3.6, 2.1, 2.5, 3.6, 4.1, 3.6, 2.1,
               2.8, 1.4, 3.2, 3.9, 2.5, 3.7, 2.1, 3.9, 2.8, 1.8,
               4.7, 2.9, 4.6, 2.0, 2.8, 2.4, 3.8, 4.1, 2.7, 1.3,
               0.8, 2.6, 3.1, 0.8, 2.6, 2.7, 4.2, 3.0, 0.9, 3.0)
rainfall <- c(55, 45, 64, 24, 61, 69, 58, 44, 73, 32,
              49, 16, 20, 46, 37, 70, 34, 75, 41, 58,
              83, 40, 52, 40, 72, 74, 44, 20, 71, 78,
              24, 17, 85, 61, 65, 17, 81, 55, 40, 61,
              33, 22, 24, 38, 16, 46, 30, 30, 82, 15,
              76, 79, 81, 19, 41, 78, 61, 25, 67, 63)
temperature <- c(15.1, 14.9, 15.8, 14.8, 16.7, 16.5, 14.8, 15.2, 16.0, 15.5,
                 15.5, 14.5, 14.5, 16.8, 16.5, 16.3, 16.7, 14.4, 15.2, 15.1,
                 14.6, 17.0, 15.7, 14.9, 16.7, 15.7, 14.7, 15.7, 15.7, 15.5,
                 16.3, 14.6, 14.6, 16.4, 15.2, 14.6, 15.8, 15.4, 16.2, 14.5,
                 16.1, 16.5, 16.3, 16.5, 15.6, 14.1, 17.0, 15.2, 14.4, 14.1,
                 14.3, 15.2, 16.6, 14.7, 14.8, 16.2, 14.6, 14.6, 14.1, 16.5) 
 
Model1 <- lm(diversity ~ rainfall + temperature) 
summary(Model1)
Model2 <- lm(diversity ~ rainfall ) 
summary(Model2)
Model3 <- lm(diversity ~ temperature ) 
summary(Model3)
29/(29+47)

# simple linear regression:numerical variables (independent variables)

# data from csv
marks <- read.csv("marks.csv")
head(marks)
# Fit a linear regression 
marks.lm <- lm(Final_Mark ~ Midterm_Test, data = marks)
# ANOVA table
anova(marks.lm)
summary(marks.lm)
# Produce a scatterplot
plot(x = marks$Midterm_Test, y = marks$Final_Mark, 
     main = "Scatterplot of Assessment Marks with Fitted Regression Line",
     xlab = "Mid-term test mark (/50)", ylab = "Final mark (%)")
abline(marks.lm)
# studentized residuals versus fitted values
plot(x = marks.lm$fitted.values, y = rstudent(marks.lm), 
     main = "Studentized residuals vs. fitted values\n regression for assessment example",
     xlab = "Predicted value", ylab = "Studentized residual")
abline(h = 0)
abline(h = c(-2, 2), lty = 2)
# Normal Q-Q plot
qqnorm(marks.lm$residuals,
       main = "Normal Q-Q plot of residuals\n regression for assessment example")
qqline(marks.lm$residuals)
# Cook's distances
ols_plot_cooksd_chart(marks.lm)
# Obtain 95% confidence intervals for beta_0 and beta_1.
confint.default(marks.lm) # with original data
# Prediction
predict(marks.lm, newdata = data.frame(Midterm_Test = 20), 
        interval = "prediction")
###############################################################################

# enter data
mercury <- c(1.23, 1.33, 0.04, 0.44, 1.2, 0.27, 0.48, 0.19, 0.83, 
         0.81, 0.71, 0.5, 0.49, 1.16, 0.05, 0.15, 0.19, 0.77, 
         1.08, 0.98, 0.63, 0.56, 0.41, 0.73, 0.34, 0.59, 0.34, 
         0.84, 0.5, 0.34, 0.28, 0.34, 0.87, 0.56, 0.17, 0.18, 
         0.19, 0.04, 0.49, 1.1, 0.16, 0.1, 0.48, 0.21, 0.86, 
         0.52, 0.65, 0.27, 0.94, 0.4, 0.43, 0.25, 0.27)
pH <- c(6.1, 5.1, 9.1, 6.9, 4.6, 7.3, 5.4, 8.1, 5.8, 
       6.4, 5.4, 7.2, 7.2, 5.8, 7.6, 8.2, 8.7, 7.8, 
    5.8, 6.7, 4.4, 6.7, 6.1, 6.9, 5.5, 6.9, 7.3, 
    4.5, 4.8, 5.8, 7.8, 7.4, 3.6, 4.4, 7.9, 7.1, 
    6.8, 8.4, 7, 7.5, 7, 6.8, 5.9, 8.3, 6.7, 
    6.2, 6.2, 8.9, 4.3, 7, 6.9, 5.2, 7.9)
head(cbind(mercury,pH))
# Fit a linear regression
bass.lm <- lm(log(mercury) ~ pH)
# ANOVA table
anova(bass.lm)
summary(bass.lm)
# Produce a scatterplot
plot(x = pH, y = log(mercury),
main = "Scatterplot of mercury in largemouth bass vs. water pH\n with fitted regression line",
xlab = "Water pH", ylab = "Average mercury")
abline(bass.lm)
# Studentized residuals versus fitted values
plot(x = bass.lm$fitted.values, y = rstudent(bass.lm),
main = "Studentized residuals vs. fitted values\n regression for largemouth bass data",
xlab = "Predicted value", ylab = "Studentized residual")
abline(h = 0)
abline(h = c(-2, 2), lty = 2)
# Normal Q-Q plot of residuals
qqnorm(bass.lm$residuals,
main = "Normal Q-Q plot of residuals\n regression for largemouth bass data")
qqline(bass.lm$residuals)
# Plot Cook's distances for the largemouth bass data.
ols_plot_cooksd_chart(bass.lm)
# Obtain 95% confidence intervals for beta_0 and beta_1.
confint.default(lm(mercury ~ pH))
# Prediction
predict(lm(mercury ~ pH), newdata = data.frame(pH = 7), 
        interval = "prediction")



# mutiple linear regression:numerical variables (independent variables)

loyn <- read.csv("loyn.csv") 
lyon.lm <- lm(ABUND ~ log(AREA) + log(DIST) + GRAZE, data = loyn)
anova(lyon.lm) #F-test P-value
summary(lyon.lm)
# Fit each model in turn
#3 predictors
mod1.lm <- lm(ABUND ~ log(AREA) + log(DIST) + GRAZE, data = loyn)
summary(mod1.lm) #t-test P-value
# 2 predictors
mod2.lm <- lm(ABUND ~ log(AREA) + log(DIST), data = loyn)
summary(mod2.lm)
#2 predictors
mod3.lm <- lm(ABUND ~ log(AREA) + GRAZE, data = loyn)
summary(mod3.lm)
#2 predictors
mod4.lm <- lm(ABUND ~ log(AREA) + GRAZE, data = loyn)
summary(mod4.lm)
# 1 predictor
mod5.lm <- lm(ABUND ~ log(AREA), data = loyn)
summary(mod5.lm)
# 1 predictor
mod6.lm <- lm(ABUND ~ log(DIST), data = loyn)
summary(mod6.lm)
# 1 predictor
mod7.lm <- lm(ABUND ~ GRAZE, data = loyn)
summary(mod7.lm)
# null model
mod8.lm <- lm(ABUND ~ NULL, data = loyn)
summary(mod8.lm)


#ANCOVA:  categorical and numerical variables

Height1 <- c(154, 155, 157, 157, 158, 163, 165, 168, 171, 173,
             156, 160, 163, 168, 168, 169, 173, 175, 178, 179)
Height2 <- c(157, 160, 159, 161, 160, 166, 166, 171, 172, 173,
             162, 163, 166, 172, 170, 175, 176, 179, 179, 182)
gain <- Height2-Height1
Sex <- factor(rep(c("Female", "Male"), each = 10))
# scatter of data
plot(x = Height1, y = gain, xlab = "Height1", 
     ylab = "gain", col = as.numeric(factor(Sex)),
     pch = c(rep(16,times=10), rep(15,times=10)))
legend("topright", legend = unique(Sex), 
       col = unique(as.numeric(factor(Sex))),
        pch = c(16,15))

# Enter Height1 (the covariate) first in this model
height.gain.lm1 <- lm(gain ~ Height1*Sex) #carrot.lm <- lm(increase ~ initial * factor(fertiliser))
# ANOVA table output for the ANCOVA.
anova(height.gain.lm1)
#studentized residuals versus fitted values
plot(x = height.gain.lm1$fitted.values, y = rstudent(height.gain.lm1), main = "Studentized
residuals vs. fitted values\n ANCOVA for height gain data",
xlab = "Predicted value", ylab = "Studentized residual")
abline(h = 0)
abline(h = c(-2, 2), lty = 2)
#normal Q-Q
qqnorm(height.gain.lm1$residuals,
main = "Normal Q-Q plot of residuals\n ANCOVA for height gain data")
qqline(height.gain.lm1$residuals)
#Cook's distances
ols_plot_cooksd_chart(height.gain.lm1)

# Enter Height1 (the covariate) second in this model
height.gain.lm2 <- lm(gain ~ Sex*Height1)
# ANOVA table output for the ANCOVA.
anova(height.gain.lm2)


#ANCOVA: Exploratory Analysis - no specific hypotheses

fertiliser <- rep(c("Growmost", "Superior"), each = 10)
initial <- c(40, 43, 43, 45, 48, 51, 52, 57, 59, 65,
             42, 44, 46, 52, 52, 57, 59, 63, 68, 69)
increase <- c(6.2, 8.7, 5.6, 7, 4.5, 3.2, 4.8, 2.2, 1.3, 2,
              7.1, 9.3, 8.2, 8.9, 7.7, 7.2, 7, 5.5, 3.2, 3.9)
#Interactive model (Non-parallel lines)
carrot.m1 <- lm(increase ~ initial+factor(fertiliser)+initial * factor(fertiliser))
anova(carrot.m1)   # F-test P-value, SSE & MSE
summary(carrot.m1) # t-test P-value, adj R & R-squared
#Parallel lines model
carrot.m2 <- lm(increase ~ initial+factor(fertiliser))#1. Initial (the covariate) specified first
anova(carrot.m2)
summary(carrot.m2) # for adj R & R-squared
carrot.m3 <- lm(increase ~ factor(fertiliser)+initial)# 2. Fertiliser (the factor) specified first
anova(carrot.m3)
summary(carrot.m3) # for adj R & R-squared
#One-way ANOVA of fertiliser factor
carrot.m4 <- lm(increase ~ factor(fertiliser))
anova(carrot.m4)
summary(carrot.m4) # for adj R & R-squared
#The regression model with x = initial yield
carrot.m5 <- lm(increase ~ initial)
anova(carrot.m5)
summary(carrot.m5) # for adj R & R-squared
#The null model with only an overall mean
carrot.m6 <- lm(increase ~ NULL)
anova(carrot.m6)
summary(carrot.m6) # for adj R & R-squared






