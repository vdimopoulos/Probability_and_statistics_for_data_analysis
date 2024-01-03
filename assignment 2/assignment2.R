rm(list = ls())
setwd("C:\\Users\\vassi\\OneDrive\\Έγγραφα\\MSc in Data Science\\Fall Semester\\Probability and statistics for data analysis\\assignment 2")
library(car)
library(alr4)
library(leaps)


#1. In file “data.txt” (available on the e-class assignments site), you will
#find the recorded variables Y, X1, X2, X3, X4 (continuous), and W (categorical with three levels) on 150 cases. Using these data, answer the
#following questions


data <- read.delim("data.txt", header = TRUE, quote = "\"",, sep = " ")
View(data)

weightloss <- read.delim("weightloss.txt", header = TRUE, quote = "\"",, sep = " ")
View(weightloss)

#(a) Run the parametric one-way ANOVA of each of the continuous variables (Y, X1, X2, X3, X4) on the categorical variable (W). Specifically,
#(i) provides a graphical representation of each of the continuous versus
#the categorical variable

unique_W <- unique(data$W)
cat("Unique W Categories:", unique_W, "\n")

par(mfrow = c(2, 3))
par(mar = c(1, 1 ,1, 1))
boxplot(X1~factor(W),data=data,
        main=paste("X1", "vs W Categories"),
        xlab="W Categories", ylab="X1",
        names=c("A","C","B"))

boxplot(X2~factor(W),data=data,
        main=paste("X2", "vs W Categories"),
        xlab="W Categories", ylab="X2",
        names=c("A","C","B"))
boxplot(X3~factor(W),data=data,
        main=paste("X3", "vs W Categories"),
        xlab="W Categories", ylab="X3",
        names=c("A","C","B"))
boxplot(X4~factor(W),data=data,
        main=paste("X4", "vs W Categories"),
        xlab="W Categories", ylab="X4",
        names=c("A","C","B"))
boxplot(Y~factor(W),data=data,
        main=paste("Y", "vs W Categories"),
        xlab="W Categories", ylab="Y",
        names=c("A","C","B"))
par(mfrow = c(1, 1))

#(ii) provide the ANOVA output
#(iii) check the assumptions.

fitx1<-aov(X1~factor(W),data=data)
summary(fitx1)
qqnorm(fitx1$residuals,main="NPP for residuals")
qqline(fitx1$residuals,col="red",lty=1,lwd=2)
shapiro.test(fitx1$residuals)
bartlett.test(X1~factor(W),data=data)
fligner.test(X1~factor(W),data=data)
leveneTest(X1~factor(W),data=data)
layout(matrix(1:4,2,2))
plot(fitx1)

fitx2<-aov(X2~factor(W),data=data)
summary(fitx2)
qqnorm(fitx2$residuals,main="NPP for residuals")
qqline(fitx2$residuals,col="red",lty=1,lwd=2)
shapiro.test(fitx2$residuals)
bartlett.test(X2~factor(W),data=data)
fligner.test(X2~factor(W),data=data)
leveneTest(X2~factor(W),data=data)
layout(matrix(1:4,2,2))
plot(fitx2)


fitx3<-aov(X3~factor(W),data=data)
summary(fitx3)
qqnorm(fitx3$residuals,main="NPP for residuals")
qqline(fitx3$residuals,col="red",lty=1,lwd=2)
shapiro.test(fitx3$residuals)
bartlett.test(X3~factor(W),data=data)
fligner.test(X3~factor(W),data=data)
leveneTest(X3~factor(W),data=data)
layout(matrix(1:4,2,2))
plot(fitx3)


fitx4<-aov(X4~factor(W),data=data)
summary(fitx4)
qqnorm(fitx4$residuals,main="NPP for residuals")
qqline(fitx4$residuals,col="red",lty=1,lwd=2)
shapiro.test(fitx4$residuals)
bartlett.test(X4~factor(W),data=data)
fligner.test(X4~factor(W),data=data)
leveneTest(X4~factor(W),data=data)
layout(matrix(1:4,2,2))
plot(fitx4)


fity<-aov(Y~factor(W),data=data)
summary(fity)
qqnorm(fity$residuals,main="NPP for residuals")
qqline(fity$residuals,col="red",lty=1,lwd=2)
shapiro.test(fity$residuals)
bartlett.test(Y~factor(W),data=data)
fligner.test(Y~factor(W),data=data)
leveneTest(Y~factor(W),data=data)
layout(matrix(1:4,2,2))
plot(fity)

#(b) Provide a scatter-plot matrix of Y, X1, X2, X3, and X4, annotating
#the different levels of W in each plot using a different color.

data$W <- as.factor(data$W)
pairs(data[, c("Y", "X1", "X2", "X3", "X4")], col = as.numeric(data$W))
legend("topright", legend = levels(data$W), col = 1:length(levels(data$W)), pch = 1, title = "W")

# (c) Run the regression model of Y on X4

model <- lm(Y ~ X4, data = data)
summary(model)

# (d) Run the regression model of Y on all the remaining variables (X1,
# X2, X3, X4, W), including the non-additive terms (i.e., interactions of the
# continuous predictors with the categorical).


model2 <- lm(Y ~ X1 + X2 + X3 + X4 + W + X1:W + X2:W + X3:W + X4:W, data = data)
summary(model2)

#(e) Examine the regression assumptions and provide alternatives if any of
#them fails.

shapiro.test(model2$residuals)
layout(matrix(1:4,2,2))
plot(model2)

#(f) Use the “stepwise regression” approach to examine whether you can
#reduce the dimension of the model.

fitnull<-lm(Y ~ 1,data=data)
stepSR<-step(fitnull, scope=list(lower = ~ 1,
                                 upper = ~ X1 + X2 + X3 + X4 + W + X1:W + X2:W + X3:W + X4:W),
             direction="both", data=data)

# (g) Using the model found in (f), provide a point estimate and a 95%
# confidence interval for the prediction of Y when: (X1, X2, X3, X4, W) =
# (120, 30, 10, 90, B)

step_model = lm(Y ~ X1 +X2 + X3 + W + X1:W,data=data)
summary(step_model)

new_data <- data.frame(X1 = 120, X2 = 30, X3 = 10, W = "B")
prediction_with_interval <- predict(step_model, newdata = new_data, interval = "confidence")
predicted_Y <- prediction_with_interval[1]
lower_bound <- prediction_with_interval[2]
upper_bound <- prediction_with_interval[3]
cat("Predicted Y:", predicted_Y, "\n")
cat("95% Confidence Interval:", lower_bound, "to", upper_bound, "\n")


# (h) Using the cut() function, create a categorical variable (named Z) with
# three levels based on the quantiles of X4. Provide the contingency table
# of X4 and W.

data$Z <- cut(data$X4, breaks = quantile(data$X4, probs = c(0, 1/3, 2/3, 1)), labels = c("Level1", "Level2", "Level3"))

# Create a contingency table for X4 and W
contingency_table <- table(data$X4, data$W)

# Print the contingency table
print(contingency_table)

# (i) Run the parametric two-way ANOVA of Y on the categorical variables
# W and Z (including the interaction term). Provide the fit, examine the
# assumptions, and comment on the significance of the terms.

model2wayAnova <- aov(Y ~ W * Z, data = data)
summary(model2wayAnova)
shapiro.test(model2wayAnova$residuals)
layout(matrix(1:4,2,2))
plot(model2wayAnova)


# 2. In the file “weightloss.txt”(available on the e-class assignments site)
# you will find the recorded variables work (categorical with three levels),
# diet (categorical with four levels), and loss (continue, in calories). More
# specifically, the data provide the weight loss per day in a 3×4 factorial experiment. The two factors include 3 types of workout and 4 types of diet.
# Each combination of the two factors is used to be completely randomized.

# (a) Provide boxplots of the weight loss per workout, per diet, and for the
# combinations of the two categorical factors.

unique_work <- unique(weightloss$workout)
cat("Unique Workout Categories:", unique_work, "\n")

unique_d <- unique(weightloss$diet)
cat("Unique Diet Categories:", unique_d, "\n")

par(mfrow = c(1, 2))
par(mar = c(2, 2 , 2, 2))
boxplot(loss~factor(workout),data=weightloss,
        main=paste("Weight Loss", "vs Workout Type"),
        xlab="Workout Types", ylab="Loss",
        names=c("W3","W1","W2"))
boxplot(loss~factor(diet),data=weightloss,
        main=paste("Weight Loss", "vs Diet Type"),
        xlab="Workout Types", ylab="Loss",
        names=c("D1","D2","D3","D4"))

weightloss$combinations <- interaction(weightloss$workout, weightloss$diet)
unique_combinations <- levels(weightloss$combinations)
unique_combinations_array <- as.array(unique_combinations)

par(mfrow = c(1, 1))
boxplot(loss ~ combinations, data=weightloss,
        main = "Boxplot per Combination of Categorical Columns",
        xlab = "Combination",
        ylab = "Loss",
        names=unique_combinations_array)

# (b) Fit a One-Way ANOVA model with the weight loss as a response and
# the workout (as a factor). Interpret the model parameters.

model_work <- aov(loss ~ workout, data = weightloss)
summary(model_work)

# (c) In the ANOVA model of (b), is the expected difference between W2
# and W3 significant? [TIP: change the reference level appropriately and
# refit the ANOVA model of question (b)]

weightloss$workout <- factor(weightloss$workout)
weightloss$workout <- relevel(weightloss$workout, ref = "W3")
model_ref_W3 <- aov(loss ~ workout, data = weightloss)
summary(model_ref_W3)

weightloss$workout <- factor(weightloss$workout)
weightloss$workout <- relevel(weightloss$workout, ref = "W2")
model_ref_W2 <- aov(loss ~ workout, data = weightloss)
summary(model_ref_W2)

# (d) Fit a One-Way ANOVA model for the weight loss as response and
# diet. Interpret the model parameters. Are all treatments significant?

model_diet <- aov(loss ~ diet, data = weightloss)
summary(model_diet)
TukeyHSD(model_diet)

weightloss_excluded <- subset(weightloss, diet != "D1")
model_excluded <- aov(loss ~ diet, data = weightloss_excluded)
summary(model_excluded)

#(f) Fit a Two-Way ANOVA model of main effects. Provide the interpretation for the parameters.

model2wayAnova_loss <- aov(loss ~ workout + diet, data = weightloss)
summary(model2wayAnova_loss)
TukeyHSD(model2wayAnova_loss)

# (g) Exclude the non-significant levels of the factors and refit the model.
# Provide the interpretation for the parameters of the new, simplified, model.

weightloss_excluded_2 <- subset(weightloss, !(diet == "D2"))
weightloss_excluded_2 <- subset(weightloss_excluded_2, !(workout == "W3"))
model2wayAnova_loss_ex <- aov(loss ~ workout + diet, data = weightloss_excluded_2)
summary(model2wayAnova_loss)


# (h) Fit a Two-Way ANOVA model with interactions. Are all the parameters significant

model2wayAnova_loss_IN <- aov(loss ~ workout * diet, data = weightloss)
summary(model2wayAnova_loss_IN)
TukeyHSD(model2wayAnova_loss_IN)

# (i) Using the stepwise method, choose a model based on the AIC criterion
# starting from the full model (including the main effects and the interaction
# term). Are all coefficients significant?

weightloss$workout = as.factor(weightloss$workout)
weightloss$diet = as.factor(weightloss$diet)
dummy_variables <- model.matrix(~ workout - 1, data = weightloss)
weightloss <- cbind(weightloss, dummy_variables)
dummy_variables <- model.matrix(~ diet - 1, data = weightloss)
weightloss <- cbind(weightloss, dummy_variables)
View(weightloss)

fitnull<-lm(loss ~ workoutW1 +workoutW2 + workoutW3 + dietD1 + dietD2 + dietD3 + dietD4 + workoutW1:dietD1 + workoutW1:dietD2 + workoutW1:dietD3 + workoutW1:dietD4 + workoutW2:dietD1 + workoutW2:dietD2 + workoutW2:dietD3 + workoutW2:dietD4 + workoutW3:dietD1 + workoutW3:dietD2 + workoutW3:dietD3 + workoutW3:dietD4,data=weightloss)
stepSR<-step(fitnull, scope=list(lower = ~ 1,
                                 upper = ~ workoutW1 +workoutW2 + workoutW3 + dietD1 + dietD2 + dietD3 + dietD4 + workoutW1:dietD1 + workoutW1:dietD2 + workoutW1:dietD3 + workoutW1:dietD4 + workoutW2:dietD1 + workoutW2:dietD2 + workoutW2:dietD3 + workoutW2:dietD4 + workoutW3:dietD1 + workoutW3:dietD2 + workoutW3:dietD3 + workoutW3:dietD4),
             direction="both", data=weightloss)

final_model = lm(loss~workoutW1 +workoutW2 + dietD1 + dietD2 + dietD3 + workoutW1:dietD2 + workoutW2:dietD2, data=weightloss )
summary(final_model)

# (j) Provide a graphical representation for the final model.

leaps<-regsubsets(loss~workoutW1 +workoutW2 + dietD1 + dietD2 + dietD3 + workoutW1:dietD2 + workoutW2:dietD2, data=weightloss )
plot(leaps,scale="r2")
plot(leaps,scale="adjr2")
subsets(leaps, statistic="rsq")
subsets(leaps, statistic="adjr2")
subsets(leaps, statistic="bic")


# (k) Compare the constant model against the (full) main effects model and
# the (full) interaction model. Are the models different?

fitM0<-lm(loss~workoutW1 +workoutW2 + workoutW3 + dietD1 + dietD2 + dietD3 + dietD4 + workoutW1:dietD1 + workoutW1:dietD2 + workoutW1:dietD3 + workoutW1:dietD4 + workoutW2:dietD1 + workoutW2:dietD2 + workoutW2:dietD3 + workoutW2:dietD4 + workoutW3:dietD1 + workoutW3:dietD2 + workoutW3:dietD3 + workoutW3:dietD4,data=weightloss)
fitM1<-lm(loss~workoutW1 +workoutW2 + workoutW3 + dietD1 + dietD2 + dietD3 + dietD4,data=weightloss)
fitM2<-lm(loss~1,data=weightloss)
anova(fitM0,fitM2)
anova(fitM1,fitM2)
