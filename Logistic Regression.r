print("The code section below will first install two R packages: ResourceSelection and pROC.") 
print("Please do not move to the next step until the packages are fully installed.") 
print("This will take some time.") 
print("Once the installation is complete, this step will print first 6 rows of the data set.")


# Loading R packages that are needed for some calculations below
install.packages("ResourceSelection")
install.packages("pROC")

# Loading credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# Converting appropriate variables to factors  
credit_default <- within(credit_default, {
  default <- factor(default)
  sex <- factor(sex)
  education <- factor(education)
  marriage <- factor(marriage)
  assets <- factor(assets)
  missed_payment <- factor(missed_payment)
})

print("installation completed!")
print("data set (first 6 observations)")
head(credit_default, 6)

# Create the complete model
logit <- glm(default ~ credit_utilize + education , data = credit_default, family = "binomial")

summary(logit)

conf_int <- confint.default(logit, level=0.95)
round(conf_int,4)

library(ResourceSelection)


print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(logit$y, fitted(logit), g=50)
hl

# Predict default or no_default for the data set using the model
default_model_data <- credit_default[c('education', 'credit_utilize')]
pred <- predict(logit, newdata=default_model_data, type='response')

# If the predicted probability of default is >=0.50 then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(pred >= 0.5, '1', '0'))

# This creates the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# Print nicely formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(pROC)

labels <- credit_default$default
predictions <- logit$fitted.values

roc <- roc(labels ~ predictions)

print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: education is high school (education='1'), credit utilization is 40% (credit_utilize=0.40)")
newdata1 <- data.frame(education="1", credit_utilize=0.40)
pred1 <- predict(logit, newdata1, type='response')
round(pred1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
newdata2 <- data.frame(education="3", credit_utilize=0.35)
pred2 <- predict(logit, newdata2, type='response')
round(pred2, 4)

print("The code section will first install two R packages ResourceSelection and pROC.") 

# This line will Load R packages
install.packages("ResourceSelection")
install.packages("pROC")

# This line will load credit card default data set
credit_default <- read.csv(file='credit_card_default.csv', header=TRUE, sep=",")

# This line will convert the variables to factors  
credit_default <- within(credit_default, {
  default <- factor(default)
  sex <- factor(sex)
  education <- factor(education)
  marriage <- factor(marriage)
  assets <- factor(assets)
  missed_payment <- factor(missed_payment)
})

print("installation completed!")
print("data set (first 10 observations)")
head(credit_default, 10)

# This line will show the column count
ncol(credit_default)

# This line will show the row count
nrow(credit_default)

# Line that creates logistic regression model
lrm <- glm(default ~ credit_utilize + education, data = credit_default, family = "binomial")

summary(lrm)

# This line will show the prediction, default or no_default for this model
dfm <- credit_default[c('education', 'credit_utilize')]
predix <- predict(lrm, newdata=dfm, type='response')

# This line will show the predicted probability of default that is >=0.50 
# then predict credit default (default='1'), otherwise predict no credit 
# default (default='0') 
depvar_pred = as.factor(ifelse(predix <- predict(lrm, newdata=dfm, type='response')
                               >= 0.5, '1', '0'))

# This line will create the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# This line will format confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(ResourceSelection)
# This line will calculate and print the Hosmer-Lemeshow GOF
print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(lrm$y, fitted(lrm), g=50)
hl

# This line will perform and calculate Wald's test at 5% level of significance
conf_int <- confint.default(lrm, level=0.95)
round(conf_int,4)

library(pROC)

labels <- credit_default$default
predictions <- lrm$fitted.values

roc <- roc(labels ~ predictions)

# This line will show the AUC and ROC curve
print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# This line will indicate the true Positive Rate (Sensitivity) and 
# False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction: education is high school (education='1'), credit utilization is 35% (credit_utilize=0.35)")
modcredit1 <- data.frame(education="1", credit_utilize=0.35)
predix1 <- predict(lrm, modcredit1, type='response')
round(predix1, 4)

print("Prediction: education is postgraduate (education='3'), credit utilization is 35% (credit_utilize=0.35)")
modcredit2 <- data.frame(education="3", credit_utilize=0.35)
predix2 <- predict(lrm, modcredit2 <- data.frame(education="3", credit_utilize=0.35)
                   , type='response')
round(predix2, 4)

# 4. Second Logistic Regression Model
# Line that creates second logistic regression model
slrm <- glm(default ~ credit_utilize + assets+ missed_payment, data = credit_default, family = "binomial")

summary(slrm)

# This line will predict default or no_default for the data set using the model
dfm2 <- credit_default[c('credit_utilize', 'assets', 'missed_payment')]
predix2 <- predict(slrm, newdata=dfm2, type='response')

# This line will show the predicted probability of default that is >=0.50 
# then predict credit default (default='1'), otherwise predict no credit 
# default (default='0')
depvar_pred = as.factor(ifelse(predix2 <- predict(slrm, newdata=dfm2, type='response')
                               >= 0.5, '1', '0'))

# This line will create the confusion matrix
conf.matrix <- table(credit_default$default, depvar_pred)[c('0','1'),c('0','1')]
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ": default=")
colnames(conf.matrix) <- paste("Prediction", colnames(conf.matrix), sep = ": default=")

# This line will print and formatted confusion matrix
print("Confusion Matrix")
format(conf.matrix,justify="centre",digit=2)

library(ResourceSelection)
# This line will calculate and print the Hosmer-Lemeshow GOF
print("Hosmer-Lemeshow Goodness of Fit Test")
hl = hoslem.test(slrm$y, fitted(slrm), g=50)
hl

# This line will perform and calculate Wald's test at 5% level of significance
conf_int <- confint.default(slrm, level=0.95)
round(conf_int,4)

library(pROC)

labels <- credit_default$default
predictions <- slrm$fitted.values

roc <- roc(labels ~ predictions)

# This line will show the AUC and ROC curve
print("Area Under the Curve (AUC)")
round(auc(roc),4)

print("ROC Curve")
# True Positive Rate (Sensitivity) and False Positive Rate (1 - Specificity)
plot(roc, legacy.axes = TRUE)

print("Prediction (assets='1', missed_payment='1'), credit utilization is 35% (credit_utilize=0.35)")
modcredit3 <- data.frame(assets="1", missed_payment="1", credit_utilize=0.35)
predix4 <- predict(slrm, modcredit3, type='response')
round(predix4, 4)

print("Prediction  (assets='3', missed_payment='0'), credit utilization is 35% (credit_utilize=0.35)")
modcredit4 <- data.frame(assets="3", missed_payment="0", credit_utilize=0.35)
predix5 <- predict(slrm, modcredit4, type='response')
round(predix5, 4)