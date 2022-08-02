library(MASS)
library(ROCR)
library(tidyverse)
library(broom)
library(car)

## DATA LOADING
setwd(dir="~/Desktop/Data Science - MA4/Applied Biostatistics/Project/Individual Project/")
train.datafile.path <- "data/adult.data"
test.datafile.path <- "data/adult.test"

train.data.raw <- read.csv(train.datafile.path, header=FALSE)
test.data.raw <- read.csv(test.datafile.path, header=FALSE)

## DATA PRE-PROCESSING
# Remove the row with unknown values
is.na(train.data.raw) <- train.data.raw == ' ?'
train.data <- na.omit(train.data.raw)

is.na(test.data.raw) <- test.data.raw == ' ?'
test.data <- na.omit(test.data.raw)

# Rename the variables with relevant names
colnames(train.data) <- c("age", "workclass", "demographic_weight", "education",
                          "education_num", "marital_status", "occupation",
                          "relationship", "race", "sex", "capital_gain",
                          "capital_loss", "hours_per_week", "native_country",
                          "upper_50K")

colnames(test.data) <- c("age", "workclass", "demographic_weight", "education",
                         "education_num", "marital_status", "occupation",
                         "relationship", "race", "sex", "capital_gain",
                         "capital_loss", "hours_per_week", "native_country",
                         "upper_50K")

# Convert the target variable into binary
train.data$`upper_50K` <- ifelse(train.data$`upper_50K`==" >50K", 1, 0)
test.data$`upper_50K` <- ifelse(test.data$`upper_50K`==" >50K.", 1, 0)

# Convert age as a numeric variable in the test data
test.data$age <- as.numeric(test.data$age)

## MODEL SELECTION

# Model choice base on effect size and AIC
adult.glm.1 <- glm(upper_50K ~ age + workclass + education_num + education +
                   marital_status + occupation + relationship + race + sex + 
                   capital_gain + capital_loss + hours_per_week + 
                   native_country + demographic_weight, 
                   data = train.data, family = binomial()) 
summary(adult.glm.1)

adult.glm.2 <- glm(upper_50K ~ age + workclass + education_num +
                   marital_status + occupation + relationship + race + sex + 
                   capital_gain + capital_loss + hours_per_week,
                   data = train.data, family = binomial())
summary(adult.glm.2)

adult.glm.3 <- glm(upper_50K ~ age + sex + education_num + relationship +  
                   occupation + capital_gain + capital_loss + hours_per_week, 
                   data = train.data, family = binomial())
summary(adult.glm.3)

# Compute accuracy on test set
# Model 1
pred.1 <- predict(adult.glm.1, newdata=test.data, type="response")
confusion_mat.1 <- table(test.data$upper_50K, pred.1 > 0.5)
accuracy.1 <- sum(diag(confusion_mat.1)) / sum(confusion_mat.1)

# Model 2
pred.2 <- predict(adult.glm.2, newdata=test.data, type="response")
confusion_mat.2 <- table(test.data$upper_50K, pred.2 > 0.5)
accuracy.2 <- sum(diag(confusion_mat.2)) / sum(confusion_mat.2)

# Model 3
pred.3 <- predict(adult.glm.3, newdata=test.data, type="response")
confusion_mat.3 <- table(test.data$upper_50K, pred.3 > 0.5)
accuracy.3 <- sum(diag(confusion_mat.3)) / sum(confusion_mat.3)

# MODEL ASSESSMENT 

# Linear relation between logit and linear predictors
test.data.logit <- test.data %>%
  dplyr::select(all_of(c("age", "capital_gain", "capital_loss", 
                         "education_num", "hours_per_week"))) 
predictors <- colnames(test.data)

test.data.logit <- test.data.logit %>%
  mutate(logit = log(pred.3/(1-pred.3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

#ggplot(test.data.logit, aes(logit, predictor.value))+
#  geom_point(size = 0.5, alpha = 0.5) +
#  geom_smooth(method = "loess") + 
#  theme_bw() + 
#  facet_wrap(~predictors, scales = "free_y")

# Multicollinearity & VIF

car::vif(adult.glm.3)

# Outliers & Cook's distance

plot(adult.glm.3, which = 4, id.n = 3)

# Compute ROC curve & AUC

# Final model
ROCRpred.3 <- prediction(pred.3, test.data$upper_50K)
ROCRperf.3 <- performance(ROCRpred.3, measure = "tpr", x.measure = "fpr")
plot(ROCRperf.3, main="ROC curve of the Final model", cex.lab=1.5, cex.main=2)

auc.3 <- performance(ROCRpred.3, measure = "auc")
auc.3 <- auc.3@y.values[[1]]
auc.3

# Kohavi's model
ROCRpred.1 <- prediction(pred.1, test.data$upper_50K)
ROCRperf.1 <- performance(ROCRpred.1, measure = "tpr", x.measure = "fpr")
plot(ROCRperf.1, main="ROC curve of Kohavi's model", cex.lab=1.5, cex.main=2)

auc.1 <- performance(ROCRpred.1, measure = "auc")
auc.1 <- auc.1@y.values[[1]]
auc.1