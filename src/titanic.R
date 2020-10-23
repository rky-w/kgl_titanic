
library(tidyverse)
library(mice)
library(fastDummies)
library(car)
library(factoextra)
library(caret)
library(naivebayes)
library(kernlab)
library(mboost)
library(ranger)
library(xgboost)

set.seed(2210)

## DATA LOADING ----

# Get raw data
train <- read.csv("./data/raw/titanic/train.csv")
test <- read.csv("./data/raw/titanic/test.csv")

# Survived indicator
surv <- train[, 2]

## INITIAL CLEANING ----

# Combine data for joint processing
comb.pre <- cbind(rbind(train[,-2], test), set = c(rep('Train', nrow(train)), rep('Test', nrow(test)))) %>% 
  mutate(Pclass = Pclass,
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked),
         Fare = ifelse(Fare == 0, NA, Fare),
         cabin.pfx = as.factor(str_extract(Cabin, '^[A-z]+')),
         ticket.pfx = str_extract(Ticket, '^[A-z\\.]+')
  )

# Replace less common ticket prefixes with 'Other'
comb.pre$ticket.pfx[comb.pre$ticket.pfx %in% names(which(prop.table(sort(table(comb.pre$ticket.pfx))) < .015))] <- 'Other'
# Set to factor
comb.pre$ticket.pfx <- as.factor(comb.pre$ticket.pfx)

# Set blank Embarked levels to missing
levels(comb.pre$Embarked)[levels(comb.pre$Embarked) == ''] <- NA

# Check missing patterns
md.pattern(comb.pre) # 1014 on cabin.pfx, 957 on ticket.pfx, 263 on age, 18 on Fare, 2 on Embarked

# Use mice package to impute missing
imp <- mice(comb.pre, m = 1, maxit = 50)

# Check convergence
plot(imp)

# Look at plausibility
stripplot(imp, Age~.imp, pch=20, cex=2)
stripplot(imp, cabin.pfx~.imp, pch=20, cex=2)
stripplot(imp, Embarked~.imp, pch=20, cex=2)
stripplot(imp, Fare~.imp, pch=20, cex=2)
stripplot(imp, ticket.pfx~.imp, pch=20, cex=2)


# Impute the missing values
comb <- complete(imp)



## EDA and Feature Engineering ----

## Pclass
comb %>%
  group_by(set, Pclass) %>% 
  summarise(freq = n()) %>% 
  group_by(set) %>% 
  mutate(pct = freq / sum(freq)) %>% 
  ggplot(aes(x = Pclass, y = pct, fill = set)) +
  geom_col(position = "dodge")


## Name

# Surname
surname <- str_extract(comb$Name, '^[A-z]+')

# Title
title <- str_replace(comb$Name, '.*, ([A-z ]+).*', '\\1')
title[title %in% c('Ms', 'Mme')] <- 'Mrs'
title[title %in% c('Mlle')] <- 'Miss'
title[!title %in% c('Mr', 'Mrs', 'Master', 'Miss', 'Rev', 'Dr')] <- 'Other'

data.frame(title = title, set = comb$set) %>%
  group_by(set, title) %>% 
  summarise(freq = n()) %>% 
  group_by(set) %>% 
  mutate(pct = freq / sum(freq)) %>% 
  ggplot(aes(x = title, y = pct, fill = set)) +
  geom_col(position = "dodge")


## Sex
comb %>% 
  group_by(set, Sex) %>% 
  summarise(freq = n()) %>% 
  group_by(set) %>% 
  mutate(pct = freq / sum(freq)) %>% 
  ggplot(aes(x = Sex, y = pct, fill = set)) +
  geom_col(position = "dodge")


## Age
comb %>% 
  ggplot(aes(Age, fill = set)) +
  geom_boxplot() +
  coord_flip()

comb %>% 
  ggplot(aes(Age, fill = set)) + 
  geom_density(alpha = .5)

# Normality?
shapiro.test(comb$Age)

# Boxcox transformation
age.bc <- car::powerTransform(comb$Age, family="bcPower")

summary(age.bc)

bctrans <- function(var, lambda) {
  if(lambda == 0L){
    log(var)
  } else {
    (var^lambda - 1) / lambda
  }
}
age.trans <- bctrans(comb$Age, age.bc$lambda)

# Re-check
data.frame(age.trans, set = comb$set) %>% 
  ggplot(aes(age.trans, fill = set)) +
  geom_density(alpha = .5)


## SibSp and Parch

# Family counter

family <- comb$SibSp + comb$Parch

data.frame(family, set = comb$set) %>% 
  group_by(set, family) %>% 
  summarise(freq = n()) %>% 
  group_by(set) %>% 
  mutate(pct = freq / sum(freq)) %>% 
  ggplot(aes(x=family, y=pct, fill = set)) +
  geom_col(position = "dodge")


# Parents / Children / Siblings / Spouses

# Find probability of SibSp being 0 or 1 across age
comb %>% 
  ggplot(aes(Age, SibSp)) + 
  geom_point() +
  geom_smooth()

sib01 <- comb$SibSp <= 1

sib01.mod <- glm(sib01 ~ comb$Age, family = binomial(link = "logit"))

p.sib01 <- sum(sib01) / length(sib01)

(SibSp.testAge <- (log(p.sib01 / (1 - p.sib01)) - sib01.mod$coefficients[1]) / sib01.mod$coefficients[2])
# May suggest that ages 23 and above are more likely to have a value of 0 or 1 for SibSp (i.e., spouse rather than sibling)


# Try clustering across the relevant variables
addat <- comb[,c("Age", "SibSp", "Parch")]

addat.scale <- scale(addat)

adclus <- kmeans(addat.scale, 3)

addat$cluster <- factor(adclus$cluster)

table(addat$cluster)

fviz_cluster(adclus, data = addat[,-4])

addat %>% 
  ggplot(aes(Age, fill = cluster)) + 
  geom_density(alpha = .5)


## Fare
comb %>% 
  ggplot(aes(Fare, fill = set)) +
  geom_density() # some extreme values

boxplot(comb$Fare)

# Boxcox transformation
fare.bc <- car::powerTransform(comb$Fare, family="bcPower")
fare.trans <- bctrans(comb$Fare, fare.bc$lambda)

densityPlot(fare.trans)
boxplot(fare.trans)

sum(abs(scale(fare.trans)) > 1.96)


## Ticket
prop.table(table(comb$set, comb$ticket.pfx), margin = 1)

## Embarked
prop.table(table(comb$set, comb$Embarked), margin = 1)

## Cabin
prop.table(table(comb$set, comb$cabin.pfx), margin = 1)




## Final cleaning and encoding ----

comb.clean <- comb %>% 
  mutate(
    Age.trans = age.trans,
    Fare.trans = fare.trans,
    Title = title, 
    Family = family,
    Cluster = addat$cluster
  ) %>% 
  select(-c(PassengerId, Name, Ticket, Fare, Cabin, Age))


# Split back into train and test
train.clean <- comb.clean %>% filter(set == 'Train') %>% select(-set)
test.clean  <- comb.clean %>% filter(set == 'Test') %>%  select(-set)


## Modelling on train.clean ----

# CV setup
tr.ctrl <- trainControl(method = "repeatedcv", 
                        number = 10, 
                        repeats = 5,
                        allowParallel = TRUE,
                        search = "grid")


# Naive Bayes ----
nb.grid <-  expand.grid(laplace = 1,
                        usekernel = c(TRUE, FALSE),
                        adjust = 1)

nb.mod <- train(as.factor(surv)~., 
                data = cbind(surv, train.clean), 
                trControl = tr.ctrl, 
                method = "naive_bayes",
                tuneGrid = nb.grid
)

summary(nb.mod)
print(nb.mod)
confusionMatrix(table(predict(nb.mod, train.clean), surv))


# Logistic Regression ----

lr.mod <- train(as.factor(surv)~.,
                data = cbind(surv, train.clean),
                trControl = tr.ctrl,
                method = "glmStepAIC",
                family = binomial(link = "logit")
)


summary(lr.mod)
print(lr.mod)
confusionMatrix(table(predict(lr.mod, train.clean), surv))


# Boosted logistic regression ----

lb.grid <-  expand.grid(mstop = 10^(1:3),
                        prune = 'yes'
)

lb.mod <- train(as.factor(surv)~.,
                data = cbind(surv, train.clean),
                trControl = tr.ctrl,
                method = "glmboost",
                family = Binomial(link = "logit"),
                tuneGrid = lb.grid
)


summary(lb.mod)
print(lb.mod)
ggplot(lb.mod)
confusionMatrix(table(predict(lb.mod, train.clean), surv))


## SVM ----

sv.grid <-  expand.grid(C = c(0.01, 0.1, 0.5, 1)
)

sv.mod <- train(as.factor(surv)~.,
                data = cbind(surv, train.clean),
                trControl = tr.ctrl,
                method = "svmLinear",
                tuneGrid = sv.grid
)


summary(sv.mod)
print(sv.mod)
ggplot(sv.mod)
confusionMatrix(table(predict(sv.mod, train.clean), surv))

## SVM Poly ----

sp.grid <-  expand.grid(C = c(0.0001, 0.001, 0.01),
                        degree = c(2,3),
                        scale = c(TRUE)
)

sp.mod <- train(as.factor(surv)~.,
                data = cbind(surv, train.clean),
                trControl = tr.ctrl,
                method = "svmPoly",
                tuneGrid = sp.grid
)


summary(sp.mod)
print(sp.mod)
ggplot(sp.mod)
confusionMatrix(table(predict(sp.mod, train.clean), surv))


# Random forest ----

rf.grid <-  expand.grid(mtry = 2:ncol(train.clean), 
                        splitrule = c("gini", "extratrees"), 
                        min.node.size = c(1,3,5)
)

rf.mod <- train(as.factor(surv)~.,
                data = cbind(surv, train.clean),
                trControl = tr.ctrl,
                method = "ranger",
                tuneGrid = rf.grid
)


summary(rf.mod)
print(rf.mod)
ggplot(rf.mod)
confusionMatrix(table(predict(rf.mod, train.clean), surv))


# xgboost ----

# xg.ctrl <- trainControl(method = "repeatedcv", 
#                         number = 2, 
#                         repeats = 1,
#                         allowParallel = TRUE,
#                         search = "random")
# 
# xg.grid <-  expand.grid(
#   nrounds = 10^(1:3),
#   max_depth = 10^(1:3),
#   eta = c(0.2, 0.3, 0.5),
#   gamma = c(0, 1),
#   colsample_bytree = c(0.5, 1),
#   min_child_weight = c(1, 3, 5),
#   subsample = c(0.5, 1)
# )
# 
# xg.mod <- train(as.factor(surv)~.,
#                 data = cbind(surv, train.clean),
#                 trControl = xg.ctrl,
#                 method = "xgbTree",
#                 tuneGrid = xg.grid,
#                 tuneLength = 1
# )
# 
# 
# summary(xg.mod)
# print(xg.mod)
# ggplot(xg.mod)
# confusionMatrix(table(predict(xg.mod, train.clean), surv))






## Ensemble predictions ----

votes <- data.frame(
as.integer(as.character(predict(lr.mod, test.clean))),
as.integer(as.character(predict(sv.mod, test.clean))),
as.integer(as.character(predict(sp.mod, test.clean))),
as.integer(as.character(predict(lb.mod, test.clean))),
as.integer(as.character(predict(rf.mod, test.clean)))
)

test.preds <- ifelse(apply(votes, 1, mean) > .5, 1, 0)

test.output <- data.frame(test$PassengerId, Survived = test.preds)

table(test.preds, test$Sex)

write.csv(test.output, "./data/output/titanic_submission.csv", row.names = FALSE)



