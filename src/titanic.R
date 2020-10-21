
library(tidyverse)
library(mice)
library(fastDummies)
library(car)

## DATA LOADING ----

# Get raw data
train <- read.csv("./data/raw/titanic/train.csv")
test <- read.csv("./data/raw/titanic/test.csv")

# Lookup to survived flag
surv <- train[,1:2]


## INITIAL CLEANING ----

# Combine data for joint processing
comb.pre <- cbind(rbind(train[,-2], test), set = c(rep('Train', nrow(train)), rep('Test', nrow(test)))) %>% 
  mutate(Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))

# Set blank Embarked levels to missing
levels(comb.pre$Embarked)[levels(comb.pre$Embarked) == ''] <- NA

# Check missing patterns
md.pattern(comb.pre) # 263 on age, 2 on Embarked, 1 on Fare

# Use mice package to impute missing
imp <- mice(comb.pre, m = 1, maxit = 50)

# Check convergence
plot(imp)

# Look at plausibility
stripplot(imp, Age~.imp, pch=20, cex=2)
stripplot(imp, Embarked~.imp, pch=20, cex=2)
stripplot(imp, Fare~.imp, pch=20, cex=2)

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
shapiro.test(comb$Age) # nope


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

plot(sib01.mod)

p.sib01 <- sum(sib01) / length(sib01)

(SibSp.testAge <- (log(p.sib01 / (1 - p.sib01)) - sib01.mod$coefficients[1]) / sib01.mod$coefficients[2])
# May suggest that ages 23 and above are more likely to have a value of 0 or 1 for SibSp (i.e., spouse rather than sibling)


# Try clustering across the relevant variables
addat <- comb[,c("Age", "SibSp", "Parch")]

adclus <- kmeans(scale(addat), 3)
print(adclus)

addat$cluster <- factor(adclus$cluster)

table(addat$cluster)

addat %>% 
  ggplot(aes(Age, fill = cluster)) + 
  geom_density(alpha = .5)

# Find point of overlap between distributions








## Joint operations (create dummy encoding)

comb.clean <- comb %>% 
  mutate()


Pclass.dummy <- dummy_cols(as.factor(comb$Pclass), remove_first_dummy = TRUE)



levels(as.factor(comb$Pclass))

str(Pclass.dummy)

names(Pclass.dummy) <- 



