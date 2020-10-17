
library(tidyverse)
library(fastDummies)
library(car)

## DATA LOADING ----

# Get raw data
train <- read.csv("./data/raw/titanic/train.csv")
test <- read.csv("./data/raw/titanic/test.csv")

# Lookup to survived flag
surv <- train[,1:2]

# Combine data for joint processing
comb <- cbind(rbind(train[,-2], test), set = c(rep('Train', nrow(train)), rep('Test', nrow(test)))) %>% 
  mutate(Pclass = as.factor(Pclass),
         Sex = as.factor(Sex),
         Embarked = as.factor(Embarked))


## EDA and Feature Engineering ----

# First look
summary(comb)

# note missing on Age, Fare, and Embarked


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

# Will need to first deal with missing




comb %>% 
  ggplot(aes(Age, fill = set)) +
  geom_boxplot() +
  coord_flip()

comb %>% 
  ggplot(aes(Age, fill = set)) + 
  geom_density(alpha = .5)
  
# Normality?
ks.test(comb$Age, 'pnorm') # nope
shapiro.test(comb$Age) # nope


# Boxcox transformation
age.bc <- car::powerTransform(comb$Age, family="bcPower")

bctrans <- function(var, lambda) {
  if(lambda == 0L){
    log(var)
  } else {
    (var^lambda - 1) / lambda
  }
}

age.trans <- bctrans(comb$Age, age.bc$lambda)

data.frame(age.trans, set = comb$set) %>% 
  ggplot(aes(age.trans, fill = set)) +
  geom_density(alpha = .5)




## Joint operations (create dummy encoding)

comb.clean <- comb %>% 
  mutate()


Pclass.dummy <- dummy_cols(as.factor(comb$Pclass), remove_first_dummy = TRUE)



levels(as.factor(comb$Pclass))

str(Pclass.dummy)

names(Pclass.dummy) <- 



