# DATA CLEANING
# 1) selecting variables from dataset
library(dplyr)
data(starwars) # loading data
names(starwars)
head(starwars)
str(starwars)

# keep the variables name, height and gender
newdata <- select(starwars, name, height, gender)

# we can highlight using range
newdata <- select(starwars, name, mass:species)
newdata
names(newdata)

# keep all variables apart from birth_year and gender
newdata <- select(starwars, -birth_year, -gender)
newdata
names(newdata)

# 2) Selecting observations
# selecting females
n_d <- filter(starwars, gender == "female")
n_d

# select females from Alderan
nd <- select(starwars,
             sex == "male" &
             homeworld == "Alderaan")
starwars$gender
head(starwars)
?dplyr

# MISSING DATA
# 1) Feature selection
data(msleep, package = "ggplot2")
head(msleep)
names(msleep)

is.na(msleep)
# checking the proportion of missing data for each variable
miss <- colSums(is.na(msleep))/nrow(msleep)
miss

# Listwise deletion
df <- select(msleep, genus, vore, conservation)
df1 <- na.omit(newdata)
df1
is.na(df1)

# Data Imputation
library(VIM)
ndf <- kNN(msleep, k=5)
head(msleep)

#  PUTTING STRUCTURE
library(recipes)
install.packages("magrittr")
# Feature engineering
blueprint <- recipe(bodywt~., data = msleep) %>%
  step_nzv(all_nominal()) %>% # removing all near variances
  step_impute_mean(all_predictors(), neighbors = 2) %>% # Imputation
  step_YeoJohnson(all_numeric()) %>% # Normalization
  step_center(all_numeric(), -all_outcomes()) %>% # Standardization
  step_scale(all_numeric(), -all_outcomes()) %>% # Starndardization
  step_integer(matches("Qual|Cond|QC|Qu")) %>% # Label_encoding
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) # One-hot encoding

newdata <- blueprint %>%
  prep(msleep) %>%
  bake(msleep)

newdata
is.na(newdata)
