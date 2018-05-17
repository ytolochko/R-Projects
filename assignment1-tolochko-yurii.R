library(survey)
library(samplingbook)
library(plyr)
library(dplyr)
library(foreign)
library(arm)
library(MASS)
library(readstata13)
library(tidyverse)
library(Amelia)
library(Rcpp)
library(VIM)
library(colorspace)
library(data.table)
library(Zelig)

# Problem 3
# (a)
data <- read.csv('ABS2010.csv')
# The amount of missing values for each question. 
# There might be many reasons for missing values. One of them could have to do with the Chinese stance on democracy.
# One possible reason for missing values is that the respondents percieve these question to be quite sensitive to them
# and thus they do not want to answer these question.
# Some people might feel uneasy to answer about the level of democracy in China.
# One interesting finding is that the most NAs are in the question about the level of democracy in the past.
# This might indicate that people do not want to give their opinion on this topic. 

sum(is.na(data['current_democracy']))
sum(is.na(data['previous_democracy']))
sum(is.na(data['future_democracy']))

# (b)
# We have some quite interesting results. First of all, there is a very clear trend between the three variables.
# Previous democracy has mean 5.5, current democracy has mean 7.6 and future democracy has a very high mean of 9.2
# This indicates that the population percieves democracy as a desirable value which and that the respondents percieve the chinese society to move towards democracy.
# More techincally, there are also interesting differences between the variances of the three questions.
# Previous democracy has the most variance - which means that people have most diverse opinions about the level of democracy in the past.
# Present democracy has a significantly lower variance, which means that respondents are agreeing more on the same level (but still quite high).
# And future democracy, that is how important is democracy for the population has a much lower variance than the previous questions (almost 3 times as low).
# This indicates that the respondents have very similar opinions (less differences in opinions) on the desired state of democracy - it is very high (mean = 9.2).

summary(data['current_democracy'])
var(data['current_democracy'], na.rm = TRUE)
summary(data['previous_democracy'])
var(data['previous_democracy'], na.rm = TRUE)
summary(data['future_democracy'])
var(data['future_democracy'], na.rm = TRUE)

d <- density(data[,'current_democracy'], na.rm = T) # returns the density data 
plot(d)
d <- density(data[,'previous_democracy'], na.rm = T) # returns the density data 
plot(d)
d <- density(data[,'future_democracy'], na.rm = T) # returns the density data 
plot(d)

#(c)
#(i)
survey_design <- svydesign(id=~1, weight = ~CNweight, data = data)
#(ii)
# since there are some NAs, we need to get rid of them
# here we delete rows where there is no gender data, because we want to stratify by gender
# we save the data in a new dataframe, because we will need to work with the original later and we want it to remain unchange  
complete_vector <- complete.cases(data[,'gender'])
data_gender <- data[complete_vector,]

survey_design_gender <- svydesign(id=~1, weight = ~CNweight, data = data_gender)
pop_gender = data.frame(gender = c('Male', 'Female'), Freq = c(682329104, 650481765))
samp_design_ps_gender <- postStratify(survey_design_gender, strata = ~gender, population = pop_gender)

#(iii)
complete_vector <- complete.cases(data[,'region'])
data_region <- data[complete_vector,]
survey_design_region <- svydesign(id=~1, weight = ~CNweight, data = data_region)
pop_region = data.frame(region = c('urban', 'rural'), Freq = c(665575306, 674149546))
samp_design_ps_age <- postStratify(survey_design_region, strata = ~region, population = pop_region)

#(iiii)
# filter out the NAs in age
complete_vector <- complete.cases(data[,'age'])
data_age <- data[complete_vector,]

# let's see which age is the minimum and maximum, so we don't have to recode those that do not exist
# we see that the minimum age is 18
min(data_age[,'age'])
# the maximum is 93
max(data_age[,'age'])
# setup a variable in which to store the categories
data_age$age_group <- 'None'

# recode the age into categories
data_age[data_age[,'age'] <= 19,]$age_group = '15-19'
data_age[(data_age[,'age'] > 19) & (data_age[,'age'] <=24), ]$age_group <- "20-24"
data_age[(data_age[,'age'] > 24) & (data_age[,'age'] <=29), ]$age_group <- "25-29"
data_age[(data_age[,'age'] > 29) & (data_age[,'age'] <=34), ]$age_group <- "30-34"
data_age[(data_age[,'age'] > 34) & (data_age[,'age'] <=39), ]$age_group <- "35-39"
data_age[(data_age[,'age'] > 39) & (data_age[,'age'] <=44), ]$age_group <- "40-44"
data_age[(data_age[,'age'] > 44) & (data_age[,'age'] <=49), ]$age_group <- "45-49"
data_age[(data_age[,'age'] > 49) & (data_age[,'age'] <=54), ]$age_group <- "50-54"
data_age[(data_age[,'age'] > 54) & (data_age[,'age'] <=59), ]$age_group <- "55-59"
data_age[(data_age[,'age'] > 59) & (data_age[,'age'] <=64), ]$age_group <- "60-64"
data_age[(data_age[,'age'] > 64) & (data_age[,'age'] <=69), ]$age_group <- "65-69"
data_age[(data_age[,'age'] > 69) & (data_age[,'age'] <=74), ]$age_group <- "70-74"
data_age[(data_age[,'age'] > 74) & (data_age[,'age'] <=79), ]$age_group <- "75-79"
data_age[(data_age[,'age'] > 79) & (data_age[,'age'] <=84), ]$age_group <- "80-84"
data_age[(data_age[,'age'] > 84) & (data_age[,'age'] <=89), ]$age_group <- "85-89"
data_age[(data_age[,'age'] > 89) & (data_age[,'age'] <=94), ]$age_group <- "90-94"

# info about amount of people in each age group
pop_age_group <-data.frame(age_group = c('15-19', "20-24","25-29","30-34", "35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94"),
                            Freq = c(99889114,127412518,101013852,97138203,118025959,124753964,105594553,78753171,81312474,58667282,41113282,32972397,23852133,13373198,5631928,1578307))

survey_design_age <- svydesign(id=~1, weight = ~CNweight, data = data_age)

#(iv)
samp_design_rake <- rake(survey_design_age, list(~region, ~age_group), list(pop_region, pop_age_group))

# We see some differences between the 4 survey designs.
# The differences between the first three occur because we recalculate the weights based on external information.
# This, in turn affects the mean value.
# Notice, however, that the standard error between the first three estimates does not change - this is expected.
# In the fourth case, we see some difference both in mean value as well as in standard error. 
# This is a driect result of the raking procedure.  
svymean(~current_democracy, survey_design, na.rm = T)
svymean(~current_democracy, samp_design_ps_gender, na.rm = T)
svymean(~current_democracy, samp_design_ps_age, na.rm = T)
svymean(~current_democracy, samp_design_rake, na.rm = T)

#(d)
#(i)
model <- lm(current_democracy ~ region + age + year_schooling + national_interest + internet_use + pol_discuss + contact_com_leader + trust_nat_gov + trust_local_gov + local_corruption, data = data)
summary(model)
#(ii)
model_significant <- lm(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, data = data)
summary(model_significant)

# deterministic imputation
pred_current_democracy <- predict(model_significant, data)
impute <- function(a, a.impute){
  ifelse (is.na(a), a.impute, a)
}

data$deterministic_current_democracy <- impute(data$current_democracy, pred_current_democracy)

# stochastic imputation
errors <- rnorm(length(pred_current_democracy), 0, sigma.hat(model_significant)) # normal distribution with mean 0, and variance > 0
stochastic_pred_current_democracy <- predict(model_significant, data) + errors
data$stochastic_current_democracy <- impute(data$current_democracy, stochastic_pred_current_democracy)

#(iii)
# mean and variance for deterministic imputation
summary(data['deterministic_current_democracy'])
var(data['deterministic_current_democracy'], na.rm = TRUE)

# Mean and variance for stochastic imputation
# We see expected results: 
# Mean is very close (this is because errors have mean 0, so deterministic and stochastic imputations have the same mean and we expect the mean of the imputed values to be identical or very close)
# The variance of the stochastically imputed values is somewhat higher. This is also in line with our expectations, because in stocastic imputation we add the random error, which does not affect the mean
# but does affect the variance (since we add some random component, the distribution become more "spread out", which means an increased variance).
summary(data['stochastic_current_democracy'])
var(data['stochastic_current_democracy'], na.rm = TRUE)

d <- density(data[,'deterministic_current_democracy'], na.rm = T) # returns the density data 
plot(d, main = 'deterministic_current_democracy')

d <- density(data[,'stochastic_current_democracy'], na.rm = T) # returns the density data 
plot(d,main = 'stochastic_current_democracy')

#(iV)
#Let's see what number of NAs does each variable have. Then, we pick those that have the least NAs in them, because we need as much information as possible for successful hotdecking
for (name in names(data)) {
  print(name)
  print(sum(is.na(data[,name])))
}

# here we use some of the variables that have the least amount of NAs
dat_hotdeck <- hotdeck(data, variable = "current_democracy", ord_var = c("age", "trust_party", "contact_influent_non_official", "pol_interest", "occupation"), imp_var = TRUE, imp_suffix="i")
summary(data$current_democracy)
summary(dat_hotdeck$current_democracy)
var(data$current_democracy, na.rm = TRUE)
var(dat_hotdeck$current_democracy)

# We can see that the differences between the means of the imputed and original values are very small (basically negligible).
# This means that our imputation was unbiased, because otherwise we would obtain a big difference between the means
# The variances are also quite similar, but the variance in hotdeck dataset is somewhat smaller.
# This is an expected result, because hotdecking substitutes the NAs with that of the closest data points from the dataset, thus practically duplicating the the rows.
# This, in turn, decreases the variability in the dataset (because now there are more similar data points), and this is why variance is lower.
# Now let's look at the distribution histograms - they are quite similar as well. 
hist(dat_hotdeck$current_democracy)
hist(data$current_democracy)

# Problem 4
#(i)
set.seed(12345)
# set all factor variables in idvar + the id variable "idnumber" + the colinear variables of deterministic and stochastic current democracy (correlate very well with current_democracy)
idvar = c('idnumber', 'region', 'gender', 'education', 'occupation', 'vote', 'deterministic_current_democracy', 'stochastic_current_democracy')

# In our dataset, we have no time-series (survey conducted in 2010 only) and no cross-sectional data (nothing about geographical distribution), so we specify no parameters
a.out <- amelia(data, m = 5, idvars = idvar) 

# We see that the distributions acorss different imputations are quite similar
# Comparing with original data - we get a similar picture, which means that the imputed values and the original values are similarly distributed (except for some artefacts such as imputed values >10)
par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out$imputations[[i]]$current_democracy, col = "grey", border = "white", main = paste0("Imputation ", i))
} 
hist(data$current_democracy)

# now for previous democracy
# Compare with original data - similar picture, again with some values being over the possible boundaries
par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out$imputations[[i]]$previous_democracy, col = "grey", border = "white", main = paste0("Imputation ", i))
} 
hist(data$previous_democracy)

# now for future democracy
# Compare with original data - similar picture. Since future democracy is very much skewed towards the higher values - mean is 9.2 with low variance,
# we expect that the quality of imputation would be lower here, mostly because a lot of values will "spill over" to values > 10.
# This is actually what we observe - imputed datasets have very similar distributions and means, but a lot of values are >10, which is of course a negative effect and lowers the quality of imputation. 
par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out$imputations[[i]]$future_democracy, col = "grey", border = "white", main = paste0("Imputation ", i))
} 
hist(data$future_democracy)

# now for democracy_fit
# Compare with original data - similar picture as in current_democracy, again with some values spilling over to more than logical maximum
par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out$imputations[[i]]$democracy_fit, col = "grey", border = "white", main = paste0("Imputation ", i))
} 
hist(data$democracy_fit)

#(ii)
# original data
mod <- zelig(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, model = 'ls', data = data)
summary(mod)

# multiple imputed data
imputed_mod <- zelig(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, model = 'ls', data = a.out)
summary(imputed_mod)

# We see that there are some interesting differences.
# For instance, some of the variables that were statistically significant in the model with original data
# become insignificant in the model with the imputed data (age, year_schooling).
# The reason for this is that the multiple imputation procedure account for the uncertainty of the imputed values (imputation uncertainty).
# Adding this uncertainty increases the variablity of the data, and this in turn decreases the efficiency of the linear regression.
# The imputed data looks more like a cloud instead of a straight line, which makes it less suitable for linear regression prediction (errors are larger).
# This is why we see that some of the regressors become statistically insignificant when using imputed dataset.  

# Problem 4
# (i)
# first, create the bounds for each of the variables of interest (the values for each democracy question are between 0 and 10, so this will be our bounds interval)
bds <- matrix(c(3, 0, 10, 4, 0, 10, 5, 0, 10, 6, 0, 10), nrow = 4, ncol = 3, byrow = TRUE)
a.out.bounded <- amelia(data, m = 5, idvars = idvar, bounds = bds) 

# we explicitly set the bins so that our histograms have the same scale
# (since imputed values are sometimes fractions, and not integers, the histograms often have differing amount of bins). 
bins<- c(0,1,2,3,4,5,6,7,8,9,10)

# Now, we can visually inspect whether the multiple imputation with bounds was successful. 
# We see that the imputed values are now in the expected region of [0, 10]. 
# While the distributions of imputed values largely look similar to distribution of original data,
par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out.bounded$imputations[[i]]$current_democracy, col = "grey", border = "white", breaks = bins, main = paste0("Imputation ", i))
} 
hist(data$current_democracy)

par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out.bounded$imputations[[i]]$previous_democracy, col = "grey", border = "white", breaks = bins, main = paste0("Imputation ", i))
} 
hist(data$previous_democracy)

par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out.bounded$imputations[[i]]$future_democracy, col = "grey", border = "white", breaks = bins, main = paste0("Imputation ", i))
} 
hist(data$future_democracy)

par(mfrow=c(2,3))
for (i in 1:5) {
  hist(a.out.bounded$imputations[[i]]$democracy_fit, col = "grey", border = "white", breaks = bins, main = paste0("Imputation ", i))
} 
hist(data$democracy_fit)

# original data
# we are predicting current_democracy with those variables that we have found to be statistically significant in the previous parts of this assignment
mod <- zelig(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, model = 'ls', data = data)
summary(mod)

# multiple imputed data
imputed_mod <- zelig(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, model = 'ls', data = a.out)
summary(imputed_mod)

# multiple imputed data with bounds
# We see that the regressors in the case of imputation with bounds have, on average, a higher level of significance than that of the imputed dataset without bounds
imputed_mod_bounded <- zelig(current_democracy ~ age + year_schooling + national_interest + internet_use + trust_nat_gov + trust_local_gov + local_corruption, model = 'ls', data = a.out.bounded)
summary(imputed_mod_bounded)

# Let's compare the variances between the datasets on example of current_democracy (the predicted variable) to possibly see why are there differences in the level of significance

# Variance in the original dataset
var(data[,'current_democracy'], na.rm = TRUE)

# Since we have 5 cases of multiple imputation, we will calculate the average of those 5 cases

# Create two variables to store the sum, then divide the sum by 5
sum_mi <- 0
sum_mi_bounded <- 0
for (i in seq(1:5)) {
  sum_mi <- sum_mi + var(a.out$imputations[[i]]['current_democracy'])
  sum_mi_bounded <- sum_mi_bounded + var(a.out.bounded$imputations[[i]]['current_democracy'])
}
# average variance over multiple imputed dataset without bounds - similar to original dataset
sum_mi/5
# average variance over multiple imputed dataset with bounds - quite lower than original and unbounded datasets
sum_mi_bounded/5

# We see that that the variance of current_democracy in the original dataset and multiple imputed dataset without bounds are quite similar
# They differ by 0.02, with the imputed dataset having a little bit more variance. This is in line with expectations, because imputed dataset is supposed to have uncertainty due to imputation
# Variance of current_democracy in the multiple imputed dataset with bounds is quite lower.
# This is logical, since the bounds artificially limit the possible variance of the underlying values (since we don't let the values be <0 and >10).
# This, of course, leads to a decreased variance. A decreased variance in the dependent variable leads to a better fit of the model
# (the reason for that, again, is because the data is spread out less and can be better approximated with a line)

# The authors of Amelia package warn users that placing bounds for multiple imputation is not reccommended.
# They recommend using normal multiple imputation even if there are clear logical bounds for the values that are imputed.
# The reason for this is that multiple imputation takes into account the uncertainty that arises from the imputation
# However, if we use bounds, we limit the range of values that imputed values can take and thus we limit the 
# uncertainty that arises from the imputation. This is why we have obtained an imputed dataset that has a lower variance
# than the dataset with imputations without bounds as well as variance in the original dataset.
