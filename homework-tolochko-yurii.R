### Problem 1
# a) not all voters read the newspaper
# b) not all reader of the newspaper read the online version
# c) not all readers of the online edition visit the homepage

### Problem 2
library(survey)
library(magrittr)
library(dplyr)

set.seed(12345)
data(api)
names(apipop)
?api
head(apipop)

# (a) 
dim(apipop) # we see that there are 6194 observations (different schools) and 37 variables for each school


# (b)

dim(apisrs) # inspect how many observations in the sample do we have --> we have 200 observations in the sample and 39 variables (2 more than in population - sampling weights and finite population correction)
names(apisrs) 
srs_design <- svydesign(id = ~1, fpc = ~fpc, data = apisrs)

# using functions from survey package
svytotal(~enroll, srs_design) # total
svymean(~enroll, srs_design) # mean + standard error
confint(svymean(~enroll,srs_design)) # confidence interval of mean

# manually
prop <- dim(apisrs)[1] / dim(apipop)[1] # caluculate the proportion of number of sample observations to number of population observations
total <- sum(apisrs[, 'enroll']) / prop # calculate the sample total, divide by proportion to obtain the population total estimate. # the same results, total = 3621074


sample_mean <- mean(apisrs[,'enroll']) # get the mean
fpc <- sqrt((dim(apipop)[1] - dim(apisrs)[1])/(dim(apipop)[1] - 1)) # calculate the finite population correction factor. Formula is fpc = SQRT( (N-n)/(N-1) )

samp_var <- var(apisrs[, 'enroll']) # calculate the variance in the sample. Var(sample)

# calculate the standard error - standard deviaton of the sampling distribution of the sample mean. Formula is SQRT (Var(sample) / size(sample) )
# we correct the naive SE with fpc, we get practically same results as with svymean (difference within 0.001)
SE <- sqrt(samp_var/dim(apisrs)[1]) * fpc

# survey variance of mean is just SE^2, so
survey_variance_of_mean <- SE^2
#confidence intervals. Here we don't multiply by fpc because we already corrected for finite population when we calculated SE (so this information is already included in SE data)
# again, practically identical results to that of survey package function
sample_mean + 1.96*SE # upper bound
sample_mean - 1.96*SE # lower bound


# (c)
# intialize apisrs2 and draw the first sample
apisrs2 <- NA
indices = sample(1:dim(apipop)[1], 200)
apisrs2 <- apipop[indices,]

# sometimes the sample contains NAs in the enroll variable. 
# First, we can remove the NAs
# However, we might not want to remove the NAs since we will get apisrs ans apisrs2 samples of uneven sizes. We resample it until we get zero NAs
while (any(is.na(apisrs2[,'enroll'])) != F) {
  print('In previous sample at least one NA')
  indices = sample(1:dim(apipop)[1], 200)
  apisrs2 <- apipop[indices,]
}

apisrs2$pw = apisrs$pw
apisrs2$fpc = apisrs$fpc # add information of fpc - it is the same as in apisrs since population size did not change
srs_design2 <- svydesign(id = ~1, fpc = ~fpc, data = apisrs2)

mean(svymean(~enroll, srs_design2)) - mean(svymean(~enroll, srs_design)) # calculate the difference between mean in the second and the first sample. Second mean is smaller by 6.44
SE(svymean(~enroll, srs_design2)) - SE(svymean(~enroll, srs_design)) # calculate the difference between SE in the second and the first sample. Second SE is bigger by 2.8


# (d)
stratSample <- function(data, stratvar = "", sample.size = NULL, replace = TRUE) {
  require(plyr)
  require(dplyr)
  strats <- unique(data[,stratvar]) # Define the strata
  stratnames <- sort(strats) # Defining the reference of strata
  stratnum <- length(stratnames) # how many strata we want to draw?
  samplesize <- sample.size # For each stratum
  stratsample <- list()
  for (i in 1:stratnum) {
    obs <- which(data[,stratvar] == stratnames[i]) # identify the position 
    stratsample[[i]] <- data[sample(obs, size = samplesize[i], replace = replace),]
  }
  stratsample_df <- rbind.fill(stratsample)
  return(stratsample_df)
}

apistrat2 <- stratSample(apipop,'stype', c(100,50,50)) # draw the sample
table(apistrat2[,'stype']) # check that we have the correct amount of E, H, M schools


# (e)
clustSamp <- function(data, m = 1, clustvar = ""){
  data <- as.data.frame(data)
  N.clust <- length(unique(data[,clustvar]))
  samp.clust <- sample(unique(data[,clustvar]), size = m)
  dat.clust <- data[data[,clustvar] %in% samp.clust,]
  dat.clust$n_clust <- N.clust
  return(dat.clust)
}

apiclus3 <- clustSamp(apipop, 15, 'dnum')
table(apiclus3[,'dnum']) # check to see what districts were selected and how many schools are in each district. 
table(apiclus3[,'dname']) # now look at the names of the districts
sum(table(apiclus3[,'dname'])) # see the total size of the sample. It is 58, which is much lower than 1500 we had in SRS and in stratified sampling

# (f)

apistrat_design <- svydesign(id = ~1, strata = ~stype, fpc = ~fpc, data = apistrat) 
summary(apistrat_design)

svymean(~enroll, apistrat_design, deff = TRUE)
# Design effect = 0.362. Design effect is calculated by dividing the variance of the stratified sample mean by the variance of SRS sample mean.
# The bigger the design effect, the worse the stratified sample is (because then its variance of the mean is bigger).
# If design effect < 1 (which is our case), then the stratified sample actually demonstrates efficiency gains over SRS sample (because then its variance of the mean is lower than that of SRS sample)


fpc_clus <- sqrt((dim(apipop)[1] - dim(apiclus1)[1])/(dim(apipop)[1] - 1)) # calculate the finite population correction factor. Formula is fpc = SQRT( (N-n)/(N-1) )

# here we calculate the means of clusters (calculate mean of each cluster, then calculate the variance of these means)
means <- apiclus1 %>% group_by(dnum) %>% dplyr::summarise(mean = mean(enroll))
means <- means$mean

var_of_means <- var(means) # here we calculate the variance of the abovecalculated means
se_clus <- sqrt(var_of_means/length(means)) * fpc_clus # finally we calculate the standard error of the cluster means - square roor of the variance and also correct for finite population

# standard error under SRS assumption - we look at this dataset as if there were no clusters but simply randomly selected observations
se_srs <- sqrt(var(apiclus1$enroll)/length(apiclus1$enroll)) * fpc_clus

# compute design effect
deff <- se_clus**2/se_srs**2 # Again, design effect is the ration between variance of cluster sample mean and variance of SRS sample mean
deff
# Design effect = 1.89. In case of cluster sampling, design effect > 1 means that our sample 'performs' worse than the respective SRS sample of the same size. 
# Design effect of 1.89 means that in order to obtain the same variance as with SRS, we would need to have a clustered sample which is 1.89 times larger than SRS sample. 

#(e)
# within-cluster homogeneity (ICC, roh) can be calculated from design effect. ICC = (DEFF - 1) / (average_cluster_size -1)

fpc_clus <- sqrt((dim(apipop)[1] - dim(apiclus3)[1])/(dim(apipop)[1] - 1)) # calculate the finite population correction factor. Formula is fpc = SQRT( (N-n)/(N-1) )

apiclus3 <- apiclus3[!is.na(apiclus3[,'enroll']), ] # we have some NAs, here we can remove them since we do not compare this sample to an alternative one (as was the case with stratified sample)

means <- apiclus3 %>% group_by(dnum) %>% dplyr::summarise(mean = mean(enroll))
means <- means$mean

var_of_means <- var(means) # here we calculate the variance of the abovecalculated means
se_clus <- sqrt(var_of_means/length(means)) * fpc_clus # finally we calculate the standard error of the cluster means - square roor of the variance and also correct for finite population

se_srs <- sqrt(var(apiclus3$enroll)/length(apiclus3$enroll)) * fpc_clus

deff_3 <- se_clus**2/se_srs**2
deff_3
# Again, design effect is the ration between variance of cluster sample mean and variance of SRS sample mean
# Design effect = 7.62, which is pretty high. It means that the cluster sampling variance is more than 7 times larger than the SRS variance.
# Thus, we might have problems with the ICC, since design effect is so large (the larger the design effect the larger the ICC, if all others stay the same).

clust_sizes <- apiclus3 %>% group_by(dnum) %>% dplyr::summarise(n = n())
avg_clust_size = mean(clust_sizes$n)
ICC <- (deff_3 - 1) / (avg_clust_size - 1)
ICC

# ICC = 1.85. The fact that we obtained a result >1 signifies that we have a very inadequate cluster sample.
# This is because the average cluster size is just 4.5, which breaks the calculation (since the bigger the size of the clusters, the lower ICC is, if all others stay the same).
# In our case we have that the value for Design effect is actually bigger than the average cluster size, which is why we get the ICC value > 1. 
# Thus, in order to obtain meaningful results, we need to sample clusters which have bigger sizes.



### Problem 3
#(a)

data <- read.csv2('germany2017_elec.csv')
names(data)
summary(data)
head(data)

# simple random sample of 20 constituencies
set.seed(12345)
indices <- sample(1:dim(data)[1], 20)
srs <- data[indices, ]


# total votes for AfD IN THE SAMPLE.
total_afd <- sum(srs$afd) # sum up the total number of votes for AfD
prop <- dim(srs)[1] / dim(data)[1] # caluculate the proportion of number of sample observations to number of population observations
estimated_total <- total_afd/ prop # now we can estimate the total votes for AfD in the population based on the data from the sample

# share of votes for afd in the sample

# for every constituency calculate its share of AfD voter
srs$afd_share <- srs$afd/srs$all_votes

# mean of AfD voters' share
mean(srs$afd_share)

# variance of the share of AfD votes 
var(srs$afd_share)

#Standard error
SE_srs <- sqrt(var(srs$afd_share)/ dim(srs)[1])
SE_srs

# (b)
#group by federal state
data_state <- data %>% group_by(state_name) %>% dplyr::summarise(total = sum(all_votes))

# get the proportion of population in each federal state
data_state <- data_state %>% mutate(
  proportion = (data_state[,'total'][[1]]/sum(data_state[,'total'])) 
)

# sample 10 federal state names with probabilities proportional to population
sample_state_names <- sample_n(data_state, 10, replace = F, weight = proportion)['state_name'][[1]]
sample_state_names <- as.factor(sample_state_names)

# select from population data those federal states that are in the sample, save in first level variable
grouped_by_state <-  data %>% group_by(state_name)

First_level <- grouped_by_state[grouped_by_state[, 'state_name'][[1]] %in% sample_state_names, ]

# check how many constituencies in original data and in selected states
dim(data)
dim(First_level)

# Select 2 constituencies in each of the selected federal states
# do first iteration manually to create the target dataframe

Final_PPS <- NA
federal_land <- NA
name <- as.character(sample_state_names[1])
federal_land <- First_level[, 'state_name'] == name
constituencies <- First_level[federal_land, ]
current_sample <- sample_n(constituencies, 2, replace = F)
Final_PPS <- current_sample

for (name in sample_state_names[-1]) {
  federal_land <- First_level[, 'state_name'] == name
  constituencies <- First_level[federal_land, ]
  current_sample <- sample_n(constituencies, 2, replace = F)
  Final_PPS <- rbind(Final_PPS, current_sample)
}



# check the amount of constituencies. 2*10 = 20
dim(Final_PPS)

# share of AfD
Final_PPS$afd_share <- Final_PPS$afd /Final_PPS$all_votes
mean(Final_PPS$afd_share)

#difference in means between two samples. We observe that mean in PPS sample is somewhat higer (3.6 percentage points higher)
mean(Final_PPS$afd_share) - mean(srs$afd_share)

#Standard error for PPS sample
SE_pps <- sqrt(var(Final_PPS$afd_share)/dim(Final_PPS)[1])

# Let's look at the difference between the two Standard errors. 
SE_pps - SE_srs

# Difference = 0.003.
# Thus, standard error for PPS is somewhat higher than for SRS


# Design effect
Deff_PPS <- SE_pps^2 / SE_srs^2
Deff_PPS
# We get a value for design effect = 2.29. 
# This means that the variance of the mean for PPS sample is 2.85 times larger than for SRS sample.
# Thus, PPS sample is less efficient than SRS in our case.
# For example, it would mean that for PPS we need a sample size 2.85 times bigger as compared to SRS
# if we want to maintain the same level of precision (effective sample size). 

