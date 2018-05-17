# Problem 1

# Assumptions of CTT

# 1.
# A survey response (y) is an inaccurate representation of some underlying variable t (e.g. true ability of respondent).
# It is imperfect because it is prone to measurement error e. Thus, y = t + e. So, y is the measured response, which is dependent on true ability t but also subject to some error e. 

# 2.
# Main goal of CTT is to be able to separate t from e having y.
# Thus, the goal is to be able to infer the true underlying factor (e.g. ability)
# based on the available collected responses y (which, again, are prone to some error).
# Therefore, the focus is on the quality of measurements. 

# 3.
# The existence of the construct that we try to measure (t) is assumed without substantiating.
# This is in contrast with IRT, where this existence is measured explicitly. 

# 4.
# Errors e are assumed to be normally distributed with mean 0. 

# 5.
# Errors e are uncorrelated with the underlying factors t. Thus, cov(t,e) = 0.
# Also, the random errors are not correlated with each other. Thus, cov(e1, e2) = 0


# Difference between Rasch and Birnbaum models:
# These two models are both related examples of Iterm Response Theory (IRT) approach,
# which focuses on only on the quality of measurement as a whole (as in CTT)
# but also on how individual items (questions) reflect the traits that we want to measure.

# Rasch model assumes that the probability of answering a question correctly is dependent on 2 parameters: 
# the ability of a respondent and the difficulty of a quesiton. 
# Thus, the models is define as log( p_ni /( 1-p_ni )) = b_n -  d_i , with b_n signifying a person's ability and b_i signifying a question's difficulty
# So, rougly speaking, the higher the ability of a person the higher the probability of answering correctly
# and conversely, the high the difficulty of a question, the lower the probability of answering correctly


# Birnbaum can be considered as an extension to the Rasch model. It is quite similar but has more parameters (4) than Rasch (2).
# The main addition of Birnbaum model is that it has a special guessing parameter and a discrimination parameter.
# Birnbaum model formula: log ( (p_ti - c_i) / (1 - p_ti) ) = a_i * (t - b_i)
# Here, c_i is the guessing parameter - thus, this model account for the possibilty of correctly guessing the answer.
# Also note that instead of b_n (a person's ability), we have a parameter t, which is the assumed, and not actual (as in Rasch model), person sample distribution. Source: https://www.rasch.org/rmt/rmt61a.htm
# Finally, we have a_i, which is the discrimination parameter of the question i.
# This discrimination parameter makes the ICC more steep, thus better differentiating between the probability of answering and not answerting a question.
# Therefore, the question becomes better at discriminating between those who have the ability to answer it and those who do not (as these two groups become much more clearly separated).

# ------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
set.seed(12345)

library(tidyverse)
library(foreign)
library(arm)
library(MASS)
library(reshape)
library(GPArotation)
library(psych)
library(mokken)
library(plyr)
library(foreign)
library(psych)

# Problem 2
# a)
ess <- read.dta("esssurvey.dta", convert.factors = FALSE, convert.underscore = TRUE)
# using the previously defined infotable function
infotable <- function(df){
  varinfo <- data.frame(name = names(df),
                        label = attr(df,"var.labels"),
                        vallabel = attr(df,"val.labels"),
                        class = sapply(df,class),
                        id = seq(1,length(df))) # Assign each variable a number
  return(varinfo)
} 

ess_info <- infotable(ess)
ess_info[2:10,]
# The trust-labelled questions attempt to measure the level of trust using 3 different indicators (quesions).
# Moreover, the blocks of these 3 questions appear three times (block 2, 6, 11).
# If we take a look at the distribution of answers, we see that there are some important differences between them.
# The questions in block number 2 have fewer answer options than questions in block 6, which in turn have fewer options than block 11
# This is true for every question (trust1, trust2, trust3).
# Thus, the assumptions is that trust1, 2, 3 are the measured traits, and the blocks are the different methods of measurement.
# I assume that the substantive answer options are in range 0:10, while answer options 77, 88, 99 signify some metainformation (e.g. missing answer, refusal and so on)


table(ess$trust1.two)
table(ess$trust1.six)
table(ess$trust1.eleven)

table(ess$trust2.two)
table(ess$trust2.six)
table(ess$trust2.eleven)

table(ess$trust3.two)
table(ess$trust3.six)
table(ess$trust3.eleven)


# b)

# select all trust-related questions
trust_mtmm <- dplyr::select(ess, 2:10)
# set all answers > 10 (non-substantive ones) to NA
trust_mtmm[trust_mtmm > 10] <- NA


# c)
lowerCor(trust_mtmm)

# Highest heteromethod correlation: cor(trust1.eleven, trust1.six) = 0.23.
# This is a heteromethod-monotrait correlation, because the trait trust1 is measured with 2 methods - eleven and six.

# Lowest heteromethod correlation: corr(trust2.eleven, trust2.two) = -0.06. 

# Highest average heterotrait-monomethod correlations: 
# Method two produces the highest average heterotrait-monomethod correlations: cor(trust1.two, trust2.two) = 0.91; cor(trust1.two, trust3.two) = 0.92; cor(trust2.two, trust3.two) = 0.9

# ------------------------------------------------------------------------------

# Problem 3
# (a)
# (i)
# read the data
anes <- read.csv('ANES_Reduced_2016.csv')
# select variables of interest, 8th to 21st and index at the first position
ideo <- dplyr::select(anes, c(1, 8:22))
# again, some of the codes are > 10, namely 99, which would make the results of our analysis useless, so we take them out
mask <- ideo[, 'pre_guaranteed_job'] < 10
ideo <- ideo[mask, ]
mask <- ideo[, 'pre_wall_mexico'] < 10
ideo <- ideo[mask, ]
mask <- ideo[, 'pre_enviroment'] < 10
ideo <- ideo[mask, ]

ideo <- na.omit(ideo)

# show descriptive statistics of each variable, indluding min, max (i.e. range), quartiles, mean and NA count
summary(ideo)

# (ii)
# Let's take a look at the data in order to see the scores on the variables (the higher the score, the more extereme the position of a respondent on a scale)
# However, as we'll see later, some questions measure the scale in different directions. 
describe(ideo)

# intercorrelation matrix
lowerCor(ideo[, -c(1, 2)])  # -c(1, 2) is used to select columns - everything but index and pre_ideo_self
# We see that many questions are negatively correlated with each other - this signifies that they measure the scale in opposite directions
# For example, cor(pre_help_blacks, pre_syrian_refugee) = -0.65 which also means that these two quesitons measure opposite ideological attitudes.
# Cronbach's alpha

alpha(ideo[, -c(1, 2)]) # -c(1, 2) is used to select columns - everything but index and pre_ideo_self

# We see that alpha score = 0.44 which means that our battery of items is not very reliable. 
# Moreover, we see a message thet "Some items were negatively correlated with the total scale and probably should be reversed."

alpha(ideo[, -c(1, 2)], check.keys = TRUE)
# Here, having reversed the negatively correlated items, we obtain an alpha score of 0.85, which is considerably better and signifies a pretty good reliablity of a set of questions.

# (iii)
# intercorrelation matrix with pre_ideo_self together
lowerCor(ideo[, -1])  # -1is used to select columns - everything but index

# Here we see that some variables correlate strongly with pre_ideo_self, both positive and negative
# For example, cor(pre_ideo_self, pre_help_blacks) = -0.59, which is a pretty strong negative correlation. This means that these two questions measure the same scale but in opposite directions
# That is, a respondent with a high score on pre_ideo_self is expected to score low on pre_help_blacks and vice versa.
# On the other hand, cor(pre_ideo_self, pre_syrian_refugee) = 0.61.
# These are just 2 and we are looking to select those variables that have correlation > 0.3

# Create index. First, the negative correlated items with -1 sign, then the positive correlated items
# This is the index variable that we will use in the later regression tasks
ideo_with_indices <- ideo
ideo_with_indices$issue_ideo_naive <- -1 * (ideo$pre_help_blacks + ideo$pre_birthright_citizenship + ideo$pre_troops_isis) + ideo$pre_guaranteed_job +  + ideo$pre_wall_mexico + ideo$pre_enviroment + ideo$pre_syrian_refugee + ideo$pre_climate + ideo$pre_paid_leave + ideo$post_bank_regulation + ideo$post_healthcare
describe(ideo)

#(iv)

scree(ideo[,-c(1, 2)]) 
# we do not use index, obvisouly, and pre_ideo_self, since it overwhelms other variables (they don't contribute any new information)
# because it alone is, unsurprisingly, a very good predictor of ideological leanings
# - this is why -c(1, 2) is used to select columns - everything but index and pre_ideo_self

fa.parallel(ideo[,-c(1, 2)])
# The results of parallel analysis suggest that we should use 5 factors
# Run factor analysis with 5 factors, cutoff = 0.4
fact <- factanal(ideo[,-c(1, 2)], factors = 5, rotation = "varimax", scores="regression") 
print(fact, cutoff = 0.4, sort = FALSE)

# We see that there are no loading on Factor 5, so we rerun the FA this time with 4 factors
fact <- factanal(ideo[,-c(1, 2)], factors = 4, rotation = "varimax", scores="regression") 
print(fact, cutoff = 0.4, sort = FALSE) 

# There are 3 variables whose uniqueness is > 0.8: post_vaccine, post_freetrade, pre_troops_isis
# We observe 3 big factors and one factor with only one significantly correlated variable.
# The factor loadings (distribution of items on factors) are:

# F1 has pre_environment, pre_climate, pre_bank_regulation
# F2 has pre_birthrihgt_citizenship (negative), pre_help_blacks (negative), pre_wall_mexico, pre_syrian_refugee
# F3 has pre_guaranteed_job, pre_wall_mexico, post_healthcare
# F4 only has post_equal_pay


#(v)
# Here we do 2 matrix vector multiplication operations of data rows multiplied by factor loadings.
# In this way, we obtain two indexes whose elements are weighted by the respective factor loadings 
ideo_with_indices$issue_ideo_dim1 <- as.matrix(ideo[,-c(1, 2)]) %*% fact$loadings[,1] 
ideo_with_indices$issue_ideo_dim2 <- as.matrix(ideo[,-c(1, 2)]) %*% fact$loadings[,2] 
# Taking a look at the correlation between pre_ideo_self and the two new indices, we see that they are correlated at 0.75 and 0.72, which is a high correlation coefficient.
# Moreover, correlation between the 2 new indices is 0.94 which is exceptionally high. This means that the 2 indices capture the same underlying attribute (level of right/left ideology).
lowerCor(ideo_with_indices[,c(2,18,19)])

# ------------------------------------------------------------------------------
# (b)
# (i)

thermo <- dplyr::select(anes, 23:43)
thermo <- na.omit(thermo)
lowerCor(thermo)
# Variable with strongest correlations to post_feel_trump:

## NEGATIVE: liberals(-0.56), black lives matter (-0.56), feminists(-0.46);
## POSITIVE: conservatives(0.56), tea party(0.54)

# The results are somewhat expected. Since Trump represents the conservative party (republicans),
# it is no surprise that the feelings for Trump are negatively correlated with such topics as liberals, feminists and black lives matter - the convervative view of these topics is quite negative.
# Therefore, it is no surprise that people who have positive feelings towards Trump have negative feelings towards topics that are usually anti-convervative and pro-liberal.
# And conversely, convervatives and tea party (a very conservative political movement) are topics that are aligned with values that Trump represents, therefore this positive correlation is expected.

#(ii)
scree(thermo)
fa.parallel(thermo)
# Even though parallel analysis tells us that there are 6 factors,
# We chose to use 4 factors since otherwise the factors become very sparsely loaded.
fact_thermo <- factanal(thermo, factors = 4, rotation = 'varimax', scores = 'regression')
print(fact_thermo, cutoff = 0.4, sort = FALSE) 

# The 2 main factors, factor 1 and factor 2, are quite telling:
# Factor 1 represents those variables that measure liberal leanings of the population.
# Let's look at the loading to see this:
# F1 loadings: clintion, trump(negative), feminists, liberals, labor unions, conservaties(negative), muslims, tea party(negative), black lives matter
# Clearly, people who score high on these questions are liberal-leaning, and vice versa, those who score low are conservative.

# Factor 2 represents those variables that measure convervative leanings of the population.
# F2 loadings: trump, christian fundamentalists, big businessses, conservatives, us supereme court, congress, rich people, christians, tea party
# Here it is also quite obvious that people who score high on feelings about trump, convesrvatives, rich_people, christian fundamentalists etc are likely to be of convervative political leaning

# The other 2 factors are not so distinguished:
# Factor 3 seems to represent feelings towards minorities (gays and lesbians, muslims, transgender people)
# Factor 4 is also similar (poor people, chirstians, jews)

# ------------------------------------------------------------------------------
# (c)
# (i)
# recode vote preference - change to numeric then subtract 1 from each entry so we get Trump = 1, Clinton = 0
anes$recode_vote_preference <- as.numeric(anes$pre_vote_preference) - 1 
# set reference factor to 'White, non-Hispanic' 
anes$race <- relevel(anes$race, ref = 'White, non-Hispanic')
# create new variable in order to save the original one in case we need it later
anes$recode_gender <- anes$gender
# set 'Other' category to NA
anes$recode_gender[anes$recode_gender == 'Other'] <- NA
# recode gender - change to numeric, subtract one. Male = 1, Female = 0
anes$recode_gender <- as.numeric(anes$recode_gender) - 1

vars_to_merge <- anes[, c('X', 'pre_feel_trump', 'recode_vote_preference', 'race', 'recode_gender', 'age', 'edu_num', 'income_group')]
ideo_with_indices <- merge(x = ideo_with_indices, y = vars_to_merge, by = "X", all.x = TRUE)

# (ii)
library(stargazer)
model_1 <- glm(recode_vote_preference ~ pre_ideo_self, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_1)
model_2 <- glm(recode_vote_preference ~ pre_ideo_self + pre_feel_trump, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_2)
model_3 <- glm(recode_vote_preference ~ issue_ideo_naive, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_3)
model_4 <- glm(recode_vote_preference ~ issue_ideo_dim1 + issue_ideo_dim2, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_4)
model_5 <- glm(recode_vote_preference ~ pre_ideo_self + race + recode_gender + age + edu_num + income_group, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_5)
model_6 <- glm(recode_vote_preference ~ issue_ideo_dim1 + issue_ideo_dim2 + race + recode_gender + age + edu_num + income_group, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_6)

# Let's take a look at the results of the models defined above. 
# As we can see, the strongest model by AIC is model_2. It predicts voting preferences based on ideological self placement as well as attitudes towards Trump before the election.
# This is in line with our expectations, since these 2 variables have a strong logical connection with the voting outcome.

# Another interesting finding comes from model_5. Here we see that most of the socio-demographic variables are not statistically significant.
# Thus, only 1 category of race, Black, non-Hispanic, has a significant difference from the reference category(White, non-Hispanic).
# Quite surprisingly, age, income, gender and education have no statistical significance in the model.
# However, the reason for this could be that the explanatory power of other independent variables is so much higher
# that the contribution of these socio-demographic variables fades away and becomes insignificant. Still, the initial hypothesis was that these variables would contribute to the explanation.

# (iii)
# OWN MODEL
model_my <- glm(recode_vote_preference ~ pre_ideo_self + pre_feel_trump + race + issue_ideo_dim1, data = ideo_with_indices, family = binomial(link="logit"))
summary(model_my)

# My justification for choosing the independent variables was to attempt to define a model which would have a lower AIC value than the ones we created above (lowest was 346).
# Moreover, since we are trying to model a voting intention (voting for Trump or Clinton), it is logical to use the variables realted to ideological points of view
# as wel as socio-demographic variables, since these types of variables seem to have the most logical connecting with voting behaviour.
# Thus, the model chosen is predicting vote preference based on:
# pre_ideo_self (self placement on left/right scale), pre_feel_trump(attitudes towards Trump before the election),
# issue_ideo_dim1 which is a weighted index we created that captures the ideological attitudes of respondents and finally
# race, as it was the only socio-demographic variable that was found to be significant in the previous models.

# The results are quite good - our model managed to beat the previous ones according to the AIC measure - new model demonstrates 285 AIC value.
# All of the variables are statistically significant, however, not all of the race categories are. More specifically,
# Black, non-Hispanic and Hispanic were the only races that have a statistically significant difference in effects compared to the referene category (which is White, non-Hispanic) 

# Save all models into .html file
stargazer(model_1, model_2, model_3, model_4, model_5, model_6, model_my, out = 'models.html')

# ------------------------------------------------------------------------------
#(d)
#(i)
spending <- dplyr::select(anes, 44:51)
# Set all −1 to 0
spending[spending < 0] <- 0

#(ii)
library(ltm)
library(mokken)

spending <- na.omit(spending)
answers <- data.frame(descript(spending)$perc)
names(answers) <- c("not_for", "for", "logit")
# arange by difficulty: from easiest to hardest
answers <- dplyr::arrange(answers, not_for)


#(iii)
# check monotonicity - we look at the graphs, and if the graph is nondecreasing then it satisfies the monotonicity assumption.
monotonicity <- check.monotonicity(spending)
summary(monotonicity)
plot(monotonicity)
# Here we can see that every variable expect pre_budget_crime in monotone, while pre_budget_crime has a small region of decrease. 

# check non-interseciton - we look at the graphs, and if the curves do not intersect, then the assumption is satisfied
intersection <- check.restscore(spending)
summary(intersection)
plot(intersection)
# Here we see that quitle a lot of variable paris do not satisfy the non-intersection assumption, namely:
# pre_budget_sec - pre_budget_sci
# pre_budget_sec - pre_budget_crime
# pre_budget_sec - pre_budget_env
# pre_budget_school - pre_budet_sci
# pre_budget_school - pre_budget_crime
# pre_budet_sci - pre_budget_crime
# pre_budet_sci - pre_budget_childcare
# pre_budet_sci - pre_budget_poverty
# pre_budet_sci - pre_budget_env
# pre_budget_crime - pre_budget_childcare
# pre_budget_crime - pre_budget_poverty
# pre_budget_crime -pre_budget_env


#(iv)
# Rasch model
rasch_model <- rasch(spending)
summary(rasch_model)
coef(rasch_model, prob = T, order = T)
plot(rasch_model) # ICC
plot(rasch_model, type= "IIC")
# Here we see that quite a lot of questions have very similar ICC and IIC curves.
# This means that they share a similar "difficulty" parameter.
# This result is not surprising, considering the distribution of responses (0s and 1s) that we calculated -
# - a lot of questions had approximately 50% 0s and 50% 1s, which is why we see a lot of similar curves centered around 'ability' score 0.
# This is not a good result, because it means that our questions do not form a good questionnaire,
# since they can only differentiate a very small range of the latent "ability' trait (close around 0) - low discriminatory power.
# A good set of questions would be able to differentiate a wide range of "abilities".
# Thus, a good set of questions would have an easy question  - for low 'ability', another would be hard - for high 'ability',
# another would be of moderate difficulty, etc.

birnbaum_model <- ltm(spending ~ z1)
summary(birnbaum_model)
plot(birnbaum_model) # ICC
plot(birnbaum_model, type = "IIC")
# Here we see a somewhat similar picture as in the Rasch model above. 
# However, there are some positive differences. For instance, two questions seem to have a better discriminatory power in this model than in the previous one:
# Birnbaum models are better at providing more distinct discriminatory features to the questions because it has special discrimination parameter for every question, and this is why we see this difference
# In this model we see that the questions are more speread out on the 'ability' trait.
# This means that, in contrast to the rasch model abover, they are better at differentiating the latent 'ability' over a wider range of the ''ability''.
# For instance, pre_budget_welfare is a 'harder' quesiton - centered around 'ability' = 2 (which means that it is good at differeniating people at this level of 'ability'), 
# while 'pre_budget_school' is one of the 'easier' ones - it has a center around 'ability' = - 1 (good at differentiating people with lower 'ability')

# All in all, it seems that in this case our birnbaum model is a better fit than our rasch model.

# Let's compare the AICs of the two models (lower is better) to see an objective measure of fit.
summary(birnbaum_model)$AIC
summary(rasch_model)$AIC

# difference in AICs
summary(birnbaum_model)$AIC - summary(rasch_model)$AIC
# We see that our birnbaum model has a lower AIC score than our rasch model, which supports the initial hypothesis than in this case birnbaum models serves better than rasch 