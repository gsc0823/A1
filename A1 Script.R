#===================================
# Problem Set A1 - Gregory Campbell
#===================================

#=============================
# Part 1
#=============================

rm(list = ls())

# Loading data and packages for Problem Set

datjss = read.csv("datjss.csv")
datstu = read.csv("datstu.csv")
datsss = read.csv("datsss.csv")

packages = c("tidyverse","AER")
install.packages(packages)
library(tidyverse)
library(AER)

#=============================
# Exercise 1: Missing Data
#=============================

# To begin, I'm going to go ahead and reorganize my data before making any calculation. 

# Lets begin by making new datasets for programs and schools:

dat_pgm = datstu %>% select(1:4,11:18) 

# Turning my dataset for programs into the long format:

dat_pgm = dat_pgm %>% pivot_longer(cols = c("choicepgm1","choicepgm2","choicepgm3","choicepgm4","choicepgm5","choicepgm6"
                        ),names_to = "pref",names_prefix = "choicepgm",values_to = "pgm")

# Turning my Pref column into numbers:

dat_pgm = dat_pgm %>%
  mutate(Pref = gsub('\\D','',pref))


# Doing similar thing for schoolcodes

dat_school = datstu %>% select(1:10,17:18) 

# Putting data into a long format

dat_school = dat_school %>% pivot_longer(cols = c("schoolcode1","schoolcode2","schoolcode3","schoolcode4","schoolcode5","schoolcode6"
  ),names_to = "pref", names_prefix = "schoolcode",values_to = "schoolcode")


# a. Number of students

length(datstu$X)

# As we can see, the data contains 340,823 distinct students

# b. Number of schools

n_schools = dat_school %>% filter(schoolcode != "NA")

length(unique(n_schools$schoolcode))

# As we can see, there are 640 different schools. 

# c. Number of programs

programs = c(datstu$choicepgm1,datstu$choicepgm2,datstu$choicepgm3,
             datstu$choicepgm4,datstu$choicepgm5,datstu$choicepgm6)

length(unique(programs))

# I found 33 unique programs 

# d. Number of choices

# To do this, I'm going to have to merge the two datasets above

data_merge = merge(dat_pgm,dat_school)

# Getting rid of extra variable created

data_merge$Pref = NULL

# Arranging dataset so that it's in order and filtering out rows where pgm is blank

data_merge = data_merge %>% arrange(data_merge, X) %>% filter(pgm != "", schoolcode != "NA")

dat_combos = data_merge[8:9]

dat_combos = dat_combos %>% 
  mutate(combo=paste0(schoolcode,pgm))

# Number of unique school and program combos

length(unique(dat_combos$combo))

# As we can see, there are 2773 unique school and program combos
       
# e. Missing test scores

length(which(is.na(datstu$score)))

# Found that there are 179,887 missing test scores

# f. Apply to the same school

same_school = data_merge %>% group_by(X) %>% summarise(Number_of_schools=n_distinct(schoolcode)) %>% 
  filter(Number_of_schools==1)

nrow(same_school)

# So, it appears that 663 students applied to the same school 

# g. Apply to less than 6 choices

less_6 = datstu %>%
  select(5:10)

# Counting number of students that have at least one "NA" in their schoolcode columns

less_6 = rowSums(less_6)

length(which(is.na(less_6)))

# So, it looks like 17,734 students applied to less than 6 schools

#=============================
# Exercise 2: Data
#=============================

# So, to get the district information, I'm going to need to join the merged dataset I made in
# question #1 and datsss

# Missing values in datsss, so I'm taking those rows out:

datsss2 = datsss %>% 
  filter(schoolname != "")

# Getting columns I don't need in datsss

datsss2$X = NULL

datsss2$schoolname = NULL

# Making unique rows

datsss2 = unique(datsss2)

school_dataset = left_join(data_merge,datsss2,by = "schoolcode")

# So, in order to find the cutoff for each school,program combination, we need to look at the
# students who were admitted:

admitted = school_dataset %>%
  filter(rankplace == pref)

# Getting rid of potential duplicates

admitted = unique(admitted)

# Calculating cutoff, quality, and size

admitted_stats = admitted %>% group_by(schoolcode,pgm) %>%
  summarise(cutoff = min(score), quality = mean(score), size = n())

# Joining this to my school dataset

school_dataset = left_join(school_dataset, admitted_stats, by = c("schoolcode","pgm"))

# Now, I just need to organize school_dataset a little:

school_dataset_clean = school_dataset %>%
  select(8:15)

school_dataset_clean = school_dataset_clean %>%
  arrange(schoolcode,pgm)

school_dataset_clean = unique(school_dataset_clean)

rownames(school_dataset_clean) = 1:2773

# New dataset

head(school_dataset_clean, 20)

#=============================
# Exercise 3: Distance
#=============================

# For this problem, I'm going to calculate the distances from students' junior high schools
# and the senior schools they got admitted to. 

# We'll begin by creating a dataset for junior high schools

# Junior high schools

junior = datjss %>% 
  select("jssdistrict","point_x","point_y") %>% 
  rename(c("jsslong" = "point_x","jsslat" = "point_y"))


# Merging junior highschool dataset with school dataset

school_dataset = left_join(school_dataset,junior, by = "jssdistrict")

# Filtering in admitted students

school_dataset = school_dataset %>%
  filter(rankplace == pref)

# Making column of distances

school_dataset = school_dataset %>% 
  mutate(distance=sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2 +
                         (69.172*(ssslat-jsslat))^2))

# Distances between students and their admitted schools

dist_admitted = school_dataset %>%
  select("X","jssdistrict","sssdistrict","distance") %>% arrange(X)

head(dist_admitted, 20)

# Also, because I'm not completely certain what distance the professor is looking for, one
# can easily calculate the distance between all the jssdistricts and sssdistricts. 

# Making senior district dataset 

senior = datsss %>%
  select("sssdistrict","ssslong","ssslat")

# Making distance dataset and doing calculations

dist_district = merge(junior,senior)

dist_district = dist_district %>% 
  mutate(distance=sqrt((69.172*(ssslong-jsslong)*cos(jsslat/57.3))^2 +
                         (69.172*(ssslat-jsslat))^2))
# Results

head(dist_district, 20)

#==========================================
# Exercise 4: Descriptive Characteristics
#==========================================

# Table of mean and standard deviation for cutoff, quality, and distance

school_dataset %>% 
  summarise(mean_cutoff=mean(cutoff, na.rm = TRUE),sd_cutoff=sd(cutoff, na.rm = TRUE),
            mean_quality=mean(quality,na.rm = TRUE),sd_quality=sd(quality,na.rm = TRUE),
            mean_distance = mean(distance,na.rm = TRUE), sd_distance = sd(distance,na.rm = TRUE))

# Differentiating by student quantile test scores: 

school_dataset = school_dataset %>% 
  mutate(quantile = ntile(score,10)) 

# Results for student test quantile scores

school_dataset %>% group_by(quantile) %>% 
  summarise(mean_cutoff=mean(cutoff, na.rm = TRUE),sd_cutoff=sd(cutoff, na.rm = TRUE),
            mean_quality=mean(quality,na.rm = TRUE),sd_quality=sd(quality,na.rm = TRUE),
            mean_distance = mean(distance,na.rm = TRUE), sd_distance = sd(distance,na.rm = TRUE))



#==================================================================
#                             Part 2
#==================================================================


#=============================
# Exercise 5: Data Creation
#=============================

rm(list = ls())

set.seed(123)

# Making X1

X1 = runif(10000,min = 1,max = 3)

# Making X2

X2 = rgamma(10000,shape = 3,scale = 2)

# Making X3

X3 = rbinom(10000, size = 1,prob = 0.3)

# Making error term

e = rnorm(10000,2,1)

# Creating Y and ydum

Y = 0.5 + 1.2*X1 - 0.9*X2 + 0.1*X3 + e

# Mean of Y

Y_bar = mean(Y)

# ydum

ydum = as.numeric(Y > Y_bar)  


#=============================
# Exercise 6: OLS
#=============================       
  
# Calculating correlation between Y and X1

X1_bar = mean(X1)

Y_dif = Y - Y_bar

Y_dif2 = (Y - Y_bar)^2

X1_dif = X1 - X1_bar

X1_dif2 = (X1 - X1_bar)^2

# Correlation

corr = sum((X1_dif)*(Y_dif))/(sqrt(sum(X1_dif2)*sum(Y_dif2)))
corr

# As we can see, the correlation between Y and X1 is roughly 0.216, which is very different from
# 1.2. If one wants to see the estimated coefficient in front of X1, see the next few lines.  

# Creating X 

constant = 1

X = cbind(constant,X1,X2,X3)

# Calculating Coefficients

Beta = solve(t(X)%*%X)%*%t(X)%*%Y
Beta

# As we can see, I found B0 = 2.4907, B1 = 1.1976, B2 = -0.897, B3 = 0.0875. Note that B1 is
# extremely close to 1.2, but not exactly. This due the fact that we're using a sample to 
# calculate the coefficients and so they will just be estimates. 

# Finding standard errors for variables 

# Making residual variance

residuals = Y - X%*%Beta

# Number of variables

v = ncol(X) - 1

# Degrees of freedom

df = nrow(X) - v - 1

# Residual variance

r_var = sum(residuals^2)/df

# Calculating covariance matrix

cov = r_var*solve(t(X)%*%X)

# Calculating SE

SE = sqrt(diag(cov))

SE

# As we can see, the standard error for the constant is roughly 0.0406, 0.01735 for X1, 0.00287 
# for X2, and 0.0216 for X3

#=============================
# Exercise 7: Discrete Choice
#============================= 

# Linear Probability Model

linear = lm(ydum ~ X1 + X2 + X3)

# Since the linear model has a closed form solution, we can just use the same method
# that we used in question #6 to solve for coefficient estimates


Beta2 = solve(t(X)%*%X)%*%t(X)%*%ydum

Beta2

#===========================

# Logit Model 

set.seed(123)

logit = glm(ydum ~ X1 + X2 + X3, family = binomial(link = "logit"))

# No closed form solution, so we need to use maximum likelihood 

# Making likelihood function

like_logit = function(par,X1,X2,X3,ydum)
{
  xbeta           = par[1] + par[2]*X1 + par[3]*X2 + par[4]*X3
  pr              = exp(xbeta)/(1+exp(xbeta))
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}

# Checking that my function is working

test_par = logit$coefficients

like_logit(test_par,X1,X2,X3,ydum)

logLik(logit)

# Running optimization 

start = runif(4)

mle_l = optim(start,fn=like_logit,method = "BFGS",control = 
                list(trace=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum)

# Coefficient estimates

mle_l

#===========================

# Probit Model

set.seed(123)

probit = glm(ydum ~ X1 + X2 + X3, family = binomial(link = "probit"))

# Making likelihood function

like_probit = function(par,X1,X2,X3,ydum)
{
  xbeta           = par[1] + par[2]*X1 + par[3]*X2 + par[4]*X3
  pr              = pnorm(xbeta)
  pr[pr>0.999999] = 0.999999
  pr[pr<0.000001] = 0.000001
  like           = ydum*log(pr) + (1-ydum)*log(1-pr)
  return(-sum(like))
}

# Making sure that my function is working

test_par = probit$coefficients

like_probit(test_par,X1,X2,X3,ydum)

logLik(probit)

# Running optimization

start = runif(4)

mle_p = optim(start,fn=like_probit,method = "BFGS",control = 
                list(trace=1,maxit=1000),X1=X1,X2=X2,X3=X3,ydum=ydum)

# Coefficient estimates

mle_p

# Checking my work

summary(logit)

summary(probit)

summary(linear)

# I'll begin by discussing the estimated coefficients for my probit model. For X1, the estimated
# coefficient is 1.1723. The magnitude of the coefficient by itself doesn't tell us much, but 
# the fact that the coefficient is positive tells us that an increase in X1 makes it more likely
# that Y, Y being an event or something else, occurs, holding all else constant. For X2, the 
# estimated coefficient is -0.9054, which indicates that an increase in X2 makes it less likely 
# for event Y to occur. Similarly, the estimated coefficient for X3 is -0.0112, which again indicates
# that an increase in X3 makes event Y less likely to occur. Again, I'll reiterate that the
# magnitude of the coefficient estimates for the logit and probit models mean nothing by themselves.
# So, the fact that X3 has a smaller coefficient, in terms of magnitude, doesn't tell us 
# anything. Now, looking at the logit model, we see that the coefficient estimates are 
# quite different than the probit model. For the logit model, the coefficient in front of X1
# is estimated to be roughly 2.1006, -1.6185 for X2, and -0.0196 for X3. Hence, an increase in
# X1 increases the likelihood of Y occuring and an increase in X2 and X3 decreases the likelihood
# of Y occuring. For the linear probability model, the estimated coefficients are quite different
# from the probit and logit models. The estimated coefficient for X1 is roughly 0.1461, -0.1028
# for X2, and -0.00805 for X3. However, unlike the logit and probit models, the magnitude of
# the coefficients actually mean something. An increase in X1 increases probability of event
# Y occuring by roughly 0.1461 log points, an increase in X2 decreases the probability of
# Y occuring by 0.1028 log points, and an increase in X3 decreases the probability of Y
# occuring by 0.008 log points. However, it's worth noting that the linear probability model
# allows for the probability of an event occuring to be above 100%, which doesn't make any
# sense. For all three models, the coefficient estimates are statistically significant except
# for X3. 

#=============================
# Exercise 8: Marginal Effects
#============================= 

# Logit

# To compute the marginal effects for my logit model, I am going to look at the average marginal 
# effect at the mean. 

# Calculating marginal effects for logit

l_mean = mean(dnorm(predict(logit, type = "link")))

ME_logit =  as.matrix(l_mean * coef(logit))

ME_logit

# As we can see, for our logit model the average marginal effect on our y is roughly 0.147 log 
# points for an increase in X1, -0.113 log points for an increase in X2 and -0.0013 log points
# for X3. Of course, holding everything else constant

# Probit

p_mean = mean(dnorm(predict(probit, type = "link")))

ME_probit =  as.matrix(p_mean * coef(probit))

ME_probit


# So the average marginal effect on y is roughly 0.143 log points for an increase in X1, -0.111
# log points for an increase in X2, and -0.00137 log points for an increase in X3. Basically, 
# the marginal effects are very similar to what I found for the logit model. 


# Finding the standard error of marginal effects:

# Logit model

# Running Bootstraps

# Setting up parameters and making X into a dataframe

X = as.data.frame(X)
R    = 499;                      
nind = length(X1);            
nvar = length(logit$coefficients)  


set.seed(123)

outs = mat.or.vec(R,nvar)

# Creating sub sample from my dataset

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = X[samp,]
  reg1     = glm(ydum ~ X1 + X2 + X3, family = binomial(link = "logit"), data = dat_samp)
  outs[i,] = reg1$coefficients
}

# Calculating mean and standard deviation and putting it into a table

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

est = cbind(summary(logit)$coefficients[,1],
            summary(logit)$coefficients[,2],
            mean_est,
            sd_est)
colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")

est

# So, based on these results from bootstraps, it looks like the standard error of the marginal
# effect is roughly 0.0814 log points for the intercept, 0.0362 log points for X1, 0.00576 log
# points for X2, and 0.04238 log points for X3

# Probit

# Running Bootstraps

set.seed(123)

# Creating sub sample from my dataset

for (i in 1:R)
{
  samp     = sample(1:nind,nind,rep=TRUE)
  dat_samp = X[samp,]
  reg1     = glm(ydum ~ X1 + X2 + X3, family = binomial(link = "probit"), data = dat_samp)
  outs[i,] = reg1$coefficients
}

# Calculating mean and standard deviation and putting it into a table

mean_est = apply(outs,2,mean)
sd_est   = apply(outs,2,sd)

est = cbind(summary(probit)$coefficients[,1],
            summary(probit)$coefficients[,2],
            mean_est,
            sd_est)

colnames(est) = c("CF: est","CF: sd","BT: est","BT: sd")

est

# As we can see from the bootstraps estimate, the standard errors for the marginal effects is
# roughly 0.05088 for the intercept, 0.02265 for X1, 0.0036 for X2, and 0.0264 for X3


