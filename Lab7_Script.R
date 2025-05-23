# Lab 7 Script: Diff-in-means & ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Objectives ----
# In this Lab you will learn to:

# 1. Learn to apply a Difference in Means test (T-test)
# 2. Learn to apply a ANOVA test (F-test)

#---- 1. Learn to apply Difference in Means test (T-test) ----

#Steps:
 ## 1. Check your data meets the T-test assumptions: (independence, normality, equal variance)
 ## 2. Confirm the dependent variable is continuous, and independent variable has two categories only
 ## 3. Establish a H0
 ## 4. Conduct an T-test for independent samples
 ## 5. Interpret results

library(data.table)
library(foreign) # foreign version as data.table
library(haven) # haven version
HTS_household_10regions <- read_sav("Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav")
View(HTS_household_10regions)
hts <- data.table(read.spss("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav",to.data.frame = T))
# step 1 - Independence: there is no reason to think that the VMT values between these two regions are not independent
hts[,.N,by=region]
hts<-hts[region%in%c("Seattle, WA","Kansas City, MO")] #You're isolating two regions for a two-sample t-test.

# step 1 - Normality: Histogram plots look pretty normal

library(ggplot2)

library(haven)

# Replace the path with your real file path
wide_chars <- read_sav("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav")

# Force full evaluation — this avoids "lazy loading" errors
wide_chars <- as.data.frame(wide_chars)
names(wide_chars)
head(wide_chars[ , 1:5])   # Look at just the first few columns
str(wide_chars[ , 1:5])

names(wide_chars)
head(wide_chars$real_column_name)


ggplot(data=hts,aes(x=lnvmt))+
  geom_histogram()+
  facet_grid(region~.)

#You're checking: Normality via histograms

#Variance visually via boxplots
# step 1 - Variance: Box-plot looks pretty normal

ggplot(data=hts,aes(x=lnvmt, y=region))+
  geom_boxplot()

 # desc stats
hts[,.(mean=mean(lnvmt,na.rm=T),sd=sd(lnvmt,na.rm=T),Obs=.N),by=.(region)]
#Mean and SD by group 

#Step 2: dependent variable types
class(hts$lnvmt) ; class(hts$region) #Confirming data types before testing.

# Step 3: H0: t-statistic = 0 (Mean_Seattle - Mean_Kansas = 0)

# Step 4: Conduct t-test
two_tailed_t_test<-hts[,t.test(formula = lnvmt ~ region)] # two-tailed
two_tailed_t_test
#You’re comparing mean lnvmt between the two regions using:
#two-tailed test: tests for any difference
#one-tailed test: tests if one region has greater mean
one_tailed_t_test<-hts[,t.test(formula = lnvmt ~ region,alernative = 'greater')] # one-tailed
one_tailed_t_test

# Step 5. Interpret results

# Both T-test (one and and two-tailed are) statistically significant (p-value <0.05 & 0.025), which implies that the H0 can be rejected. Hence we can state that the difference in vehicle miles traveled between the two regions exist is not due to chance

# BONUS: Simulated hypothesis testing plot ----
curve(dt(x, df = 5934.7), from = -10, to = 10)
abline(h=0,col='blue')
points(x=two_tailed_t_test$statistic,y=0,col='red')

##upper value from Chi-Squared Dist (1-alpha) with alpha=0.05
upper975 <- qt(p = .975, df = 5934.7)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = 5934.7)
abline(v = lower025,y=0,col='red')


#---- 2. Learn to apply a ANOVA test (F-test) ----

# Steps:
 ## 1. Ensure the ANOVA Assumptions are meet
 ## 2. Ensure variable types
 ## 3. Propose H0: no difference between groups
    #  transit passenger miles (lntpm) ~ region
 ## 4. Conduct one-way ANOVA
 ## 5. Interpret results
 ## 6. Conduct post-hoc tests


uza <- data.table(read.spss("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/UZA.sav",to.data.frame = TRUE))


# Step 1: Independence: There is no reason to think that TPM and region are dependent
  # in fact a quick plot of the sample means reveal great difference between groups

ggplot(data=uza[,.(Mean_lntmp=mean(lntpm)),by=region], aes(x=region,y=Mean_lntmp))+
  geom_point()
#You're checking group means visually — helpful before ANOVA

# Step 1: to many outliers?: There is no reason to think that TPM and region are dependent
  #box plot shows that there are some outliers in the Midwest and South regions. Although using the LN transformation helps here, lets delete those observations)

ggplot(data=uza, aes(x=region, y= lntpm))+
  geom_boxplot()
  
  # deleting outliers 
uza_bp<-boxplot(uza$lntpm~uza$region)

outliers <- uza_bp$out

uza[lntpm%in%outliers,]
uza2<-uza[!lntpm%in%outliers,]
#Plotted boxplots, Identified outliers, Removed them for cleaner ANOVA assumptions

boxplot(uza$lntpm~uza$region)
boxplot(uza2$lntpm~uza2$region)

# Step 1: dependent variable is normal: 

hist(uza2$lntpm) #Checking whether the DV (lntpm) is roughly normal after cleaning
# looks pretty normal to me!

# Step 1: variance homogeneity?
bartlett.test(lntpm ~ region, data=uza2) # H0: variances are equal: p-value ==> accepts
#Bartlett’s test checks the equal variance assumption for ANOVA.


# Step 4: one-way anova

fit<-aov(uza2$lntpm~uza2$region) #Standard ANOVA test to compare means across multiple groups.
summary(fit)

#post-hoc test
TukeyHSD(fit) #If ANOVA is significant, Tukey's HSD tells you which groups are different.

plot(TukeyHSD(fit))


