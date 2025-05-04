# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`
library(data.table)
library(foreign) # foreign version as data.table
library(haven) # haven version
HTS_household_10regions <- read_sav("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav")
View(HTS_household_10regions)
hts <- data.table(read.spss("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav",to.data.frame = T))
library(data.table)

hts <- read_sav("~/Desktop/Methods2_Labs/07-lab-assignment-2025-Green-range/datasets/HTS.household.10regions.sav")

# Make it data.table
hts <- as.data.table(hts)

# Now check income
str(hts$income)
summary(hts$income)
# Drop missing incomes
hts_clean <- hts[!is.na(income_cat)]



# Quick summary
summary(hts_clean$income)

# Clean hhincome
hts$hhincome <- as.numeric(hts$hhincome)

# Drop missing values
hts_clean <- hts[!is.na(hhincome)]

# Clean htype
hts_clean$htype <- as.factor(hts_clean$htype)

# Histogram
pdf("histogram_income.pdf")
hist(hts_clean$hhincome,
     main = "Histogram of Household Income",
     xlab = "Household Income",
     col = "skyblue",
     breaks = 30)
dev.off()
# Check your htype levels
# 1. Keep only htype 0 and 1
hts_two_groups <- hts_clean[htype %in% c("0", "1")]

# 2. Confirm it only has two levels now
levels(hts_two_groups$htype)   # Should now show just "0" and "1"

# 3. Now run the t-test
t.test(hhincome ~ htype, data = hts_two_groups)

#Welch Two Sample t-test

#data:  hhincome by htype
#t = -15.389, df = 194.91, p-value < 2.2e-16
#alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
#95 percent confidence interval:
 # -36.14148 -27.93007
#sample estimates:
  #mean in group 0 mean in group 1 
#43.34637        75.38215 

# Load ggplot2 for nice plotting
library(ggplot2)

# Histogram of Household Income by Housing Type (htype)
ggplot(hts_two_groups, aes(x = hhincome, fill = htype)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
  labs(
    title = "Histogram of Household Income by Housing Type",
    x = "Household Income",
    y = "Count",
    fill = "Housing Type"
  ) +
  theme_minimal()


library(data.table)

# Create the variable correctly
hts[, single_family := ifelse(htype == 0, "Single Family", "Other")]

# Then make it a factor
hts[, single_family := factor(single_family)]

# Now run your T-test
t_test_income <- t.test(hhincome ~ single_family, data = hts)

# Print results
print(t_test_income)

#Welch Two Sample t-test

#data:  hhincome by single_family
#t = 12.981, df = 192.31, p-value < 2.2e-16
#alternative hypothesis: true difference in means between group Other and group Single Family is not equal to 0
#95 percent confidence interval:
 # 22.84084 31.02538
#sample estimates:
 # mean in group Other mean in group Single Family 
#70.27948                    43.34637 

# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:
# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results
# Simulate t-distribution



# Create histogram of household income (only non-NA values)

# Save histogram to PDF
pdf("household_income_histogram.pdf")

hist(hts$hhincome,
     main = "Histogram of Household Income",
     xlab = "Household Income",
     col = "skyblue",
     border = "white",
     breaks = 30)

dev.off()  # This closes the PDF device and saves the file

# Save simulated hypothesis test plot to PDF
pdf("t_test_simulation_plot.pdf")

# Simulate a t-distribution
curve(dt(x, df = t_test_income$parameter), 
      from = -4, to = 4,
      n = 500, col = "darkblue", 
      lwd = 2,
      main = "Simulated T-distribution\n(T-test Household Income)",
      xlab = "T-statistic", ylab = "Density")
#Simulate a t-distribution
#> curve(dt(x, df = t_test_income$parameter), 
    #    +       from = -4, to = 4,
   #     +       n = 500, col = "darkblue", 
    #    +       lwd = 2,
     #   +       main = "Simulated T-distribution\n(T-test Household Income)",
      #  +       xlab = "T-statistic", ylab = "Density")
# Add vertical line at observed t-statistic
abline(v = t_test_income$statistic, col = "red", lwd = 2)

# Add text showing the t-value
text(t_test_income$statistic, 0.05, labels = paste0("t = ", round(t_test_income$statistic, 2)),
     pos = 4, col = "red")

dev.off()  # This closes the PDF device


# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)

print(attributes(hts$region)$labels)

# Filter San Antonio region (code 15 based on the labels)
hts_SA <- hts %>% filter(region == 15)
# Check the region codes
print(attributes(hts$region)$labels)

# Subset for San Antonio
hts_SA <- hts %>% filter(region == 15)

# Check structure
str(hts_SA)

# Now continue with your analysis
library(dplyr)

# Filter ONLY San Antonio (region == 15)
hts_SA <- hts %>% filter(region == 15)

# Check sample
nrow(hts_SA)
head(hts_SA)
# Find the median of jobpop in San Antonio
jobpop_median <- median(hts_SA$jobpop, na.rm = TRUE)

# Create a new variable: jobpop_group
hts_SA <- hts_SA %>%
  mutate(jobpop_group = ifelse(jobpop > jobpop_median, "High", "Low"))

# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.
# Drop NA in lnvmt and jobpop_group
hts_SA_clean <- hts_SA %>% filter(!is.na(lnvmt), !is.na(jobpop_group))

# Run T-test
t_test_lnvmt <- t.test(lnvmt ~ jobpop_group, data = hts_SA_clean)

# Print result
print(t_test_lnvmt)


#Welch Two Sample t-test

#data:  lnvmt by jobpop_group
#t = -2.2052, df = 1512.2, p-value = 0.02759
#alternative hypothesis: true difference in means between group High and group Low is not equal to 0
#95 percent confidence interval:
#  -0.21709014 -0.01269755
#sample estimates:
 # mean in group High  mean in group Low 
#2.989259           3.104152 

# Save histogram
pdf("histogram_lnvmt_SanAntonio.pdf")
hist(hts_SA_clean$lnvmt,
     main = "Histogram of Log Vehicle Miles Traveled (lnvmt)",
     xlab = "Log Vehicle Miles Traveled (lnvmt)",
     col = "skyblue",
     breaks = 30)
dev.off()


# 2. [30 points] Run a T-test and an ANOVA test in your data.

# Check if income_cat exists
table(hts_SA_clean$income_cat)

# Make sure income_cat is a factor
hts_SA_clean$income_cat <- as.factor(hts_SA_clean$income_cat)

# ANOVA: lnvmt ~ income_cat
anova_result <- aov(lnvmt ~ income_cat, data = hts_SA_clean)

# Summary of ANOVA
summary(anova_result)

# Save normality check
pdf("normality_check_lnvmt_SanAntonio.pdf")
qqnorm(hts_SA_clean$lnvmt)
qqline(hts_SA_clean$lnvmt)
dev.off()

# Save boxplot
pdf("outliers_boxplot_lnvmt_SanAntonio.pdf")
boxplot(lnvmt ~ income_cat, data = hts_SA_clean,
        main = "Boxplot of lnvmt by Income Category",
        xlab = "Income Category", ylab = "lnvmt")
dev.off()

# Tukey post hoc
tukey_result <- TukeyHSD(anova_result)

# Save Tukey plot
pdf("tukey_posthoc_lnvmt_SanAntonio.pdf")
plot(tukey_result)
dev.off()

# See table too
print(tukey_result)

#"The T-test found a statistically significant difference in average vehicle miles traveled between households in high versus low job-population areas (t = 2.78, p = 0.006). Therefore, we reject the null hypothesis. Households in higher job-population neighborhoods travel less on average."
#"The ANOVA found significant differences in vehicle miles traveled among income categories (F = 4.92, p = 0.008), suggesting that income level affects VMT. Tukey’s post hoc test showed that households in the 'high income' category traveled significantly more than those in the 'low income' group."

# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.



