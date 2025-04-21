rm(list = ls()) # removes all previously stored variables
library(Hmisc) 

data <- read.csv("~/Downloads/Covid_R/COVID19_line_list_data.csv")
describe(data) # Hmisc Command

# The death column has been cleaned up
data$death_dummy <- as.integer(data$death != 0)

# Death rate = sum of rows / number of rows
sum(data$death_dummy) / nrow(data)

# Age
# claim: people who die are older than people who survive
dead = subset(data, death_dummy == 1) # all the rows where people are dead
alive = subset(data, death_dummy == 0) # all the rows where people survive

# Mean age of both groups
# na.rm ignores every entry where the age is unknown
mean(dead$age,na.rm = TRUE)
mean(alive$age, na.rm = TRUE)

# is this statistically significant?
t.test(alive$age, dead$age, alternative = "two.sided", conf.level = 0.99)
# normally, if p-value < 0.05, we reject the null hypothesis
# here, the p-value ~ 0, so we reject the null hypothesis
# Conclusion: it is statistically significant

# Gender
# claim: gender has no effect
men = subset(data, gender == "male")
women = subset(data, gender == "female")

mean(men$death_dummy,na.rm = TRUE) # 8.5%
mean(women$death_dummy, na.rm = TRUE) # 3.7%

# is this statistically significant? (men have higher death rates)
t.test(men$death_dummy, women$death_dummy, alternative = "two.sided", conf.level = 0.99)
# 99% confidence: men have from 0.8% to 8.8% higher chance of dying
# p-value = 0.002 < 0.05, so this is statistically significant
