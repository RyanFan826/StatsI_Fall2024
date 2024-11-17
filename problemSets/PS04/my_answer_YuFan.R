##Question1
#(1)
install.packages("car")
library(car)

# Loading the Prestige dataset
data(Prestige)
help(Prestige)

# Create professional
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# View new variables in the dataset
head(Prestige)

#(2)
# Running a linear model
model <- lm(prestige ~ income * professional, data = Prestige)

# Output results
summary(model)

#(3)
#Based on the output of the linear model, we can write the prediction equation. The form of the equation is as follows：
#prestige=β0+β1*income+β2*professional+β3*(income*professional)
#Substituting the estimated coefficient values into the equation：
#prestige=21.1423+0.0032*income+37.7813*professional−0.0023*(income*professional)
#so:
# professional = 0 (blue-collar or white-collar workers)
#prestige = 21.1423+0.0032*income

# professional = 1 (professionals)
#prestige =21.1423+37.7813+(0.0032−0.0023)*income= 58.9236+0.0009*income

# Thus, these two equations show how income and occupation type affect occupational prestige.

#(4)
#Coefficient of income (income = 0.0032): This coefficient indicates that, controlling for other variables (including occupation type and interactions), for every unit increase in income, the mean value of prestige increases by about 0.0032. This suggests that there is a positive correlation between income and prestige, i.e., as income increases, prestige increases slightly.

#If the occupation type is Blue Collar or White Collar Worker (professional = 0), occupational prestige increases by 0.0032 units for each additional unit of income.
#If the occupation type is Professional (professional = 1), the effect of income on occupational prestige is adjusted by the interaction term, but in general, income still has a positive effect on occupational prestige.
#This coefficient is positive and statistically significant (p-value < 0.001), indicating that income is a significant predictor variable of occupational prestige.

#(5)
#Coefficient of Occupational Type (professional = 37.7813): This coefficient indicates that the mean value of occupational prestige is 37.7813 units higher for professionals (professional = 1) than for blue-collar or white-collar workers (professional = 0), controlling for other variables, including income and interaction terms.
#This coefficient shows a significant effect of occupation type on occupational prestige, i.e. professionals have significantly higher occupational prestige than blue- or white-collar workers, even at the same income level.

#(6)
#Based on the prediction equation from part (c), when professional = 1, the marginal effect of income on the prestige score is 0.0009. This means that for each additional unit of income, the prestige score increases by 0.0009 units. 

#For a $1,000 increase in income, the change in the prestige score can be calculated as:
#    0.0009*1000 = 0.9

#Therefore, for professional occupations, a $1,000 increase in income is associated with a 0.9-unit increase in the prestige score.

#(7)
#When an individual’s income is $6,000, changing their occupation from non-professional to professional results in an impact on the prestige score that can be calculated using the coefficients from part (c).
#The direct effect of switching to a professional job is given by the coefficient of professional, which is 37.7813. However, because of the interaction term income:professional (-0.0023), we need to account for the effect of income. At an income level of $6,000, the interaction effect is -13.8 (calculated as -0.0023 × 6,000). Therefore, the total change in the prestige score is 37.7813 - 13.8 = 23.9813.

#Thus, at an income of $6,000, switching from a non-professional to a professional occupation increases the prestige score by approximately 23.98 units.

##Question2
#(1)
#(2)