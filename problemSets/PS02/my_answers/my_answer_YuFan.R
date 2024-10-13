###Question 1
#(a)

#f_observed
a <- 14  
b <- 6   
c <- 7 
d <- 7  
e <- 7
f <- 1
N <- a + b + c + d + e + f
# N=42

# f_expected = Row total/Grand total × Column total
expected_a= (a+b+c)*(a+d)/N
expected_b= (a+b+c)*(b+e)/N
expected_c= (a+b+c)*(c+f)/N
expected_d= (d+f+e)*(a+d)/N
expected_e= (d+f+e)*(b+e)/N
expected_f= (d+f+e)*(f+e)/N

#Chi-square
chi_squared <- ((a - expected_a)^2 / expected_a) +  
  ((b - expected_b)^2 / expected_b) +  
  ((c - expected_c)^2 / expected_c) +  
  ((d - expected_d)^2 / expected_d) +
  ((e - expected_e)^2 / expected_e) +
  ((f - expected_f)^2 / expected_f) 
print(chi_squared)

#(b)
# df
df <- 2
# p-value
p_value <- pchisq(chi_squared, df, lower.tail = FALSE)
p_value
#p_value > 0.1，Can't reject the null

#(c)
O <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE, dimnames = list(c("Upper class", "Lower class"), c("Not Stopped", "Bribe requested", "Stopped/given warning")))
E <- outer(rowSums(O), colSums(O)) / sum(O)
# props
row_props <- rowSums(O) / sum(O)
col_props <- colSums(O) / sum(O)
row_props
col_props
s_residuals <- (O - E) / sqrt(E*(1-row_props) %o% (1-col_props))

print(s_residuals)

#(d)
#Upper class: Not Stopped: The observed frequencies are slightly higher than the expected frequencies, but the difference is not significant (absolute value less than 1).
#Upper class: Bribe requested: the number of observations is lower than the expected number of frequencies and the difference is close to significant (absolute value between 1 and 2).
#Upper class: Stopped/given warning: The observed frequency is higher than the expected frequency and the difference is close to significant (absolute value between 1 and 2).
#Lower class: Not Stopped: The observed frequency is lower than the expected frequency, but the difference is not significant.
#Lower class: Bribe requested: The observed frequency is higher than the expected frequency and the difference is nearly significant.
#Lower class: Stopped/given warning: the observed frequency is lower than the expected frequency and the difference is also close to significant.

#Significant differences: upper class drivers were more likely to be given stops/warnings, while lower class drivers were more likely to be asked for bribes. These differences are significant, suggesting that social class may influence police behaviour.
#Non-significant difference: the difference between the observed and expected frequencies of non-stopping behaviour across the two classes is not significant.
#These results suggest that there are significant differences in police behaviour towards drivers from different social classes, which may reflect inequalities in social structure and law enforcement practices.

###Question 2
#(a)
#Null Hypothesis (H0): There is no significant difference in the number of new or repaired drinking water facilities between villages that have retained a VDC head and villages that have not retained a VDC head.
#H0: μreserved = μunreserved
#Alternative Hypothesis (Ha): There is a significant difference in the number of new or repaired drinking water facilities between villages with and without retained VCs.
#Ha: μreserved ≠ μunreserved

#(b)
alldata <-  "https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv"
data <- read.csv(alldata)

head(data)
# Run bivariate regression
model <- lm(water ~ reserved, data = data)
summary(model)
# p-value=0.0197 

#(c)
#The coefficient estimate for the reservation policy is 9.252, 
#indicating a positive correlation between the reservation policy and the number of new or repaired drinking water facilities in villages. 
#Villages that adopted the reservation policy had 9.252 more drinking water facilities compared to those that did not implement the policy.