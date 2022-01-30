#install.packages("tidyverse")
library(tidyverse) # We call it to use the airquality database

# We calculate the confidence interval to the population variance
# taking the elements from the entire sample in the
# variable "Wind" from the database "airquality"

# We use the fact that the distribution from S^2/var_pob approaches to
# a chi-squared distribution with 1 degree of freedom

#1. We calculate S^2 = deviation * n / (n-1)
n <- length(airquality$Wind)
estim <- (n/(n-1))*sd(airquality$Wind)

#2. We use qchisq to calculate lim_inf_ini and lim_sup_ini 
# from S^2/var_pob with an alpha = 0.05
alpha = 0.05

lim_inf_ini <- qchisq(alpha/2, 1)
lim_sup_ini <- qchisq(1-alpha/2, 1)

#3. Applying elementary operations at the given interval, we can find
# the limits from the var_pob
lim_inf_final <- estim/lim_sup_ini
lim_sup_final <- estim/lim_inf_ini

#4. Then the population variance is in this interval:
v2 <- c(lim_inf_final,lim_sup_final)
v2
# (0.7058637, 3610.9260852) with a 95% confidence