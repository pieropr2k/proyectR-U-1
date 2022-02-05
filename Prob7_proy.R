#install.packages("tidyverse")
library(tidyverse) # We call it to use the airquality database

# We calculate the hypothesis test taking 
# the entire sample from the variable "Temp" from the database "airquality"

all_db<-airquality$Temp

# These are the Statistics Hypothesis
# H0: sigma^2 = 90
# Ha: sigma^2 < 90
# sigma^2_o = 90
# We'll use the Hypothesis and the Lower Tail Reject Region
var_o <- 90

# We know that the Reject Region (R.R.) at 5% is given by:
# R.R = {X2-score < X2}
# where X2 is the quantil (alpha) from the lower tail


#1. We calculate S^2 = variance
n <- length(all_db)
estim <- var(all_db)

#2. We calculate the X^2 to an alpha = 0.05
alpha <- 0.05
gl <- n-1

X2 <- qchisq(1-alpha/2, gl, lower.tail = TRUE)

#3. We calculate the statistic test X^2 = (n-1)S^2/var_o
# we use var_o = 90
X2_estadistico <- (n-1)*estim/var_o
X2_estadistico

#4. Now we're able to see if the Null Hypothesis H0 is accepted or rejected

if(X2_estadistico < X2) {
  "Se rechaza H0"
} else {
  "Se acepta H0"
}

# R.R. = {151.3098 < 188.0263} -> 151.3098 does BELONG to R.R
# There's sufficient evidence to conclude Ha