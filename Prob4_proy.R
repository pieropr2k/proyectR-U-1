#install.packages("tidyverse")
library(tidyverse) # We call it to use the airquality database

# We calculate the confidence interval for the media
# using the variable "Wind" from the database "airquality"

# We know that the 95% Confidence Interval (C.I.)
# is given by:
# (sample media) +- Z*(standard error)
# where z is the quantil (alpha/2) from the right tail

#1. We calculate the sample media

mean(airquality$Wind)
media<-mean(airquality$Wind)

#2. We calculate the Z to an alpha = 0.05
alpha = 0.05

qnorm(alpha/2, 0, 1, lower.tail = FALSE)
z<-qnorm(alpha/2, 0, 1, lower.tail = FALSE)

#3. We calculate the standard deviation

sd(airquality$Wind)
desviacion<-sd(airquality$Wind)

#4. With the standard deviation we can approach the standard error of the muestral distr.
n<-length(airquality$Wind)
errorst<-desviacion/sqrt(n)

#5. Now we're able to calculate the lim_inf and lim_sup from our C.I.
lim_inf<-media - z*errorst
lim_sup<-media + z*errorst

#6. Finally we can assure that the population media u, is in this interval:
v <- c(lim_inf,lim_sup)
v
# (9.399284 10.515749) with a 95% confidence