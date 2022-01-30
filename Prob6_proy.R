# From the pdf
# Exercise 1
install.packages("reshape2") 
install.packages("ggplot2") 
install.packages("randomForest") 
install.packages("shiny") 
install.packages("gbm") 
install.packages("xgboost") 

# Exercise 2
library(reshape2) 
library(ggplot2) 
library(randomForest) 
library(shiny) 
library(gbm) 
library(xgboost) 

# Exercise 3
install.packages("gapminder") 
install.packages("tidyverse") 
install.packages("babynames") 

library(gapminder) 
library(tidyverse) 
library(babynames) 

gapminder #gapminder
mpg #tidyverse
airquality #tidyverse
babynames #babynames


# Exercise 6

# We calculate the Hypothetic Test with a small sample for µ
# taking the first 20 elements from the entire sample in
# the variable "lifeExp" from the database "gapminder"

short_media<-gapminder[1:20, ]$lifeExp

# These are the Statistics Hypothesis
# H0: µ = 45
# Ha: µ > 45
# µ0 = 45
# We'll use the Hypothesis and the Upper Tail Reject Region
media_o = 45

# We know that the Reject Region (R.R.) at 5% is given by:
# {z-score > z}
# where z is the quantil (alpha) from the right tail

#1. We calculate the sample media

mean(short_media)
media<-mean(short_media)

#2. We calculate the Z to an alpha = 0.05
alpha = 0.05

qnorm(alpha,0,1,lower.tail = FALSE)
z<-qnorm(alpha,0,1,lower.tail = FALSE)

#3. We calculate the standard deviation

sd(short_media)
desviacion<-sd(short_media)

#4. With the standard deviation we can approach 
# the standard error of the muestral distribution
n<-length(short_media)
errorst<-desviacion/sqrt(n)

#6. We calculate the statistic test, we use media_o = 45
z_estadistico <- (media-media_o)/errorst

#7. Now we're able to see if the Null Hypothesis H0 is accepted or rejected
if(z_estadistico > z) {
  "Se rechaza H0"
} else {
  "Se acepta H0"
}

# R.R. = {1.11559 > 1.644854} -> 1.11559 does NOT belong to R.R
# There's no sufficient evidence to conclude Ha









