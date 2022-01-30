#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyverse")
library(ggplot2) # To use the ggplot graphic functions
library(dplyr) # We call this library to use the 'select' function
library(tidyverse) # We call it to use the gapminder database

# This function shows all the correlations we can obtain with all the variables
# from the database (db)
all_correlations <- function(db) { 
  # This value's gonna increase
  n <- 1
  # This value says how many times we could obtain a correlation from the database
  combinatory <- length(db)*(length(db)-1)/2  # As a combinatory
  # We create the null matrix in terms of the number of db variables
  correlations <- matrix(nrow = combinatory, ncol=3)
  # We put 3 columns: 
  # first two - db variables
  # last - correlation obtained with these two db variables
  
  for(i in 1:length(db)){
    
    if(i<length(db)){
      
      for(j in (i+1):length(db)){
        #print(c(i,j))
        # We put the names of each column from the db two by two
        # in the 'correlations' matrix
        # then the Pearson Correlation value we can obtain with these colums
        correlations[n,] <- c(names(db[i]), names(db[j]),
        cor(db[i], db[j], use="na.or.complete", method=c("pearson", "kendall", "spearman")))
        n<-n+1
      }
      
    }
    
  }
  
  return(correlations)
  #correlations[order(correlations[,3], decreasing = TRUE), ]
}

#Llamamos a la base de datos airquality
#attach(airquality)
airquality #1era opcion
View(airquality) #2da opcion

correlations = all_correlations(airquality)
# To see all the correlations we can do with the variables of 'airquality'
# this lines orders it from minor to major
correlations[order(correlations[,3]),]
# We're gonna use the highest correlation
# in this case it's conformed by the Ozone and Temp variables

# Pearson Correlation
cor.test(airquality$Temp, airquality$Ozone)

# Data Cleaning
# We're gonna use the dplyr function select
# and put the columns we will use as arguments
data = select(airquality,Ozone,Temp)
# Now we clean the NA from the rows
data = na.omit(data)
View(data)
# We create the linear regression model with these variables
#lm(y-x)
model = lm(data$Temp~data$Ozone)
summary(model)
# beta1 and beta0 coefficients
model$coefficients
# Graphic of the model
#aes(x,y)
graf = ggplot(data, aes(data$Ozone,data$Temp))
graf = graf + geom_point() + geom_smooth(method = "lm", col = "red")
graf
