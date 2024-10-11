#Load necessary libraries 
library(tidyverse)


#set working directory
setwd('/Users/jermaineatkins/Desktop/R/EconometricsFall24/ecob2000_lecture1')
getwd('/Users/jermaineatkins/Desktop/R')

#Load Dataset
load('Household_Pulse_data_ph4c2.RData')
head(primeworkage$INCOME)
head(Household_Pulse_data$EEDUC)
head(primeworkage$EEDUC)

#Filter prime work age 
Age <- 2024 - Household_Pulse_data$TBIRTH_YEAR 

primeworkage <- Household_Pulse_data %>% filter(ANYWORK == "yes employment in last 7 days" &
                                                  Age >= 25 & Age <= 55)
model <- lm(EEDUC, INCOME, data = primeworkage, geom = point)




#Setting up data for graph 

#EDUCATIONAL ATTAINMENT 
primeworkage$EEDUC <- fct_recode(primeworkage$EEDUC,
                                         "8"  = "less than hs",
                                         "10" = "some hs",
                                         "12" = "HS diploma",
                                         "13" =  "some coll",
                                         "14" = "assoc deg", 
                                         "16" = "bach deg",
                                         "18" = "adv deg") 
                                         
primeworkage$EEDUC <- as.numeric(levels(primeworkage$EEDUC))[primeworkage$EEDUC]

                                         
#INCOME
                                         
primeworkage$INCOME <- fct_recode(primeworkage$INCOME,
                                             "0" = "NA", 
                                             "1" = "HH income less than $25k",
                                             "2" = "HH income $25k - $34.9k", 
                                             "3" = "HH income $35k - 49.9", 
                                             "4" = "HH income $50k - 74.9",
                                             "5" = "HH income $75 - 99.9", 
                                             "6" = "HH income $100k - 149", 
                                             "7" = "HH income $150 - 199", 
                                             "8" = "HH income $200k +")
                                        
                                         
primeworkage$INCOME <- as.numeric(levels(Household_Pulse_data$INCOME))[Household_Pulse_data$INCOME]                                        





#Summarize the Model

 model <- lm(INCOME ~ EEDUC + PRICESTRESS, data = primeworkage )
 summary(model)
 
 # subset in order to plot...
 NNobs <- length(primeworkage$INCOME)
 set.seed(12345678) # just so you can replicate and get same "random" choices
 graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
 dat_graph <-subset(primeworkage,graph_obs)  
 
 plot(INCOME ~ jitter(EEDUC, factor = 2), pch = 18, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), 
      data = dat_graph)
 # ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
 plot(INCOME ~ jitter(EEDUC, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), 
      ylim = c(0,120000), data = dat_graph)
 # does that help much?
 
 # change this line to fit your choices about explanatory variables
 to_be_predicted2 <- data.frame(EEDUC = 8:18, RHISPANIC = "Hispanic", 
                                RRACE = "Black", EEDUC = "adv deg")
 to_be_predicted2$reg <- predict(model, newdata = to_be_predicted2)
 
 lines(yhat ~ Age, data = to_be_predicted2)
 
 
 
