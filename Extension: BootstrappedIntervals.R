library(boot)
library(dplyr)
library(arm)
library(ggplot2)
library(tidyr)

load("/Users/sallyshearer/Downloads/MainDataset.RData")

#Extract the specific data frame being used here ("crd" which are the communities before 1999) 
df <- data.frame(crd@data, stringsAsFactors = FALSE)

##Because will be replicating Charnysh & Finkel's logit regression on Investment in new houses as a log function of
##distance from Treblinka; as such we can extract only the relvant columns needed from replectation
rep.df <-df[,c("Dwel88","Dw4570","distTreb", "distRail45KM","Destr46","CityDistKm","miasto", "GG")]


df$homes.out.time <- df$Dwel88-df$Dw4570
df$homes.in.time <- df$Dw4570

A <- df[df$distTreb <=50 & df$miasto==0,]


#BOOTSTRAPPING to obtain a distribution of model parameter values and explore the inter for each variable

#Create sampling function:
##1. creat a new dataframe w/ randomly selected indices
##2 Fit the model to  new data set
###3. Return the coefficients of the model
model.boot <- function(df,indices){
  sub.data<-df[indices,]
  model<-glm(cbind(homes.in.time, homes.out.time)~log(distTreb) + log(distRail45KM),data=sub.data, 
             family = quasibinomial)
  return (coef(model))
  }

##Run boostrap for 2000 resamplings
myboot <- boot(data = A, statistic = model.boot, R = 2000)

##Put results into a dataframe
results <- data.frame(myboot$t)
colnames(results) <- names(myboot$t0)

#Create a box plot showing the quantile intervals, derived from bootstrap, for each variable in this model
##Outliers (using default value of) shown in red
##Value scales for each variable adjusted
ggplot(gather(results), aes(x='', y=value)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) + facet_wrap(~key, scales='free')


