install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
load("/Users/sallyshearer/Downloads/MainDataset.RData")

#Extract the specific data frame being used here ("crd" which are the communities before 1999) 
df <- data.frame(crd@data, stringsAsFactors = FALSE)

##Because will be replicating Charnysh & Finkel's logit regression on Investment in new houses as a log function of
##distance from Treblinka; as such we can extract only the relvant columns needed from replectation
rep.df <-df[,c("Dwel88","Dw4570","distTreb", "distRail45KM","Destr46","CityDistKm","miasto", "GG")]


#First limit to the time series of interest for analysis. 
## We subtract b/c the Dwel88 variable (from census) includes the Dw4570 as a subcategory; census grouped new homes 
##by time period in which they were contructed.
##This will allow us to compare the share of homes constructed during 1945-1970 (compared to other time periods) as the 
#dependent variable  --> this represents homes constructed OUTside timeframe
df$homes.out.time <- df$Dwel88-df$Dw4570

#Rename the variable for homes constructions b/ 45-70 to be consistant with variable name above
df$homes.in.time <- df$Dw4570

#First three models look exlusively at cases located 50 km or less from Treblinka; will only be using
##this subset for the exentsion analysis
A <- df[df$distTreb <=50 & df$miasto==0,]

##Extension: Is model dependence a concern?
#Now consider how different combinations of the three variables used in authors' second model effect outcome
#Original Model 2
m2 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)  + log(distRail45KM) + Destr46, data = A, family = quasibinomial)
#Sally's Variations of above model
m2a <- glm(cbind(homes.in.time, homes.out.time) ~ (log(distTreb)* log(distRail45KM)) + Destr46, data = A, family = quasibinomial)
m2b <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)  + (log(distRail45KM)* Destr46), data = A, family = quasibinomial)
m2c <- glm(cbind(homes.in.time, homes.out.time) ~ (log(distTreb)*Destr46)  + log(distRail45KM), data = A, family = quasibinomial)
m2d <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)* log(distRail45KM)* Destr46, data = A, family = quasibinomial)


#Begin preparations to plot
A$fit1 <- predict(m2, type = "response")
A$fit2 <- predict(m2a, type = "response")
A$fit3 <- predict(m2b, type = "response")
A$fit4 <- predict(m2c, type = "response")
A$fit5 <- predict(m2d, type = "response")

#Need to manually set y-scale or graphs will have slightly different scales (not best practice when comparing) 
plot1 <- ggplot(data = A, aes(x = distTreb, y = fit1)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 
  

plot2 <- ggplot(data = A, aes(x = distTreb, y = fit2)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 


plot3 <-ggplot(data = A, aes(x = distTreb, y = fit3)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 


plot4 <- ggplot(data = A, aes(x = distTreb, y = fit4)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))  

plot5 <- ggplot(data = A, aes(x = distTreb, y = fit5)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))  

##Plot Fit of different models
grid.arrange(plot1, plot2, plot3, plot4, plot5)



#Now consider how different combinations of the four variables in authors' third model effect outcomes
#Original Model 3
m3 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)  + log(distRail45KM) + log(CityDistKm) + Destr46, data = A, family = quasibinomial)
##Note: These models are most, but not all of the combinations
m3a <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)*log(distRail45KM) + log(CityDistKm) + Destr46, data = A, family = quasibinomial)
m3b <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb) + log(distRail45KM)*log(CityDistKm) + Destr46, data = A, family = quasibinomial)
m3c <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb) + log(distRail45KM) + log(CityDistKm)*Destr46, data = A, family = quasibinomial)
m3d <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb) + log(distRail45KM)*log(CityDistKm)*Destr46, data = A, family = quasibinomial)
m3e <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)*log(distRail45KM)*log(CityDistKm)* Destr46, data = A, family = quasibinomial)
m3f <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)*log(CityDistKm) + log(distRail45KM) + Destr46, data = A, family = quasibinomial)
m3g <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)*Destr46+ log(distRail45KM) + log(CityDistKm), data = A, family = quasibinomial)


#Begin preparations to plot
A$fitb1 <- predict(m3, type = "response")
A$fitb2 <- predict(m3a, type = "response")
A$fitb3 <- predict(m3b, type = "response")
A$fitb4 <- predict(m3c, type = "response")
A$fitb5 <- predict(m3d, type = "response")
A$fitb6 <- predict(m3e, type = "response")
A$fitb7 <- predict(m3f, type = "response")
A$fitb8 <- predict(m3g, type = "response")


#Again, need to manually set y-scale or graphs will have slightly different scales (not best practice when comparing) 
plotb1 <- ggplot(data = A, aes(x = distTreb, y = fitb1)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 


plotb2 <- ggplot(data = A, aes(x = distTreb, y = fitb2)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 


plotb3 <-ggplot(data = A, aes(x = distTreb, y = fitb3)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55)) 


plotb4 <- ggplot(data = A, aes(x = distTreb, y = fitb4)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))  

plotb5 <- ggplot(data = A, aes(x = distTreb, y = fitb5)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))  

plotb6 <- ggplot(data = A, aes(x = distTreb, y = fitb6)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))

plotb7 <- ggplot(data = A, aes(x = distTreb, y = fitb7)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))


plotb8 <- ggplot(data = A, aes(x = distTreb, y = fitb8)) +
  stat_smooth() +
  labs(x = "Distance from Treblinka (km)", y = "Fit") +
  theme_classic() + 
  coord_cartesian(ylim=c(0.4, 0.55))+
  scale_y_continuous(breaks=c(0.4,0.45,0.5,0.55))


grid.arrange(plotb1,plotb2,plotb3,plotb4,plotb5,plotb6, plotb7, plotb8)


#Continue model dependence extensions, lookiant at model 2 a different way
use_col1 <- c('distRail45KM', 'Destr46', 'Dwel88', 'Dw4570')
newdata2 <- newdata <- data.frame(distTreb = 7:50)
newdata2[,use_col1] <- matrix(rep(apply(A[,use_col1], 2, mean), nrow(newdata)), 
                                  nrow=nrow(newdata), byrow=T)

author_originala <- predict(m2, newdata = newdata2, type='response')
sally_extena <- predict(m2a, newdata = newdata2, type='response')
sally_exten_b <- predict(m2b, newdata = newdata2, type='response')
sally_exten_c <- predict(m2c, newdata = newdata2, type='response')
sally_exten_d <- predict(m2d, newdata = newdata2, type='response')

#Plot author's original model
plot(newdata$distTreb, author_originala, type='l',ylim=c(0,1), xlab='Distance From Treblinka', ylab='Propotion of Houses Built b/w 1945-70')

#Add extended models to the plot for visual comparison of implications of model dependence
lines(newdata$distTreb, sally_extena, type='l', ylim=c(0,1), col='red')
lines(newdata$distTreb, sally_exten_b, type='l', ylim=c(0,1), col='blue')
lines(newdata$distTreb, sally_exten_c, type='l', ylim=c(0,1), col='green')
lines(newdata$distTreb, sally_exten_d, type='l', ylim=c(0,1), col='purple')


#Continue model dependence extension, looking at model 3  a different way
use_col2 <- c('distRail45KM', 'Destr46', 'Dwel88', 'Dw4570', 'CityDistKm')
newdata <- data.frame(distTreb = 7:50)
newdata[,use_col2] <- matrix(rep(apply(A[,use_col2], 2, mean), nrow(newdata)), 
                             nrow=nrow(newdata), byrow=T)


author_original <- predict(m3, newdata = newdata, type='response')
sally_exten <- predict(m3a, newdata = newdata, type='response')
sally_exten_2 <- predict(m3b, newdata = newdata, type='response')
sally_exten_3 <- predict(m3c, newdata = newdata, type='response')
sally_exten_4 <- predict(m3d, newdata = newdata, type='response')
sally_exten_5 <- predict(m3e, newdata = newdata, type='response')
sally_exten_6 <- predict(m3f, newdata = newdata, type='response')
sally_exten_7 <- predict(m3g, newdata = newdata, type='response')

#Plot author's original model
plot(newdata$distTreb, author_original, type='l',ylim=c(0,1), xlab='Distance From Treblinka', ylab='Propotion of Houses Built b/w 1945-70')

#Add extended models to the plot for visual comparison of implications of model dependence
lines(newdata$distTreb, sally_exten, type='l', ylim=c(0,1), col='red')
lines(newdata$distTreb, sally_exten_2, type='l', ylim=c(0,1), col='blue')
lines(newdata$distTreb, sally_exten_3, type='l', ylim=c(0,1), col='green')
lines(newdata$distTreb, sally_exten_4, type='l', ylim=c(0,1), col='purple')
lines(newdata$distTreb, sally_exten_5, type='l', ylim=c(0,1), col='black')
lines(newdata$distTreb, sally_exten_6, type='l', ylim=c(0,1), col='yellow')
lines(newdata$distTreb, sally_exten_7, type='l', ylim=c(0,1), col='brown')


#Show the actual data v. the author's model and one of extension models to show
## *if* the model was 100% acurate
actual <- A$homes.in.time/(A$homes.in.time + A$homes.out.time)
plot(actual, m3$fitted.values, xlim=c(0,1), ylim=c(0,1), xlab = "Actual proportion of homes built b/w 1945-70", ylab = "Model Predicted Proportion")
points(actual, m3a$fitted.values, xlim=c(0,1), ylim=c(0,1), col='red')
abline(a=0, b=1)
text(0.2, 0.1)





