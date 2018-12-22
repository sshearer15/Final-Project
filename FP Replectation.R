list.of.packages <- c("tm","foreign","dplyr","plyr",
                     "fuzzyjoin")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]; if(length(new.packages)){install.packages(new.packages)}

lapply(list.of.packages, require, character.only = TRUE)

rm(list.of.packages, new.packages)

#Load original data, downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/XRZW4O 
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


#First three models look exlusively at cases located 50 km or less from Treblinka; simplify for quicker entry into regression
A <- df[df$distTreb <=50 & df$miasto==0,]

#Latter two models look at 60 km or less, and 70 km or less, respectively
##Per Charnysh & Finkel, will exclude cases outside the General Government (GG) area
B <- df[df$distTreb <=60 & df$miasto==0 & df$GG==1,]
C <- df[df$distTreb <=70 & df$miasto==0 & df$GG==1,]

##Next we replicate the acctual regressions which use the natural log of distance variables;
##This is done becauese because of the non-normal distribution of this variable:
##"the relationship where the exposure to Jewish valuables first falls off rapidly with small increases in distance from the death camp
##and then continues to decrease, but only slightly at greater distances."(p. 11)

#Model 1: predicting construction using only distance
##Quasibinomial helps to account for overdispersion in the data
##We take the log of certain variables that do not have normal distribution
m1 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb), data = A, family = quasibinomial)

#Model 2: adding addition predictor variables to control for distance from Railway and wartime distruction of property
m2 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)  + log(distRail45KM) + Destr46, data = A, family = quasibinomial)

#Model 3: adds an additional distance variable (distance from nearest largest city) to Model 2
m3 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)  + log(distRail45KM) + log(CityDistKm) + Destr46, data = A, family = quasibinomial)

#Model 4: Same as model 3, but widens the radius to 60km from Triblenka
m4 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data = B, family = quasibinomial)

#Model 5: Wides the radius to 70km from Triblenka;
m5 <- glm(cbind(homes.in.time, homes.out.time) ~ log(distTreb)+log(distRail45KM) +log(CityDistKm)+ Destr46, data = C, family = quasibinomial)

#Compare models in a table, replicating Charnysh & Finkel's Table 2 (p.24)
results <- data.frame( model = rep(c('50 km (1)', '50 km (2)', '50 km (3)',  '60 km only GG', '70 km, only GG'), rep(2,5)),
                       stats = rep(c('Coeff', 'SE'), 5))

results <- cbind(results, rbind.fill(data.frame(t(summary(m1)$coefficients[,1:2])), 
           data.frame(t(summary(m2)$coefficients[,1:2])),
           data.frame(t(summary(m3)$coefficients[,1:2])),
           data.frame(t(summary(m4)$coefficients[,1:2])),
           data.frame(t(summary(m5)$coefficients[,1:2]))))

##Print table (replicating the author's Table 2)
results

