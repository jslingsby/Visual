##############################################################################
######## UNHCR displaced persons by year, relative to ENSO and drought
##############################################################################
######## Compiled by Jasper Slingsby 2015
######## Last edited: 5 November 2015
##############################################################################
######## 
###Steps:

##############################################################################
###1) Setwd and get libraries
##############################################################################

setwd("/Users/jasper/Documents/Databases/AAA_Interest/")

library(dplyr)

##############################################################################
###2) Get data
##############################################################################

people <- read.csv("unhcr_popstats_export_persons_of_concern_all_data.csv", skip=3, stringsAsFactors = F)
popln <- read.csv("world_population.csv", stringsAsFactors = F)
enso <- read.table("Multivariate_ENSO_Index1950-2015Oct.txt", stringsAsFactors = F, na.strings=-999.9)
ann_enso <- data.frame(Year=enso[,1], ENSO=rowMeans(enso[,2:13]))

people[people == "*"] <- 1 #replace *'s with 1's

people[,4:11] <- sapply(people[,4:11], as.numeric)

ppl <- tbl_df(people)

y <- summarise(group_by(ppl, Year), Displaced_people=sum(Total.Population))
y <- merge(y, popln)
y <- merge(y, ann_enso)
y$Proportion_displaced <- y$Displaced_people/y$global_population_midyear
y <- y[,which(colnames(y)%in%c("Year","Displaced_people","global_population_midyear","ENSO","Proportion_displaced"))]

plot(Displaced_people ~ Year, data=y, type="l")
plot(Proportion_displaced ~ Year, data=y, type="l")
plot(ENSO ~ Year, data=y, type="l")

enso_ts <- ts(y$ENSO)
dp_ts <- ts(y$Displaced_people)
pd_ts <- ts(y$Proportion_displaced)
gp_ts <- ts(y$global_population_midyear)

ccf(dp_ts, enso_ts)
ccf(pd_ts, enso_ts)
ccf(dp_ts, gp_ts)
