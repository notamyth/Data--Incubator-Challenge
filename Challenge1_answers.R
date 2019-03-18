
## 
# Data set - http://www1.nyc.gov/assets/ccrb/downloads/excel/ccrb_datatransparencyinitiative_20170207.xlsx
# NYC Demographic - https://en.wikipedia.org/wiki/Demographics_of_New_York_City
# NYC precincts - https://www1.nyc.gov/site/nypd/bureaus/patrol/precincts-landing.page

###
#Q1
# How many unique complaints (identified by 'UniqueComplaintId') with complete information
# (i.e. there are no missing values) appear in the dataset?

#Q2
# What proportion of complaints occur in the borough with the largest number of complaints? For this question,
# only consider unique complaints with complete informatio

#Q3
# How many complaints per 100k residents were there in the borough with the highest number of complaints per capita resulting from incidents in 2016? 
# Find the 2016 population estimates of each borough on Wikipedia. 
# Ignore complaints from "Outside NYC". For this question, only consider unique complaints with complete information.

#Q4
# What is the average number of years it takes for a complaint to be closed? 
# For this question, only consider unique complaints with complete information.

#Q5
# Complaints about stop and frisk have been declining. Use linear regression from the year complaints about stop and frisk peaked through 2016 (inclusive) 
# to predict how many stop and frisk incidents in 2018 will eventually lead to a complaint.
# For this question, only consider unique complaints with complete information. Remember that the count of complaints must be an integer (round to nearest).

#Q6
# Calculate the chi-square test statistic for testing whether a complaint is more likely to receive a full investigation when it has video evidence.
# For this question, only consider unique complaints with complete information.

#Q7
# Each row in the data set corresponds with a specific allegation. Therefore a particular complaint, designated by 'UniqueComplaintId', may have multiple allegations.
# Consider only allegations with complete information.
# Is the presence of a certain type of allegation (i.e. 'Allegation FADO Type') indicative that a complaint will contain multiple allegations? 
# Create indicator variables for whether a complaint contains each type of allegation, 
# and perform a linear regression of the number of allegations per complaint against these indicator variables.
# What is the maximum coefficient of the linear regression?

#Q8
# According to NYC.gov there are approximately 36000 officers in New York. The website additionally lists information on all the precincts in each borough.
# Consider unique complaints (not necessarily with complete information) from incidents in 2016.
# Assuming that complaints per capita are proportional to officers per capita in each borough, calculate the average number of officers per precinct in each borough (ignore complaints from outside of NYC). 
# What is the ratio of the highest number of officers per precinct to the lowest number of officers per precinct?
# 
###


MyData <- read.csv(file="challenge1.csv", header=TRUE, sep=",")
summary(MyData)

# omiting the missing value records
newdata <- na.omit(MyData)
summary(newdata)

## Highest complaints are in brooklyn borough. from Summary - 71580
## proportion of Brooklyn to total = 71580/202390

brookyln_prop = 71580/202390
brookyln_prop
print(brookyln_prop, digits=10)
# 0.3536736005

# Brooklyn population 2016= 2629150 - Wikipedia
# total complaint - outside nyc = 202390 - 175=   202215
#
# Complaints per capita per 100K 
brooklyn_complainy_per_capita = (71580/202215)*100000
print(brooklyn_complainy_per_capita, digit=10)
#35397.96751

# Average number of years taken for closing the case
yearForClose = newdata$Close.Year-newdata$Received.Year
summary(yearForClose)
print(mean(yearForClose),digit= 10)
#0.5874697366

# Find the year having Peak stop and frisk
# take only incident year and stop and frisk columns
subset_frisk<- newdata[c(10,12)]
# Take onlt those columns havning Stop and frisk true and subsequent year

subset2_frisk <- subset_frisk[ which(subset_frisk$Complaint.Contains.Stop...Frisk.Allegations==TRUE ), ]
summary(subset2_frisk)
# Complaint.Contains.Stop...Frisk.Allegations Incident.Year 
# Mode:logical                                Min.   :1999  
# TRUE:83281                                  1st Qu.:2007  
# Median :2009  
# Mean   :2010  
# 3rd Qu.:2012  
# Max.   :2017  

w = table(subset2_frisk$Incident.Year)
w
t = as.data.frame(w)
t
 ###### Frequence of Stop and frisk based on the year
# Var1  Freq
# 1  1999    13
# 2  2002    11
# 3  2003    18
# 4  2004   420
# 5  2005  5739
# 6  2006 10561
# 7  2007 11418
# 8  2008  9263
# 9  2009  8741
# 10 2010  7052
# 11 2011  6559
# 12 2012  5969
# 13 2013  5397
# 14 2014  4374
# 15 2015  4310
# 16 2016  3418
# 17 2017    18

# 2007 is the year where Stop and frisk was peak

 year = c(2007, 2008, 2009,2010,2011,2012,2013,2014,2015,2016) 
 numberofc =  c(11418,9263,8741,7052,6559,5969,5397,4374,4310,3418) 
 df = data.frame(year, numberofc)



model1 <- lm(numberofc~year, data =df)
summary(model1)

new.df <- data.frame(year=c(2018))
predict(model1, new.df)
noofc = predict(model1, new.df)
print(noofc, digits = 10)
round(noofc)
# For year 2018, the model predicts 1368.909091 ~ 1369 complaints


#### Chi-Square test

chiss<- chisq.test(newdata$Is.Full.Investigation, newdata$Complaint.Has.Video.Evidence)

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  newdata$Is.Full.Investigation and newdata$Complaint.Has.Video.Evidence
# X-squared = 4761, df = 1, p-value < 2.2e-16
# 

allg.f <- factor(newdata$Allegation.FADO.Type)
allg.f
#Number of allg per complaint?


##Levels: Abuse of Authority, Discourtesy, Force, Offensive Language
model2 <- lm( UniqueComplaintId~ allg.f, data = newdata)
summary(model2)

library(plyr)
?count
coefficients(model2)

 max_coeff<- summary(model2)$coefficients[2]
print(max_coeff, digits = 10)
#71.47279262
# data for complaints inside NYC only
insidenyc <- MyData[ which(MyData$Borough.of.Occurrence!='Outside NYC'), ]
insidenyc
summary(insidenyc)
## Data for 2016 only
sixteen <- insidenyc[ which(insidenyc$Incident.Year=="2016" ), ]
summary(sixteen)
nrow(sixteen)
## Number of complaints in NYC 10289 in 2016
## Manhattan    Brooklyn       Queens    Bronx     Staten
##  2316        3231          1646      2603        493


## population
##  Man       Brooklyn    Queens    Bronx   Staten
## 1643734	2629150	    2333054	  1455720	  476,015	


# number of precinct per burrow
##  Man       Brooklyn    Queens    Bronx   Staten
## 	22	       12         23        16      4 	  	 


## As given, the percapita complaints in 2016 = Number of officers percapita in each borough
# so the number of complaints = number of officers

##Now we divide number of officers in each borough by numberof precinct to get numberof officers per precinct


##
manpre = 2316/22
manpre
#105.2727

bropre = 3231/12
bropre
#269.25

quepre = 1646/23
quepre
#71.56522

brxpre = 2603/16
brxpre
#162.6875

stapre = 493/4
stapre
#123.25

# ratio Highest/ lowest
ratio = bropre/quepre
ratio
print(ratio,digits = 10)
#3.762302552