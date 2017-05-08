library(foreign)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
## STEP ONE - LOAD DATA#################I renamed the each fireincidnet dbf according to its year wihtin my directory of all NFIRS files. 
fire07 <- read.dbf("fireincident07.dbf")
fire08 <- read.dbf("fireincident08.dbf")
fire09 <- read.dbf("fireincident09.dbf")
fire10 <- read.dbf("fireincident10.dbf")
fire11 <- read.dbf("fireincident11.dbf")
fire12 <- read.table("fireincident12.txt", sep = "^")
fire13 <- read.table("fireincident13.txt", sep = "^")
fire14 <- read.table("fireincident14.txt", sep = "^")
fire15 <- read.table("fireincident15.txt", sep = "^")

################# STEP TWO- SUBEST DATA ##########replacing the first row of dbf from 2012-2015
colnames(fire12) <- as.character(unlist(fire12[1,])) # the first row will be the header
fire12 = fire12[-1, ]          # removing the first row.

colnames(fire13) <- as.character(unlist(fire13[1,])) # the first row will be the header
fire13 = fire13[-1, ]          # removing the first row.

colnames(fire14) <- as.character(unlist(fire14[1,])) # the first row will be the header
fire14 = fire14[-1, ]          # removing the first row.

colnames(fire15) <- as.character(unlist(fire15[1,])) # the first row will be the header
fire15 = fire15[-1, ]          # removing the first row.

############### subseting ea. fire(x) file to include only variables I am interested in analyzing
tidyfire <- subset(fire07, select = c(  ## selecting the columns that i want to keep from the fire dataset
                                    STATE,
                                    FDID,
                                    INC_DATE,
                                    INC_NO,
                                    NUM_UNIT,
                                    NOT_RES,
                                    BLDG_INVOL,
                                    ACRES_BURN,
                                    AREA_ORIG,
                                    HEAT_SOURC,
                                    FIRST_IGN,
                                    TYPE_MAT,
                                    CAUSE_IGN,
                                    FACT_IGN_1,
                                    EQ_POWER,
                                    STRUC_TYPE,
                                    FIRE_ORIG,
                                    FLAME_SPRD,
                                    ITEM_SPRD,
                                    DETECTOR
))

tidyfire <-tidyfire[ which(tidyfire$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire$STATE <- as.factor(as.character(tidyfire$STATE)) ## removing unused factors
levels(tidyfire$STATE) ## check if removed unused factors 


tidyfire2 <- subset(fire08, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))

tidyfire2 <-tidyfire2[ which(tidyfire2$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire2$STATE <- as.factor(as.character(tidyfire2$STATE))

tidyfire3 <- subset(fire09, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))

tidyfire3 <-tidyfire3[ which(tidyfire3$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire3$STATE <- as.factor(as.character(tidyfire3$STATE))
                        
                            

tidyfire4 <- subset(fire10, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))

tidyfire4 <-tidyfire4[ which(tidyfire4$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire4$STATE <- as.factor(as.character(tidyfire4$STATE)) 


tidyfire5 <- subset(fire11, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))

tidyfire5 <-tidyfire5[ which(tidyfire5$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire5$STATE <- as.factor(as.character(tidyfire5$STATE)) ## removing unused factors


tidyfire6 <- subset(fire12, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))
tidyfire6 <-tidyfire6[ which(tidyfire6 $STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire6 $STATE <- as.factor(as.character(tidyfire6 $STATE)) ## removing unused factors


tidyfire7 <- subset(fire13, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))
tidyfire7 <-tidyfire7[ which(tidyfire7$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire7$STATE <- as.factor(as.character(tidyfire7$STATE)) ## removing unused factors


tidyfire8 <- subset(fire14, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,## take out potentially
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))
tidyfire8 <-tidyfire8[ which(tidyfire8$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire8$STATE <- as.factor(as.character(tidyfire8$STATE)) ## removing unused factors


tidyfire9 <- subset(fire15, select = c(  ## selecting the columns that i want to keep from the fire dataset
  STATE,
  FDID,
  INC_DATE,
  INC_NO,
  NUM_UNIT,
  NOT_RES,
  BLDG_INVOL,
  ACRES_BURN,
  AREA_ORIG,
  HEAT_SOURC,
  FIRST_IGN,
  TYPE_MAT,
  CAUSE_IGN,
  FACT_IGN_1,
  EQ_POWER,
  STRUC_TYPE,
  FIRE_ORIG,
  FLAME_SPRD,
  ITEM_SPRD,
  DETECTOR
))
tidyfire9 <-tidyfire9[ which(tidyfire9$STATE == c("TX", "CA", "NC","GA","OR","FL","AL", "MT", "AZ", "WA")), ] ## subseting dataset to only include values of specified states

tidyfire9$STATE <- as.factor(as.character(tidyfire9$STATE)) ## removing unused factors


####################
main <- rbind(tidyfire, tidyfire2, tidyfire3, tidyfire4, tidyfire5, tidyfire6, tidyfire7, tidyfire8, tidyfire9)## creating a main dataset that i will be refering to 
main <- with(main,  main[order(STATE) , ])  ## sorting main so that the States are in alphabetical order
summary(main$CAUSE_IGN) ## want to look at leading causes of residential fires

##gettingrid of all occurances of "0 " as a cause
main <- main[which(main$CAUSE_IGN != 0 ), ] ## creating a new dataseet that only includes incidents caused by failer of equipment or heat source
main$CAUSE_IGN <- as.factor(as.character(main$CAUSE_IGN)) ## removing unused factors
summary(main$CAUSE_IGN) ##here we can see that intentional, unintentional, and faliur of equipment are main causes leading to fires
main$INC_DATE <-  str_sub(main$INC_DATE, -4, -1) ## getting rid of days and months, I only need the exact years. 
## I tried to keep months and years, but I was not able to get rid of the right numbers
## for example there are numbers like 1142001 was not able to determin if this would be considered
## 01-14-2001 or 11-04-2001. since this affects the data tremendously, I just decided to keep the year, since it was the most accurate way of creating some sort of timeline

############### solely looking at electrical malfunctions
electricalFail <- main[which(main$CAUSE_IGN == 3), ] ## creating a new dataseet that only includes incidents caused by failer of equipment or heat source
electricalFail <- electricalFail[, !(colnames(electricalFail) %in% "CAUSE_IGN")]   ## getting rid of Cause_IGN column because it does not change.. we already know that this data is caused by failiur of equipment or heat sources 

electricalFail1 <- subset(electricalFail, FACT_IGN_1 %in% c(30,31,32,33,34,35,36,37,38)) ## subsetting to only include instances of Electrical faliure, malfunction
electricalFail1$FACT_IGN_1 <- as.factor(as.character(electricalFail1$FACT_IGN_1)) ## getting rid of unused factors

electricSource <- subset(electricalFail, EQ_POWER %in% c(10,11,12))
electricSource$EQ_POWER <- as.factor(as.character(electricSource$EQ_POWER))
GasFuleSource <- subset(electricalFail, EQ_POWER %in% c(20,21,22,31))
GasFuleSource$EQ_POWER <- as.factor(as.character(GasFuleSource$EQ_POWER))


unintentionalIncident <- main[which(main$CAUSE_IGN == 2), ]
unintentionalIncident <- unintentionalIncident[, !(colnames(unintentionalIncident) %in% "CAUSE_IGN")]   ## getting rid of Cause_IGN column because it does not change.. we already know that this data is caused by failiur of equipment or heat sources 
misuse <- subset(unintentionalIncident, FACT_IGN_1 %in% c(10,11,12,13,14,15,16,17,18,19)) ## subsetting to only include instances of Electrical faliure, malfunction
misuse$FACT_IGN_1 <- as.factor(as.character(misuse$FACT_IGN_1)) ## getting rid of unused factors

GasFuleSource2 <- subset(unintentionalIncident, EQ_POWER %in% c(20,21,22,31))
GasFuleSource2$EQ_POWER <- as.factor(as.character(GasFuleSource2$EQ_POWER))
electricSource2 <- subset(unintentionalIncident, EQ_POWER %in% c(10,11,12))
electricSource2$EQ_POWER<- as.factor(as.character(electricSource2$EQ_POWER))
#############################,20,21,22,31

############################################
## STEP 4 - GRAPHS##########################

## bar plot of the number of incidents per electrical factor that cuased a fire. 
plot(factor(electricalFail1$FACT_IGN_1) , main="Number of Incidents per Factor 2007-2011", col.main="red", 
      xlab = "Electrical Factors Contributing to Incident",  ylab = "Number of Incidents",
      col.lab="blue", cex.lab=1.5
)
legend(6.2,3500,cex=0.8,box.lty=0,legend = c(
                         "30 = Electrical failure",
                         "31 = Water-caused S.C. ",
                         "32 = S.C. arc from mech dmg",
                         "33 = S.C. arc from def insulation",
                         "34 = Unspecified S.C.",
                         "35 = Faulty contact, ",
                         "36 = Arc from operating EQ",
                         "37 = Fluorescent light ballast"
))

plot(factor(main$CAUSE_IGN) , main="Number of Incidents per Cause 2007-2011", col.main="red", 
     xlab = "Factors Contributing to Incident",  ylab = "Number of Incidents",
     col.lab="blue", cex.lab=1.5, by = desc
)
legend(4,75000,cex=0.8,box.lty=0,legend = c(
  "1 = Intentional",
  "2 = Unintentional ",
  "3 = Faliure of EQ or heat source",
  "4 = Act of nature",
  "5 = Cause under invest",
  "U = Cause undetermined after Invest "
))

plot(factor(electricSource$EQ_POWER) , main="Number of Incidents per Factor 2007-2011", col.main="red", 
     xlab = "Electrical Power sources Contributing to Incident started by electronics ",  ylab = "Number of Incidents",
     col.lab="blue", cex.lab=1.5
)
legend(2.5,3500,cex=0.8,box.lty=0,legend = c(
  "10 = Electrical",
  "11 = Elct Voltage>= 50",
  "12 = Elct Voltage < 50"
))

plot(factor(GasFuleSource$EQ_POWER) , main="Number of Incidents per Factor 2007-2011", col.main="red", 
     xlab = "Gas Fule sources Contributing to Incident started by electronics ",  ylab = "Number of Incidents",
     col.lab="blue", cex.lab=1.5
)
legend(2.5,350,cex=0.8,box.lty=0,legend = c(
  "20 = Gas fuel",
  "21 = Natural gas",
  "22 = LP Gas",
  "31 = Gasoline"
))

plot(factor(GasFuleSource2$EQ_POWER) , main="Number of Incidents per Factor 2007-2011", col.main="red", 
     xlab = "Gas Fule sources Contributing to Unintentional Incident",  ylab = "Number of Incidents",
     col.lab="blue", cex.lab=1.5
)
legend(2.5,1000,cex=0.8,box.lty=0,legend = c(
  "20 = Gas fuel",
  "21 = Natural gas",
  "22 = LP Gas",
  "31 = Gasoline"
))

plot(factor(electricSource2$EQ_POWER) , main="Number of Incidents per Factor 2007-2011", col.main="red", 
     xlab = "Electrical Power sources Contributing to Unintentional Incident",  ylab = "Number of Incidents",
     col.lab="blue", cex.lab=1.5
)
legend(2.5,5000,cex=0.8,box.lty=0,legend = c(
  "10 = Electrical",
  "11 = Elct Voltage>= 50",
  "12 = Elct Voltage < 50"
))
###########################################################################################################################################
  
##greating a graph dataset to make graphing with ggplot easier
GraphData <- data.frame(table(electricalFail1$INC_DATE)) ## graph data to show trend of electrical faliure factors from 2007-2015
GraphData1 <- data.frame(table(misuse$INC_DATE))
finalGraph <- merge(GraphData, GraphData1, by = "Var1")
colnames(finalGraph) <- c("Year", "ElectricalFail", "Unintentional")

## plot of Electrical faliures from 2007-2015
ggplot(data = finalGraph, aes(x=Year, y=ElectricalFail, group=1)) + ## assigning x and y vairables , the group =1 just tells ggplot to connect the points
  geom_line(colour="blue", linetype="solid", size=1.5) + ## adding aesthetic attributes to graphed trend line
  geom_point(colour="red", size=4, shape=21, fill="white")+ ## adding a red circle to ea. point representing incidents of a specific year
  ggtitle( "Number of Incidents caused by Electrical Faliures")+ ## adding title for graph
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) ## adding a linear trendline to make it easier to visualize the trend



## plot of Unintentional incidents from 2007-2015
ggplot(data = finalGraph, aes(x=Year, y=Unintentional, group=1)) + 
  geom_line(colour="red", linetype="solid", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  ggtitle( "Number of Incidents caused by Unintentional Actions")+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
  
###################################################################################
## creating a graph dataset for Equipment Power sources to better be able to visualize trends in equipment fires that use x type of power source

GraphData2 <- data.frame(table(electricSource$INC_DATE)) ## graph data to show trend of electrical faliure factors from 2007-2015
GraphData3 <- data.frame(table(electricSource2$INC_DATE))
finalGraph2 <- merge(GraphData2, GraphData3, by = "Var1")
colnames(finalGraph2) <- c("Year", "ElectricalFail", "Unintentional")

ggplot(data = finalGraph2, aes(x=Year, y=ElectricalFail, group=1)) + 
  geom_line(colour="blue", linetype="solid", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  ggtitle( "Number of Incidents caused by Electrical Faliures onset by electric power sources")+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

ggplot(data = finalGraph2, aes(x=Year, y=Unintentional, group=1)) + 
  geom_line(colour="red", linetype="solid", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  ggtitle( "Number of Incidents caused by Unintentional Actions onset by electric power sources")+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))


GraphData4 <- data.frame(table(GasFuleSource$INC_DATE)) ## graph data to show trend of electrical faliure factors from 2007-2015
GraphData5 <- data.frame(table(GasFuleSource2$INC_DATE))
finalGraph3 <-merge(GraphData4, GraphData5, by = "Var1")
colnames(finalGraph3) <- c("Year", "ElectricalFail", "Unintentional")

ggplot(data = finalGraph3, aes(x=Year, y=ElectricalFail, group=1)) + 
  geom_line(colour="blue", linetype="solid", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  ggtitle( "Number of Incidents caused by Electrical Faliure onset by gas power sources")+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))

ggplot(data = finalGraph3, aes(x=Year, y=Unintentional, group=1)) + 
  geom_line(colour="red", linetype="solid", size=1.5) + 
  geom_point(colour="red", size=4, shape=21, fill="white")+
  ggtitle( "Number of Incidents caused by Unintentional Actions onset by gas power sources")+
  geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1))
##############################################################################################################

