library(data.table)
library(knitr)
library(ggplot2)

#Task:
#1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?
#2. Across the United States, which types of events have the greatest economic consequences?



#Check if dataset file is already present, else download it from the course site:
if (!file.exists("stormdata.csv.bz2")) {
    dlurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(dlurl, "stormdata.csv.bz2")
}
#Load the dataset with inline unpacking (Linux only)
if (toupper(Sys.info()["sysname"]) == "LINUX") {ds <- fread("bzcat stormdata.csv.bz2")}
#Load the dataset Windows style
if (toupper(Sys.info()["sysname"]) == "WINDOWS") {ds <- read.csv("stormdata.csv.bz2")}
#NOAAs 48 standard event types
stdevtype <- read.csv("stdevtype.csv")

#We'll reduce the number of columns to use, in order to reduce the memory footprint
usednames <- names(ds)[c(2,8,23:28)]

#This will be our base working dataset
ds1 <- subset(ds,select = usednames)

#Convert BGN_DATE column:
ds1$BGN_DATE <- as.Date(ds1$BGN_DATE[], "%m/%d/%Y %H:%M:%S")

#Remove trailing spaces in EVTYPE:
ds1$EVTYPE <- trimws(ds1$EVTYPE)
#Convert all values to upper-case
ds1$EVTYPE <- toupper(ds1$EVTYPE)

#Check the amount of different values (there are 48 official EVTYPEs)
length(unique(ds1$EVTYPE)) #returns 890 different EVTYPEs ! 
#obviously misspelling and other invalid inputs are highly present

#Some cleaning of various typos and abnormal entries, not extensive
ds1$EVTYPE <- gsub("AVALANCE","AVALANCHE",ds1$EVTYPE)
ds1$EVTYPE <- gsub("GUSTY[[:print:]]","",ds1$EVTYPE)
ds1$EVTYPE <- gsub("WINTER STORM[[:print:]]*|BLOWING SNOW","WINTER STORM",ds1$EVTYPE)
ds1$EVTYPE <- gsub("WINTER WEATHER[[:print:]]*|WINTRY MIX","WINTER WEATHER",ds1$EVTYPE)
ds1$EVTYPE <- gsub("HEAT[[:print:]]*","HEAT",ds1$EVTYPE)
ds1$EVTYPE <- gsub("FLASH FLOOD[[:print:]]*","FLASH FLOOD",ds1$EVTYPE)
ds1$EVTYPE <- gsub("[[:print:]]*FLOOD[[:print:]]*","FLOOD",ds1$EVTYPE)
ds1$EVTYPE <- gsub("HURRICANE[[:print:]]*","HURRICANE",ds1$EVTYPE)
ds1$EVTYPE <- gsub("RIP CURRENT[[:print:]]*","RIP CURRENT",ds1$EVTYPE)
ds1$EVTYPE <- gsub("TSTM","THUNDERSTORM",ds1$EVTYPE)
ds1$EVTYPE <- gsub("THUNDERSTORM[[:print:]]*|THUNDERTORM[[:print:]]*","THUNDERSTORM WIND",ds1$EVTYPE)
ds1$EVTYPE <- gsub("COLD[[:print:]]*","COLD/WIND CHILL",ds1$EVTYPE)
ds1$EVTYPE <- gsub("WILD[[:print:]]*","WILDFIRE",ds1$EVTYPE)
ds1$EVTYPE <- gsub("HIGH SEA[[:print:]]*|HEAVY SURF[[:print:]]*|HEAVY SEA[[:print:]]*|HIGH WA[[:print:]]*|HIGH SW[[:print:]]*","HIGH SURF",ds1$EVTYPE)
ds1$EVTYPE <- gsub("HIGH WI[[:print:]]*|^WINDS$|^WIND$","HIGH WIND",ds1$EVTYPE)
ds1$EVTYPE <- gsub("TROPICAL[[:print:]]*","TROPICAL STORM",ds1$EVTYPE)
ds1$EVTYPE <- gsub("RECORD[[:print:]]","",ds1$EVTYPE)

# Convert CROPDMGEXP and PROPDMGEXP to "real" numbers before aggregating the values!
# reference: http://rpubs.com/flyingdisc/PROPDMGEXP

ds1$CROPDMGEXP[ds1$CROPDMGEXP == ""] <- 0
ds1$CROPDMGEXP <- gsub("[-\\?]", "0", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("\\+", "1", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("[[:digit:]]", "10", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("[Hh]", "100", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("[Kk]", "1000", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("[Mm]", "1000000", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- gsub("[Bb]", "1000000000", ds1$CROPDMGEXP)
ds1$CROPDMGEXP <- as.numeric(ds1$CROPDMGEXP)
#Adding the multipla as a separate column to aggregate on
ds1 <- cbind(ds1,"CROPDMGxEXP" = ds1$CROPDMG*ds1$CROPDMGEXP)

ds1$PROPDMGEXP[ds1$PROPDMGEXP == ""] <- 0
ds1$PROPDMGEXP <- gsub("[-\\?]", "0", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("\\+", "1", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("[[:digit:]]", "10", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("[Hh]", "100", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("[Kk]", "1000", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("[Mm]", "1000000", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- gsub("[Bb]", "1000000000", ds1$PROPDMGEXP)
ds1$PROPDMGEXP <- as.numeric(ds1$PROPDMGEXP)
#Adding the multipla as a separate column to aggregate on
ds1 <- cbind(ds1,"PROPDMGxEXP" = ds1$PROPDMG*ds1$PROPDMGEXP)

#Joining the numbers in separate columns
ds1 <- cbind(ds1,"TOTALHARM" = ds1$FATALITIES + ds1$INJURIES)
ds1 <- cbind(ds1,"TOTALDMG" = ds1$CROPDMGxEXP+ds1$PROPDMGxEXP)


#Aggregating by eventtype
dsharm <- aggregate(ds1$TOTALHARM, list(ds1$EVTYPE), FUN = sum)
dsdmg <- aggregate(ds1$TOTALDMG / 1000000, list(ds1$EVTYPE), FUN = sum)

dsfatal <- aggregate(ds1$FATALITIES, list(ds1$EVTYPE), FUN = sum)
dsinjury <- aggregate(ds1$INJURIES, list(ds1$EVTYPE), FUN = sum)
dscrop <- aggregate(ds1$CROPDMGxEXP, list(ds1$EVTYPE), FUN = sum)
dsprop <- aggregate(ds1$PROPDMGxEXP, list(ds1$EVTYPE), FUN = sum)

barplot(dsfatal[,2],col=heat.colors(5),legend.text = dsfatal[,1],ylab = "Fatalities",xlab = "Event types")


dsharmorder <- dsharm[order(dsharm$x, decreasing = TRUE), ]
topfiveharm <- head(dsharmorder,5)
barplot(topfiveharm[,2],col=heat.colors(5),legend.text = topfiveharm[,1],ylab = "People impacted",xlab = "Event types")

dsdmgorder <- dsdmg[order(dsdmg$x, decreasing = TRUE), ]
topfivedmg <- head(dsdmgorder,5)
barplot(topfivedmg[,2],col=heat.colors(5),legend.text = topfivedmg[,1],ylab = "million $",xlab = "Event types")
