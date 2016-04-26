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
#Load the dataset with inline unpacking 
ds <- fread("bzcat stormdata.csv.bz2")
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

#Subset to only those events that have more than 0 fatalities, injury, crop- or property damage
dsfatal <- subset(ds1, ds1$FATALITIES >  0, select = usednames[c(1:3)])
dsinjury <- subset(ds1, ds$INJURIES >  0, select = usednames[c(1:2,4)])
dscrop <- subset(ds1, ds$CROPDMG >  0, select = usednames[c(1:2,7:8)])
dsprop <- subset(ds1, ds$PROPDMG >  0, select = usednames[c(1:2,5:6)])

# TODO: clean up spelling an formating errors in EVTYPE
#Check the amount of different values (there are 48 official EVTYPEs)
length(unique(ds1$EVTYPE)) #returns 890 different EVTYPEs ! 
#obviously misspelling and other invalid inputs are highly present

#In order to reduce the number of cleaning up we will concentrate on the 4 subset
length(unique(dsfatal$EVTYPE))
length(unique(dsinjury$EVTYPE))
length(unique(dscrop$EVTYPE))
length(unique(dsprop$EVTYPE))




# Convert CROPDMGEXP and PROPDMGEXP to "real" numbers before aggregating the values!
# reference: http://rpubs.com/flyingdisc/PROPDMGEXP

dscrop$CROPDMGEXP[dscrop$CROPDMGEXP == ""] <- 0
dscrop$CROPDMGEXP <- gsub("[-\\?]", "0", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("\\+", "1", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("[[:digit:]]", "10", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("[Hh]", "100", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("[Kk]", "1000", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("[Mm]", "1000000", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- gsub("[Bb]", "1000000000", dscrop$CROPDMGEXP)
dscrop$CROPDMGEXP <- as.numeric(dscrop$CROPDMGEXP)
dscrop <- cbind(dscrop,"DMGxEXP" = dscrop$CROPDMG*dscrop$CROPDMGEXP)

dsprop$PROPDMGEXP[dsprop$PROPDMGEXP == ""] <- 0
dsprop$PROPDMGEXP <- gsub("[-\\?]", "0", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("\\+", "1", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("[[:digit:]]", "10", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("[Hh]", "100", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("[Kk]", "1000", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("[Mm]", "1000000", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- gsub("[Bb]", "1000000000", dsprop$PROPDMGEXP)
dsprop$PROPDMGEXP <- as.numeric(dsprop$PROPDMGEXP)
#Adding the multipla as a separate column to aggregate on
dsprop <- cbind(dsprop,"DMGxEXP" = dsprop$PROPDMG*dsprop$PROPDMGEXP)

dsf <- aggregate(dsfatal$FATALITIES,list(dsfatal$EVTYPE), FUN = sum)
dsi <- aggregate(dsinjury$INJURIES, list(dsinjury$EVTYPE), FUN = sum)
dsc <- aggregate(dscrop$V2, list(toupper(dscrop$EVTYPE)), FUN = sum)
dsp <- aggregate(dsprop$V2, list(toupper(dsprop$EVTYPE)), FUN = sum)

dsfatal$EVTYPE <- gsub("AVALANCE","AVALANCHE",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("BLOWING SNOW","WINTER STORM",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("WINTER STORM[[:print:]]*","WINTER STORM",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("WINTER WEATHER[[:print:]]*","WINTER WEATHER",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("HEAT[[:print:]]*","HEAT",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("FLASH FLOOD[[:print:]]*","FLASH FLOOD",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("FLOOD[[:print:]]*","FLOOD",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("HURRICANE[[:print:]]*","HURRICANE",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("RIP CURRENT[[:print:]]*","RIP CURRENT",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("TSTM","THUNDERSTORM",dsfatal$EVTYPE)
dsfatal$EVTYPE <- gsub("THUNDERSTORM","THUNDERSTORM WIND",dsfatal$EVTYPE)
