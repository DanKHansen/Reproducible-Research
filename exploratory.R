library(data.table)
library(knitr)
library(ggplot2)

#Check if file is already present, else download it from the course site:
if (!file.exists("stormdata.csv.bz2")) {
    dlurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
    download.file(dlurl, "stormdata.csv.bz2")
}
#Load the dataset with inline unpacking 
ds <- fread("bzcat stormdata.csv.bz2")


#Convert BGN_DATE column:
ds$BGN_DATE <- as.Date(ds$BGN_DATE[], "%m/%d/%Y %H:%M:%S")

#Subset to only those events that have more than 0 fatalities, injury, crop- or property damage
dsfatal <- subset(ds, ds$FATALITIES >  0)
dsinjury <- subset(ds, ds$INJURIES >  0)
dscrop <- subset(ds, ds$CROPDMG >  0)
dsprop <- subset(ds, ds$PROPDMG >  0)

#1. Across the United States, which types of events (as indicated in the EVTYPE variable) 
#are most harmful with respect to population health?
mostfatal <- subset(ds,ds$FATALITIES == max(ds$FATALITIES))
mostinjury <- subset(ds,ds$INJURIES == max(ds$INJURIES))

ds2 <- cbind(ds,ds$FATALITIES+ds$INJURIES)
mostpeople <- subset(ds2,ds2$V2 == max(ds2$V2))

#2. Across the United States, which types of events have the greatest economic consequences?

prop <- subset(ds,ds$PROPDMGEXP == "B")
maxprop <- subset(prop, prop$PROPDMG == max(prop$PROPDMG))

crop <- subset(ds,ds$CROPDMGEXP == "B")
maxcrop <- subset(crop, crop$CROPDMG == max(crop$CROPDMG))

