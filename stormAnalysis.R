##Install and load packages
packages <- c("RCurl", "googleVis", "stringdist")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}
lapply(packages, library, character.only=TRUE)

##Download Data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "~/stormdata.csv.bz2")

##Load the csv data into R
stormData <- read.csv("~/stormdata.csv.bz2", header = TRUE)

##Impact on population health
##Convert EVTYPE to upper case
stormData$EVTYPE <- toupper(stormData$EVTYPE)
##Calculate health impact by EVTYPE
healthImpactByEventType <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, stormData, FUN = "sum")

healthImpactByEventType <- healthImpactByEventType[healthImpactByEventType$FATALITIES != 0 | healthImpactByEventType$INJURIES != 0, ]
healthImpactByEventType$TOTALHEALTHIMPACT <- with(healthImpactByEventType, FATALITIES + INJURIES)

##Create a bar plot of numeric factors vs EVTYPE to see highest impact
hi_barplot <- gvisColumnChart(data = healthImpactByEventType, 
                           xvar = "EVTYPE", 
                           yvar = c("FATALITIES", "INJURIES", "TOTALHEALTHIMPACT"), 
                           options = list(width = "720px", 
                                          height = "480px", 
                                          hAxis.slantedText = TRUE,
                                          hAxis.slantedTextAngle = 90, 
                                          hAxis.showTextEvery = 4))
plot(hi_barplot)

##We will print EVTYPEs with FATALITIES or INJURIES >=100
greatestHealthImpact <- healthImpactByEventType[with(healthImpactByEventType, FATALITIES >= 1000 | INJURIES >= 1000),]
print(greatestHealthImpact[with(greatestHealthImpact, order(-FATALITIES, -INJURIES)), ])

##Since EVTYPE has many entries for one event type e.g. misspellings
##we will use string matching and hierarchical clustering to
##group similar strings together
eventNames <- healthImpactByEventType$EVTYPE
distMatrix <- stringdistmatrix(eventNames, eventNames, method = "lcs")
eventNameCluster <- hclust(as.dist(distMatrix))
event_df <- data.frame(eventNames, cutree(eventNameCluster, k = 100))
colnames(event_df) <- c("eventNames", "Cluster")

nhi_merged <- merge(healthImpactByEventType, event_df, by.x = "EVTYPE", by.y = "eventNames", all.x = TRUE)
nhi_aggregate <- aggregate(cbind(FATALITIES, INJURIES, TOTALHEALTHIMPACT) ~ Cluster, data = nhi_merged, FUN = "sum")
nhi <- merge(nhi_aggregate, nhi_merged, by.x = "Cluster", by.y = "Cluster", all.x = TRUE)
nhi <- nhi[!duplicated(nhi[, 1]), c("EVTYPE", "FATALITIES.x", "INJURIES.x", "TOTALHEALTHIMPACT.x")]
##Clean up column names before printing
colnames(nhi) <- c("EVTYPE", "FATALITIES", "INJURIES", "TOTALHEALTHIMPACT")

##We will only consider EVTYPES with FATALITIES OR INJURIES >= 1000
nhi <- nhi[with(nhi, FATALITIES >= 1000 | INJURIES >= 1000),]
hi_cluster_barplot <- gvisColumnChart(data = nhi, 
                              xvar = "EVTYPE", 
                              yvar = c("FATALITIES", "INJURIES", "TOTALHEALTHIMPACT"), 
                              options = list(width = "720px", 
                                             height = "480px", 
                                             hAxis.slantedText = TRUE,
                                             hAxis.slantedTextAngle = 90, 
                                             hAxis.showTextEvery = 4))
plot(hi_cluster_barplot)

##We will return the data frame for display
print(nhi[with(nhi, order(-FATALITIES, -INJURIES)), ])

##Impact on property
##First we will remove rows with zero (0) PROPDMG
propDamageData <- stormData[stormData$PROPDMG != 0, c("EVTYPE", "PROPDMG", "PROPDMGEXP")]

##PROPDMG contains value of poperty damage
##PROPDMGEXP contains the multiplier to convert damage value to dollars 
##(K = 1000, m or M = 1,000,000, B = 1,000,000,000)
##We will ignore all other values of PROPDMGEXP per NWSI 10-1605 AUGUST 17, 2007 
##We will create a new column for proper damage value in dollars
propDamageMult <- data.frame(EXP = c("K", "M", "B"), PROPDMGMULT = c(1000, 1000000, 1000000000))
propDamageData <- merge(propDamageData, propDamageMult, by.x = toupper("PROPDMGEXP"), by.y = "EXP", all.x = TRUE)
propDamageData$PROPDMGVAL <- with(propDamageData, PROPDMG*PROPDMGMULT)
propDamageData <- propDamageData[complete.cases(propDamageData),]

##We will aggregate the PROPDMGVAL factor by EVTYPE
##We will omit hierachical clustering for EVTYPE this time around
##Because it did not yield much of a gain the first time around
propDamageAgr <- aggregate(PROPDMGVAL ~ EVTYPE, propDamageData, FUN = "sum")

##We will only consider events that cause damage in Billions
propDamageAgr <- propDamageAgr[propDamageAgr$PROPDMGVAL >= 1000000000, ]

pi_barplot <- gvisColumnChart(data = propDamageAgr, 
                                      xvar = "EVTYPE", 
                                      yvar = c("PROPDMGVAL"), 
                                      options = list(width = "720px", 
                                                     height = "480px", 
                                                     hAxis.slantedText = TRUE,
                                                     hAxis.slantedTextAngle = 90, 
                                                     hAxis.showTextEvery = 4))
plot(pi_barplot)

##We will display the property damage values greater than or equal to 1 Billion
print(propDamageAgr[with(propDamageAgr, order(-PROPDMGVAL)), ])