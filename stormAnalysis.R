##Install and load packages
packages <- c("RCurl", "googleVis")
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
fatalitiesByEventType <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, stormData, FUN = "sum")


nonZeroHealthImpact <- fatalitiesByEventType[fatalitiesByEventType$FATALITIES != 0 | fatalitiesByEventType$INJURIES != 0, ]
nonZeroHealthImpact$TOTALHEALTHIMPACT <- with(nonZeroHealthImpact, FATALITIES + INJURIES)

##Create a bar plot of numeric factors vs EVTYPE to see highest impact
hi_barplot <- gvisColumnChart(data = nonZeroHealthImpact, 
                           xvar = "EVTYPE", 
                           yvar = c("FATALITIES", "INJURIES", "TOTALHEALTHIMPACT"), 
                           options = list(width = "720px", 
                                          height = "480px", 
                                          hAxis.slantedText = TRUE,
                                          hAxis.slantedTextAngle = 90, 
                                          hAxis.showTextEvery = 4))
plot(hi_barplot)

##Since EvTYPE has many entries for one event type e.g. misspellings
##we will use string matching and hierarchical clustering to
##group similar strings together
eventNames <- nonZeroHealthImpact$EVTYPE
distMatrix <- stringdistmatrix(eventNames, eventNames, method = "osa")
eventNameCluster <- hclust(as.dist(distMatrix))
event_df <- data.frame(eventNames, cutree(eventNameCluster, k = 100))
colnames(event_df) <- c("eventNames", "Cluster")

nhi_merged <- merge(nonZeroHealthImpact, event_df, by.x = "EVTYPE", by.y = "eventNames", all.x = TRUE)
nhi_aggregate <- aggregate(cbind(FATALITIES, INJURIES, TOTALHEALTHIMPACT) ~ Cluster, nhi_merged, FUN = "sum")
nhi <- merge(nhi_aggregate, nhi_merged, by.x = "Cluster", by.y = "Cluster", all.x = TRUE)
nhi <- nhi[!duplicated(nhi[, 1]),]

hi_cluster_barplot <- gvisColumnChart(data = nhi, 
                              xvar = "EVTYPE", 
                              yvar = c("FATALITIES.x", "INJURIES.x", "TOTALHEALTHIMPACT.x"), 
                              options = list(width = "720px", 
                                             height = "480px", 
                                             hAxis.slantedText = TRUE,
                                             hAxis.slantedTextAngle = 90, 
                                             hAxis.showTextEvery = 4))
plot(hi_cluster_barplot)


##