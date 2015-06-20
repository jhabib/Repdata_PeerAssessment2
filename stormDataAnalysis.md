# Health and Property Impact of Storm (Weather) Events
Jawad Habib  
June 19, 2015  

#Synopsis
In this report, we analyze the health and property damage caused by various weather events in the United States. We identify the events, denoted by `EVTYPE` that cause the highest health damage (`FATALITIES` and `INJURIES`) and the highest property damage `PROPDMGVAL`. Total health impact is calculated by simply adding `FATALITIES` and `INJURIES`. Data is analyzed across all US locations for all recoded weather events. 

Our results indicate that Tornados cause the highest health impact at 5633 fatalities and	91346 injuries.

And that Floods cause the highest property damage at USD 144,657,709,800.

#Data Processing
Data is processed in several steps.

First, we load the dependencies required for our data analysis.


```
## Loading required package: bitops
## 
## Welcome to googleVis version 0.5.8
## 
## Please read the Google API Terms of Use
## before you start using the package:
## https://developers.google.com/terms/
## 
## Note, the plot method of googleVis will by default use
## the standard browser to display its output.
## 
## See the googleVis package vignettes for more details,
## or visit http://github.com/mages/googleVis.
## 
## To suppress this message use:
## suppressPackageStartupMessages(library(googleVis))
```

```
## Warning: package 'knitr' was built under R version 3.2.1
```

```
## [[1]]
## [1] "RCurl"     "bitops"    "stats"     "graphics"  "grDevices" "utils"    
## [7] "datasets"  "methods"   "base"     
## 
## [[2]]
##  [1] "googleVis" "RCurl"     "bitops"    "stats"     "graphics" 
##  [6] "grDevices" "utils"     "datasets"  "methods"   "base"     
## 
## [[3]]
##  [1] "stringdist" "googleVis"  "RCurl"      "bitops"     "stats"     
##  [6] "graphics"   "grDevices"  "utils"      "datasets"   "methods"   
## [11] "base"      
## 
## [[4]]
##  [1] "rmarkdown"  "stringdist" "googleVis"  "RCurl"      "bitops"    
##  [6] "stats"      "graphics"   "grDevices"  "utils"      "datasets"  
## [11] "methods"    "base"      
## 
## [[5]]
##  [1] "knitr"      "rmarkdown"  "stringdist" "googleVis"  "RCurl"     
##  [6] "bitops"     "stats"      "graphics"   "grDevices"  "utils"     
## [11] "datasets"   "methods"    "base"
```


```r
##Set options to print googleVis charts
op <- options(gvis.plot.tag='chart')
```

Then we download the data from source URL using `download.file` and load it into R using `read.csv`. The `read.csv` function takes care of extracting data from the bzipped source file.

```r
##Download Data
url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(url, "~/stormdata.csv.bz2")

##Load the csv data into R
stormData <- read.csv("~/stormdata.csv.bz2", header = TRUE)
```

Before proceeding with further analysis, we convert the `EVTYPE` column in our data to uppercase to facilitate string comparisons.

```r
##Convert EVTYPE to upper case
stormData$EVTYPE <- toupper(stormData$EVTYPE)
```

##Calculating Health Impact
Our data includes two columns that identify impact to population health. These columns are `FATALITIES` and `INJURIES`. We aggregate each variable by `EVTYPE` to get the total count of fatalities and injuries. We also add fatalities and injuries to calculate total health impact denoted by `TOTALHEALTHIMPACT`.


```r
##Calculate health impact by EVTYPE
healthImpactByEventType <- aggregate(cbind(FATALITIES, INJURIES) ~ EVTYPE, stormData, FUN = "sum")

healthImpactByEventType <- healthImpactByEventType[healthImpactByEventType$FATALITIES != 0 | healthImpactByEventType$INJURIES != 0, ]
healthImpactByEventType$TOTALHEALTHIMPACT <- with(healthImpactByEventType, FATALITIES + INJURIES)
```


###Cluster Analysis of EVTYPE and Health Impact
A review of the `EVTYPE` variable shows that several event types are repeated, mis-spelled, or otherwise added more than once. We use hierarchical clustering on `EVTYPE` to group together similar strings.

We use the `stringdist::stringdistmatrix` function to calculate the relative distance of strings in `EVTYPE` column. We use `lcs` (longest common string) as the distance function. This generates a distance matrix for each value of `EVTYPE`. We the use the `hclust` and `cutree` functions to create `k = 100` clusters of similar `EVTYPE` values. `k = 100` was chosen after manually experimenting with several values of `k` as it yielded the best results. Hierarchical clustering did not yield significantly different results from the original analysis of health impact. Therefore, we chose to skip hierarchical clustering on `EVTYPE` for property damage analysis.


```r
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
```


##Calculating Property Impact
Property damage is given as a numeric value in the `PROPDMG` variable. Another variable `PROPDMGEXP` is provided to convert the property damage value to a dollar figure. `PROPDMGEXP` consists of the following values:

```
##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M
```

[Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) shows that `K = 1000`, `M = 1,000,000` and `B = 1,000,000,000`. No other character values are assigned a numeric value. We assume small case `m` to have the same numeric value as upper case `M` for our analysis.


```r
##First we will remove rows with zero (0) PROPDMG from stormData
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
```

#Results
This section shows the results of our analysis. First we present the results of health impact by event type and health impact by hierarchically clustered event types. After that, we show the results of propery damage by event type.

##Results: Health Impact by Event Type
The plot below shows the health impact by event types.

```r
hi_barplot <- gvisColumnChart(data = healthImpactByEventType, 
                           xvar = "EVTYPE", 
                           yvar = c("FATALITIES", "INJURIES", "TOTALHEALTHIMPACT"), 
                           options = list(title = "Health Impact by Event Type", 
                                          width = "720px", 
                                          height = "480px", 
                                          hAxis.slantedText = TRUE,
                                          hAxis.slantedTextAngle = 90, 
                                          hAxis.showTextEvery = 1))
plot(hi_barplot)
```

<!-- ColumnChart generated in R 3.2.0 by googleVis 0.5.8 package -->
<!-- Fri Jun 19 23:12:55 2015 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataColumnChartID14d42f30506a () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "AVALANCE",
1,
0,
1 
],
[
 "AVALANCHE",
224,
170,
394 
],
[
 "BLACK ICE",
1,
24,
25 
],
[
 "BLIZZARD",
101,
805,
906 
],
[
 "BLOWING SNOW",
2,
14,
16 
],
[
 "BRUSH FIRE",
0,
2,
2 
],
[
 "COASTAL FLOOD",
3,
2,
5 
],
[
 "COASTAL FLOODING",
3,
0,
3 
],
[
 "COASTAL FLOODING/EROSION",
0,
5,
5 
],
[
 "COASTAL STORM",
3,
2,
5 
],
[
 "COASTALSTORM",
1,
0,
1 
],
[
 "COLD",
38,
48,
86 
],
[
 "COLD AND SNOW",
14,
0,
14 
],
[
 "COLD TEMPERATURE",
2,
0,
2 
],
[
 "COLD WAVE",
3,
0,
3 
],
[
 "COLD WEATHER",
5,
0,
5 
],
[
 "COLD/WIND CHILL",
95,
12,
107 
],
[
 "COLD/WINDS",
1,
0,
1 
],
[
 "DENSE FOG",
18,
342,
360 
],
[
 "DROUGHT",
0,
4,
4 
],
[
 "DROUGHT/EXCESSIVE HEAT",
2,
0,
2 
],
[
 "DROWNING",
1,
0,
1 
],
[
 "DRY MICROBURST",
3,
28,
31 
],
[
 "DRY MIRCOBURST WINDS",
0,
1,
1 
],
[
 "DUST DEVIL",
2,
43,
45 
],
[
 "DUST STORM",
22,
440,
462 
],
[
 "EXCESSIVE HEAT",
1903,
6525,
8428 
],
[
 "EXCESSIVE RAINFALL",
2,
21,
23 
],
[
 "EXCESSIVE SNOW",
0,
2,
2 
],
[
 "EXTENDED COLD",
1,
0,
1 
],
[
 "EXTREME COLD",
162,
231,
393 
],
[
 "EXTREME COLD/WIND CHILL",
125,
24,
149 
],
[
 "EXTREME HEAT",
96,
155,
251 
],
[
 "EXTREME WINDCHILL",
17,
5,
22 
],
[
 "FALLING SNOW/ICE",
1,
1,
2 
],
[
 "FLASH FLOOD",
978,
1777,
2755 
],
[
 "FLASH FLOOD/FLOOD",
14,
0,
14 
],
[
 "FLASH FLOODING",
19,
8,
27 
],
[
 "FLASH FLOODING/FLOOD",
5,
0,
5 
],
[
 "FLASH FLOODS",
2,
0,
2 
],
[
 "FLOOD",
470,
6789,
7259 
],
[
 "FLOOD & HEAVY RAIN",
1,
0,
1 
],
[
 "FLOOD/FLASH FLOOD",
17,
15,
32 
],
[
 "FLOOD/RIVER FLOOD",
1,
0,
1 
],
[
 "FLOODING",
6,
2,
8 
],
[
 "FOG",
62,
734,
796 
],
[
 "FOG AND COLD TEMPERATURES",
1,
1,
2 
],
[
 "FREEZE",
1,
0,
1 
],
[
 "FREEZING DRIZZLE",
2,
15,
17 
],
[
 "FREEZING RAIN",
7,
23,
30 
],
[
 "FREEZING RAIN/SNOW",
1,
0,
1 
],
[
 "FREEZING SPRAY",
1,
0,
1 
],
[
 "FROST",
1,
3,
4 
],
[
 "FUNNEL CLOUD",
0,
3,
3 
],
[
 "GLAZE",
7,
216,
223 
],
[
 "GLAZE/ICE STORM",
0,
15,
15 
],
[
 "GUSTY WIND",
1,
1,
2 
],
[
 "GUSTY WINDS",
4,
11,
15 
],
[
 "HAIL",
15,
1361,
1376 
],
[
 "HAZARDOUS SURF",
0,
1,
1 
],
[
 "HEAT",
937,
2100,
3037 
],
[
 "HEAT WAVE",
172,
379,
551 
],
[
 "HEAT WAVE DROUGHT",
4,
15,
19 
],
[
 "HEAT WAVES",
5,
0,
5 
],
[
 "HEAVY RAIN",
98,
251,
349 
],
[
 "HEAVY RAINS",
0,
4,
4 
],
[
 "HEAVY SEAS",
3,
0,
3 
],
[
 "HEAVY SNOW",
127,
1021,
1148 
],
[
 "HEAVY SNOW AND HIGH WINDS",
2,
0,
2 
],
[
 "HEAVY SNOW SHOWER",
0,
2,
2 
],
[
 "HEAVY SNOW/BLIZZARD/AVALANCHE",
0,
1,
1 
],
[
 "HEAVY SNOW/ICE",
0,
10,
10 
],
[
 "HEAVY SURF",
8,
40,
48 
],
[
 "HEAVY SURF AND WIND",
3,
0,
3 
],
[
 "HEAVY SURF/HIGH SURF",
42,
48,
90 
],
[
 "HIGH",
0,
1,
1 
],
[
 "HIGH SEAS",
5,
8,
13 
],
[
 "HIGH SURF",
104,
156,
260 
],
[
 "HIGH SWELLS",
1,
0,
1 
],
[
 "HIGH WATER",
3,
0,
3 
],
[
 "HIGH WAVES",
1,
0,
1 
],
[
 "HIGH WIND",
248,
1137,
1385 
],
[
 "HIGH WIND 48",
0,
1,
1 
],
[
 "HIGH WIND AND SEAS",
3,
20,
23 
],
[
 "HIGH WIND/HEAVY SNOW",
0,
1,
1 
],
[
 "HIGH WIND/SEAS",
4,
0,
4 
],
[
 "HIGH WINDS",
35,
302,
337 
],
[
 "HIGH WINDS/COLD",
0,
4,
4 
],
[
 "HIGH WINDS/SNOW",
3,
6,
9 
],
[
 "HURRICANE",
61,
46,
107 
],
[
 "HURRICANE-GENERATED SWELLS",
0,
2,
2 
],
[
 "HURRICANE EDOUARD",
0,
2,
2 
],
[
 "HURRICANE EMILY",
0,
1,
1 
],
[
 "HURRICANE ERIN",
6,
1,
7 
],
[
 "HURRICANE FELIX",
1,
0,
1 
],
[
 "HURRICANE OPAL",
1,
1,
2 
],
[
 "HURRICANE OPAL/HIGH WINDS",
2,
0,
2 
],
[
 "HURRICANE/TYPHOON",
64,
1275,
1339 
],
[
 "HYPERTHERMIA/EXPOSURE",
1,
0,
1 
],
[
 "HYPOTHERMIA",
1,
0,
1 
],
[
 "HYPOTHERMIA/EXPOSURE",
7,
0,
7 
],
[
 "ICE",
6,
137,
143 
],
[
 "ICE ON ROAD",
1,
0,
1 
],
[
 "ICE ROADS",
0,
1,
1 
],
[
 "ICE STORM",
89,
1975,
2064 
],
[
 "ICE STORM/FLASH FLOOD",
0,
2,
2 
],
[
 "ICY ROADS",
5,
31,
36 
],
[
 "LANDSLIDE",
38,
52,
90 
],
[
 "LANDSLIDES",
1,
1,
2 
],
[
 "LIGHT SNOW",
1,
2,
3 
],
[
 "LIGHTNING",
816,
5230,
6046 
],
[
 "LIGHTNING AND THUNDERSTORM WIN",
0,
1,
1 
],
[
 "LIGHTNING INJURY",
0,
1,
1 
],
[
 "LIGHTNING.",
1,
0,
1 
],
[
 "LOW TEMPERATURE",
7,
0,
7 
],
[
 "MARINE ACCIDENT",
1,
2,
3 
],
[
 "MARINE HIGH WIND",
1,
1,
2 
],
[
 "MARINE MISHAP",
7,
5,
12 
],
[
 "MARINE STRONG WIND",
14,
22,
36 
],
[
 "MARINE THUNDERSTORM WIND",
10,
26,
36 
],
[
 "MARINE TSTM WIND",
9,
8,
17 
],
[
 "MINOR FLOODING",
1,
0,
1 
],
[
 "MIXED PRECIP",
2,
26,
28 
],
[
 "MUDSLIDE",
4,
2,
6 
],
[
 "MUDSLIDES",
1,
0,
1 
],
[
 "NON-SEVERE WIND DAMAGE",
0,
7,
7 
],
[
 "NON TSTM WIND",
0,
1,
1 
],
[
 "OTHER",
0,
4,
4 
],
[
 "RAIN/SNOW",
4,
2,
6 
],
[
 "RAIN/WIND",
1,
0,
1 
],
[
 "RAPIDLY RISING WATER",
1,
0,
1 
],
[
 "RECORD COLD",
1,
0,
1 
],
[
 "RECORD HEAT",
2,
50,
52 
],
[
 "RECORD/EXCESSIVE HEAT",
17,
0,
17 
],
[
 "RIP CURRENT",
368,
232,
600 
],
[
 "RIP CURRENTS",
204,
297,
501 
],
[
 "RIP CURRENTS/HEAVY SURF",
5,
0,
5 
],
[
 "RIVER FLOOD",
2,
2,
4 
],
[
 "RIVER FLOODING",
2,
1,
3 
],
[
 "ROGUE WAVE",
0,
2,
2 
],
[
 "ROUGH SEAS",
8,
5,
13 
],
[
 "ROUGH SURF",
4,
1,
5 
],
[
 "SLEET",
2,
0,
2 
],
[
 "SMALL HAIL",
0,
10,
10 
],
[
 "SNOW",
5,
31,
36 
],
[
 "SNOW AND ICE",
4,
1,
5 
],
[
 "SNOW SQUALL",
2,
35,
37 
],
[
 "SNOW SQUALLS",
1,
0,
1 
],
[
 "SNOW/ BITTER COLD",
1,
0,
1 
],
[
 "SNOW/HIGH WINDS",
0,
36,
36 
],
[
 "STORM SURGE",
13,
38,
51 
],
[
 "STORM SURGE/TIDE",
11,
5,
16 
],
[
 "STRONG WIND",
103,
280,
383 
],
[
 "STRONG WINDS",
8,
21,
29 
],
[
 "THUNDERSNOW",
1,
1,
2 
],
[
 "THUNDERSTORM",
1,
12,
13 
],
[
 "THUNDERSTORM  WINDS",
0,
10,
10 
],
[
 "THUNDERSTORM WIND",
133,
1488,
1621 
],
[
 "THUNDERSTORM WIND (G40)",
1,
0,
1 
],
[
 "THUNDERSTORM WIND G52",
1,
0,
1 
],
[
 "THUNDERSTORM WINDS",
64,
908,
972 
],
[
 "THUNDERSTORM WINDS 13",
0,
1,
1 
],
[
 "THUNDERSTORM WINDS/HAIL",
0,
1,
1 
],
[
 "THUNDERSTORM WINDSS",
0,
4,
4 
],
[
 "THUNDERSTORMS WINDS",
0,
1,
1 
],
[
 "THUNDERSTORMW",
0,
27,
27 
],
[
 "THUNDERTORM WINDS",
1,
0,
1 
],
[
 "TIDAL FLOODING",
0,
1,
1 
],
[
 "TORNADO",
5633,
91346,
96979 
],
[
 "TORNADO F2",
0,
16,
16 
],
[
 "TORNADO F3",
0,
2,
2 
],
[
 "TORNADOES, TSTM WIND, HAIL",
25,
0,
25 
],
[
 "TORRENTIAL RAINFALL",
0,
4,
4 
],
[
 "TROPICAL STORM",
58,
340,
398 
],
[
 "TROPICAL STORM GORDON",
8,
43,
51 
],
[
 "TSTM WIND",
504,
6957,
7461 
],
[
 "TSTM WIND (G35)",
1,
0,
1 
],
[
 "TSTM WIND (G40)",
0,
1,
1 
],
[
 "TSTM WIND (G45)",
0,
3,
3 
],
[
 "TSTM WIND/HAIL",
5,
95,
100 
],
[
 "TSUNAMI",
33,
129,
162 
],
[
 "TYPHOON",
0,
5,
5 
],
[
 "UNSEASONABLY COLD",
2,
0,
2 
],
[
 "UNSEASONABLY WARM",
11,
17,
28 
],
[
 "UNSEASONABLY WARM AND DRY",
29,
0,
29 
],
[
 "URBAN AND SMALL STREAM FLOODIN",
1,
0,
1 
],
[
 "URBAN/SML STREAM FLD",
28,
79,
107 
],
[
 "WARM WEATHER",
0,
2,
2 
],
[
 "WATERSPOUT",
3,
29,
32 
],
[
 "WATERSPOUT TORNADO",
0,
1,
1 
],
[
 "WATERSPOUT/TORNADO",
3,
42,
45 
],
[
 "WHIRLWIND",
1,
0,
1 
],
[
 "WILD FIRES",
3,
150,
153 
],
[
 "WILD/FOREST FIRE",
12,
545,
557 
],
[
 "WILDFIRE",
75,
911,
986 
],
[
 "WIND",
23,
86,
109 
],
[
 "WIND STORM",
1,
0,
1 
],
[
 "WINDS",
1,
1,
2 
],
[
 "WINTER STORM",
206,
1321,
1527 
],
[
 "WINTER STORM HIGH WINDS",
1,
15,
16 
],
[
 "WINTER STORMS",
10,
17,
27 
],
[
 "WINTER WEATHER",
33,
398,
431 
],
[
 "WINTER WEATHER MIX",
0,
68,
68 
],
[
 "WINTER WEATHER/MIX",
28,
72,
100 
],
[
 "WINTRY MIX",
1,
77,
78 
] 
];
data.addColumn('string','EVTYPE');
data.addColumn('number','FATALITIES');
data.addColumn('number','INJURIES');
data.addColumn('number','TOTALHEALTHIMPACT');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartColumnChartID14d42f30506a() {
var data = gvisDataColumnChartID14d42f30506a();
var options = {};
options["allowHtml"] = true;
options["title"] = "Health Impact by Event Type";
options["width"] = "720px";
options["height"] = "480px";
options["hAxis.slantedText"] = true;
options["hAxis.slantedTextAngle"] =     90;
options["hAxis.showTextEvery"] =      1;

    var chart = new google.visualization.ColumnChart(
    document.getElementById('ColumnChartID14d42f30506a')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartColumnChartID14d42f30506a);
})();
function displayChartColumnChartID14d42f30506a() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartColumnChartID14d42f30506a"></script>
 
<!-- divChart -->
  
<div id="ColumnChartID14d42f30506a" 
  style="width: 720px; height: 480px;">
</div>

The data table below shows event types where either `FATALITIES >= 1000` or `INJURIES >= 1000`.

```r
##We will use EVTYPEs with FATALITIES or INJURIES >=1000
greatestHealthImpact <- healthImpactByEventType[with(healthImpactByEventType, FATALITIES >= 1000 | INJURIES >= 1000),]
kable(greatestHealthImpact[with(greatestHealthImpact, order(-FATALITIES, -INJURIES)), ])
```

      EVTYPE               FATALITIES   INJURIES   TOTALHEALTHIMPACT
----  ------------------  -----------  ---------  ------------------
758   TORNADO                    5633      91346               96979
116   EXCESSIVE HEAT             1903       6525                8428
138   FLASH FLOOD                 978       1777                2755
243   HEAT                        937       2100                3037
418   LIGHTNING                   816       5230                6046
779   TSTM WIND                   504       6957                7461
154   FLOOD                       470       6789                7259
320   HIGH WIND                   248       1137                1385
888   WINTER STORM                206       1321                1527
685   THUNDERSTORM WIND           133       1488                1621
274   HEAVY SNOW                  127       1021                1148
387   ICE STORM                    89       1975                2064
372   HURRICANE/TYPHOON            64       1275                1339
212   HAIL                         15       1361                1376

###Results: Health Impact by Hierarchically Clustered Event Types
The plot below shows the health impact by hierarchically clustered event types. As it can be observed, the results are not significantly different from the ones obtainned without hierarchical clustering.

```r
##We will only consider EVTYPES with FATALITIES or INJURIES >= 1000
nhi <- nhi[with(nhi, FATALITIES >= 1000 | INJURIES >= 1000),]
hi_cluster_barplot <- gvisColumnChart(data = nhi, 
                              xvar = "EVTYPE", 
                              yvar = c("FATALITIES", "INJURIES", "TOTALHEALTHIMPACT"), 
                              options = list(title = "Health Impact by Clustered Event Type", 
                                             width = "720px", 
                                             height = "480px", 
                                             hAxis.slantedText = TRUE,
                                             hAxis.slantedTextAngle = 90, 
                                             hAxis.showTextEvery = 1))
plot(hi_cluster_barplot)
```

<!-- ColumnChart generated in R 3.2.0 by googleVis 0.5.8 package -->
<!-- Fri Jun 19 23:12:55 2015 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataColumnChartID14d414c77b12 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "DROUGHT/EXCESSIVE HEAT",
1922,
6525,
8447 
],
[
 "FLOOD/FLASH FLOOD",
1016,
1800,
2816 
],
[
 "TIDAL FLOODING",
477,
6792,
7269 
],
[
 "HEAT",
991,
3727,
4718 
],
[
 "HEAVY SNOW SHOWER",
127,
1033,
1160 
],
[
 "HIGH WIND 48",
286,
1450,
1736 
],
[
 "HURRICANE/TYPHOON",
64,
1275,
1339 
],
[
 "ICE STORM",
306,
3313,
3619 
],
[
 "LIGHTNING",
817,
5231,
6048 
],
[
 "NON TSTM WIND",
509,
7053,
7562 
],
[
 "THUNDERSTORM WIND G52",
200,
2412,
2612 
],
[
 "TORNADO",
5633,
91364,
96997 
],
[
 "WILD/FOREST FIRE",
90,
1606,
1696 
] 
];
data.addColumn('string','EVTYPE');
data.addColumn('number','FATALITIES');
data.addColumn('number','INJURIES');
data.addColumn('number','TOTALHEALTHIMPACT');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartColumnChartID14d414c77b12() {
var data = gvisDataColumnChartID14d414c77b12();
var options = {};
options["allowHtml"] = true;
options["title"] = "Health Impact by Clustered Event Type";
options["width"] = "720px";
options["height"] = "480px";
options["hAxis.slantedText"] = true;
options["hAxis.slantedTextAngle"] =     90;
options["hAxis.showTextEvery"] =      1;

    var chart = new google.visualization.ColumnChart(
    document.getElementById('ColumnChartID14d414c77b12')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartColumnChartID14d414c77b12);
})();
function displayChartColumnChartID14d414c77b12() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartColumnChartID14d414c77b12"></script>
 
<!-- divChart -->
  
<div id="ColumnChartID14d414c77b12" 
  style="width: 720px; height: 480px;">
</div>

The data table below shows event types where either `FATALITIES >= 1000` or `INJURIES >= 1000`.

```r
kable(nhi[with(nhi, order(-FATALITIES, -INJURIES)), ])
```

      EVTYPE                    FATALITIES   INJURIES   TOTALHEALTHIMPACT
----  -----------------------  -----------  ---------  ------------------
178   TORNADO                         5633      91364               96997
33    DROUGHT/EXCESSIVE HEAT          1922       6525                8447
47    FLOOD/FLASH FLOOD               1016       1800                2816
75    HEAT                             991       3727                4718
134   LIGHTNING                        817       5231                6048
146   NON TSTM WIND                    509       7053                7562
53    TIDAL FLOODING                   477       6792                7269
125   ICE STORM                        306       3313                3619
101   HIGH WIND 48                     286       1450                1736
168   THUNDERSTORM WIND G52            200       2412                2612
88    HEAVY SNOW SHOWER                127       1033                1160
198   WILD/FOREST FIRE                  90       1606                1696
117   HURRICANE/TYPHOON                 64       1275                1339

##Results: Property Damage by Event Types
We ignore hierarchical cluster of `EVTYPE` for property damage. The plot below shows the property damage from event types.

```r
##Create a column plot of PROPDMGVAL vs EVTYPE 
pi_barplot <- gvisColumnChart(data = propDamageAgr, 
                                      xvar = "EVTYPE", 
                                      yvar = c("PROPDMGVAL"), 
                                      options = list(title = "Property Impact by Event Type", 
                                                     width = "720px", 
                                                     height = "480px", 
                                                     hAxis.slantedText = TRUE,
                                                     hAxis.slantedTextAngle = 90, 
                                                     hAxis.showTextEvery = 4))
plot(pi_barplot)
```

<!-- ColumnChart generated in R 3.2.0 by googleVis 0.5.8 package -->
<!-- Fri Jun 19 23:12:55 2015 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataColumnChartID14d45c923e57 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "DROUGHT",
1046106000 
],
[
 "FLASH FLOOD",
16140811510 
],
[
 "FLOOD",
144657709800 
],
[
 "HAIL",
15727366720 
],
[
 "HEAVY RAIN/SEVERE WEATHER",
2.5e+09 
],
[
 "HIGH WIND",
5270046260 
],
[
 "HURRICANE",
11868319010 
],
[
 "HURRICANE OPAL",
3152846000 
],
[
 "HURRICANE/TYPHOON",
69305840000 
],
[
 "ICE STORM",
3944927810 
],
[
 "RIVER FLOOD",
5118945500 
],
[
 "SEVERE THUNDERSTORM",
1205360000 
],
[
 "STORM SURGE",
43323536000 
],
[
 "STORM SURGE/TIDE",
4641188000 
],
[
 "THUNDERSTORM WIND",
3483121140 
],
[
 "THUNDERSTORM WINDS",
1733452850 
],
[
 "TORNADO",
56925660480 
],
[
 "TORNADOES, TSTM WIND, HAIL",
1.6e+09 
],
[
 "TROPICAL STORM",
7703890550 
],
[
 "TSTM WIND",
4484958440 
],
[
 "WILD/FOREST FIRE",
3001829500 
],
[
 "WILDFIRE",
4765114000 
],
[
 "WINTER STORM",
6688497250 
] 
];
data.addColumn('string','EVTYPE');
data.addColumn('number','PROPDMGVAL');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartColumnChartID14d45c923e57() {
var data = gvisDataColumnChartID14d45c923e57();
var options = {};
options["allowHtml"] = true;
options["title"] = "Property Impact by Event Type";
options["width"] = "720px";
options["height"] = "480px";
options["hAxis.slantedText"] = true;
options["hAxis.slantedTextAngle"] =     90;
options["hAxis.showTextEvery"] =      4;

    var chart = new google.visualization.ColumnChart(
    document.getElementById('ColumnChartID14d45c923e57')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartColumnChartID14d45c923e57);
})();
function displayChartColumnChartID14d45c923e57() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartColumnChartID14d45c923e57"></script>
 
<!-- divChart -->
  
<div id="ColumnChartID14d45c923e57" 
  style="width: 720px; height: 480px;">
</div>


The table below shows the `EVTYPE` values with the highest property damage. We only consider `EVTYPE` values where `PROPDMGVAL >= 1000000000`.


```r
kable(propDamageAgr[with(propDamageAgr, order(-PROPDMGVAL)), ])
```

      EVTYPE                          PROPDMGVAL
----  ---------------------------  -------------
58    FLOOD                         144657709800
164   HURRICANE/TYPHOON              69305840000
303   TORNADO                        56925660480
255   STORM SURGE                    43323536000
46    FLASH FLOOD                    16140811510
91    HAIL                           15727366720
156   HURRICANE                      11868319010
311   TROPICAL STORM                  7703890550
364   WINTER STORM                    6688497250
141   HIGH WIND                       5270046260
222   RIVER FLOOD                     5118945500
356   WILDFIRE                        4765114000
256   STORM SURGE/TIDE                4641188000
316   TSTM WIND                       4484958440
172   ICE STORM                       3944927810
267   THUNDERSTORM WIND               3483121140
162   HURRICANE OPAL                  3152846000
354   WILD/FOREST FIRE                3001829500
112   HEAVY RAIN/SEVERE WEATHER       2500000000
279   THUNDERSTORM WINDS              1733452850
308   TORNADOES, TSTM WIND, HAIL      1600000000
228   SEVERE THUNDERSTORM             1205360000
31    DROUGHT                         1046106000

This concludes our analysis of Health and Property impacts of weather events captured in Storm Data.

```r
##Set options back to original options
options(op)
```
