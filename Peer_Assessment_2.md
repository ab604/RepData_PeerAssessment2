# Reproducible Research: Peer Assessment 2
##Exploration of the NOAA Storm database

###Synopsis
In this report I aim to describe the storm events in the USA that are most 
harmful in terms of death and injuries caused, and in terms of economic impact to 
property and crops. The data covers a period from 1950 to 2011. 

I found in terms of harm to humans, by far the biggest cause of harm in terms of 
deaths and injuries are Tornado events. Tornadoes account for more than 60% of 
the total harm done. Excessive heat is second in terms of death only, accounting 
for over 10% of deaths.

- The total number of deaths in the period 1950 to 2011 was 15,145. 
- The total number of injuries in the period 1950 to 2011 was 140,528. 
- The total number of cases of harm in the period 1950 to 2011 was 155,673.

In terms of economic costs, Flooding is the biggest cause of damage costs to 
property and agriculture, accounting for over 30% of the total costs. Hurricanes
and Tornadoes each cause over 10% of the total damage costs.

- The total economic costs caused by storm events during this period was 
approximately 476 billion US Dollars.

An interesting extension to this analysis would be to examine any changes in 
harm and economic costs with respect to time to assist with planning how best to
allocate resources.

###Global Rmarkdown settings
The global settings for the Rmarkdown document used to generate this report are
to print the R code, cache the data, and set the figure width and height, and
suppress messages: 

```r
library(knitr)
opts_chunk$set(echo=TRUE,cache=TRUE,fig.width=12, fig.height=8, message=FALSE, 
               warning=FALSE)
```

###Loading and processing the raw data
A bzipped dataset was provided on the course website: 
[Storm Data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2) [47 Mb], along with links to 
[Storm Data Documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf) and a 
[FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf) document.

The data is downloaded and loaded as follows:

```r
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"

# Download and uzip data if not already present        
if(!file.exists("stormData.csv.bz2")){
        download.file(fileUrl,destfile="StormData.csv.bz2")
        dateDownloaded <- date()
        log_con <- file("storm_download.log")
        cat (fileUrl,"\n","destfile= StormData.csv.bz2",
             "\n","destdir =", getwd(),"\n",dateDownloaded, 
             file = log_con)
        close(log_con)
        gc(verbose = FALSE) # release memory
}
```

On my system in takes about 42 seconds to read in data from the csv file:

```r
# If not already loaded, load the data
if(!exists("storm.raw")){
storm.raw <- read.csv(bzfile("StormData.csv.bz2"),
                      stringsAsFactors = FALSE, na.strings="NA",header=TRUE)
}
```



```r
# Get dimensions
dim.raw <- dim(storm.raw)
```
Checking the dimensions of the dataset indicates that there are 
902297 observations of 37 variables.

##Results

###1. Which events are most harmful to human health?
To answer this question we are interested in the variables `EVTYPE`, 
`FATALITIES` and `INJURIES`, representing the Storm Event type, number of deaths
caused and number of injuries caused respectively.

These variables are in columns 8,23 and 24, as found by:

```r
which(colnames(storm.raw)=="EVTYPE")
```

```
## [1] 8
```

```r
which(colnames(storm.raw)=="FATALITIES")
```

```
## [1] 23
```

```r
which(colnames(storm.raw)=="INJURIES")
```

```
## [1] 24
```

Subsetting and summarising these these columns only indicates no missing values.

```r
health.dat <- storm.raw[,c(8,23,24)]
# Summarise the data, no NAs
summary(health.dat[,2:3])
```

```
##    FATALITIES          INJURIES        
##  Min.   :  0.0000   Min.   :   0.0000  
##  1st Qu.:  0.0000   1st Qu.:   0.0000  
##  Median :  0.0000   Median :   0.0000  
##  Mean   :  0.0168   Mean   :   0.1557  
##  3rd Qu.:  0.0000   3rd Qu.:   0.0000  
##  Max.   :583.0000   Max.   :1700.0000
```

I then did the following using the `dpylr` package:

1. Grouped by `EVTYPE`
2. Summed the total deaths and injuries for each event type.
3. Added the total deaths and injuries for each event type together to calculate
the total harm done by each event type.
4. Arranged the events in descending order of the total harm.


```r
suppressMessages(library(dplyr)) # supress loading message
# Group by event
by_event <- health.dat %>% group_by(EVTYPE) %>%
        summarise_each(funs(sum)) %>% # Sum each event
        mutate(DEATH.AND.INJURY = FATALITIES+INJURIES) %>% # Add death or injury
        # Select only events where death or injury occurs
        filter(DEATH.AND.INJURY > 0) %>%
        arrange(desc(DEATH.AND.INJURY))  # Arrange by death or injury descending
# order

# Which are the most impactful events.
max.death <- by_event[which.max(by_event$FATALITIES),]
max.injuries <- by_event[which.max(by_event$INJURIES),]
max.harm <- by_event[which.max(by_event$DEATH.AND.INJURY),]
```

This reveals that the storm events causing the most harm overall during this period
1950 to 2011 was by TORNADO,
causing 5633 deaths and 
91 thousand injuries.

####Table of Storm Events causing harm to human health
I've then put the full output into a searchable table using the `data.table` and
`DT` packages so we can see the full breakdown of events, deaths and injuries: 

```r
suppressMessages(library(data.table)) # supress loading message
library(DT)
# Create data table
harm.table <- data.table(by_event)

datatable(harm.table,rownames = FALSE,
          colnames = c('Storm Event Type', 'Total Number of Deaths', 
                       'Total Number of Injuries)',
                       'Total Number of Deaths and Injuries'),
          class="row-border hover")
```

<!--html_preserve--><div id="htmlwidget-322" style="width:100%;height:auto;" class="datatables"></div>
<script type="application/json" data-for="htmlwidget-322">{ "x": {
 "data": [
 [ "TORNADO", "EXCESSIVE HEAT", "TSTM WIND", "FLOOD", "LIGHTNING", "HEAT", "FLASH FLOOD", "ICE STORM", "THUNDERSTORM WIND", "WINTER STORM", "HIGH WIND", "HAIL", "HURRICANE/TYPHOON", "HEAVY SNOW", "WILDFIRE", "THUNDERSTORM WINDS", "BLIZZARD", "FOG", "RIP CURRENT", "WILD/FOREST FIRE", "RIP CURRENTS", "HEAT WAVE", "DUST STORM", "WINTER WEATHER", "TROPICAL STORM", "AVALANCHE", "EXTREME COLD", "STRONG WIND", "DENSE FOG", "HEAVY RAIN", "HIGH WINDS", "HIGH SURF", "EXTREME HEAT", "GLAZE", "TSUNAMI", "WILD FIRES", "EXTREME COLD/WIND CHILL", "ICE", "WIND", "COLD/WIND CHILL", "HURRICANE", "URBAN/SML STREAM FLD", "TSTM WIND/HAIL", "WINTER WEATHER/MIX", "HEAVY SURF/HIGH SURF", "LANDSLIDE", "COLD", "WINTRY MIX", "Heat Wave", "WINTER WEATHER MIX", "RECORD HEAT", "STORM SURGE", "TROPICAL STORM GORDON", "HEAVY SURF", "WATERSPOUT/TORNADO", "DUST DEVIL", "SNOW SQUALL", "ICY ROADS", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND", "SNOW/HIGH WINDS", "SNOW", "FLOOD/FLASH FLOOD", "WATERSPOUT", "DRY MICROBURST", "FREEZING RAIN", "UNSEASONABLY WARM AND DRY", "MIXED PRECIP", "STRONG WINDS", "UNSEASONABLY WARM", "FLASH FLOODING", "THUNDERSTORMW", "WINTER STORMS", "BLACK ICE", "TORNADOES, TSTM WIND, HAIL", "EXCESSIVE RAINFALL", "HIGH WIND AND SEAS", "EXTREME WINDCHILL", "HEAT WAVE DROUGHT", "FREEZING DRIZZLE", "MARINE TSTM WIND", "RECORD/EXCESSIVE HEAT", "STORM SURGE/TIDE", "TORNADO F2", "WINTER STORM HIGH WINDS", "GLAZE/ICE STORM", "BLOWING SNOW", "COLD AND SNOW", "FLASH FLOOD/FLOOD", "HIGH SEAS", "ROUGH SEAS", "THUNDERSTORM", "GUSTY WINDS", "MARINE MISHAP", "HEAVY SNOW/ICE", "SMALL HAIL", "THUNDERSTORM  WINDS", "HIGH WINDS/SNOW", "FLOODING", "HURRICANE ERIN", "High Surf", "LOW TEMPERATURE", "NON-SEVERE WIND DAMAGE", "Mudslide", "RAIN/SNOW", "COASTAL FLOOD", "COASTAL FLOODING/EROSION", "COLD WEATHER", "FLASH FLOODING/FLOOD", "HEAT WAVES", "RIP CURRENTS/HEAVY SURF", "ROUGH SURF", "SNOW AND ICE", "TYPHOON", "COASTAL STORM", "DROUGHT", "FROST", "HEAVY RAINS", "HIGH WIND/SEAS", "HIGH WINDS/COLD", "Hypothermia/Exposure", "OTHER", "RIVER FLOOD", "THUNDERSTORM WINDSS", "Torrential Rainfall", "COLD WAVE", "Cold", "FUNNEL CLOUD", "HEAVY SEAS", "HIGH WATER", "HYPOTHERMIA/EXPOSURE", "Heavy surf and wind", "LIGHT SNOW", "Marine Accident", "TSTM WIND (G45)", "BRUSH FIRE", "Coastal Flooding", "Cold Temperature", "DROUGHT/EXCESSIVE HEAT", "EXCESSIVE SNOW", "Extreme Cold", "FALLING SNOW/ICE", "FLASH FLOODS", "FOG AND COLD TEMPERATURES", "GUSTY WIND", "Gusty winds", "HEAVY SNOW AND HIGH WINDS", "HURRICANE OPAL", "HURRICANE OPAL/HIGH WINDS", "HURRICANE-GENERATED SWELLS", "Heavy snow shower", "Hurricane Edouard", "ICE STORM/FLASH FLOOD", "LANDSLIDES", "MARINE HIGH WIND", "RIVER FLOODING", "ROGUE WAVE", "SLEET", "Snow", "THUNDERSNOW", "TORNADO F3", "UNSEASONABLY COLD", "WARM WEATHER", "WINDS", "blowing snow", "AVALANCE", "COASTAL FLOODING", "COASTALSTORM", "COLD/WINDS", "Coastal Storm", "DROWNING", "DRY MIRCOBURST WINDS", "Dust Devil", "Extended Cold", "FLOOD &amp; HEAVY RAIN", "FLOOD/RIVER FLOOD", "FREEZE", "FREEZING RAIN/SNOW", "Freezing Spray", "Gusty Winds", "HAZARDOUS SURF", "HEAVY SNOW/BLIZZARD/AVALANCHE", "HIGH", "HIGH SWELLS", "HIGH WAVES", "HIGH WIND 48", "HIGH WIND/HEAVY SNOW", "HURRICANE EMILY", "HURRICANE FELIX", "HYPERTHERMIA/EXPOSURE", "HYPOTHERMIA", "Heavy Surf", "ICE ON ROAD", "ICE ROADS", "LIGHTNING AND THUNDERSTORM WIN", "LIGHTNING INJURY", "LIGHTNING.", "MINOR FLOODING", "Mudslides", "NON TSTM WIND", "RAIN/WIND", "RAPIDLY RISING WATER", "RECORD COLD", "River Flooding", "SNOW/ BITTER COLD", "Snow Squalls", "Strong Winds", "THUNDERSTORM WIND (G40)", "THUNDERSTORM WIND G52", "THUNDERSTORM WINDS 13", "THUNDERSTORM WINDS/HAIL", "THUNDERSTORMS WINDS", "THUNDERTORM WINDS", "TIDAL FLOODING", "TSTM WIND (G35)", "TSTM WIND (G40)", "URBAN AND SMALL STREAM FLOODIN", "WATERSPOUT TORNADO", "WIND STORM", "Whirlwind" ],
[              5633,              1903,               504,               470,               816,               937,               978,                89,               133,               206,               248,                15,                64,               127,                75,                64,               101,                62,               368,                12,               204,               172,                22,                33,                58,               224,               160,               103,                18,                98,                35,               101,                96,                 7,                33,                 3,               125,                 6,                23,                95,                61,                28,                 5,                28,                42,                38,                35,                 1,                 0,                 0,                 2,                13,                 8,                 7,                 3,                 2,                 2,                 5,                14,                10,                 0,                 5,                17,                 3,                 3,                 7,                29,                 2,                 7,                11,                19,                 0,                10,                 1,                25,                 2,                 3,                17,                 4,                 2,                 9,                17,                11,                 0,                 1,                 0,                 1,                14,                14,                 5,                 8,                 1,                 4,                 7,                 0,                 0,                 0,                 3,                 6,                 6,                 3,                 7,                 0,                 4,                 4,                 3,                 0,                 5,                 5,                 5,                 5,                 4,                 4,                 0,                 3,                 0,                 1,                 0,                 4,                 0,                 4,                 0,                 2,                 0,                 0,                 3,                 3,                 0,                 3,                 3,                 3,                 3,                 1,                 1,                 0,                 0,                 2,                 2,                 2,                 0,                 2,                 1,                 2,                 1,                 1,                 0,                 2,                 1,                 2,                 0,                 0,                 0,                 0,                 1,                 1,                 2,                 0,                 2,                 0,                 1,                 0,                 2,                 0,                 1,                 1,                 1,                 1,                 1,                 1,                 0,                 1,                 0,                 0,                 1,                 1,                 1,                 1,                 1,                 1,                 0,                 0,                 0,                 0,                 1,                 1,                 0,                 0,                 0,                 1,                 1,                 1,                 1,                 1,                 0,                 0,                 0,                 1,                 1,                 1,                 0,                 1,                 1,                 1,                 0,                 1,                 1,                 1,                 1,                 1,                 0,                 0,                 0,                 1,                 0,                 1,                 0,                 1,                 0,                 1,                 1 ],
[             91346,              6525,              6957,              6789,              5230,              2100,              1777,              1975,              1488,              1321,              1137,              1361,              1275,              1021,               911,               908,               805,               734,               232,               545,               297,               309,               440,               398,               340,               170,               231,               280,               342,               251,               302,               152,               155,               216,               129,               150,                24,               137,                86,                12,                46,                79,                95,                72,                48,                52,                48,                77,                70,                68,                50,                38,                43,                40,                42,                42,                35,                31,                22,                26,                36,                29,                15,                29,                28,                23,                 0,                26,                21,                17,                 8,                27,                17,                24,                 0,                21,                20,                 5,                15,                15,                 8,                 0,                 5,                16,                15,                15,                13,                 0,                 0,                 8,                 5,                12,                 8,                 5,                10,                10,                10,                 6,                 2,                 1,                 4,                 0,                 7,                 2,                 2,                 2,                 5,                 0,                 0,                 0,                 0,                 1,                 1,                 5,                 1,                 4,                 3,                 4,                 0,                 4,                 0,                 4,                 2,                 4,                 4,                 0,                 0,                 3,                 0,                 0,                 0,                 0,                 2,                 2,                 3,                 2,                 0,                 0,                 0,                 2,                 0,                 1,                 0,                 1,                 1,                 2,                 0,                 1,                 0,                 2,                 2,                 2,                 2,                 1,                 1,                 0,                 2,                 0,                 2,                 1,                 2,                 0,                 2,                 1,                 1,                 0,                 0,                 0,                 0,                 1,                 0,                 1,                 1,                 0,                 0,                 0,                 0,                 0,                 0,                 1,                 1,                 1,                 1,                 0,                 0,                 1,                 1,                 1,                 0,                 0,                 0,                 0,                 0,                 1,                 1,                 1,                 0,                 0,                 0,                 1,                 0,                 0,                 0,                 1,                 0,                 0,                 0,                 0,                 0,                 1,                 1,                 1,                 0,                 1,                 0,                 1,                 0,                 1,                 0,                 0 ],
[             96979,              8428,              7461,              7259,              6046,              3037,              2755,              2064,              1621,              1527,              1385,              1376,              1339,              1148,               986,               972,               906,               796,               600,               557,               501,               481,               462,               431,               398,               394,               391,               383,               360,               349,               337,               253,               251,               223,               162,               153,               149,               143,               109,               107,               107,               107,               100,               100,                90,                90,                83,                78,                70,                68,                52,                51,                51,                47,                45,                44,                37,                36,                36,                36,                36,                34,                32,                32,                31,                30,                29,                28,                28,                28,                27,                27,                27,                25,                25,                23,                23,                22,                19,                17,                17,                17,                16,                16,                16,                15,                14,                14,                14,                13,                13,                13,                12,                12,                10,                10,                10,                 9,                 8,                 7,                 7,                 7,                 7,                 6,                 6,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 4,                 4,                 4,                 4,                 4,                 4,                 4,                 4,                 4,                 4,                 4,                 3,                 3,                 3,                 3,                 3,                 3,                 3,                 3,                 3,                 3,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1 ] 
],
"container": "<table class=\"row-border hover\">\n  <thead>\n    <tr>\n      <th>Storm Event Type</th>\n      <th>Total Number of Deaths</th>\n      <th>Total Number of Injuries)</th>\n      <th>Total Number of Deaths and Injuries</th>\n    </tr>\n  </thead>\n</table>",
"options": {
 "columnDefs": [
 {
 "className": "dt-right",
"targets": [                 1,                 2,                 3 ] 
} 
],
"order": [],
"autoWidth": false 
},
"callback": "function(table) {\nreturn table;\n}",
"filter": "none" 
},"evals": [ "callback" ] }</script><!--/html_preserve-->

####Bar plot of Top 15 Storm Events that cause over 90% of recorded harm to human health

The plot uses the `ggplot` and `GridExtra` packages. First I calculated the 
total deaths, injuries and harm for all events, and then used these to calculate
what proportion of harm each event accounted for. I found that the top 15 events
accounted for over 90% of the total harm caused in terms of both deaths and 
injuries.
This is then plotted as bar plots for Deaths, Injuries, and combined Deaths 
and Injuries.

```r
library(ggplot2)
suppressMessages(library(gridExtra)) # supress loading message
# Calculate total deaths, injuries and harm for all events
Total.Deaths <- sum(by_event$FATALITIES)
Total.Injuries <- sum(by_event$INJURIES)
Total.Death.and.Injuries <- sum(by_event$DEATH.AND.INJURY)

# Extract the top 15
top.harm <- by_event[1:15,]
# Calculate the proportion of total harm caused by each event type
top.harm <- top.harm %>% 
        mutate(Prop.Death = FATALITIES/Total.Deaths,
               Prop.Inj = INJURIES/Total.Injuries,
               Prop.Death.and.Inj = DEATH.AND.INJURY/Total.Death.and.Injuries) %>%
        transmute(EVTYPE,Prop.Death,Prop.Inj,Prop.Death.and.Inj)

# Check that the top 15 account for > 90%
sum(top.harm$Prop.Death.and.Inj)
```

```
## [1] 0.9212323
```

```r
# Plot the three catergories, Deaths, Injuries and Death and Injuries
plot.1 <- ggplot(top.harm, aes(EVTYPE, Prop.Death.and.Inj)) +
        geom_bar(stat="identity", fill="white", colour="darkgreen", width=0.5) + 
        coord_flip() +
        ggtitle("Top 15 events\n accounting > 90% of Total Harm") +
        ylab("Proportion of Total Deaths and Injuries") +
        xlab("")

plot.2 <- ggplot(top.harm, aes(EVTYPE, Prop.Death)) +
        geom_bar(stat="identity", fill="white", colour="darkgreen", width=0.5) + 
        coord_flip() +
        ggtitle("Top 15 events\n accounting > 90% of Total Deaths") +
        ylab("Proportion of Total Deaths") +
        xlab("")

plot.3 <- ggplot(top.harm, aes(EVTYPE, Prop.Inj)) +
        geom_bar(stat="identity", fill="white", colour="darkgreen", width=0.5) + 
        coord_flip() +
        ggtitle("Top 15 events\n accounting > 90% of Total Injuries") +
        ylab("Proportion of Total Injuries") +
        xlab("")
```


```r
# Print three panels onto a single plot
grid.arrange(plot.1,plot.2,plot.3,ncol=3,nrow=1)
```
![**Figure 1:** Top 15 Storm Events that cause over 90% of recorded 
harm to human health](./figure/plot_harm-1.png)

The total number of deaths in the period 1950 to 2011 was 
15145.

The total number of injuries in the period 1950 to 2011 was
140528.

The total number of cases of harm in the period 1950 to 2011 was
155673.

###2. Which events have the greatest economic impact?

To answer this question, in addition to the event type I needed the property 
damage and crop damage observations.
These are recorded as `PROPDMG`, `PROPDMGEXP`, `CROPDMG` and `CROPDMGEXP`. The
`EXP` variables provide factors by which to multiply the `DMG` values. For the
purposes of this report I've used `K` as 1 thousand, `M` as 1 million and `B` as
1 billion, as indicated in the accompanying documentation. I ignored the other
character values as it was unclear what they were.


```r
# I need to clean-up the data to get total economic costs of the events from
# PROPDMG, PROPDMGEXP, CPROPDMG, CPROPDMGEXP
# Columns, 25:28
which(colnames(storm.raw)=="PROPDMG")
```

```
## [1] 25
```

```r
which(colnames(storm.raw)=="PROPDMGEXP")
```

```
## [1] 26
```

```r
which(colnames(storm.raw)=="CROPDMG")
```

```
## [1] 27
```

```r
which(colnames(storm.raw)=="CROPDMGEXP")
```

```
## [1] 28
```

I  initially split the property and crop data to process separately. Again I used
`dplyr` for the analysis:

```r
library(dplyr)
# Subset these Property columns
prop.costs.dat <- storm.raw[,c(8,25:26)] %>%
        filter(PROPDMG > 0) # Only where cost has occured

# Subset the Crop columns
crop.costs.dat <- storm.raw[,c(8,27:28)] %>%
        filter(CROPDMG > 0)
```

Then I did the following for the property and crop data:

 1. Found which strings to replace in the `EXP` column and replaced them with integers.
 2. Removed missing values.
 3. Multiplied the `DMG` and `EXP` columns to get the damage costs and added this
 as a new column of the damage costs.
 4. Arranged the data in descending order of damage costs.
 

```r
suppressMessages(library(dplyr))
# Find which strings to replace in property data
unique(prop.costs.dat$PROPDMGEXP)
```

```
##  [1] "K" "M" "B" "m" "+" "0" ""  "5" "6" "4" "h" "2" "7" "3" "H" "-"
```

```r
# Replace characters K,M and B with integers, ignore others and NAs warning
prop.costs.dat$PROPDMGEXP <- gsub("[Kk]",1e3, prop.costs.dat$PROPDMGEXP)
prop.costs.dat$PROPDMGEXP <- gsub("[Mm]",1e6, prop.costs.dat$PROPDMGEXP)
prop.costs.dat$PROPDMGEXP <- gsub("[Bb]",1e9, prop.costs.dat$PROPDMGEXP)
prop.costs.dat$PROPDMGEXP <- as.integer(prop.costs.dat$PROPDMGEXP)
```

```
## Warning: NAs introduced by coercion
```

```r
# Remove NAs as I don't know how to interpret the other symbols
prop.costs.dat <- na.omit(prop.costs.dat)

# Mutate the data frame
prop.costs.dat <- prop.costs.dat %>% 
        mutate(Prop.Damage.Costs = PROPDMG*PROPDMGEXP) %>%
        arrange(desc(Prop.Damage.Costs))

# Find which strings to replace in crop data
unique(crop.costs.dat$CROPDMGEXP) 
```

```
## [1] "M" "K" "m" "B" "k" "0" ""
```

```r
# Replace characters K,M and B with integers, ignore others and NAs warning
crop.costs.dat$CROPDMGEXP <- gsub("[Kk]",1e3, crop.costs.dat$CROPDMGEXP)
crop.costs.dat$CROPDMGEXP <- gsub("[Mm]",1e6, crop.costs.dat$CROPDMGEXP)
crop.costs.dat$CROPDMGEXP <- gsub("B",1e9, crop.costs.dat$CROPDMGEXP)
crop.costs.dat$CROPDMGEXP <- as.integer(crop.costs.dat$CROPDMGEXP)

# Remove NAs as I don't know how to interpret them
crop.costs.dat <- na.omit(crop.costs.dat)

# Mutate the data frame
crop.costs.dat <- crop.costs.dat %>% 
        mutate(Crop.Damage.Costs = CROPDMG*CROPDMGEXP) %>%
        arrange(desc(Crop.Damage.Costs))
```

I've then summarised this output by event as previously and merged the property
and crop damage costs into a single data frame:

```r
library(dplyr)
# Summarize data by event
prop_cost_by_event <- prop.costs.dat %>% group_by(EVTYPE) %>%
        summarise_each(funs(sum)) # Sum Prop Costs for each event

crop_cost_by_event <- crop.costs.dat %>% group_by(EVTYPE) %>%
        summarise_each(funs(sum)) # Sum Crop Costs for each event

# Merge the Property and Crop Costs
clean.costs <- merge(prop_cost_by_event, crop_cost_by_event, 
                     "EVTYPE", all=TRUE) %>%
        # Keep only the events and total costs columns
        select(EVTYPE, Prop.Damage.Costs, Crop.Damage.Costs)
# Set NAs to zero
clean.costs[is.na(clean.costs)] <- 0
clean.costs <- clean.costs %>% 
        # Sum the Property and Crop costs
        mutate(Total.Cost = Prop.Damage.Costs+Crop.Damage.Costs) %>%
        arrange(desc(Total.Cost)) # Arrange in descending order
```

####Table of the economic costs of Storm Events

As before I've put the summary of the storm event costs into a a searchable table
so we can see the full breakdown of events and costs:

```r
library(data.table)
library(DT)
# Create data table
cost.table <- data.table(clean.costs)
cost.table$Prop.Damage.Costs <- round(cost.table$Prop.Damage.Costs/1e6,0)
cost.table$Crop.Damage.Costs <- round(cost.table$Crop.Damage.Costs/1e6,0)
cost.table$Total.Cost <- round(cost.table$Total.Cost/1e6,0)
datatable(cost.table,rownames = FALSE,
          colnames = c('Storm Event Type', 'Property Damage Cost (Millions USD)', 
                       'Crop Damage Cost (Millions USD)',
                        'Total Damage Cost (Millions USD)'),
          class="row-border hover")
```

<!--html_preserve--><div id="htmlwidget-4947" style="width:100%;height:auto;" class="datatables"></div>
<script type="application/json" data-for="htmlwidget-4947">{ "x": {
 "data": [
 [ "FLOOD", "HURRICANE/TYPHOON", "TORNADO", "STORM SURGE", "HAIL", "FLASH FLOOD", "DROUGHT", "HURRICANE", "RIVER FLOOD", "ICE STORM", "TROPICAL STORM", "WINTER STORM", "HIGH WIND", "WILDFIRE", "TSTM WIND", "STORM SURGE/TIDE", "THUNDERSTORM WIND", "HURRICANE OPAL", "WILD/FOREST FIRE", "HEAVY RAIN/SEVERE WEATHER", "THUNDERSTORM WINDS", "TORNADOES, TSTM WIND, HAIL", "HEAVY RAIN", "EXTREME COLD", "SEVERE THUNDERSTORM", "FROST/FREEZE", "HEAVY SNOW", "LIGHTNING", "BLIZZARD", "HIGH WINDS", "WILD FIRES", "TYPHOON", "EXCESSIVE HEAT", "FREEZE", "HEAT", "HURRICANE ERIN", "LANDSLIDE", "FLASH FLOODING", "FLASH FLOOD/FLOOD", "DAMAGING FREEZE", "FLOOD/FLASH FLOOD", "HAILSTORM", "STRONG WIND", "COASTAL FLOOD", "TSUNAMI", "EXCESSIVE WETNESS", "River Flooding", "COASTAL FLOODING", "HIGH WINDS/COLD", "FLOODING", "FLOOD/RAIN/WINDS", "HURRICANE OPAL/HIGH WINDS", "TSTM WIND/HAIL", "MAJOR FLOOD", "WILDFIRES", "HIGH SURF", "HEAVY RAINS", "URBAN/SML STREAM FLD", "FROST", "COLD AND WET CONDITIONS", "WINTER STORM HIGH WINDS", "RECORD COLD", "WATERSPOUT/TORNADO", "HURRICANE EMILY", "Early Frost", "LAKE-EFFECT SNOW", "WINTER WEATHER", "Damaging Freeze", "SEVERE THUNDERSTORM WINDS", "AGRICULTURAL FREEZE", "UNSEASONABLY COLD", "Coastal Flood", "SMALL HAIL", "TROPICAL STORM JERRY", "COASTAL FLOODING/EROSION", "Extreme Cold", "URBAN FLOOD", "SEVERE THUNDERSTORMS", "EXTREME WINDCHILL", "Erosion/Cstl Flood", "HEAT WAVE", "COASTAL  FLOODING/EROSION", "Heavy Rain/High Surf", "SNOW", "FOG", "HARD FREEZE", "ICE", "ICE JAM FLOODING", "Freeze", "RIVER FLOODING", "FLOOD &amp; HEAVY RAIN", "UNSEASONAL RAIN", "HEAVY SURF/HIGH SURF", "DENSE FOG", "ASTRONOMICAL HIGH TIDE", "WATERSPOUT", "WIND", "FLASH FLOODS", "EXTREME COLD/WIND CHILL", "DUST STORM", "FREEZING RAIN", " TSTM WIND", "LAKESHORE FLOOD", "HIGH WINDS HEAVY RAINS", "STRONG WINDS", "DRY MICROBURST", "WINTER WEATHER/MIX", "Coastal Flooding", "RAIN", "FLOODS", "URBAN FLOODING", "FOREST FIRES", "MARINE TSTM WIND", "HEAVY RAINS/FLOODING", "EXTREME HEAT", "Unseasonable Cold", "SNOWMELT FLOODING", "COOL AND WET", "HEAVY RAIN/LIGHTNING", "HEAVY SNOW/BLIZZARD/AVALANCHE", "HEAVY SNOW/FREEZING RAIN", "HEAVY SNOWPACK", "HIGH WINDS/COASTAL FLOOD", "ICE AND SNOW", "LIGHTNING FIRE", "TROPICAL STORM ALBERTO", "AVALANCHE", "ICE/STRONG WINDS", "COLD/WIND CHILL", "TORNADO F1", "FREEZING FOG", "EXCESSIVE SNOW", "FLASH FLOODING/FLOOD", "THUNDERSTORM", "LIGHT SNOW", "THUNDERSTORM WINDSS", "TROPICAL DEPRESSION", "HEAVY SNOW AND STRONG WINDS", "TORNADO F2", "HEAVY SNOW/HIGH WINDS &amp; FLOOD", "HEAVY SURF", "SNOW/SLEET", "HEAVY MIX", "SNOW FREEZING RAIN", "MARINE HIGH WIND", "MUDSLIDE", "RIVER AND STREAM FLOOD", "SNOW/FREEZING RAIN", "WINDS", "Frost/Freeze", "HIGH WIND DAMAGE", "OTHER", "DAM BREAK", "FLOOD/FLASH", "HURRICANE FELIX", "RECORD SNOW", "SNOW/COLD", "TROPICAL STORM GORDON", "WIND AND WAVE", "WINTER STORMS", "SEICHE", "Gusty Winds", "GLAZE", "COASTAL EROSION", "THUNDERSTORM WINDS HAIL", "TORNADO F3", "DUST DEVIL", "HEAVY SNOW SQUALLS", "SNOW SQUALL", "SNOW SQUALLS", "HEAVY SNOW/ICE", "THUNDERTORM WINDS", "MUD SLIDE", "HEAVY RAIN AND FLOOD", "Light Snow", "Landslump", "MIXED PRECIPITATION", "HAIL/WINDS", "DUST STORM/HIGH WINDS", "GUSTY WINDS", "THUNDERSTORMS WINDS", "HIGH WATER", "BLIZZARD/WINTER STORM", "COASTAL SURGE", "COLD", "FLASH FLOOD/", "FLASH FLOODING/THUNDERSTORM WI", "FLOOD/RIVER FLOOD", "FROST\\FREEZE", "HEAVY LAKE SNOW", "HEAVY PRECIPITATION", "HEAVY RAIN/SNOW", "HEAVY SNOW/WINTER STORM", "HIGH WIND/SEAS", "HURRICANE GORDON", "SLEET/ICE STORM", "SNOW AND ICE STORM", "SNOW/HEAVY SNOW", "VOLCANIC ASH", "MARINE THUNDERSTORM WIND", "LIGHT FREEZING RAIN", "MICROBURST WINDS", "TROPICAL STORM DEAN", "MARINE STRONG WIND", "THUNDERSTORM WINDS/HAIL", "High Surf", "GUSTY WIND", "ICY ROADS", "ASTRONOMICAL LOW TIDE", "GLAZE ICE", "WIND STORM", "FLASH FLOOD FROM ICE JAMS", "HEAT WAVE DROUGHT", "Mixed Precipitation", "HAIL 275", "TSTM WIND (G45)", "WATERSPOUT-", "   HIGH SURF ADVISORY", "HAIL 450", "STORM FORCE WINDS", "THUNDERSTORM WINDS LIGHTNING", "FUNNEL CLOUD", "LATE SEASON SNOW", "RIP CURRENTS", "HAIL DAMAGE", "HIGH WINDS/SNOW", "ROCK SLIDE", "WIND DAMAGE", "THUNDERSTORMW", "HAIL 175", "LANDSLIDES", "GUSTNADO", "Beach Erosion", "DENSE SMOKE", "Extended Cold", "GROUND BLIZZARD", "HEAVY SNOW/SQUALLS", "ICE FLOES", "SNOW AND HEAVY SNOW", "SNOW/HIGH WINDS", "SNOW/ICE STORM", "SNOW/SLEET/FREEZING RAIN", "Strong Winds", "TSTM WIND G58", "Glaze", "HEAVY SNOW-SQUALLS", "Light Snowfall", "TORNADO F0", "TSTM WIND AND LIGHTNING", "FREEZING DRIZZLE", "FREEZING RAIN/SNOW", "HURRICANE-GENERATED SWELLS", "TUNDERSTORM WIND", "THUNDERSTORM WIND 60 MPH", "Snow", "MICROBURST", "WINTER WEATHER MIX", "TSTM WINDS", "BRUSH FIRE", "Freezing Drizzle", "Snow Squalls", "THUDERSTORM WINDS", "THUNDERSTORM HAIL", "Cold", "URBAN/SMALL STREAM FLOOD", " FLASH FLOOD", "Coastal Storm", "EXTREME WIND CHILL", "FLASH FLOOD LANDSLIDES", "FREEZING RAIN/SLEET", "HAIL 150", "HEAVY SNOW/BLIZZARD", "HEAVY SNOW/WIND", "Heavy Surf", "HEAVY SURF COASTAL FLOODING", "HIGH WIND AND SEAS", "HIGH WIND/BLIZZARD", "HIGH WIND/HEAVY SNOW", "HIGH WINDS/", "Lake Effect Snow", "Marine Accident", "MUD SLIDES", "MUDSLIDES", "Other", "RAINSTORM", "SEVERE TURBULENCE", "SNOW/ BITTER COLD", "SNOW/BLOWING SNOW", "THUNDERSNOW", "THUNDERSTORM WINDS 13", "THUNDERSTORM WINDS/ FLOOD", "THUNDERSTORM WINDS53", "TSTMW", "TSTM WIND (G40)", "NON-TSTM WIND", "THUNDERSTORM WINDSHAIL", "WATERSPOUT TORNADO", "Freezing Rain", "WET MICROBURST", "WILD/FOREST FIRES", "LAKE FLOOD", "LIGHTNING  WAUSEON", "SNOW/ICE", "Tstm Wind", "TSTM WIND (G35)", "TSTM WIND 55", "LIGHTNING AND HEAVY RAIN", "THUNDERSTORM  WINDS", "THUNDERSTORM WIND 98 MPH", "THUNDERSTORM WINDS 63 MPH", "THUNDERSTORMS WIND", "BLOWING DUST", "GUSTY WIND/HAIL", "Microburst", "THUNDERSTORM WIND/ TREES", "TSTM WIND 65)", "Dust Devil", "HIGH WIND (G40)", "Strong Wind", "gradient wind", "LAKE EFFECT SNOW", "HIGH SEAS", "blowing snow", "Freezing drizzle", "HAIL 100", "HEAVY SWELLS", "WATERSPOUT-TORNADO", "Gradient wind", "FLASH FLOOD - HEAVY RAIN", "ICE ROADS", "FLOOD/FLASH/FLOOD", "GRASS FIRES", "HAIL 075", "HAIL 125", "HAIL 200", "Heavy snow shower", "HIGH WIND 48", "LIGHTNING THUNDERSTORM WINDS", "ROUGH SURF", "THUNDERSTORM WIND G60", "THUNDERSTORM WINDS G60", "THUNDERSTORMS", "Tidal Flooding", "TSTM WIND 45", "UNSEASONABLY WARM", "Wind", "Wind Damage", "WINTRY MIX", " TSTM WIND (G45)", "TSTM WIND (41)", "LANDSPOUT", "WHIRLWIND", "GRADIENT WIND", "DROUGHT/EXCESSIVE HEAT", "?", "APACHE COUNTY", "FLOOD FLASH", "FLOOD/FLASHFLOOD", "HEAVY RAIN/SMALL STREAM URBAN", "HIGH SWELLS", "HIGH WINDS/HEAVY RAIN", "ICE JAM", "Light snow", "LIGHTING", "LIGHTNING/HEAVY RAIN", "LIGNTNING", "MINOR FLOODING", "MUD SLIDES URBAN FLOODING", "NON-SEVERE WIND DAMAGE", "SMALL STREAM FLOOD", "SNOW ACCUMULATION", "SNOW/ ICE", "THUNDEERSTORM WINDS", "THUNDERSTORM WIND G55", "THUNDERSTORM WIND/LIGHTNING", "THUNDERSTORM WINDS AND", "THUNDERSTORM WINDS.", "THUNDERSTORM WINDS/FLOODING", "THUNDERSTORM WINDS/FUNNEL CLOU", "THUNDERSTROM WIND", "TSTM WIND  (G45)", "TSTM WIND DAMAGE", "URBAN AND SMALL", "URBAN/SMALL STREAM", "Whirlwind", "MARINE HAIL", "THUNDERSTORM WIND 65 MPH", "THUNDERSTORM WIND 65MPH", "THUNDERSTORM WIND TREES", "HIGH  WINDS", "HVY RAIN", "THUNDERSTORM WIND.", "TIDAL FLOODING", "Wintry Mix", "DOWNBURST", "FLASH FLOOD/ STREET", "GUSTY WIND/HVY RAIN", "Gusty wind/rain", "THUNDERSTORM DAMAGE TO", "THUNDERSTORM WIND/AWNING", "URBAN FLOODS", "RURAL FLOOD", "FLASH FLOOD/LANDSLIDE", "HAIL 0.75", "HAIL 75", "Ice jam flood (minor", "RIP CURRENT", "THUNDERESTORM WINDS", "THUNDERSTORM WIND/ TREE", "THUNDERSTORM WINS", "THUNERSTORM WINDS", "TORNADOES", "TSTM WIND 40", "TSTM WIND G45", "WATERSPOUT/ TORNADO", "HAIL/WIND", "DUST DEVIL WATERSPOUT", "HEAVY SHOWER", "RECORD RAINFALL", "THUNDERSTORM WIND/HAIL", "THUNDERSTORMWINDS", "TORNDAO", "WIND/HAIL", "COLD AIR TORNADO", "SNOW AND ICE", "URBAN SMALL", "HIGH TIDES" ],
[            144658,             69306,             56937,             43324,             15732,             16141,              1046,             11868,              5119,              3945,              7704,              6688,              5270,              4765,              4485,              4641,              3483,              3173,              3002,              2500,              1736,              1600,               694,                68,              1205,                 9,               933,               929,               659,               608,               624,               600,                 8,                 0,                 2,               258,               325,               308,               272,                 8,               174,               241,               175,               238,               144,                 0,               106,               127,               110,               108,                 0,               100,                44,               105,               100,                90,                12,                58,                 0,                 0,                60,                56,                51,                50,                 0,                40,                21,                 0,                 0,                 0,                 0,                22,                 0,                 5,                20,                 0,                18,                 2,                 1,                16,                10,                15,                14,                15,                13,                 0,                13,                 6,                 0,                10,                10,                 0,                10,                10,                 9,                 9,                 9,                 9,                 9,                 6,                 8,                 8,                 8,                 8,                 2,                 7,                 6,                 6,                 5,                 6,                 6,                 5,                 5,                 5,                 0,                 0,                 5,                 0,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 4,                 4,                 2,                 2,                 2,                 2,                 2,                 1,                 2,                 2,                 2,                 2,                 2,                 2,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 0,                 1,                 1,                 0,                 1,                 1,                 0,                 1,                 0,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 0,                 0,                 0,                 1,                 1,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0 ],
[              5662,              2608,               415,                 0,              3026,              1421,             13973,              2742,              5029,              5022,               678,                27,               639,               295,               554,                 1,               415,                19,               107,                 0,               191,                 2,               733,              1293,                 0,              1094,               135,                12,               112,                41,                 0,                 1,               492,               446,               401,               136,                20,                15,                 1,               262,                95,                 0,                65,                 0,                 0,               142,                28,                 0,                 7,                 9,               113,                10,                65,                 0,                 0,                 0,                60,                 8,                66,                66,                 5,                 0,                 0,                 0,                42,                 0,                15,                34,                29,                29,                25,                 0,                21,                16,                 0,                20,                 1,                17,                17,                 0,                 6,                 0,                 2,                 0,                 0,                13,                 0,                 5,                10,                 0,                 0,                10,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 3,                 0,                 0,                 0,                 0,                 5,                 0,                 0,                 0,                 1,                 0,                 0,                 0,                 0,                 0,                 5,                 5,                 0,                 5,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 1,                 0,                 0,                 0,                 0,                 1,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 1,                 0,                 0,                 1,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0 ],
[            150320,             71914,             57352,             43324,             18758,             17562,             15019,             14610,             10148,              8967,              8382,              6715,              5909,              5061,              5039,              4642,              3898,              3192,              3109,              2500,              1927,              1602,              1428,              1361,              1206,              1104,              1067,               941,               771,               649,               624,               601,               500,               446,               403,               394,               345,               323,               273,               270,               269,               241,               240,               238,               144,               142,               134,               127,               118,               117,               113,               110,               109,               105,               101,                90,                73,                67,                66,                66,                65,                56,                51,                50,                42,                40,                36,                34,                29,                29,                25,                22,                21,                21,                20,                20,                19,                19,                18,                16,                16,                15,                15,                15,                13,                13,                13,                11,                10,                10,                10,                10,                10,                10,                 9,                 9,                 9,                 9,                 9,                 9,                 8,                 8,                 8,                 8,                 7,                 7,                 6,                 6,                 6,                 6,                 6,                 6,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 5,                 4,                 4,                 3,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 2,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 1,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0,                 0 ] 
],
"container": "<table class=\"row-border hover\">\n  <thead>\n    <tr>\n      <th>Storm Event Type</th>\n      <th>Property Damage Cost (Millions USD)</th>\n      <th>Crop Damage Cost (Millions USD)</th>\n      <th>Total Damage Cost (Millions USD)</th>\n    </tr>\n  </thead>\n</table>",
"options": {
 "columnDefs": [
 {
 "className": "dt-right",
"targets": [                 1,                 2,                 3 ] 
} 
],
"order": [],
"autoWidth": false 
},
"callback": "function(table) {\nreturn table;\n}",
"filter": "none" 
},"evals": [ "callback" ] }</script><!--/html_preserve-->

###Bar plot of Top 15 Storm Events that cause over 90% of recorded economic costs
Finally, as before I calculated the total economic cost for all events, and 
then used this to calculate what proportion of the total each event accounted for. 
I found that the top 15 events accounted for over 90% of the total economic costs
caused.


```r
library(ggplot2)
# Calculate the total cost of all events
Total.Damage.Cost <- sum(clean.costs$Total.Cost)
# Subset top 15 events
top.costs <- clean.costs[1:15,]
top.costs <- top.costs %>% mutate(Proportion = Total.Cost/Total.Damage.Cost) %>%
        transmute(EVTYPE,Proportion)
# Check this accounts for >90% of costs
sum(top.costs$Proportion)
```

```
## [1] 0.9216174
```

```r
# Create barplot
plot.4 <- ggplot(top.costs, aes(x= EVTYPE,y=Proportion))
plot.4 + geom_bar(stat="identity", fill="white", colour="darkgreen", width=0.5) + 
        coord_flip() +
        ggtitle("Top 15 events accounting > 90% of Total Damage Costs") +
        ylab("Proportion of Total Damage Costs") +
        xlab("")
```

![](Peer_Assessment_2_files/figure-html/plot_costs-1.png) 

**Figure 2:** Top 15 Storm Events that cause over 90% of recorded 
economic costs

The total economic cost of storm events in the period 1950 to 2011 was 
approximately 476 billion US Dollars

###Session information
Here is the session information about the packages I used, their versions, and 
the version of R that I used for this assignment:

```r
sessionInfo()
```

```
## R version 3.2.0 (2015-04-16)
## Platform: x86_64-w64-mingw32/x64 (64-bit)
## Running under: Windows 7 x64 (build 7601) Service Pack 1
## 
## locale:
## [1] LC_COLLATE=English_United Kingdom.1252 
## [2] LC_CTYPE=English_United Kingdom.1252   
## [3] LC_MONETARY=English_United Kingdom.1252
## [4] LC_NUMERIC=C                           
## [5] LC_TIME=English_United Kingdom.1252    
## 
## attached base packages:
## [1] grid      stats     graphics  grDevices utils     datasets  methods  
## [8] base     
## 
## other attached packages:
## [1] gridExtra_0.9.1  ggplot2_1.0.1    DT_0.0.35        data.table_1.9.4
## [5] dplyr_0.4.1     
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_0.11.5       rstudioapi_0.3.1  knitr_1.9        
##  [4] magrittr_1.5      MASS_7.3-40       munsell_0.4.2    
##  [7] colorspace_1.2-6  stringr_0.6.2     plyr_1.8.1       
## [10] tools_3.2.0       parallel_3.2.0    gtable_0.1.2     
## [13] DBI_0.3.1         htmltools_0.2.6   yaml_2.1.13      
## [16] lazyeval_0.1.10   assertthat_0.1    digest_0.6.8     
## [19] RJSONIO_1.3-0     reshape2_1.4.1    formatR_1.1      
## [22] htmlwidgets_0.3.2 evaluate_0.6      rmarkdown_0.5.1  
## [25] labeling_0.3      scales_0.2.4      jsonlite_0.9.16  
## [28] chron_2.3-45      proto_0.3-10
```
