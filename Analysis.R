# suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(ggplot2))
# suppressPackageStartupMessages(library(lubridate))
# 
stormData <- read.csv('repdata-data-StormData.csv')
stormData <- tbl_df(stormData)
stormData

stormData <- mutate(stormData, YEAR = year(mdy_hms(BGN_DATE))) %>%
    mutate(healthImpact = FATALITIES+INJURIES) %>%
    filter(YEAR > 1996)

observations <- dim(stormData)[1]
numEVTYPE <- length(levels(stormData$EVTYPE))

eventsTally <- group_by(stormData, EVTYPE) %>% tally(sort=TRUE)
sigEvents <- eventsTally[eventsTally$n > 15, ]

subStormData <- filter(stormData, EVTYPE %in% sigEvents$EVTYPE)

subStormData$EVTYPE <- gsub('TSTM','THUNDERSTORM',subStormData$EVTYPE, ignore.case = TRUE)
subStormData$EVTYPE <- gsub('WINDS','WIND',subStormData$EVTYPE, ignore.case = TRUE)
subStormData$EVTYPE <- gsub('HEAT WAVE','HEAT',subStormData$EVTYPE, ignore.case = TRUE)

healthImpact <- group_by(subStormData, YEAR, EVTYPE) %>%
    summarise(hImpact = sum(healthImpact)) %>%
    arrange(YEAR, desc(hImpact)) %>%
    filter(hImpact > 0) %>%
    top_n(5)