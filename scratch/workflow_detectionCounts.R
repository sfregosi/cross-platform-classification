# Workflow to calculate the number of each detection type for each event

# Modified from A. Simonis script 'Detection_Counts_aes_20201019.R'
# Last updated 30 April 2024, S. Fregosi

#The number of events with mean number and standard deviation of detections per event
#are included in the variables "ClickStats", "CepStats", and "WhistleStats"

##See some exploratory plots at the bottom

#Load required packages
# library(PAMpal)
# library(dplyr)

#Choose folder where Lasker and Sette data are stored
setwd(choose.dir())
load('setteData_HICEAS17-SetteTA_ch2.rdata')
load('laskerData_HICEAS17-LaskerTA_ch2.rdata')

SetteDet<-getDetectorData(ch2Sette)
LaskerDet<-getDetectorData(ch2Lasker)

#Combine detections from both ships
HICEASDet<-squishList(c(SetteDet,LaskerDet))

HICEASClick<-HICEASDet$click
HICEASCep<-HICEASDet$cepstrum
HICEASWhistle<-HICEASDet$whistle

#Remove Species that are not included in the model
goodSpecies<-c(2,13,15,21,26,31,32,33,36,46)

HICEASClick<-filter(HICEASClick,species %in% goodSpecies)
HICEASCep<-filter(HICEASCep,species %in% goodSpecies)
HICEASWhistle<-filter(HICEASWhistle,species %in% goodSpecies)

ProblemEvents<-c('S44','L152','L210','S258','S259')

HICEASClick<-filter(HICEASClick,!eventId %in% ProblemEvents)
HICEASCep<-filter(HICEASCep,!eventId %in% ProblemEvents)
HICEASWhistle<-filter(HICEASWhistle,!eventId %in% ProblemEvents)

#Remove Unclassified Clicks (Click_Detector_0)
goodDetectors<-c("Click_Detector_1", "Click_Detector_2", "Click_Detector_3", "Click_Detector_4", "Click_Detector_5")
HICEASClick<-filter(HICEASClick, detectorName %in% goodDetectors)

#Remove detections that are likely noise
HICEASClick<-filter(HICEASClick, peak > 5 & peak < 80)
HICEASClick<-filter(HICEASClick, BW_10dB > 5)
HICEASClick<-filter(HICEASClick, duration > 2 & duration < 1000)

#Filter: Remove clicks in individual detectors
CD1 <- HICEASClick %>% 
  filter(detectorName == "Click_Detector_1", peak>2 & peak<15)
CD2 <- HICEASClick %>% 
  filter(detectorName == "Click_Detector_2", peak>15 & peak<30)
CD3 <- HICEASClick %>% 
  filter(detectorName == "Click_Detector_3", peak>30 & peak<50)
CD4 <- HICEASClick %>% 
  filter(detectorName == "Click_Detector_4", peak>30 & peak<50)
CD5 <- HICEASClick %>% 
  filter(detectorName == "Click_Detector_5", peak>50 & peak<80)

#HICEASClickFilter <- squishList(c(CD1,CD2,CD3,CD4,CD5))

HICEASClickFilter <- bind_rows(CD1,CD2,CD3,CD4,CD5)

# #Extract event info about clicks
# ClickCount<-HICEASClick %>%
#   group_by(species,eventId) %>%
#   summarise(NClicks=n())
# 
# ClickStats<-ClickCount %>%
#   group_by(species) %>%
#   summarise(NEvents=n(),MeanClicks=mean(NClicks),SDClicks=sd(NClicks))
# 
# ClickStats$MeanClicks<-round(ClickStats$MeanClicks,digits=0)
# ClickStats$SDClicks<-round(ClickStats$SDClicks,digits=0)

# Extract click counts from UID
ClickCountU <- HICEASClickFilter %>% 
  group_by(species,eventId) %>%
  summarize(UClicks=unique(UID)) %>% 
  summarize(NClicks=n())

ClickStatsU<-ClickCountU %>%
  group_by(species) %>%
  summarise(NEvents=n(),MeanClicks=mean(NClicks),SDClicks=sd(NClicks),TotalClicks=sum(NClicks))

ClickStatsU$MeanClicks<-round(ClickStatsU$MeanClicks,digits=0)
ClickStatsU$SDClicks<-round(ClickStatsU$SDClicks,digits=0)

# #Extract event info about cepstral detections
# CepCount<-HICEASCep %>%
#   group_by(species,eventId) %>%
#   summarise(NCep=n())
# 
# CepStats<-CepCount %>%
#   group_by(species) %>%
#   summarise(NEvents=n(),MeanCep=mean(NCep),SDCep=sd(NCep))
# 
# CepStats$MeanCep<-round(CepStats$MeanCep,digits=0)
# CepStats$SDCep<-round(CepStats$SDCep,digits=0)

#Extract event info about cepstral detections from UID
CepCountU<-HICEASCep %>%
  group_by(species,eventId) %>%
  summarise(UCep=unique(UID)) %>%
  summarise(NCep=n())

CepStatsU<-CepCountU %>%
  group_by(species) %>%
  summarise(NEvents=n(),MeanCep=mean(NCep),SDCep=sd(NCep),TotalCep=sum(NCep))

CepStatsU$MeanCep<-round(CepStatsU$MeanCep,digits=0)
CepStatsU$SDCep<-round(CepStatsU$SDCep,digits=0)

# #Extract event info about whistle detections
# WhistleCount<-HICEASWhistle %>%
#   group_by(species,eventId) %>%
#   summarise(NWhistle=n())
# 
# WhistleStats<-WhistleCount %>%
#   group_by(species) %>%
#   summarise(NEvents=n(),MeanWhistle=mean(NWhistle),SDWhistle=sd(NWhistle))
# 
# WhistleStats$MeanWhistle<-round(WhistleStats$MeanWhistle,digits=0)
# WhistleStats$SDWhistle<-round(WhistleStats$SDWhistle,digits=0)

#Extract event info about whistle detections
WhistleCountU<-HICEASWhistle %>%
  group_by(species,eventId) %>%
  summarise(UWhistle=unique(UID)) %>%
  summarise(NWhistle=n())

WhistleStatsU<-WhistleCountU %>%
  group_by(species) %>%
  summarise(NEvents=n(),MeanWhistle=mean(NWhistle),SDWhistle=sd(NWhistle),TotalWhis=sum(NWhistle))

WhistleStatsU$MeanWhistle<-round(WhistleStatsU$MeanWhistle,digits=0)
WhistleStatsU$SDWhistle<-round(WhistleStatsU$SDWhistle,digits=0)



##Curious about the Pm whistle detections
library(ggplot2)
PmWhistle<-filter(HICEASWhistle,species=="46")
hist(PmWhistle$duration,60)

PcWhistle<-filter(HICEASWhistle,species=="33")
hist(PcWhistle$duration,60)

ggplot(HICEASWhistle,aes(as.factor(species),duration))+geom_boxplot()


# Total Count of Events per species
AllWCC <- bind_rows(WhistleCountU,CepCountU,ClickCountU)

EventCounts <- AllWCC %>% 
  group_by(species) %>% 
  summarise(UID=unique(eventId)) %>%
  summarise(total=n())

DetectionCountsList <- unique(AllWCC$eventId)


#AllEventInfo <- bind_cols(banterEvents, CSVeventList, DetectionCountsList)

HICEASClickFilterPM <- HICEASClickFilter %>%
  group_by(eventId) %>%
  filter(species == '46') %>% 
  summarise(NEvents=n())
