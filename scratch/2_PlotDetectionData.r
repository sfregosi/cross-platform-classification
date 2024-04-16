
#### Examine detection data used for training BANTER models ####
# by Yvonne Barkley
# March 28, 2024

# load libraries
library(ggplot2)

# load Acoustic Study, 'AcStudy_Pc'
load(file = here('data/Acoustic_Studies/TA_AcStudies_Pc_MHIUNK_DupsCombd.rdata') )


## 1a. Edit Acoustic Study for BANTER

# First, make any changes to the Acoustic Study if needed.
# For example, change the event IDs for any combined events
AcStudy_Pc@events[["1641325a_1641325b"]]@id <- "1641325"
AcStudy_Pc@events[["1303088a_1303088b"]]@id <- "1303088"

## 1b. Convert Acoustic Study to BANTER format

banter_sub <- export_banter(c(events(AcStudy_Pc)), dropSpecies =  dropStk)

## 1c. Filter banter data

for(d in c(2,3,4)){  #these are the click detectors 1,2,3 from Pamguard
  banter_sub$detectors[[d]]<-filter(banter_sub$detectors[[d]],BW_10dB > 5)
  banter_sub$detectors[[d]]<-filter(banter_sub$detectors[[d]],peak > 5 & peak < 80)
  banter_sub$detectors[[d]]<-filter(banter_sub$detectors[[d]],duration > 2 & duration < 1000)
}

#Remove clicks in individual detectors
banter_sub$detectors$Click_Detector_1<-filter(banter_sub$detectors$Click_Detector_1, peak> 2 & peak<15)
banter_sub$detectors$Click_Detector_2<-filter(banter_sub$detectors$Click_Detector_2, peak>15 & peak<30)
banter_sub$detectors$Click_Detector_3<-filter(banter_sub$detectors$Click_Detector_3, peak>30 & peak<50)


## 2. Organize and manipulate training data before plotting

# Make table of total calls per detector per event using the banter list
# Can look at this to get an idea of where NA's exist
dets_totals_table <- as.data.frame(bind_rows(sapply(banter_sub$detectors, function(x) table(x$event.id)), .id = "list_name"))

# This helps handle the event IDs since they contain only numbers
numsOnly <- colnames(dets_totals_table)[grep("^\\d+$", colnames(dets_totals_table))]

# Convert table to long format dataframe
dets_total_long <- as.data.frame(pivot_longer(dets_totals_table, cols = all_of(numsOnly), names_to = "EventID", values_to = "Total_Detections"))

colnames(dets_total_long)[1] <- "detector"


## 3. Make plots of the total detections per detector per event
 
# Plot data with NA included
plot <- ggplot(dets_total_long, aes(x = EventID, y = Total_Detections) ) +
  geom_point( size = .5 ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 6)) +
  facet_wrap(~ detector, scales = "free_y") 
  
  plot 
  
# Save Plot
ggsave( 'Total_Detections.png', plot = plot, width = 10, height = 6, dpi = 300)  

 