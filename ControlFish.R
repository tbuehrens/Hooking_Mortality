#Cowlitz  10/15/2021


## packages
require(reshape2)
require(plyr)
require(tidyr)
require(lubridate)
require(shapes)



#### file is direct export from 'All Captures Query' in Access DB 
capture.df <- read.csv("C:/Users/tarab/Mount Hood Environ. Dropbox/Client Access/WDFW - Cowlitz DB/All Captures Query.csv")
head(capture.df)


capture.df$Year = year(mdy(as.character(capture.df$Survey_Date)))
capture.df$Month = month(mdy(as.character(capture.df$Survey_Date)))
capture.df$jdate = julian(mdy(as.character(capture.df$Survey_Date)))
colnames(capture.df)


##rename capcode
levels(as.factor(capture.df$Capcode))
capture.df$Capture_Type = ifelse(capture.df$Capcode=="0", "No_Captures",
                                 ifelse(capture.df$Capcode=="1", "New_Fish",
                                    ifelse(capture.df$Capcode=="2", "Recapture",
                                      ifelse(capture.df$Capcode=="3", "Natural_Origin",
                                        ifelse(capture.df$Capcode=="5", "New_Fish","Lost"))))) #all variants of Capcode 4 were lost (we may want to differentiate for some analyses)

####define initial capture as a control or treatment
firstcapture = subset(capture.df[,c(3:6,33,34)], Capcode=="1"|Capcode =="5")  #these are initial captures and excludes Capcode 3s and 4s                                  

firstcapture$Treatment = ifelse(firstcapture$Survey_type=="Control Release TP"|firstcapture$Survey_type=="Control Release MHE"|firstcapture$Survey_type=="Control Recycle", "Control",
                                ifelse(firstcapture$Survey_type=="Angling", "Treatment","Other"))

#Add run year to initial capture event
firstcapture$run_year = ifelse(as.numeric(firstcapture$Year=="2017")&as.numeric(firstcapture$Month>8)&firstcapture$SpeciesCode=="COHO","2017",
                               ifelse(as.numeric(firstcapture$Year=="2018")&as.numeric(firstcapture$Month<5)&firstcapture$SpeciesCode=="COHO","2017",
                                      ifelse(as.numeric(firstcapture$Year=="2018")&as.numeric(firstcapture$Month>8)&firstcapture$SpeciesCode=="COHO","2018",
                                             ifelse(as.numeric(firstcapture$Year=="2019")&as.numeric(firstcapture$Month<5)&firstcapture$SpeciesCode=="COHO","2018",
                                                    ifelse(as.numeric(firstcapture$Year=="2019")&as.numeric(firstcapture$Month>8)&firstcapture$SpeciesCode=="COHO","2019",
                                                           ifelse(as.numeric(firstcapture$Year=="2020")&as.numeric(firstcapture$Month<5)&firstcapture$SpeciesCode=="COHO","2019",
                                                                  ifelse(as.numeric(firstcapture$Year=="2017")&firstcapture$SpeciesCode=="STLHD","2017",
                                                                         ifelse(as.numeric(firstcapture$Year=="2018")&firstcapture$SpeciesCode=="STLHD","2018",
                                                                                ifelse(as.numeric(firstcapture$Year=="2019")&firstcapture$SpeciesCode=="STLHD","2019",
                                                                                       ifelse(as.numeric(firstcapture$Year=="2020")&as.numeric(firstcapture$Month<2)&firstcapture$SpeciesCode=="STLHD","2019",
                                                                                              ifelse(as.numeric(firstcapture$Year=="2020")&as.numeric(firstcapture$Month>2)&firstcapture$SpeciesCode=="STLHD","2020",as.numeric(firstcapture$Year))))))))))))







df1 = merge(firstcapture[,c(2,7,8)], capture.df, by="Fish_ID", all=TRUE) 
tail(df1)


#control fish dataframe
ctrl.df = subset(df1, Treatment=="Control")


#may want to filter out multiple recapture events; see levels below (i.e. seperator + brood)
levels(as.factor(ctrl.df$Survey_type))


