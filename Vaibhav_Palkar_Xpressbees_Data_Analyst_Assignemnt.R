## -------------- Xpressbees Data Analyst Assignemnt --------------- ##

#########################################################################################################
#                                           Business objective                                          #

# Exploratory Data Analysis for organization to get insight and to improve the business process
# FInd out observation and provide suggestions, Recommendations and Future scope aligning your objectives defined.

#########################################################################################################
#                                           DATA UNDERSTANDING                                          #

# Here we have data for typical shipment flow from origin to destination location
# We have different variable's as below

# Lanes	- Shipment travelling in different Lanes Metro, North East, ROI, State, ZOne
# Shipping ID	 - Unique ID for shipment
# Hubs - Unique ID for origin/destination
# Hub City/Hub Zone	- City & Zone of Hubs
# Process_Location - Touch Points of the shipment in its movement
# Process_Location/ Origin/Destination _Baginscandate -	Shipment scanned for date time once reaches the Touch Point/Origin/Destination
# Process_Location/ Origin/Destination _Bagoutscandate - Shipment scanned for date time once it moves out from Touch Point/Origin/Destination
# DeliveryDate - shipment delivery date time
# Physical wt/vol wt - physical wt in kg/vol wt in mtr.cube
# Ship Pincode - Destination Pincode
# Route Mode - Mode in which shipment has travelled

#########################################################################################################
#                       Data Collection & Data Cleaning & Data Preparation                              #

# We have shipment data and have converted xlxs file to csv format for reading

# Read data
xb_data <- read.csv("Assignment_Xpressbees21_19.csv", header = T, stringsAsFactors = F)
str(xb_data) # 25000 observation and 29 variables

summary(xb_data)

# lets check NA values
sum(is.na(xb_data)) # 0 NA values found

# lets check for blank values
blank_values <- sapply(xb_data, function(x) length(which(x == "")))
blank_values # 0 blank values

# lets check for Null values
null_values <- sapply(xb_data, function(x) length(which(x == "NULL")))
null_values # 259 blank values

# ProcessLocation3_BagOutScanDate - 7
# Destination_BagOutScanDate - 252

# lets check duplicate values in shippind ID
duplicate_ship_id <- sum(duplicated(xb_data$Shipping.ID)) # all shipping ID's are unique

# here route mode AIR and Air is same, so R is case sensitive so lets handle it
xb_data[which(xb_data$routemode == "AIR"), ]$routemode <- "Air"
table(xb_data$routemode)
# here we have time and date format of our shipment data for origine, touch point and destination
# lets convert date and time as per our required data format Ymd hms

library(lubridate)
# lets convert the date in r date format using parsed date function

# lets convert ProcessLocation1_InscanDate to stanadard date format 
xb_data$ProcessLocation1_InscanDate <- parse_date_time(xb_data$ProcessLocation1_InscanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation1_BagOutScanDate to stanadard date format 
xb_data$ProcessLocation1_BagOutScanDate <- parse_date_time(xb_data$ProcessLocation1_BagOutScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation2_BagInScanDate to stanadard date format 
xb_data$ProcessLocation2_BagInScanDate <- parse_date_time(xb_data$ProcessLocation2_BagInScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation2_BagOutScanDate to stanadard date format 
xb_data$ProcessLocation2_BagOutScanDate <- parse_date_time(xb_data$ProcessLocation2_BagOutScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation3_BagInScanDate to stanadard date format 
xb_data$ProcessLocation3_BagInScanDate <- parse_date_time(xb_data$ProcessLocation3_BagInScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation3_BagOutScanDate to stanadard date format 
xb_data$ProcessLocation3_BagOutScanDate <- parse_date_time(xb_data$ProcessLocation3_BagOutScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# here we have 7 NULL values for ProcessLocation3_BagOutScanDate
ProcessLocation3_BagOutScanDate_blank <- xb_data[which(is.na(xb_data$ProcessLocation3_BagOutScanDate)), ]

# lets convert ProcessLocation4_BagInScanDate to stanadard date format 
xb_data$ProcessLocation4_BagInScanDate <- parse_date_time(xb_data$ProcessLocation4_BagInScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert ProcessLocation4_BagOutScanDate to stanadard date format 
xb_data$ProcessLocation4_BagOutScanDate <- parse_date_time(xb_data$ProcessLocation4_BagOutScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert Destination_BagInScanDate to stanadard date format 
xb_data$Destination_BagInScanDate <- parse_date_time(xb_data$Destination_BagInScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert Destination_BagOutScanDate to stanadard date format 
xb_data$Destination_BagOutScanDate <- parse_date_time(xb_data$Destination_BagOutScanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# here we have 252 NULL values for ProcessLocation3_BagOutScanDate
Destination_BagOutScanDate_blank <- xb_data[which(is.na(xb_data$Destination_BagOutScanDate)), ]

# lets convert DeliveryDate to stanadard date format 
xb_data$DeliveryDate <- parse_date_time(xb_data$DeliveryDate, c("%m-%d-%Y", "%d-%m-%Y"))

# lets extract date from PocessLocation1_InscanDate
xb_data$shipstartdate <- format(xb_data$ProcessLocation1_InscanDate, "%Y-%m-%d")

########################################################################################################
#                                      Univariate Analysis                                             #

library(ggplot2)
library(dplyr)
## variable Lane
table(xb_data$lane)
# observation for each category
# Metro - 9601, North East - 106, ROI - 11323, State - 285, Zone - 3685

#ggplot(xb_data, aes(xb_data$lane, fill = lane)) + geom_bar() + geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="Lane", y="count") + ggtitle("Lane Vs total count")

ggplot(xb_data, aes(xb_data$lane, fill = lane)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="lane", y="shipment count") + ggtitle("lane Vs shipment count") +
  scale_x_discrete(limits = c("ROI", "Metro", "Zone", "State", "North East"))


# variable origin hub name
length(levels(as.factor(as.character(xb_data$OriginHubName))))
# there are total 97 hubs from where shipment has been started

#ggplot(xb_data, aes(xb_data$OriginHubName)) + geom_bar(fill = "red") + geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="OriginHubName", y="count") + ggtitle("OriginHubName Vs total count") + theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$OriginHubName)) + geom_bar(aes(y = (..count..)), fill = "red") +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubName", y="shipment count") + ggtitle("OriginHubName Vs shipment count")

origin_hubs_count <- summarise(group_by(xb_data, xb_data$OriginHubName), n())
colnames(origin_hubs_count) <- c("origin_hub", "count")

origin_hubs_count <- origin_hubs_count %>% arrange(desc(origin_hubs_count$count))

# top 6 starting point hubs from where max shipment happens by count
head(origin_hubs_count)
# 1 PNQ/CHK                  5421
# 2 BLR/FC1                  5143
# 3 Bhiwandi                 3599
# 4 Surat/HUB                2177
# 5 BLR/CNT                  1270
# 6 MAA/HUB                  1123

# bottom 6 starting point hubs from where min shipment happens by count
tail(origin_hubs_count)
# 1 SIKAR                       1
# 2 Sirsa                       1
# 3 TRP/SNR                     1
# 4 Tumkur                      1
# 5 Udhampur                    1
# 6 VPI/HUB                     1

# variable origin hub city
length(levels(as.factor(as.character(xb_data$OriginHubCity))))
# there are total 85 cities from where shipment has been started

#ggplot(xb_data, aes(xb_data$OriginHubCity)) + geom_bar(fill = "red") + geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="OriginHubCity", y="count") + ggtitle("OriginHubCity Vs total count") + theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$OriginHubCity)) + geom_bar(aes(y = (..count..)), fill = "red") +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("OriginHubCity Vs shipment count")


origin_hubs_city_count <- summarise(group_by(xb_data, xb_data$OriginHubCity), n())
colnames(origin_hubs_city_count) <- c("origin_hub_city", "count")

origin_hubs_city_count <- origin_hubs_city_count %>% arrange(desc(origin_hubs_city_count$count))

# top 6 starting point hubs cities from where max shipment happens by count
head(origin_hubs_city_count)

# 1 BANGALORE                6413
# 2 PUNE                     5666
# 3 Bhiwandi                 3599
# 4 Surat                    2177
# 5 Chennai                  1123
# 6 Panipat                  1058

# bottom 6 starting point hubs cities from where min shipment happens by count
tail(origin_hubs_city_count)

# 1 Sikar                       1
# 2 Sirsa                       1
# 3 Somanur                     1
# 4 Tumkur                      1
# 5 Udhampur                    1
# 6 Vapi                        1

# variable origin hub zone name
table(xb_data$OriginHubZoneName)
# observation for each origin zone
# Central - 147, East - 388, North - 3970, North East - 106, South-1 - 1440, South-2 - 6452, West - 12497

#ggplot(xb_data, aes(xb_data$OriginHubZoneName, fill = OriginHubZoneName)) + geom_bar() + geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="Origin Hub Zone Name", y="count") + ggtitle("Origin Hub Zone Name Vs total count")

ggplot(xb_data, aes(xb_data$OriginHubZoneName, fill = OriginHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubZoneName", y="shipment count") + ggtitle("OriginHubZoneName Vs shipment count") +
  scale_x_discrete(limits = c("West", "South-2", "North", "South-1", "East", "Central", "North East"))

# variable ship pin code (destination pin code)
length(levels(as.factor(xb_data$ShipPinCode))) # there are total 153 ship pin code
shipping_destination_pin <- summarise(group_by(xb_data, xb_data$ShipPinCode), n())
colnames(shipping_destination_pin) <- c("shippincode", "count")
shipping_destination_pin <- shipping_destination_pin %>% arrange(desc(shipping_destination_pin$count))

# top 6 shipping destinations pincode where max shipment happened by count
head(shipping_destination_pin)

# 1      201301  1774
# 2      122001  1499
# 3      122002   741
# 4      110059   622
# 5      110085   620
# 6      110092   617

# bottom 6 destinations pincode where min shipment happened by count
tail(shipping_destination_pin)

# 1      110079     1
# 2      110090     1
# 3      121101     1
# 4      122010     1
# 5      201019     1
# 6      201302     1

# variable shipment status
table(xb_data$ShipmentStatus) # all shipments are delivered

ggplot(xb_data, aes(xb_data$ShipmentStatus, fill = ShipmentStatus)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="ShipmentStatus", y="shipment count") + ggtitle("Shipment Status")

# variable route mode
table(xb_data$routemode)

#ggplot(xb_data, aes(xb_data$routemode, fill = routemode)) + geom_bar() + geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="route mode", y="count") + ggtitle("routemode Vs total count")

ggplot(xb_data, aes(xb_data$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface", "Air"))
# observations for each mode
# Air - 8249, Surface - 16751
# most of the shipments are happened by surface route

# variable origin hub name and hub city
length(levels(as.factor(as.character(xb_data$Origin_HubName_HubCity)))) # 97 levels which is correct

Origin_HubName_HubCity_count <- summarise(group_by(xb_data, xb_data$Origin_HubName_HubCity), n())
colnames(Origin_HubName_HubCity_count) <- c("Origin_HubName_HubCity", "count")

Origin_HubName_HubCity_count <- Origin_HubName_HubCity_count %>% arrange(desc(Origin_HubName_HubCity_count$count))

# variable ProcessLocation2 hub and city name
table(xb_data$ProcessLocation2_HubName_HubCity)
# there are total 31 processlocations2_points
#ggplot(xb_data, aes(xb_data$ProcessLocation2_HubName_HubCity)) + geom_bar(fill = "red") +
#  geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="ProcessLocation2_HubName_HubCity", y="count") + ggtitle("ProcessLocation2_HubName_HubCity Vs total count") + 
#  theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$ProcessLocation2_HubName_HubCity, fill = ProcessLocation2_HubName_HubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="ProcessLocation2_HubName_HubCity", y="shipment count") + ggtitle("ProcessLocation2_HubName_HubCity Vs total count")


ProcessLocation2_HubName_HubCity_data <- summarise(group_by(xb_data, xb_data$ProcessLocation2_HubName_HubCity), n()) 
colnames(ProcessLocation2_HubName_HubCity_data) <- c("process_loc2_hub_city", "count")
ProcessLocation2_HubName_HubCity_data <- ProcessLocation2_HubName_HubCity_data %>% arrange(desc(ProcessLocation2_HubName_HubCity_data$count))
# most of the shipping processing at DEL/LH1_GURGAON       15201

# touch points/process location 2 where max time shipment has been process
head(ProcessLocation2_HubName_HubCity_data)
# 1 DEL/LH1_GURGAON       15201
# 2 BOM/HUB_Mumbai         2739
# 3 DEL/APT_Delhi NCR      2003
# 4 DEL/HUB-TSP_DELHI NCR  1584
# 5 BLR/HUB_BANGALORE      1058
# 6 Bhiwandi_BHIWANDI       977

#  variable ProcessLocation3 hub and city name
length(levels(as.factor(as.character(xb_data$ProcessLocation3_HubName_HubCity)))) # total 15 process location 3 points
table(xb_data$ProcessLocation3_HubName_HubCity)

#ggplot(xb_data, aes(xb_data$ProcessLocation3_HubName_HubCity)) + geom_bar(fill = "red") +
#  geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="ProcessLocation3_HubName_HubCity", y="count") + ggtitle("ProcessLocation3_HubName_HubCity Vs total count") + 
#  theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$ProcessLocation3_HubName_HubCity, fill = ProcessLocation3_HubName_HubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="ProcessLocation3_HubName_HubCity", y="shipment count") + ggtitle("ProcessLocation3_HubName_HubCity Vs total count")

# most of the shipping processing at IB2_GURGAON 16127

# touch points/process location 3 where max time shipment has been process
# IB2_GURGAON           16127
# DEL/APT_Delhi NCR     3673
# DEL/LH1_GURGAON       2438
# DEL/IB1_DELHI NCR     2233

# variable processLocation4 hub and city name
length(levels(as.factor(as.character(xb_data$ProcessLocation4_HubName_HubCity)))) # total 20 process location 3 points
table(xb_data$ProcessLocation4_HubName_HubCity)

#ggplot(xb_data, aes(xb_data$ProcessLocation4_HubName_HubCity)) + geom_bar(fill = "red") +
#  geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="ProcessLocation4_HubName_HubCity", y="count") + ggtitle("ProcessLocation4_HubName_HubCity Vs total count") + 
#  theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$ProcessLocation4_HubName_HubCity, fill = ProcessLocation4_HubName_HubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="ProcessLocation4_HubName_HubCity", y="shipment count") + ggtitle("ProcessLocation4_HubName_HubCity Vs total count")

# most of the shipping processing at DEL/IB1_DELHI NCR 16313

# touch points/process location 3 where max time shipment has been process
# DEL/IB1_DELHI NCR           16313
# DEL/WDL_Delhi NCR           4778
# DEL/OKH_Delhi               3047

# variable destination hub name
length(levels(as.factor(as.character(xb_data$DestinationHubName))))
# there are total 37 hubs from where shipment has been ended

#ggplot(xb_data, aes(xb_data$DestinationHubName)) + geom_bar(fill = "red") +
#  geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="DestinationHubName", y="count") + ggtitle("DestinationHubName Vs total count") + 
#  theme(axis.text.x = element_text(face="bold", size=5, angle=90))

ggplot(xb_data, aes(xb_data$DestinationHubName, fill = DestinationHubName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubName", y="shipment count") + ggtitle("DestinationHubName Vs total count")

destination_hubs_count <- summarise(group_by(xb_data, xb_data$DestinationHubName), n())
colnames(destination_hubs_count) <- c("destination_hub", "count")

destination_hubs_count <- destination_hubs_count %>% arrange(desc(destination_hubs_count$count))

# top 6 destination point hubs where max shipment happens by count
head(destination_hubs_count)

# 1 NOIDA            1926
# 2 DEL/ISR          1505
# 3 DEL/BGN          1167
# 4 DEL/DLF          1139
# 5 DEL/GZB          1116
# 6 DEL/KMP          1090

# bottom 6 destination point hubs where min shipment happens by count
tail(destination_hubs_count)

# 1 DEL/MGK           339
# 2 GZB/MHN           334
# 3 DEL/JML           293
# 4 DEL/NFG           211
# 5 DEL/KWN           184
# 6 DEL/IB1             9

# variable destination city name
length(levels(as.factor(as.character(xb_data$DestinationHubCity))))
# there are total 7 cities from where shipment has been started


#ggplot(xb_data, aes(xb_data$DestinationHubCity, fill = DestinationHubCity)) + geom_bar() +
#  geom_text(stat = 'count', aes(label =..count..), vjust=-0.1) +
#  labs(x="DestinationHubCity", y="count") + ggtitle("DestinationHubCity Vs total count")

ggplot(xb_data, aes(xb_data$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5) +
  labs(x="DestinationHubCity", y="shipment count") + ggtitle("DestinationHubCity Vs total count")
# most number of shipments destination hub city is Delhi NCR and New Delhi

destination_hubs_city_count <- summarise(group_by(xb_data, xb_data$DestinationHubCity), n())
colnames(destination_hubs_city_count) <- c("destination_hub_city", "count")

destination_hubs_city_count <- destination_hubs_city_count %>% arrange(desc(destination_hubs_city_count$count))

# all 7 destination point city cities from where number of shipments happens by count
head(destination_hubs_city_count,7)

# 1 Delhi NCR             8908
# 2 New Delhi             7725
# 3 Noida                 3314
# 4 Ghaziabad             2048
# 5 Gurugram              1139
# 6 Faridabad             1066
# 7 Gurgaon                800

# variable destination hub zone name
table(xb_data$DestinationHubZoneName) # al destination are in North zone

# variable physical wieght
length(levels(as.factor(xb_data$PhysicalWeight))) # there are 537 different weights
table(xb_data$PhysicalWeight)
summary(xb_data$PhysicalWeight)
# physical weight is ranging from 0 to 34
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2200  0.3700  0.7886  0.6300 34.0000 

phy_wt <- summarise(group_by(xb_data, xb_data$PhysicalWeight), n())
colnames(phy_wt) <- c("physcial_wt", "count")
phy_wt <- phy_wt %>% arrange(desc(phy_wt$count))

head(phy_wt)
# maximum shipments count by same physical weights maximum times 
# 1        0.35   897
# 2        0.15   762
# 3        0.49   635
# 4        0.25   631
# 5        0.2    508
# 6        0.1    482

# variable volumetric wight
length(levels(as.factor(xb_data$VolumetricWeight))) # there are 651 different weights
table(xb_data$VolumetricWeight)
summary(xb_data$VolumetricWeight)
# physical weight is ranging from 0 to 70.40

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2700  0.5000  0.9618  1.0500 70.4000 

vol_wt <- summarise(group_by(xb_data, xb_data$VolumetricWeight), n())
colnames(vol_wt) <- c("vol_wt", "count")

vol_wt$count <- as.double(vol_wt$count)
vol_wt <- vol_wt %>% arrange(desc(vol_wt[count]))

summary(as.numeric(xb_data$VolumetricWeight))

head(vol_wt)
# maximum shipments count by same volumetric weights maximum times 
# 1   0.33   585
# 2   0.1    514
# 3   1.17   372
# 4   2      372
# 5   0.78   370
# 6   0.23   318

## Lets find the time total time of each shipment and time difference between process touch point
View(xb_data)
xb_data$total_shipping_time_days <- abs(round(difftime(xb_data$shipstartdate, xb_data$DeliveryDate, units = "days"), 0))
?difftime()
#########################################################################################################

# lets get the time difference in descending order
xb_data_delivery_time <- xb_data[, c(1, 4, 6, 8, 24, 30, 27, 31)]
View(xb_data_delivery_time)

xb_data_delivery_time <- xb_data_delivery_time %>% arrange(desc(xb_data_delivery_time$total_shipping_time_days))

table(xb_data_delivery_time$total_shipping_time_days)

# lets check number of shipment for different days of deliveries by plottting graph

#ggplot(xb_data_delivery_time, aes(as.factor(xb_data_delivery_time$total_shipping_time_days), fill = as.factor(total_shipping_time_days))) + geom_bar() + 
#  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + labs(x="days for shipment", y="number of shipments") + ggtitle("Number of shipments in days")


ggplot(xb_data_delivery_time,aes(as.factor(xb_data_delivery_time$total_shipping_time_days), fill = as.factor(xb_data_delivery_time$total_shipping_time_days))) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5) +
  labs(x="days for shipment", y="number of shipments(%)") + ggtitle("Number of shipments in days")

# average shipment time in days 
mean(xb_data_delivery_time$total_shipping_time_days) # 3.55612 days

#geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

# here maximum shipping have taken 1 to 6 days to be delivered for 24480 shipments
24480/25000*100 # 97.62 % shipments are delivered within 6 days

# and 520 shipments have taken more than 6 days
520/25000*100 # 2.08 % shipments are delivered in more than 6 days
#########################################################################################################

## lets find the time taken in each touch point
# lets get ProcessLocation1 touch point time difference for in and out time
xb_data$ProcessLocation1_time_diff <- abs(round(difftime(xb_data$ProcessLocation1_InscanDate, xb_data$ProcessLocation1_BagOutScanDate, units = "hours"), 2))

# lets get ProcessLocation2 touch point time difference for in and out time
xb_data$ProcessLocation2_time_diff <- abs(round(difftime(xb_data$ProcessLocation2_BagInScanDate, xb_data$ProcessLocation2_BagOutScanDate, units = "hours"), 2))

# lets get ProcessLocation3 touch point time difference for in and out time
xb_data$ProcessLocation3_time_diff <- abs(round(difftime(xb_data$ProcessLocation3_BagInScanDate, xb_data$ProcessLocation3_BagOutScanDate, units = "hours"), 2))

# lets get ProcessLocation4 touch point time difference for in and out time
xb_data$ProcessLocation4_time_diff <- abs(round(difftime(xb_data$ProcessLocation4_BagInScanDate, xb_data$ProcessLocation4_BagOutScanDate, units = "hours"), 2))

# lets get destination processing point time difference for in and out time
xb_data$Destination_time_diff <- abs(round(difftime(xb_data$Destination_BagInScanDate, xb_data$Destination_BagOutScanDate, units = "hours"), 2))

## lets arrange xb_data again in new data.frame xb_data_2
xb_data_2 <- xb_data[, c(1:11, 32, 12:14, 33, 15:17, 34, 18:20, 35, 21, 22, 26, 36, 30, 27:29, 23:25, 31)]

##########################################################################################################
###### Time difference analysis for each origin/touch point/destination point
library(scales)

## lets check histgram for ProcessLocation1_time_diff
ggplot(xb_data_2, aes(xb_data_2$ProcessLocation1_time_diff)) + geom_histogram(bins = 20, fill = "coral", col = "black", aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 260, by = 20)) + 
  labs(x="processing time in Hr", y="percentage of shipments") + ggtitle("process location 1 shipments processing time in hours")


summary(as.numeric(xb_data_2$ProcessLocation1_time_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.07    5.30    9.63   12.53   15.08  259.93
# here for most of the shipments processing time is between 0 to 48 hrs

PL1_time_diff_more_2days <- filter(xb_data_2, xb_data_2$ProcessLocation1_time_diff > 48)
# and very few(318) shipments have taken processing time more than 48 hr(upto 260)

## lets check histgram for ProcessLocation2_time_diff
ggplot(xb_data_2, aes(xb_data_2$ProcessLocation2_time_diff)) + geom_histogram(bins = 20, fill = "blue", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 74, by = 10)) + 
  labs(x="processing time in Hr", y="percentage of shipments") + ggtitle("process location 2 shipments processing time in hours")

summary(as.numeric(xb_data_2$ProcessLocation2_time_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.020   0.730   1.520   3.088   3.400  73.550
# here for most of the shipments processing time is between 0 to 24 hrs

PL2_time_diff_more_1days <- filter(xb_data_2, xb_data_2$ProcessLocation2_time_diff > 24)
# and very few(207) shipments have taken processing time more than 24 hr(upto 74)

## lets check histgram for ProcessLocation3_time_diff
ggplot(xb_data_2, aes(x=xb_data_2$ProcessLocation3_time_diff)) + geom_histogram(bins = 30, fill = "purple", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 409, by = 40)) + 
  labs(x="processing time in Hr", y="percentage of shipments") + ggtitle("process location 3 shipments processing time in hours")

#scale_y_continuous(labels = percent)
summary(as.numeric(xb_data_2$ProcessLocation3_time_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   3.020   6.120   6.734   8.880 408.120       7
# here for most of the shipments processing time is between 0 to 24 hrs

PL3_time_diff_more_1days <- filter(xb_data_2, xb_data_2$ProcessLocation3_time_diff > 24)
# and very few(78) shipments have taken processing time more than 24 hr(409)


## lets check histgram for ProcessLocation4_time_diff
ggplot(xb_data_2, aes(x=xb_data_2$ProcessLocation4_time_diff)) + geom_histogram(bins = 30, fill = "orange", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 180, by = 20)) + 
  labs(x="processing time in Hr", y="percentage of shipments") + ggtitle("process location 4 shipments processing time in hours")

summary(as.numeric(xb_data_2$ProcessLocation4_time_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.880   2.130   3.358   4.030 174.650 
# here for most of the shipments processing time is between 0 to 24 hrs

PL4_time_diff_more_1days <- filter(xb_data_2, xb_data_2$ProcessLocation4_time_diff > 24)
# and very few(142) shipments have taken processing time more than 24 hr(409)

## lets check histgram for destination_time_diff
ggplot(xb_data_2, aes(x=xb_data_2$Destination_time_diff)) + geom_histogram(bins = 30, fill = "yellow", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 120, by = 10)) + 
  labs(x="processing time in Hr", y="percentage of shipments") + ggtitle("destination shipments processing time in hours")

summary(as.numeric(xb_data_2$Destination_time_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   1.200   2.170   3.396   3.070 116.400     252 

destination_time_diff_more_1days <- filter(xb_data_2, xb_data_2$Destination_time_diff > 24)
# and very few(346) shipments have taken processing time more than 24 hr(409)

### lets check average processing time at origine/touchpoint/destination and average delivery days

# avg processing time at processing location 1
mean(xb_data_2$ProcessLocation1_time_diff) # 12.53307 hours

# avg processing time at processing location 2
mean(xb_data_2$ProcessLocation2_time_diff) # 3.088276 hours

# avg processing time at processing location 3
mean(xb_data_2$ProcessLocation3_time_diff, na.rm = TRUE) # 6.734173 hours

# avg processing time at processing location 4
mean(xb_data_2$ProcessLocation4_time_diff) # 3.357794 hours

# avg processing time at destination
mean(xb_data_2$Destination_time_diff, na.rm = TRUE) # 3.395911 hours

# avg delivery time days
mean(xb_data_2$total_shipping_time_days) # 3.55612 days

#########################################################################################################
####### lets check the time taken to reach each point

xb_data_2$pl1_pl2_time <- abs(round(difftime(xb_data_2$ProcessLocation1_BagOutScanDate, xb_data_2$ProcessLocation2_BagInScanDate, units = "days"),2))

xb_data_2$pl2_pl3_time <- abs(round(difftime(xb_data_2$ProcessLocation2_BagOutScanDate, xb_data_2$ProcessLocation3_BagInScanDate, units = "days"),2))

xb_data_2$pl3_pl4_time <- abs(round(difftime(xb_data_2$ProcessLocation3_BagOutScanDate, xb_data_2$ProcessLocation4_BagInScanDate, units = "days"),2))

xb_data_2$pl4_dest_time <- abs(round(difftime(xb_data_2$ProcessLocation4_BagOutScanDate, xb_data_2$Destination_BagInScanDate, units = "days"),2))

### lets plot histogram for time between touch points
# touch point 1 and 2
ggplot(xb_data_2, aes(x=xb_data_2$pl1_pl2_time)) + geom_histogram(bins = 8, fill = "brown", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(x="processing time in days", y="percentage of shipments") + ggtitle("process loc 1 to 2 shipping time in days")

summary(as.numeric(xb_data_2$pl1_pl2_time))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.420   1.310   1.297   2.180   7.980

pl1_pl2_time_more_3days <- filter(xb_data_2, xb_data_2$pl1_pl2_time > 3)
# and very few(422) shipments have taken processing time more than 3 days

# touch point 2 and 3
ggplot(xb_data_2, aes(x=xb_data_2$pl2_pl3_time)) + geom_histogram(bins = 8, fill = "violet", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 8, by = 2)) + 
  labs(x="processing time in days", y="percentage of shipments") + ggtitle("process loc 2 to 3 shipping time in days")

summary(as.numeric(xb_data_2$pl2_pl3_time))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0300  0.0800  0.2875  0.2900  6.3500

pl2_pl3_time_more_1days <- filter(xb_data_2, xb_data_2$pl2_pl3_time > 1)
# and very few(2025) shipments have taken processing time more than 1 day

# touch point 3 and 4
ggplot(xb_data_2, aes(x=xb_data_2$pl3_pl4_time)) + geom_histogram(bins = 8, fill = "darkgreen", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 16, by = 2)) + 
  labs(x="processing time in days", y="percentage of shipments") + ggtitle("process loc 3 to 4 shipping time in days")

summary(as.numeric(xb_data_2$pl3_pl4_time))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.0700  0.1400  0.2913  0.2600 14.9800       7

pl3_pl4_time_more_1days <- filter(xb_data_2, xb_data_2$pl3_pl4_time > 1)
# and very few(236) shipments have taken processing time more than 1 day

# touch point 4 and destination
ggplot(xb_data_2, aes(x=xb_data_2$pl4_dest_time)) + geom_histogram(bins = 4, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 8, by = 2)) + 
  labs(x="processing time in days", y="percentage of shipments") + ggtitle("process loc 4 to destination shipping time in days")

summary(as.numeric(xb_data_2$pl4_dest_time))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0600  0.0900  0.1436  0.1300  7.8200

pl4_dest_time_more_1days <- filter(xb_data_2, xb_data_2$pl4_dest_time > 1)
# and very few(226) shipments have taken processing time more than 1 day

## lets find mean time in days for shipping between each touch point

# avg shipping time between touch point 1 and 2
mean(xb_data_2$pl1_pl2_time) # 1.297318 days

# avg shipping time between touch point 2 and 3
mean(xb_data_2$pl2_pl3_time) # 0.28755 days

# avg shipping time between touch point 3 and 4
mean(xb_data_2$pl3_pl4_time, na.rm = TRUE) # 0.2913396 days

# avg shipping time between touch point 4 and destination
mean(xb_data_2$pl4_dest_time) # 0.143574 days

#########################################################################################################
# lets plot the boxplot for time difference between processing location and shipping between location/hubs

test_box_data <- xb_data_2[,c(12, 16, 20, 24, 28)]
test_box_data <- sapply(test_box_data, function(x) round((x)/24,2))

test_box_data2 <- xb_data_2[,c(1, 2, 4, 5, 34, 36:40)]

test_box_data <- cbind(test_box_data2, test_box_data)

## here we will try to find how much tume has taken for processing in touch point and shipping between locationa and total shipping time

# box plot for lane and destination shipping in days distribution

ggplot(test_box_data, aes(test_box_data$lane, test_box_data$total_shipping_time_days, color = lane)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 24, by = 2)) + labs(x="Lane", y="shipment time in days") + 
  ggtitle("Lane vs shipment time distribution in days")

ggplot(test_box_data, aes(test_box_data$DestinationHubCity, test_box_data$total_shipping_time_days, color = DestinationHubCity)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 24, by = 2)) + labs(x="destination hub city", y="days") + 
  ggtitle("destination hub city vs shipment time distribution in days")

boxplot(test_box_data$ProcessLocation1_time_diff~test_box_data$ProcessLocation4_time_diff, data=test_box_data, main="Different boxplots for process locations", xlab="ahipment counr", ylab="Degree Fahrenheit", col="orange", border="brown")

summary(as.numeric(test_box_data$total_shipping_time_days))

#########################################################################################################

# lets subset data based on route mode and then check 

# route mode Air
route_mode_air <- subset(xb_data_2, xb_data_2$routemode == "Air")

ggplot(route_mode_air, aes(route_mode_air$OriginHubZoneName, fill = OriginHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubZoneName", y="shipment count") + ggtitle("Air route OriginHubZoneName Vs shipment count") +
  scale_x_discrete(limits = c("West", "North", "South-1", "South-2", "East", "North East", "Central"))

ggplot(route_mode_air, aes(route_mode_air$OriginHubCity, fill = OriginHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("Air route OriginHubCity Vs shipment count")

ggplot(route_mode_air, aes(route_mode_air$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubCity", y="shipment count") + ggtitle("Air route DestinationHubCity Vs shipment count") +
  scale_x_discrete(limits = c("Delhi NCR", "New Delhi", "Noida", "Ghaziabad", "Gurugram", "Faridabad", "Gurgaon"))

ggplot(route_mode_air, aes(route_mode_air$DestinationHubZoneName, fill = DestinationHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubZoneName", y="shipment count") + ggtitle("Air route DestinationHubZoneName Vs shipment count")

# avg shipment time for air mode
mean(route_mode_air$total_shipping_time_days) # 2.314462 days

# route mode surfcae
route_mode_surface <- subset(xb_data_2, xb_data_2$routemode == "Surface")

ggplot(route_mode_surface, aes(route_mode_surface$OriginHubZoneName, fill = OriginHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubZoneName", y="shipment count") + ggtitle("surface route OriginHubZoneName Vs shipment count") +
  scale_x_discrete(limits = c("West",  "South-2", "North", "East", "South-1", "Central"))

ggplot(route_mode_surface, aes(route_mode_surface$OriginHubCity, fill = OriginHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("route surface OriginHubCity Vs shipment count")


ggplot(route_mode_surface, aes(route_mode_surface$DestinationHubZoneName, fill = DestinationHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubZoneName", y="shipment count") + ggtitle("route surface DestinationHubZoneName Vs shipment count")

ggplot(route_mode_surface, aes(route_mode_surface$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubCity", y="shipment count") + ggtitle("Surface route DestinationHubCity Vs shipment count") +
  scale_x_discrete(limits = c("Delhi NCR", "New Delhi", "Noida", "Ghaziabad", "Gurugram", "Faridabad", "Gurgaon"))

# avg shipment time for surface mode
mean(route_mode_surface$total_shipping_time_days) # 4.167572 days

########################################################################################################
#                     shippin days more than 6 and check origin and destination                        #

xb_data_shippind_days_more_6 <- filter(xb_data_2, xb_data_2$total_shipping_time_days > 6)

## lets find avg shipping time at touch points

# lets check what is mean shipping time
mean(xb_data_shippind_days_more_6$total_shipping_time_days) # avg shipping days are 13.34 days (3.75 times)

# lets check what is mean processing time at location 1
mean(xb_data_shippind_days_more_6$ProcessLocation1_time_diff) # avg shipping days are 17.56 hr (1.5 times)

# lets check what is mean processing time at location 2
mean(xb_data_shippind_days_more_6$ProcessLocation2_time_diff) # avg shipping days are 3.58 hr (1.15 times)

# lets check what is mean processing time at location 3
mean(xb_data_shippind_days_more_6$ProcessLocation3_time_diff) # avg shipping days are 8.67 hr (1.28 times)

# lets check what is mean processing time at location 4
mean(xb_data_shippind_days_more_6$ProcessLocation4_time_diff) # avg shipping days are 7.54 hr (2 times)

# lets check what is mean processing time at destination
mean(xb_data_shippind_days_more_6$Destination_time_diff, na.rm = TRUE) # avg shipping days are 4.58 hr (1.34 times)

## lets find mean time in days for shipping between each touch point

# avg shipping time between touch point 1 and 2
mean(xb_data_shippind_days_more_6$pl1_pl2_time) # 1.951173 days 1.5 times

# avg shipping time between touch point 2 and 3
mean(xb_data_shippind_days_more_6$pl2_pl3_time) # 0.4254423 days 1.5 times 

# avg shipping time between touch point 3 and 4
mean(xb_data_shippind_days_more_6$pl3_pl4_time, na.rm = TRUE) # 5.92775 days 20 times

# avg shipping time between touch point 4 and destination
mean(xb_data_shippind_days_more_6$pl4_dest_time) # 1.334788 days double

# only single NULL value found here in destination bag out

# lest check lane, origine hub/cities, origin and destination cities
ggplot(xb_data_shippind_days_more_6, aes(xb_data_shippind_days_more_6$lane, fill = lane)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="lane", y="shipment count") + ggtitle("lane Vs shipment count") +
  scale_x_discrete(limits = c("Metro", "ROI", "Zone", "State", "North East"))

ggplot(xb_data_shippind_days_more_6, aes(xb_data_shippind_days_more_6$OriginHubZoneName, fill = OriginHubZoneName)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubZoneName", y="shipment count") + ggtitle("OriginHubZoneName Vs shipment count") +
  scale_x_discrete(limits = c("West", "South-2", "North", "East", "South-1", "Central", "North East"))

ggplot(xb_data_shippind_days_more_6, aes(xb_data_shippind_days_more_6$OriginHubCity, fill = OriginHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("OriginHubCity Vs shipment count")

ggplot(xb_data_shippind_days_more_6, aes(xb_data_shippind_days_more_6$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubCity", y="shipment count") + ggtitle("DestinationHubCity Vs shipment count")

ggplot(xb_data_shippind_days_more_6, aes(xb_data_shippind_days_more_6$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface", "Air"))

# routmode data
table(xb_data_shippind_days_more_6$routemode)
# Air Surface 
# 58     462

xb_data_shippind_days_more_6_air <- filter(xb_data_shippind_days_more_6, xb_data_shippind_days_more_6$routemode == "Air")
mean(xb_data_shippind_days_more_6_air$total_shipping_time_days)  # 8.75 days

xb_data_shippind_days_more_6_surface <- filter(xb_data_shippind_days_more_6, xb_data_shippind_days_more_6$routemode == "Surface")
mean(xb_data_shippind_days_more_6_surface$total_shipping_time_days) # 13.91775 days

#########################################################################################################
## check for why there is more time(more than 6 days) in 3 and 4 touch point
shipping_pl3_and_pl4 <- filter(xb_data_2, xb_data_2$pl3_pl4_time > 6)

# lets understand trends by plots
summary(as.numeric(shipping_pl3_and_pl4$pl3_pl4_time))
summary(as.numeric(shipping_pl3_and_pl4$total_shipping_time_days))
# bar plots
ggplot(shipping_pl3_and_pl4, aes(shipping_pl3_and_pl4$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count")

ggplot(shipping_pl3_and_pl4, aes(shipping_pl3_and_pl4$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count")

# histogram
ggplot(shipping_pl3_and_pl4, aes(x=shipping_pl3_and_pl4$total_shipping_time_days)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 25, by = 2)) + 
  labs(x="shipping time (days)", y="percentage of shipments") + ggtitle("total shipping time in days distribution")

ggplot(shipping_pl3_and_pl4, aes(x=shipping_pl3_and_pl4$pl3_pl4_time)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 15, by = 2)) + 
  labs(x="shipping time (days)", y="percentage of shipments") + ggtitle("total shipping time in days distribution")

summary(as.numeric(shipping_pl3_and_pl4$PhysicalWeight))

ggplot(shipping_pl3_and_pl4, aes(x=shipping_pl3_and_pl4$PhysicalWeight)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 35, by = 2)) + 
  labs(x="physical wt", y="percentage of shipments") + ggtitle("physical wt distribution")


summary(as.numeric(shipping_pl3_and_pl4$VolumetricWeight))
ggplot(shipping_pl3_and_pl4, aes(x=shipping_pl3_and_pl4$VolumetricWeight)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 16, by = 2)) + 
  labs(x="volumetric wt", y="percentage of shipments") + ggtitle("volumetric wt distribution")

########################################################################################################
## lets check impact of shipments by physical weight
wt_more_than_4kg <- filter(xb_data_2, xb_data_2$PhysicalWeight > 4)

wt_more_than_4kg <- wt_more_than_4kg %>% arrange(desc(wt_more_than_4kg$PhysicalWeight))

ggplot(wt_more_than_4kg, aes(wt_more_than_4kg$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface","Air"))


ggplot(wt_more_than_4kg, aes(wt_more_than_4kg$lane, fill = lane)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="lane", y="shipment count") + ggtitle("lane Vs shipment count") +
  scale_x_discrete(limits = c("Metro","ROI", "Zone", "State"))

ggplot(wt_more_than_4kg, aes(x=wt_more_than_4kg$total_shipping_time_days)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 35, by = 2)) + 
  labs(x="shipping time (days)", y="percentage of shipments") + ggtitle("total shipping time in days distribution")

summary(as.numeric(wt_more_than_4kg$PhysicalWeight))

ggplot(wt_more_than_4kg, aes(x=wt_more_than_4kg$PhysicalWeight)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 35, by = 2)) + 
  labs(x="physical weight", y="percentage of shipments") + ggtitle("physical wt distribution")

# Here shipment lane is Metro and ROI by surface(distance between metros is in thousands) so more 
# transit time.
# 
# And all heavy shipments transit mode is surface because shipping through air leads to more 
# transportation cost, transport through the surface (like railway or other vehicle is good option).


########################################################################################################
# lets check impact of shipments by volumetric weight weight
vol_wt_more_than_5cub_mtr <- filter(xb_data_2, xb_data_2$VolumetricWeight > 5)

# vol_wt_more_than_5cub_mtr <- wt_more_than_cub_mtr %>% arrange(desc(wt_more_than_cub_mtr$VolumetricWeight))

ggplot(vol_wt_more_than_5cub_mtr, aes(vol_wt_more_than_5cub_mtr$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface","Air"))

ggplot(vol_wt_more_than_5cub_mtr, aes(vol_wt_more_than_5cub_mtr$lane, fill = lane)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="lane", y="shipment count") + ggtitle("lane Vs shipment count") +
  scale_x_discrete(limits = c("Metro","ROI", "Zone", "State"))

ggplot(vol_wt_more_than_5cub_mtr, aes(x=vol_wt_more_than_5cub_mtr$total_shipping_time_days)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 35, by = 2)) + 
  labs(x="shipping time (days)", y="percentage of shipments") + ggtitle("total shipping time in days distribution")

summary(as.numeric(wt_more_than_4kg$PhysicalWeight))

ggplot(wt_more_than_4kg, aes(x=wt_more_than_4kg$PhysicalWeight)) + geom_histogram(bins = 17, fill = "pink", col = "black", aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels = scales::percent) + scale_x_continuous(breaks = seq(0, 35, by = 2)) + 
  labs(x="physical weight", y="percentage of shipments") + ggtitle("physical wt distribution")

# For volumetric weight shipments with more than 4 cubic meter volume have took more than
# 6 days to be shipped
# 
# And all high volumetric weight shipment mode is surface because shipping through air leads 
# to more transportation cost.

#########################################################################################################
# need to analyse data for NULL values in process location bagout 3 and destination location bagout
NULL_xb_data <- read.csv("Assignment_Xpressbees21_19.csv", header = T, stringsAsFactors = F)

NULL_data <- sapply(NULL_xb_data, function(x) length(which(x == "NULL")))
NULL_data

NULL_data_frame <- filter(NULL_xb_data, NULL_xb_data$ProcessLocation3_BagOutScanDate == "NULL" | NULL_xb_data$Destination_BagOutScanDate == "NULL")

# lets convert ProcessLocation1_InscanDate to stanadard date format
NULL_data_frame$ProcessLocation1_InscanDate <- parse_date_time(NULL_data_frame$ProcessLocation1_InscanDate, c("%m-%d-%y %H:%M %p", "%d-%m-%Y %H:%M:%S"))

# lets convert DeliveryDate to stanadard date format 
NULL_data_frame$DeliveryDate <- parse_date_time(NULL_data_frame$DeliveryDate, c("%m-%d-%Y", "%d-%m-%Y"))

# lets extract date from PocessLocation1_InscanDate
NULL_data_frame$shipstartdate <- format(NULL_data_frame$ProcessLocation1_InscanDate, "%Y-%m-%d")

NULL_data_frame$total_shipping_time_days <- abs(round(difftime(NULL_data_frame$shipstartdate, NULL_data_frame$DeliveryDate, units = "days"), 0))
NULL_data_frame <- NULL_data_frame %>% arrange(desc(NULL_data_frame$total_shipping_time_days))

########################################################################################################

# Lets check trends for shiplemt lenght ROI
Lane_ROI <- filter(xb_data_2, xb_data_2$lane == "ROI")

ggplot(Lane_ROI, aes(Lane_ROI$OriginHubCity, fill = OriginHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("OriginHubCity Vs shipment count")
# most orogin city - bhiwandi, surat, bangalore, Pune

ggplot(Lane_ROI, aes(Lane_ROI$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubCity", y="shipment count") + ggtitle("DestinationHubCity Vs shipment count") +
  scale_x_discrete(limits = c("New Delhi", "Delhi NCR", "Gurugram", "Noida", "Ghaziabad", "Faridabad", "Gurgaon"))
# most destination cities - New delhi, Delhi NCR and Noida

ggplot(Lane_ROI, aes(Lane_ROI$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface", "Air"))


# mean shipping time
mean(as.numeric(Lane_ROI$total_shipping_time_days)) # 3.37 days

# mean precessing time at proc loc 1
mean(as.numeric(Lane_ROI$ProcessLocation1_time_diff)) # 11 hr

# mean precessing time at proc loc 2
mean(as.numeric(Lane_ROI$ProcessLocation2_time_diff)) # 3.32 hr

# mean precessing time at proc loc 3
mean(as.numeric(Lane_ROI$ProcessLocation3_time_diff), na.rm = TRUE) # 6.45 hr

# mean precessing time at proc loc 4
mean(as.numeric(Lane_ROI$ProcessLocation4_time_diff)) # 3.53 hr

# mean precessing time at destination loc
mean(as.numeric(Lane_ROI$Destination_time_diff), na.rm = TRUE) # 3.31 hr

# mean precessing time between process loc 1 and 2
mean(as.numeric(Lane_ROI$pl1_pl2_time)) # 1.21 days

# mean precessing time between process loc 2 and 3
mean(as.numeric(Lane_ROI$pl2_pl3_time)) # 0.32 days

# mean precessing time between process loc 3 and 4
mean(as.numeric(Lane_ROI$pl3_pl4_time), na.rm = TRUE) # 0.16 days

# mean precessing time between process loc 4 and destination
mean(as.numeric(Lane_ROI$pl4_dest_time)) # 0.11 days

Lane_ROI_wt4 <- filter(Lane_ROI, Lane_ROI$PhysicalWeight > 4)

# vol_wt_more_than_5cub_mtr <- wt_more_than_cub_mtr %>% arrange(desc(wt_more_than_cub_mtr$VolumetricWeight))

# Lets check trends for shiplemt lenght Metro
Lane_Metro <- filter(xb_data_2, xb_data_2$lane == "Metro")

ggplot(Lane_Metro, aes(Lane_Metro$OriginHubCity, fill = OriginHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="OriginHubCity", y="shipment count") + ggtitle("OriginHubCity Vs shipment count")
# most orogin city - bangalore, Pune

ggplot(Lane_Metro, aes(Lane_Metro$DestinationHubCity, fill = DestinationHubCity)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="DestinationHubCity", y="shipment count") + ggtitle("DestinationHubCity Vs shipment count") +
  scale_x_discrete(limits = c("Delhi NCR", "Noida", "Ghaziabad", "Faridabad", "Gurgaon"))
# most destination cities - Delhi NCR and Noida

ggplot(Lane_Metro, aes(Lane_Metro$routemode, fill = routemode)) + geom_bar(aes(y = (..count..))) +
  geom_text(aes(y = (..count..),label =   ifelse((..count..)==0,"",scales::percent((..count..)/sum(..count..)))), stat="count",colour="black", vjust = -0.5, size = 3.5) +
  theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + labs(x="routemode", y="shipment count") + ggtitle("routemode Vs shipment count") +
  scale_x_discrete(limits = c("Surface", "Air"))


# mean shipping time
mean(as.numeric(Lane_Metro$total_shipping_time_days)) # 4.23 days

# mean precessing time at proc loc 1
mean(as.numeric(Lane_Metro$ProcessLocation1_time_diff)) # 15.71 hr

# mean precessing time at proc loc 2
mean(as.numeric(Lane_Metro$ProcessLocation2_time_diff)) # 2.38 hr

# mean precessing time at proc loc 3
mean(as.numeric(Lane_Metro$ProcessLocation3_time_diff), na.rm = TRUE) # 7.50 hr

# mean precessing time at proc loc 4
mean(as.numeric(Lane_Metro$ProcessLocation4_time_diff)) # 3.39 hr

# mean precessing time at destination loc
mean(as.numeric(Lane_Metro$Destination_time_diff), na.rm = TRUE) # 3.34 hr

# mean precessing time between process loc 1 and 2
mean(as.numeric(Lane_Metro$pl1_pl2_time)) # 1.71 days

# mean precessing time between process loc 2 and 3
mean(as.numeric(Lane_Metro$pl2_pl3_time)) # 0.30 days

# mean precessing time between process loc 3 and 4
mean(as.numeric(Lane_Metro$pl3_pl4_time), na.rm = TRUE) # 0.47 days

# mean precessing time between process loc 4 and destination
mean(as.numeric(Lane_Metro$pl4_dest_time)) # 0.18 days

Lane_Metro_wt4 <- filter(Lane_Metro, Lane_Metro$PhysicalWeight > 4)

# Lets check trends for shiplemt lenght zone
Lane_Zone <- filter(xb_data_2, xb_data_2$lane == "Zone")

mean(as.numeric(Lane_Zone$total_shipping_time_days)) # 2.46 days

# Lets check trends for shiplemt lenght NE
Lane_NE <- filter(xb_data_2, xb_data_2$lane == "North East")

mean(as.numeric(Lane_NE$total_shipping_time_days)) # 2.04 days

# Lets check trends for shiplemt lenght state
Lane_State <- filter(xb_data_2, xb_data_2$lane == "State")

mean(as.numeric(Lane_State$total_shipping_time_days)) # 2.55 days

#######################################################################################################
# lets calculate cost of transport by assuming 100 Rs per kg/ cubic meter

# cost by physical wt
xb_data_2$phy_cost <- xb_data_2$PhysicalWeight*100

# lets check mean cost
summary(xb_data_2$phy_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   22.00   37.00   78.86   63.00 3400.00 

mean(xb_data_2$phy_cost) # 78.86 Rs

# cost by volumeric wt
xb_data_2$vol_cost <- xb_data_2$VolumetricWeight*100

# lets check mean cost
summary(xb_data_2$vol_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   27.00   50.00   96.18  105.00 7040.00

mean(xb_data_2$vol_cost) # 96.18 Rs

## lets check cost by route mode by assuming surface cost per kg is 80 and air cost is 160

# Air mode
route_mode_air$phy_cost <- route_mode_air$PhysicalWeight*160
# lets check mean cost
summary(route_mode_air$phy_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   36.80   56.00   84.85   96.00 1600.00 

mean(route_mode_air$phy_cost) # 84.84 Rs

route_mode_air$vol_cost <- route_mode_air$VolumetricWeight*160
# lets check mean cost
summary(route_mode_air$vol_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   35.20   56.00   91.58  107.20 2844.80 

mean(route_mode_air$vol_cost) # 91.58 Rs

# surface mode
route_mode_surface$phy_cost <- route_mode_surface$PhysicalWeight*80
# lets check mean cost
summary(route_mode_surface$phy_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00   17.60   30.40   73.27   51.20 2720.00 

mean(route_mode_surface$phy_cost) # 73.26 Rs

route_mode_surface$vol_cost <- route_mode_surface$VolumetricWeight*80
# lets check mean cost
summary(route_mode_surface$vol_cost)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.80   31.20   40.00   92.29   86.40 5632.0 

mean(route_mode_surface$vol_cost) # 92.28 Rs

## cost distribution using bpxplot for lane and destination city
ggplot(xb_data_2, aes(xb_data_2$lane, xb_data_2$phy_cost, color = lane)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 3400, by = 100)) + labs(x="Lane", y="shipment time in days") + 
  ggtitle("Lane vs shipment time in days")

ggplot(xb_data_2, aes(xb_data_2$lane, xb_data_2$vol_cost, color = lane)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 7040, by = 200)) + labs(x="Lane", y="shipment time in days") + 
  ggtitle("Lane vs shipment time in days")

ggplot(xb_data_2, aes(xb_data_2$DestinationHubCity, xb_data_2$phy_cost, color = DestinationHubCity)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 3400, by = 100)) + labs(x="Lane", y="shipment time in days") + 
  ggtitle("Lane vs shipment time in days")

ggplot(xb_data_2, aes(xb_data_2$DestinationHubCity, xb_data_2$vol_cost, color = DestinationHubCity)) + geom_boxplot() + 
  scale_y_continuous(breaks = seq(0, 7040, by = 200)) + labs(x="Lane", y="shipment time in days") + 
  ggtitle("Lane vs shipment time in days")

########################################################################################################

