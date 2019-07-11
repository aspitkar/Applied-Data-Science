# GROUP 5 
# IST 687 Project
# Group Members: 
# 1) Anuj Jayaswal
# 2) Nidhi Ketan
# 3) Aditi Patel
# 4) Prachi Nanda
# 5) Adwait Pitkar
# 6) Noor Al Jiboury


#################################################################################

# Install Required Packages

install.packages("ggplot2")
install.packages("maps")
install.packages("mapproj")
install.packages("sqldf")
install.packages("plotrix")
install.packages("kernlab")

# Load Packages

library("maps")
library("mapproj")
library("sqldf")
library("kernlab")
library("ggmap")
library("ggplot2")
library("plotrix")
# Read data file
file=read.csv(file.choose()) #read.csv(file="C:\\Users\\Adwait Pitkar\\Google Drive\\SYRACUSE FALL 15\\IST 687\\Project\\hyatt.csv", stringsAsFactors = FALSE)

# Inspect the structure of data file
head(file)
colnames(file)
str(file)

# Loading required columns from the file
Hyatt <- file[,c("PMS_ROOM_REV_USD_C","State_PL","City_PL","Overall_Sat_H", "Likelihood_Recommend_H","Condition_Hotel_H","Customer_SVC_H","Internet_Sat_H","POV_CODE_C","Tranquility_H","NPS_Type","Guest_Room_H","Shuttle.Service_PL","Staff_Cared_H","Valet.Parking_PL","Total.Meeting.Space_PL","Dry.Cleaning_PL","Restaurant_PL","Mini.Bar_PL","Limo.Service_PL")]

# Changing column names
colnames(Hyatt) <- c("RoomRevenue","State","City","OverallSatisfaction","LikelihoodToRecommend","ConditionOfHotel","CustomerService","Internet","PurposeOfVisit","Tranquility","NPSType","GuestRoomService","SHuttleService","Staff","ValetParking","TotalMeetingSPace","DryCleaning","Restaurant","MiniBar","LimoService")
head(Hyatt)

# Omitting any NA values
Hyatt <- na.omit(Hyatt)
head(Hyatt)


# Changing Factors to numericals
Hyatt$DryCleaning <- ifelse(Hyatt$DryCleaning=="Y",1,0)
Hyatt$Restaurant <- ifelse(Hyatt$Restaurant=="Y",1,0)
Hyatt$MiniBar <- ifelse(Hyatt$MiniBar=="Y",1,0)
Hyatt$LimoService <- ifelse(Hyatt$LimoService=="Y",1,0)



############### Counting Detractors Promotors Passives State Wise ######################

# Detractors
NumberofDetractors <- sqldf("Select State,count(NPSType) from Hyatt where NPSType='Detractor' group by State")
colnames(NumberofDetractors) <- c("State","Count")
NumberofDetractors <- NumberofDetractors[order(NumberofDetractors$Count,decreasing = TRUE),]
NumberofDetractors


# Passives
NumberofPassives <- sqldf("Select State,count(NPSType) from Hyatt where NPSType='Passive' group by State")
colnames(NumberofPassives) <- c("State","Count")
NumberofPassives <- NumberofPassives[order(NumberofPassives$Count,decreasing = TRUE),]
NumberofPassives


# Promoters
NumberofPromoter <- sqldf("Select State,count(NPSType) from Hyatt where NPSType='Promoter' group by State")
colnames(NumberofPromoter) <- c("State","Count")
NumberofPromoter <- NumberofPromoter[order(NumberofPromoter$Count,decreasing = TRUE),]
NumberofPromoter

# Sorting the 3 dataframes alphabetically
NumberofPromoter$State <- sort(NumberofPromoter$State)
NumberofDetractors$State <- sort(NumberofDetractors$State)
NumberofPassives$State <- sort(NumberofPassives$State)


#############################################################################


# Finding the difference between promoter and detractor count
Difference <- NumberofPromoter$Count - NumberofDetractors$Count
NewState <- NumberofPromoter$State

# Creating new dataframe with difference and state name
NewFrame <- data.frame(NewState,Difference)

# Counting the frequency for each state
NewFrame$Frequency <- NumberofPromoter$Count+NumberofDetractors$Count+NumberofPassives$Count

# AVerage NPS per state
NewFrame$Average <- (NewFrame$Difference/NewFrame$Frequency)

# Changing column naes
colnames(NewFrame) <- c("State","Difference","Frequency","Average")
NewFrame <- NewFrame[order(NewFrame$Average),]
NewFrame

# Creating colors for bar chart
num.cols <- nrow(NewFrame)
my.col.rev <- rev(heat.colors(num.cols))
rank <- round(rescale(NewFrame$Average,c(1,num.cols)))
NewFrame$Rank <- my.col.rev[rank]

# Bar plot of Average NPS per state
ggplot(NewFrame,aes(x=reorder(State,+Average),y=Average,fill=Rank))+geom_bar(stat = "identity")+coord_flip()+ggtitle("Average NPS score per State")+labs(y="Average",x="State")


############################################################################################

# Loading US map
us <- map_data("state")

# Changing state name to lower case
NumberofDetractors$State <- tolower(NumberofDetractors$State)

# Plotting number of detractors per state
ggplot(NumberofDetractors,aes(map_id=State))+geom_map(map=us,aes(fill=Count))+expand_limits(x = us$long, y = us$lat) + coord_map()+scale_fill_gradient(low="pink",high = "red")+ggtitle("Number of Detractors per state")+xlab("Latitude")+ylab("Longitude")

# Plotting number of promoters per state
NumberofPromoter$State <- tolower(NumberofPromoter$State)
ggplot(NumberofPromoter,aes(map_id=State))+geom_map(map=us,aes(fill=Count))+expand_limits(x = us$long, y = us$lat) + coord_map()+scale_fill_gradient(low="light blue",high = "dark blue")+ggtitle("Number of Promoters per state")+xlab("Latitude")+ylab("Longitude")

# Plotting Average NPS per state
NewFrame$State <- tolower(NewFrame$State)
ggplot(NewFrame,aes(map_id=State))+geom_map(map=us,aes(fill=Average))+expand_limits(x = us$long, y = us$lat) + coord_map()+scale_fill_gradient(low="light green",high = "dark green")+ggtitle("Average NPS per state")+xlab("Latitude")+ylab("Longitude")

##############################################################################################

# Taking subset of detracting state California
DetractorSubset <- Hyatt[Hyatt$State=="California",]

# Applying Linear model to find dependency on LikelihoodToRecommend
Model1 <- lm(formula = LikelihoodToRecommend ~ CustomerService, data=DetractorSubset  )
Model1 <- summary(Model1)$r.squared 
Model1
# 46%

Model2 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel, data=DetractorSubset  )
Model2 <- summary(Model2)$r.squared 
Model2
# 61%

Model3 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService, data=DetractorSubset  )
Model3 <- summary(Model3)$r.squared 
Model3
# 66.6%

Model4 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+Staff, data=DetractorSubset  )
Model4 <- summary(Model4)$r.squared 
Model4
# 68% Maximum dependency



# Applying KSVM for for Model 4 to find out root mean sqaure error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

Hyatt.ksvm.detractor <- ksvm(LikelihoodToRecommend ~ ConditionOfHotel + CustomerService + GuestRoomService + +Staff, data = DetractorSubset )
Hyatt.ksvm.dectractor.error <- (Hyatt$LikelihoodToRecommend - predict(Hyatt.ksvm.detractor,Hyatt)) 
rmse(Hyatt.ksvm.dectractor.error)

# Error: 1.05
# Hence , CustomerService+ConditionOfHotel+GuestRoomService+Staff majorly defines LikelihoodToRecommend
##########################################################

# Finding number of Promoters, detractors and passives with the above columns when their value is <6
Set1 <- DetractorSubset[DetractorSubset$CustomerService<6,]
Set2 <- DetractorSubset[DetractorSubset$ConditionOfHotel<6,]
Set3 <- DetractorSubset[DetractorSubset$GuestRoomService<6,]
Set5 <- DetractorSubset[DetractorSubset$Staff < 6,]

NumberofDetractorsSubset1 <- sqldf("Select NPSType, count(NPSType) from Set1 group by NPSType")
NumberofDetractorsSubset1$Type <- "CustomerService"
Sum1 <- sum(NumberofDetractorsSubset1$`count(NPSType)`)
NumberofDetractorsSubset1$Average <- NumberofDetractorsSubset1$`count(NPSType)`/Sum1
colnames(NumberofDetractorsSubset1) <- c("NPSType","Count","Type","Average")

NumberofDetractorsSubset2 <- sqldf("Select NPSType, count(NPSType) from Set2 group by NPSType")
NumberofDetractorsSubset2$Type <- "ConditionOfHotel"
Sum2 <- sum(NumberofDetractorsSubset2$`count(NPSType)`)
NumberofDetractorsSubset2$Average <- NumberofDetractorsSubset2$`count(NPSType)`/Sum2
colnames(NumberofDetractorsSubset2) <- c("NPSType","Count","Type","Average")

NumberofDetractorsSubset3 <- sqldf("Select NPSType, count(NPSType) from Set3 group by NPSType")
NumberofDetractorsSubset3$Type <- "GuestRoomService"
Sum3 <- sum(NumberofDetractorsSubset3$`count(NPSType)`)
NumberofDetractorsSubset3$Average <- NumberofDetractorsSubset3$`count(NPSType)`/Sum3
colnames(NumberofDetractorsSubset3) <- c("NPSType","Count","Type","Average")


NumberofDetractorsSubset5 <- sqldf("Select NPSType, count(NPSType) from Set5 group by NPSType")
NumberofDetractorsSubset5$Type <- "Staff"
Sum5 <- sum(NumberofDetractorsSubset5$`count(NPSType)`)
NumberofDetractorsSubset5$Average <- NumberofDetractorsSubset5$`count(NPSType)`/Sum5
colnames(NumberofDetractorsSubset5) <- c("NPSType","Count","Type","Average")



NewSubset <- rbind(NumberofDetractorsSubset1,NumberofDetractorsSubset2,NumberofDetractorsSubset3,NumberofDetractorsSubset5)

ggplot(NewSubset, aes(factor(Type), Average, fill = NPSType)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")+ggtitle("Average Promoter/Detractor/Passive in California where Conditions are <6")+xlab("Column Type")


###############################################################################


# Finding number of Promoters, detractors and passives with the above columns when their value is >8
Set11 <- DetractorSubset[DetractorSubset$CustomerService>8,]
Set22 <- DetractorSubset[DetractorSubset$ConditionOfHotel>8,]
Set33 <- DetractorSubset[DetractorSubset$GuestRoomService>8,]
Set55 <- DetractorSubset[DetractorSubset$Staff >8,]

NumberofDetractorsSubset11 <- sqldf("Select NPSType, count(NPSType) from Set11 group by NPSType")
NumberofDetractorsSubset11$Type <- "CustomerService"
Sum11 <- sum(NumberofDetractorsSubset11$`count(NPSType)`)
NumberofDetractorsSubset11$Average <- NumberofDetractorsSubset11$`count(NPSType)`/Sum11
colnames(NumberofDetractorsSubset11) <- c("NPSType","Count","Type","Average")

NumberofDetractorsSubset22 <- sqldf("Select NPSType, count(NPSType) from Set22 group by NPSType")
NumberofDetractorsSubset22$Type <- "ConditionOfHotel"
Sum22 <- sum(NumberofDetractorsSubset22$`count(NPSType)`)
NumberofDetractorsSubset22$Average <- NumberofDetractorsSubset22$`count(NPSType)`/Sum22
colnames(NumberofDetractorsSubset22) <- c("NPSType","Count","Type","Average")

NumberofDetractorsSubset33 <- sqldf("Select NPSType, count(NPSType) from Set33 group by NPSType")
NumberofDetractorsSubset33$Type <- "GuestRoomService"
Sum33 <- sum(NumberofDetractorsSubset33$`count(NPSType)`)
NumberofDetractorsSubset33$Average <- NumberofDetractorsSubset33$`count(NPSType)`/Sum33
colnames(NumberofDetractorsSubset33) <- c("NPSType","Count","Type","Average")


NumberofDetractorsSubset55 <- sqldf("Select NPSType, count(NPSType) from Set55 group by NPSType")
NumberofDetractorsSubset55$Type <- "Staff"
Sum55 <- sum(NumberofDetractorsSubset55$`count(NPSType)`)
NumberofDetractorsSubset55$Average <- NumberofDetractorsSubset55$`count(NPSType)`/Sum55
colnames(NumberofDetractorsSubset55) <- c("NPSType","Count","Type","Average")



NewSubsett <- rbind(NumberofDetractorsSubset11,NumberofDetractorsSubset22,NumberofDetractorsSubset33,NumberofDetractorsSubset55)

ggplot(NewSubsett, aes(factor(Type), Average, fill = NPSType)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")+ggtitle("Average Promoter/Detractor/Passive in California where Conditions are >8")+xlab("Column Type")


#######################################################################################

#Zoom in to the region around California

zoomcali <- geocode("Sierra Nevada, California")
california <- map_data("state",region = "California")
zoomamt <- 5
zoomcali

xlon <- zoomcali$lon
ylat <- zoomcali$lat
xlon
ylat
xlimitcali <- c(xlon-zoomamt, xlon+zoomamt)
ylimitcali <- c(ylat-zoomamt, ylat+zoomamt)
xlimitcali
ylimitcali

# FInding average revenue in the state of california

# Revenue when conditions are > 8 (Promoting Side)
DetractorSubset2 <- DetractorSubset[DetractorSubset$ConditionOfHotel>8 & DetractorSubset$CustomerService>8 & DetractorSubset$GuestRoomService>8,]
agg.room.revenue <- aggregate(DetractorSubset2$RoomRevenue,by=list(DetractorSubset2$City),sum)

agg.room.count <- sqldf("Select City,count(City) from DetractorSubset2 group by City")
agg.room.revenue$Average <- agg.room.revenue$x/agg.room.count$`count(City)`
agg.room.revenue <- agg.room.revenue[order(agg.room.revenue$x, decreasing = TRUE),]

Statesss <- replicate(nrow(agg.room.revenue),"California")
agg.room.revenue$States <- Statesss

colnames(agg.room.revenue) <- c("City","Revenue","Average","State")
agg.room.revenue$Average <- round(agg.room.revenue$Average)
agg.room.revenue$State <- tolower(agg.room.revenue$State)

geocodee <- geocode(agg.room.revenue$City)
agg.room.revenue <- cbind(agg.room.revenue,geocodee)
colnames(agg.room.revenue) <- c("City","Revenue","Average","State","Long","Lat")
agg.room.revenue <- agg.room.revenue[!agg.room.revenue$City=="Dublin",]
agg.room.revenue <- agg.room.revenue[!agg.room.revenue$City=="Valencia",]
ggplot(california,aes(x=long,y=lat))+coord_map()+geom_polygon(color="white")+expand_limits(x=-120,y=37)+
  geom_point(data=agg.room.revenue, aes(x=Long, y=Lat, size=Revenue,color=Revenue))+scale_fill_gradient(low="pink",high="red")


# Revenue when conditions are < 6( Detracting SIde)
DetractorSubset1 <- DetractorSubset[DetractorSubset$ConditionOfHotel<6 & DetractorSubset$CustomerService<6 & DetractorSubset$GuestRoomService<6,]

agg.room.revenue <- aggregate(DetractorSubset1$RoomRevenue,by=list(DetractorSubset1$City),sum)

agg.room.count <- sqldf("Select City,count(City) from DetractorSubset1 group by City")
agg.room.revenue$Average <- agg.room.revenue$x/agg.room.count$`count(City)`
agg.room.revenue <- agg.room.revenue[order(agg.room.revenue$x, decreasing = TRUE),]

Statesss <- replicate(nrow(agg.room.revenue),"California")
agg.room.revenue$States <- Statesss

colnames(agg.room.revenue) <- c("City","Revenue","Average","State")
agg.room.revenue$Average <- round(agg.room.revenue$Average)
agg.room.revenue$State <- tolower(agg.room.revenue$State)

geocodee <- geocode(agg.room.revenue$City)
agg.room.revenue <- cbind(agg.room.revenue,geocodee)
colnames(agg.room.revenue) <- c("City","Revenue","Average","State","Long","Lat")
agg.room.revenue <- agg.room.revenue[!agg.room.revenue$City=="Dublin",]
agg.room.revenue <- agg.room.revenue[!agg.room.revenue$City=="Valencia",]
ggplot(california,aes(x=long,y=lat))+coord_map()+geom_polygon(color="white")+expand_limits(x=-120,y=37)+
  geom_point(data=agg.room.revenue, aes(x=Long, y=Lat, size=Revenue,color=Revenue))+scale_fill_gradient(low="pink",high="red")




###########################################################################

# SUbset with promoter state : Oklahoma
PromoterSubet <- Hyatt[Hyatt$State=="Oklahoma",]

# APplying linear model

Model1 <- lm(formula = LikelihoodToRecommend ~ CustomerService, data=PromoterSubet  )
Model1 <- summary(Model1)$r.squared 
Model1
# 24%

Model2 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel, data=PromoterSubet )
Model2 <- summary(Model2)$r.squared 
Model2
# 52%

Model3 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService, data=PromoterSubet  )
Model3 <- summary(Model3)$r.squared 
Model3
# 57.7%

Model8 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+Staff+Internet, data=PromoterSubet  )
Model8 <- summary(Model8)$r.squared 
Model8
# 59%


Model4 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+SHuttleService+Staff+Internet, data=PromoterSubet  )
Model4 <- summary(Model4)$r.squared 
Model4
# 61%

Model5 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+SHuttleService+ValetParking, data=PromoterSubet  )
Model5 <- summary(Model5)$r.squared 
Model5
# 61%

Model6 <- lm(formula = LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+SHuttleService+ValetParking+TotalMeetingSPace, data=PromoterSubet  )
Model6 <- summary(Model6)$r.squared 
Model6
# 65.25 # Maximum dependency

# KSVM for promoters
Hyatt.ksvm.promoter <- ksvm(LikelihoodToRecommend ~ CustomerService+ConditionOfHotel+GuestRoomService+SHuttleService+ValetParking+TotalMeetingSPace, data = PromoterSubet )
Hyatt.ksvm.promoter.error <- (Hyatt$LikelihoodToRecommend - predict(Hyatt.ksvm.promoter,Hyatt)) 
rmse(Hyatt.ksvm.promoter.error)

# Hence, CustomerService+ConditionOfHotel+GuestRoomService+SHuttleService+ValetParking+TotalMeetingSPace define LikelihoodToRecommend

############################################################################

# Finding average promoters, detractors and passives when above conditions are >8

Sett <- PromoterSubet[PromoterSubet$CustomerService>8,]
Sett1 <- PromoterSubet[PromoterSubet$ConditionOfHotel>8,]
Sett2 <- PromoterSubet[PromoterSubet$GuestRoomService>8,]
Sett3 <- PromoterSubet[PromoterSubet$SHuttleService>8,]
Sett4 <- PromoterSubet[PromoterSubet$ValetParking>8,]
Sett5 <- PromoterSubet[PromoterSubet$TotalMeetingSPace>8,]

NumberofPromoterSubset4 <- sqldf("Select NPSType, count(NPSType) from Sett group by NPSType")
NumberofPromoterSubset4$Type <- "CustomerService"
Sum4 <- sum(NumberofPromoterSubset4$`count(NPSType)`)
NumberofPromoterSubset4$Average <- NumberofPromoterSubset4$`count(NPSType)`/Sum4
colnames(NumberofPromoterSubset4) <- c("NPSType","Count","Type","Average")

NumberofPromoterSubset5 <- sqldf("Select NPSType, count(NPSType) from Sett1 group by NPSType")
NumberofPromoterSubset5$Type <- "ConditionOfHotel"
Sum5<- sum(NumberofPromoterSubset5$`count(NPSType)`)
NumberofPromoterSubset5$Average <- NumberofPromoterSubset5$`count(NPSType)`/Sum5
colnames(NumberofPromoterSubset5) <- c("NPSType","Count","Type","Average")

NumberofPromoterSubset6 <- sqldf("Select NPSType, count(NPSType) from Sett2 group by NPSType")
NumberofPromoterSubset6$Type <- "GuestRoomService"
Sum6<- sum(NumberofPromoterSubset6$`count(NPSType)`)
NumberofPromoterSubset6$Average <- NumberofPromoterSubset6$`count(NPSType)`/Sum6
colnames(NumberofPromoterSubset6) <- c("NPSType","Count","Type","Average")

NumberofPromoterSubset7 <- sqldf("Select NPSType, count(NPSType) from Sett3 group by NPSType")
NumberofPromoterSubset7$Type <- "SHuttleService"
Sum7<- sum(NumberofPromoterSubset7$`count(NPSType)`)
NumberofPromoterSubset7$Average <- NumberofPromoterSubset7$`count(NPSType)`/Sum7
colnames(NumberofPromoterSubset7) <- c("NPSType","Count","Type","Average")

NumberofPromoterSubset8 <- sqldf("Select NPSType, count(NPSType) from Sett4 group by NPSType")
NumberofPromoterSubset8$Type <- "ValetParking"
Sum8<- sum(NumberofPromoterSubset8$`count(NPSType)`)
NumberofPromoterSubset8$Average <- NumberofPromoterSubset8$`count(NPSType)`/Sum8
colnames(NumberofPromoterSubset8) <- c("NPSType","Count","Type","Average")

NumberofPromoterSubset9 <- sqldf("Select NPSType, count(NPSType) from Sett5 group by NPSType")
NumberofPromoterSubset9$Type <- "TotalMeetingSPace"
Sum9<- sum(NumberofPromoterSubset9$`count(NPSType)`)
NumberofPromoterSubset9$Average <- NumberofPromoterSubset9$`count(NPSType)`/Sum9
colnames(NumberofPromoterSubset9) <- c("NPSType","Count","Type","Average")

NewSubset5 <- rbind(NumberofPromoterSubset4,NumberofPromoterSubset5,NumberofPromoterSubset6,NumberofPromoterSubset7,NumberofPromoterSubset8,NumberofPromoterSubset9)

ggplot(NewSubset5, aes(factor(Type), Average, fill = NPSType)) + 
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1")+ggtitle("Average Promoter/Detractor/Passive in Oklahoma where Conditions are >8")+xlab("Column Type")


######################## END OF CODE ########################################################




