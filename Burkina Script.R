##Import libraries##

 library(foreign)
 library (utils)
 library(dplyr)
 library(plyr)
 library(ggplot2)
 library(car)
 library(lme4)

##Set Workspace##
    #help to find the file locations - just uncomment it if needed
      #file.choose()
  
    setwd("~/Documents/College/Graduate/Spatial Analysis & Modeling/Semester Project/Data")

  
##Import Data##

    #WomensDHSData <- read.spss("/Users/admin/Documents/College/Graduate/Spatial Analysis & Modeling/Semester Project/Data/DHS_Individual_2010/BFIR62FL.SAV", stringsAsFactors = FALSE, to.data.frame=TRUE)
    #write.csv(WomensDHSData, file = "Womens_DHS_DATA.csv", row.names = FALSE)
    Individuals<- read.csv("/Users/admin/Documents/College/Graduate/Spatial Analysis & Modeling/Semester Project/Data/Womens_DHS_DATA.csv", stringsAsFactors = FALSE, header=TRUE)
    Clusters <- read.csv("/Users/admin/Documents/College/Graduate/Spatial Analysis & Modeling/Semester Project/Data/DHS_Clusters/DHSClusters.csv", stringsAsFactors = FALSE, header=TRUE)

  
##Prepare Data## 

  #Narrow Data Down
    
      #include only children with a reported weight(HW3), age_months(HW1), height(HW2), and get rid of some categories of water sources that are not needed-- narrow down rows
        HWAData <- Individuals[which(Individuals$HW1.1 != 'NA' & Individuals$HW3.1!='NA' & Individuals$HW2.1!='NA' 
                                  & Individuals$HW5.1!= "NA" & Individuals$V113!= "Rainwater" & Individuals$V113!="Cart with small tank" 
                                  & Individuals$V113!= "Bottled water" & Individuals$V113!="Not a dejure resident" & Individuals$V113!="Unprotected spring" 
                                  & Individuals$V113!="Protected spring"),  ]

      #select only the variables that I need -- narrow down the columns
        variables <- c("V001", "V024", "V025", "V012", "V106", "B4.01", "HW1.1", "HW2.1", "HW3.1", "V136", "V137",
                    "V006", "V113", "HW5.1", "HW8.1")
        Children <- HWAData[variables]

  #Recode Data
     
      #Rename Columns
        Children <- rename(Children, c("V001" = "ClusterID", "V024" = "Region", "V025" = "Setting", "V012" = "MothersAge" , 
                                    "V106" = "MothersEd", "B4.01" = "Sex", "HW1.1" = "Age.mos", "HW2.1" = "Weight.kg", 
                                    "HW3.1" = "Height.cm", "V136" = "HHmembers", "V137" = "Under5", "V006" = "Interview.mo", 
                                    "V113" = "WaterSource", "HW5.1" = "HAZ", "HW8.1" = "WAZ"))
        

        Children$WaterSource <- revalue(Children$WaterSource, c("Public tap/standpipe"="Public tap", "River/dam/lake/ponds/stream/canal/irrigation channel"="Surface", 
                                                                "Tube well or borehole"="Tube well", "Piped to yard/plot"="Piped into yard"))
        
      #Change the month number to month name
        Children$Interview.mo<-recode(Children$Interview.mo,"'5'='May'; '6'='June'; '7'='July'; '8'='August'; '9'='September';
                                   '10'='October'; '11'='November'; '12'='December'")
      
      #Change the variable types (as.character/as.numeric/as.factor)
        Children$Weight.kg <- as.numeric(Children$Weight.kg)
        Children$Height.cm <- as.numeric(Children$Height.cm)
        Children$Interview.mo <- as.factor(Children$Interview.mo)
        Children$ClusterID <- as.factor(Children$ClusterID)
        Children$MothersEd <- as.factor(Children$MothersEd)
        Children$WaterSource <-as.factor(Children$WaterSource)
        Children$Region <- as.factor(Children$Region)
        Children$Setting <- as.factor(Children$Setting)
        Children$Sex <- as.factor(Children$Sex)

        Clusters$DHSCluster <- as.factor(Clusters$DHSCluster)
        Clusters$GeoUnit <- as.factor(Clusters$GeoUnit)
        Clusters$UrbanRural <- as.factor(Clusters$UrbanRural)
        ClustersDHSRegion <- as.factor(Clusters$DHSRegion)
     
    #Convert variables to correct units
        Children$Weight.kg <- Children$Weight.kg/10
        Children$Height.cm <- Children$Height.cm/10
        Children$HAZ <- Children$HAZ/100
        Children$WAZ <- Children$WAZ/100
     
    #Change the reference or baseline factor level
        Children$MothersEd <- relevel(Children$MothersEd, ref="No education")
        Children$Sex <- relevel(Children$Sex, ref = "Male")
        Children$Interview.mo <- relevel(Children$Interview.mo, ref="May")
     
  #Join Clusters to individual data     
     bfdata <- merge(Children, Clusters, by.x = "ClusterID", by.y = "DHSCluster")
     bfdata$GeoUnit <- relevel(bfdata$GeoUnit, ref="Greenstone")
     
  #Do not need to do this but want to keep code - Subset the data for children over 12kgs and taller than 10cms
        #bfdata <- merged[ which(merged$Weight.kg >= 12 & merged$Height.cm >= 10), ]
        #bfdata2 <- merged2[ which(merged2$Weight.kg >= 12 & merged2$Height.cm >= 10), ]

     
##Build the Models##
 
 #HAZ						
     #mod1: mothers variables
      mod1 <- lm(HAZ ~ MothersAge + MothersEd, data = bfdata)
      summary(mod1)
     #mod2: mod1 + childrens variables
      mod2 <- lm(HAZ ~ MothersAge + MothersEd + Sex + Age.mos, data = bfdata)
      summary(mod2)
     #mod3: mod2 + household variables	
      mod3 <- lm(HAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource, data = bfdata)
      summary(mod3)
     #mod4: mod3 + community level variables
      mod4 <- lm(HAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region, data = bfdata)
      summary(mod4)
     #mod5: mod4 + greenstone
      mod5 <- lm(HAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region + GeoUnit, data = bfdata)
      summary(mod5)
     #mod6: mod4 + (random) DHS cluster	
      mod6 <- lmer(HAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region 
                   + (1|ClusterID), data = bfdata)
      summary(mod6)
      
  #WAZ						
     #mod7: mothers variables
      mod7 <- lm(WAZ ~ MothersAge + MothersEd, data = bfdata)
      summary(mod7)
     #mod8: mod7 + childrens variables
      mod8 <- lm(WAZ ~ MothersAge + MothersEd + Sex + Age.mos, data = bfdata)
      summary(mod8)
     #mod9: mod8 + household variables	
      mod9 <- lm(WAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource, data = bfdata)
      summary(mod9)
     #mod10: mod9 + community level variables
      mod10 <- lm(WAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region, data = bfdata)
      summary(mod10)
     #mod11: mod10 + (random) greenstone
      mod11 <- lm(WAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region + (1|GeoUnit), data = bfdata)
      summary(mod11)
     #mod12: mod10 + (random) DHS cluster	
      mod12 <- lmer(WAZ ~ MothersAge + MothersEd + Sex + Age.mos + HHmembers + Under5 + WaterSource + Setting + Region + GeoUnit 
                    + (1|ClusterID), data = bfdata)
      summary(mod12)    
 

      plot(bfdata$Age.mos, bfdata$Height.cm)
      plot(bfdata$Age.mos, bfdata$Weight.kg)
      

    
  