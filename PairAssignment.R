library(pdftools)
library(tidyverse)
library(measurements)
library(dplyr)

#Read foot01.csv 
footData1<-read.csv("foot01.csv")
str(footData1)
summary(footData1)
colnames(footData1)<-c("Record","Gender","Height(cm)","EUSize")

#Remove NA data, outlier, and unified standard of height in cm
footData1<-na.omit(footData1)
footData1[footData1$`Height(cm)`<2,3]<-100*footData1[footData1$`Height(cm)`<2,3]
footData1<-footData1[footData1$`Height(cm)`<280,]
footData1[footData1$Gender == "woman", 2]<- "F"
footData1[footData1$Gender == "man", 2]<- "M"
str(footData1)
summary(footData1)
footData1[,1]<-1:99
footData1

#Read foot-length-and-body-height-data-2017-18.pdf 2 tables
pdfData<-pdf_text("foot-length-and-body-height-data-2017-18.pdf") %>% 
  readr::read_lines() 
#pdf data1
table1<-pdfData[4:56]
table1<-str_squish(table1)
var_lines1<-table1[1] %>% 
  unlist()

var_lines1[c(1,2,3,4)]<-c("Record","Gender","Foot (cm ± 0.10 cm) ","Height (cm ± 0.10 cm)")
data_lines1<-table1[2:52]
data_lines1 [data_lines1 ==""] <-NA
footTable1<-data.frame(data_lines1)
class(footTable1)
footTable1<-footTable1[complete.cases(footTable1),]
str(footTable1)
footTable1<-str_split(footTable1," ")
footTable1<-t(data.frame(footTable1))
colnames(footTable1)<-var_lines1
rownames(footTable1)<-NULL
footTable1
class(footTable1)
footTable1<-as.data.frame(footTable1)

#pdf data2
table2<-pdfData[60:112]
table2<-str_squish(table2)
var_lines2<-table2[1] %>% 
  unlist()

var_lines2[c(1,2,3,4)]<-c("Record","Gender","Foot (cm ± 0.10 cm) ","Height (cm ± 0.10 cm)")
data_lines2<-table2[2:52]
data_lines2 [data_lines2 ==""] <-NA
footTable2<-data.frame(data_lines2)
footTable2<-footTable2[complete.cases(footTable2),]
str(footTable2)
footTable2<-str_split(footTable2," ")
footTable2<-t(data.frame(footTable2))
footTable2[,1]<-26:50
colnames(footTable2)<-var_lines2
rownames(footTable2)<-NULL
footTable2
class(footTable2)
#Merge pdf 2 tables to footData2
footData2<-rbind(footTable1,footTable2)
footData2
class(footTable2)
footTable2<-as.data.frame(footTable2)
footData2$Record<-as.integer(footData2$Record)
footData2$`Height (cm ± 0.10 cm)`<-as.numeric(footData2$`Height (cm ± 0.10 cm)`)
footData2$`Foot (cm ± 0.10 cm) `<-as.numeric(footData2$`Foot (cm ± 0.10 cm) `)
str(footData2)
summary(footData2)

#Read FLtoEUSize.csv and Convert cm to EU size
fLtoEUSize<-read.csv("FLtoEUSize.csv")
str(fLtoEUSize)
#Convert foot length to numeric
fLtoEUSize$foot.length<-as.numeric(gsub("[^0-9]","",fLtoEUSize$foot.length))
fLtoEUSize$foot.length<-conv_unit(as.numeric(gsub("[^0-9]","",fLtoEUSize$foot.length)), "mm","cm")
str(fLtoEUSize)
fLtoEUSize
#Change cm to EU size
footData2$Foot<-NA
for (i in 1:50){
  for (j in 1:31){
    if ( fLtoEUSize[j,2] < footData2[i,3] & footData2[i,3] <= fLtoEUSize[j+1,2] )
      footData2[i,5]=fLtoEUSize[j+1,1]
    else j=j+1
  }
}
footData2
footData2<-na.omit(footData2)
str(footData2)
summary(footData2)
footData2<-footData2[,-3]
colnames(footData2)<-c("Record","Gender","Height(cm)","EUSize")

#Combine two data set
footdata<-rbind(footData1, footData2)
footdata<-arrange(footdata,footdata$`Height(cm)`)
str(footdata)
summary(footdata)
class(footdata)
footdata[,1]<-1:147



#What is the correlation between body height and shoe size
    ##consider height as response variable and shoe size as explanator variable
ggplot(data=footdata, aes(x=EUSize,y=footdata$`Height(cm)`)) +
  geom_point()
    ##scatterplot indicates that there is a positive linear correlation 
    ##between height and shoe size. 

#create a histogram based on foot size value

footsizehist<-ggplot(data=footdata,aes(x=EUSize))
footsizehist+geom_histogram(binwidth=1,colour="#42f5b3",fill="blue")+xlab("Foot Size in EU Size") + ylab("Frequency")+ggtitle("Foot Size Distribution")

#Enhance the figure 
footsizehist+geom_histogram(binwidth=1,colour="#42f5b3",fill="blue")+xlab("Foot Size in EU Size") + ylab("Frequency")+facet_grid(Gender~.)
    ##from the plot, we can see that the distribution of Female shoe size is approximately normal distribution.
    ##Male tends to have bigger shoe size than female on average. 
   ##


#Create linear regression
regfull<-lm(formula=footdata$`Height(cm)`~footdata$EUSize)
summary(datafull)

datafemale<-footdata[footdata$Gender=='F',]
regfemale<-lm(formula=datafelmale$`Height(cm)`~datafemale$EUSize)
summary(regfemale)

datamale<-footdata[footdata$Gender=='M',]
regmale<-lm(formula=datamale$`Height(cm)`~datamale$EUSize)
summary(regmale)


#Analyse residuals
ggplot(data=footdata, aes(x=EUSize,y=footdata$`Height(cm)`), colour=footdata$Gender) +
  geom_point()
##I try to use this one to separate data point by gender, not sure whether not working


#Tcopy from internet, can use later the Pearson’s r between height and shoe size is 0.6 (height and shoesize are moderately correlated). 
#As the p < 0.05, the correlation is statistically significant.



