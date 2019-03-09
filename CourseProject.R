#SCC: The name of the source as indicated by a digit string (see source code classification table)
#Pollutant: A string indicating the pollutant
#Emissions: Amount of PM2.5 emitted, in tons
#type: The type of source (point, non-point, on-road, or non-road)
#year: The year of emissions recorded

getwd()
dir()

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


NEI<-transform(NEI,year=factor(year))  ###transform year(integer to factor)
temit<-tapply(NEI$Emissions/10^6,NEI$year,sum) 

#q1

barplot(temit,xlab = "Year",ylab ="Total PM2.5 emission(100'000 tons)",main = "Total Pm2.5 emissions from all USA sourses" )
dev.copy(png, file = "plot1.png")  ## Copy my plot to a PNG file
dev.off()

#q2
baltimore<-subset(NEI,NEI$fips %in% c("24510"))
bal<-transform(baltimore,year=factor(year))
tbaltimore<-tapply(baltimore$Emissions,baltimore$year,sum)

barplot(tbaltimore,xlab = "Year",
      ylab ="Total PM2.5 emission(tons)",
      main = "Total Pm2.5 emissions from Baltimore City")
dev.copy(png, file = "plot2.png")  ## Copy my plot to a PNG file
dev.off()

#q3
library(ggplot2)

ggplot(bal,aes(x=year,y=Emissions,fill=type))+
  geom_bar(stat = "identity",alpha=0.8)+
  facet_wrap(~type,scale="free")+
  xlab("Year")+
  ylab("Total Emissions(Tons)")+
  labs(title = expression("PM"[2.5]*" Emissions in Baltimore City from 1999to2008 by source type"))
dev.copy(png, file = "plot3.png")  ## Copy my plot to a PNG file
dev.off()


#4
combution<-grepl("comb",SCC$SCC.Level.One,ignore.case = T)
coal<-grepl("coal",SCC$SCC.Level.Four,ignore.case = T)
coalCom<-(combution&coal)
ComSCC<-SCC[coalCom,]$SCC
ComNEI<-NEI[NEI$SCC %in% ComSCC,]
ComNEI<-transform(ComNEI,year=factor(year))

require(ggplot2)
ggplot(ComNEI,aes(year,Emissions/10^5))+
  geom_bar(stat = "identity",alpha=0.5,fill="red")+
  xlab("Year")+
  ylab("Total Emissions(00'000tons)")+
  ggtitle(expression("PM"[2.5]*" Coal Combustion Source Emissions Across US from 1999-2008"))

dev.copy(png, file = "plot4.png")  ## Copy my plot to a PNG file
dev.off()



#q5
balcar<-subset(bal,bal$type=="ON-ROAD")
BaltiCarTotal<-aggregate(Emissions~year+fips,balcar,sum)

ggplot(BaltiCarTotal,aes(x=year,y=Emissions))+
  geom_bar(stat = "identity",fill="orange")+
  xlab("Year")+
  ylab("Total Emissions(Tons)")+
  labs(title = expression("PM"[2.5]*" Emissions in Baltimore City from 1999to2008 by car"))
dev.copy(png, file = "plot5.png")  ## Copy my plot to a PNG file
dev.off()

#q6
labal<-subset(NEI,NEI$fips %in% c("24510","06037")&NEI$type=="ON-ROAD")
labal<-transform(labal,year=factor(year))
Bothtotal<-aggregate(Emissions~year+fips,labal,sum)
baltiCartotal<-Bothtotal[5:8,]
baltiCartotal$City<-"Baltimore City"

laCarTotal<-Bothtotal[1:4,]
laCarTotal$City<-"Los Angels City"

FinalBoth<-rbind(baltiCartotal,laCarTotal)
FinalBoth

ggplot(FinalBoth,aes(year,Emissions,fill=City))+
  geom_bar(stat = "identity",position = position_dodge(),alpha=0.5)+
  ggtitle(expression("Baltimore and Los Angels"~PM[2.5]~"Car Emissions by Year"))+
  xlab("Year")+
  ylab("Total Emissions")+
  scale_color_discrete(name="City",labels=c("Los Angels","Baltimore"))+
  theme(legend.title = element_text(face = "bold")) 
dev.copy(png, file = "plot6.png")  ## Copy my plot to a PNG file
dev.off()

  