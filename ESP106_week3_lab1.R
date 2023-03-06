##########################

# ESP 106
# Lab 3 (Monday) - graphing

##########################

#In this lab we will start by reading merging in data on economic development and indoor and outdoor air pollution. Then we will practice making some graphs with it.



#1. First read in the three csv files: gdppercapitaandgini and airpollution

#Both datasets are from Our World in Data: ourworldindata.org
#The GDP dataset has GDP per capita and the GINI index (a measure of income inequality: https://en.wikipedia.org/wiki/Gini_coefficient)
#The air pollution dataset has death rates from indoor and outdoor air pollution - units are in deaths per 100,000 people
#Indoor air pollution is the Household Air Pollution from Solid Fules
#Outdoor air pollution is split into particulate matter and ozone

#Hint: The column names are long and cumbersome (because they contain information about units et) - you might want to rename some of the columns to make them easier to work with
setwd("D:/UC Davis/Courses and Research/My Courses/ESP 106/Lab/Lab3/ESP106_week3_lab1")
GDP <- read.csv("gdppercapiandgini.csv")
print(GDP)
AirPollu <- read.csv("airpollution.csv")
print(AirPollu)
colnames(AirPollu) <- c("Country",'Code','Year','PM_Death_Ambient','SolidFuels_Death_HouseHold','Ozone_Death_Ambient','Death_Airpollution')
colnames(GDP) <- c('Country','Code','Year','Total_Population_GDP','Continent','Gini','Outputside_RealGDP_Per_Capita')
#I simplified the names a bit
AirPollu$OutDoorPolluDeath <- AirPollu$PM_Death_Ambient+AirPollu$Ozone_Death_Ambient
#I created a new column of ambient pollution death rate by combining both PM and Ozone caused ambient deaths
GDP$LogPopGDP <- log10(GDP$Total_Population_GDP)
#I also created a new column in the GDP dataframe by logging the Total Population GDP

#2. Chose two countries that you are interested in and make a plot showing the death rates from indoor air pollution and outdoor air pollution (sum of particulate matter and ozone) over time
#Distinguish the countries using different colored lines and the types of pollution using different line types
#Make sure to add a legend and appropriate titles for the axes and plot 

#Hint: you can see all the different country names using unique(x$Entity) where x is the data frame containing the air pollution data
#Then create two new data frames that countain only the rows corresponding to each of the two countries you want to look at
#Create a new column of total outdoor air pollution deaths by summing death rates from particulate matter and ozone
#Use these to make your plot and add the lines you need

#Hint: you might have to set the y scale manually to make sure your plot is wide enough to show both countries. You can do this using the "ylim" argument in plot

unique(AirPollu$Entity)
ChinaAir <- AirPollu[AirPollu$Country=="China",]
IndiaAir <- AirPollu[AirPollu$Country=='India',]
#I Extracted the rows

ChinaAir$China_outdoor_death <-ChinaAir$Ozone_Death_Ambient+ChinaAir$PM_Death_Ambient
IndiaAir$India_outdoor_death <-IndiaAir$Ozone_Death_Ambient+IndiaAir$PM_Death_Ambient
#I summed the Ambient Ozone Death and the Ambient Particulate matter death of both countries, and assigned them to a new column

plot(ChinaAir$Year,ChinaAir$China_outdoor_death,'l',xlab = 'Year',ylab = 'Death Caused by Ourdoor Air pollution in China',cex=30,col='red',lty='solid',main = 'China Death Caused by Ambient Airpollution')
plot(IndiaAir$Year,IndiaAir$India_outdoor_death,'l',xlab = 'Year',ylab = 'Death Caused by Ourdoor Air pollution in India',cex=30,col='green',lty='longdash',main = 'India Death Caused by Ambient Airpollution')

China_IndiaAirDeath <- merge(ChinaAir,IndiaAir,by='Year',all = TRUE)
plot(China_IndiaAirDeath$Year,China_IndiaAirDeath$China_outdoor_death,type='l',xlab='year',ylab='Death Caused by outdoor airpollution',col='red',cex=20,main = 'death caused by outdoor air pollution')
lines(China_IndiaAirDeath$Year,China_IndiaAirDeath$SolidFuels_Death_HouseHold.x,col='red3',cex=20,lty='solid')
lines(China_IndiaAirDeath$Year,China_IndiaAirDeath$SolidFuels_Death_HouseHold.y,col='sienna',cex=20,lty='longdash')
lines(China_IndiaAirDeath$Year,China_IndiaAirDeath$India_outdoor_death,col='sienna2',cex=20,lty='longdash')
legend('topright',legend = c('China Air Outdoor pollution Deathrate','China Air Household pollution Deathrate','India Air Household Pollution Death Rate','India Air Outdoor Pollution Death Rate'),col = c('red','red3','sienna','sienna2'),lty=1:3,cex = 1)
#I plotted the China plot first and followed by India.I Finally added my legend.

#3. Merge the air pollution data with the gdp data using merge()
# Merge is a function that combines data across two data frames by matching ID rows
#By default merge will identify ID rows as those where column names are the same between datasets, but it is safer to specify the columns you want to merge by yourself using "by"
#In our case, we want to merge both by country (either the "Entity" or "Code" columns) and year columns
#Note that by default, the merge function keeps only the entries that appear in both data frames - that is fine for this lab. If you need for other applications, you can change using the all.x or all.y arguments to the function - check out the documentation at ?merge

AirPollu_GDP <- merge(AirPollu,GDP,by=c("Country",'Code','Year'),all.x = TRUE,all.y = FALSE)
#I Merged the two dataframes by Country, Code and Year. I excluded the years without pollution data


#4. I then plotted ou
#Make sure to add appropraite titles to the plots and axes
#Use ylim to keep the range of the y axis the same between the two plots - this makes it easier for the reader to compare across the two graphs
#STRECTH GOAL CHALLENGE - color the points based on continent. NOT REQUIRED FOR FULL POINTS - a challenge if you want to push yourself - continent info is included in the GDP dataset, but it is only listed for the year 2015
#If you are trying this and getting stuck ASK FOR HELP - there are some tips and tricks for making it easier 
par(mfrow=c(1,2))
#I try to partition the plot into two parts
plot(AirPollu_GDP$LogPopGDP,AirPollu_GDP$SolidFuels_Death_HouseHold,xlab = 'log of Population GDP',type='p',pch=20,ylab = "Air Pollution Deathrate(Death per 100,000)",ylim = c(0,250),main = 'indoor air pollution')
plot(AirPollu_GDP$LogPopGDP,AirPollu_GDP$OutDoorPolluDeath,xlab = 'log of Population GDP',type='p',pch=20,ylab = 'Air Pollution Deathrate(Death per 100,000)',ylim = c(0,250),main = 'outdoor air pollution')
#I then plotted the two plots, but they look different from what was presented in class. I am not sure why, so please let me know. Thank you! 

