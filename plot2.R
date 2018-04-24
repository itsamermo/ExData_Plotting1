##plot2.R

require(data.table)
require(lubridate)

setwd("./Coursera/exploratory_data_analysis")

##read main data set
dt <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = "character") ##stringsAsFactors=TRUE)


##checking if records are malformed//issue with delims
##dtcols <- count.fields("household_power_consumption.txt",sep=";")

###set dataset to dat atable in memory
setDT(dt)

### convert class of Date to posixct
dt$Date <- as.POSIXct(dt$Date,format="%d/%m/%Y")

## subset based on date 
dt1 <- dt[ year(dt$Date)==2007 & month(dt$Date)==02 & ( mday(dt$Date)==1 | mday(dt$Date)==2 ),]

### convert class of Date to posixct
dt1$Time2 <- as.POSIXct(paste(dt1$Date,dt1$Time,sep=" "),format="%Y-%m-%d %H:%M:%S")
dt1$date1 <- as.Date.date(dt1$Date,format="%d/%m/%Y")

###derive day of week from date
dt1$DOW <- weekdays(as.Date(dt1$Date))

### convert class of Global active power to floats/numeric
dt1$Global_active_power <- as.numeric(dt1$Global_active_power)

## subset based on day of week 
##dt1 <- dt1[ DOW=='Thrusday' | DOW=='Friday'  | DOW=='Saturday',]



require(ggplot2)
old.par <- par(mar = c(0, 0, 0, 0))

## do plotting stuff with new settings
mp <- 	ggplot(dt1,
		aes(x=Time2, y=Global_active_power),
		  ylab = "Global Active Power (kilowatts)"
 
		  ) + geom_line() + scale_x_datetime(date_breaks = 'day',  date_labels = '%a') + xlab("") + ylab("Global Active Power (kilowatts)")  
		  
mp + theme_bw() + theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.text.y = element_text(angle=90)
) 
dev.copy(png,'plot2.png')
dev.off()		  
		  
par(old.par)