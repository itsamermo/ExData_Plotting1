##plot4.R

require(data.table)
require(lubridate)
require(gridExtra)

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
dt1$datetime <- as.POSIXct(paste(dt1$Date,dt1$Time,sep=" "),format="%Y-%m-%d %H:%M:%S")


### convert class of Sub_metering_1 to numeric
options(digits=5)
dt1$Sub_metering_1 <- as.double(dt1$Sub_metering_1)
dt1$Sub_metering_2 <- as.double(dt1$Sub_metering_2)
dt1$Sub_metering_3 <- as.double(dt1$Sub_metering_3)


require(ggplot2)
old.par <- par(mar = c(0, 0, 0, 0))

## do plotting stuff with new settings

dt2 <- reshape2::melt(dt1, id.vars="datetime", 
                      measure.vars=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

ggp1 <- ggplot(dt2,
			aes(x=datetime, y=value, color=variable)
			) + geom_line() + scale_x_datetime(date_breaks = 'day',  date_labels = '%a') + xlab("") + ylab( "Energy sub metering " )

ggp1 + scale_color_manual(values=c("black", "red", "blue")) + theme_bw() + theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.text.y = element_text(angle=90),
  legend.position = c(1, 1),
  legend.justification = c("right", "top"),
  legend.box.just = "right",
  legend.box.background = element_rect(),
  legend.box.margin = margin(6, 6, 6, 6),
  legend.title = element_blank()
  ) 

  
## do plotting stuff with new settings
require(data.table)
require(lubridate)

### convert class of Global active power to floats/numeric
dt1$Global_active_power <- as.numeric(dt1$Global_active_power)
dt1$Global_reactive_power <- as.numeric(dt1$Global_reactive_power)

ggp2 <- 	ggplot(dt1,
		aes(x=datetime, y=Global_active_power)
 
		  ) + geom_line() + scale_x_datetime(date_breaks = 'day',  date_labels = '%a') + xlab("") + ylab("Global Active Power")  
		  
ggp2 + theme_bw() + theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.text.y = element_text(angle=90)
) 

### convert class of Global active power to floats/numeric
dt1$Voltage <- as.numeric(dt1$Voltage)


ggp3 <- 	ggplot(dt1,
		aes(x=datetime, y=Voltage)
 
		  ) + geom_line() + scale_x_datetime(date_breaks = 'day',  date_labels = '%a') + xlab("datetime") + ylab("Voltage")  
		  
ggp3 + theme_bw() + theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.text.y = element_text(angle=90)
) 

ggp4 <- 	ggplot(dt1,
		aes(x=datetime, y=Global_reactive_power)
		   
		  ) + geom_line() + scale_x_datetime(date_breaks = 'day',  date_labels = '%a')  
		  
ggp4 + theme_bw() + theme(
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.text.y = element_text(angle=90)
) 


install.packages(gridExtra)
require(gridExtra)
grid.arrange(ggp2, ggp3, ggp1, ggp4, nrow = 2, ncol =2,
			
			layout_matrix = rbind(c(1,2), c(3,4)))
			)




dev.copy(png,'plot4.png')
dev.off()		  
		  
par(old.par)