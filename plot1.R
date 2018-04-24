##plot1.R
##plot1.R

require(data.table)
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


### convert class of Global active power to floats/numeric
dt1$Global_active_power <- as.numeric(dt1$Global_active_power)

###plot
##hist(dt1$Global_active_power, col="red")
##title(main="Global Active Power", xlab = "Global Active Power (kilowatts)")

require(ggplot2)
old.par <- par(mar = c(0, 0, 0, 0))
## do plotting stuff with new settings

mp <- 	qplot(dt1$Global_active_power,
		  geom="histogram",
		  breaks = seq( 0,6, by=.5),
		  binwidth = 0.5,  
		  main = "Global Active Power", 
		  xlab = "Global Active Power (kilowatts)",  
		  ylab = "Frequency",
		  fill=I("red"),
		  col=I("black"))
		  
		  
mp + theme_bw() + theme(
panel.border = element_blank(), 
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), 
axis.line = element_line(colour = "black"),
axis.ticks.y = element_line(c(0,1200,by=200)),
axis.text.y = element_text(angle=90)
) + scale_y_continuous(breaks = seq(0, 1200, by = 200)) + coord_cartesian(xlim=c(0,6), ylim=c(0,1200))##, expand = TRUE)

dev.copy(png,'plot1.png')
dev.off()		  
		  
par(old.par)		  