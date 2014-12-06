##
## @author nelson.okello
## @created on 06-Dec-2014
##

##
## Take care of date format
##
as.ConsDate <- function(from) {
  as.Date(from, format='%d/%m/%Y')
}

plot4 <- function(fileName = paste(getwd(), 
                                   'household_power_consumption.txt', 
                                   sep = '/')) {
  
  ##
  ## Set the custom date format
  ##
  setAs('character', 'consDate', as.ConsDate)
  
  ##
  ## Register the custom date class to avoid warnings
  ##
  setClass('consDate')
  
  ##
  ## Set colum classes
  ##
  col_classes <- c(Date = 'consDate', Time = 'factor', 'numeric', 'numeric', 
                   'numeric', 'numeric', 'numeric', 'numeric', 'numeric')
  
  ##
  ## Now, read the file
  ##
  print("Loading data...")
  cons_data <- read.csv('data/household_power_consumption.txt', 
                        header = T, sep = ';', stringsAsFactors = T, 
                        colClasses = col_classes, na.strings = c('?'))
  print("Done.")
  
  ##
  ## Subset the data to include only those for '2007-02-01' and '2007-02-02'
  ##
  print("Subsetting the dataset to '2007-02-01' and '2007-02-02' only...")
  subs_data <- subset(cons_data, Date == as.Date('2007-02-01') | 
                        Date == as.Date('2007-02-02'))
  print('Done.')
  
  ##
  ## Add Date/Time variable that combines the 'Date' and the 'Time' variables
  ##
  subs_data$DateTime <- as.POSIXct(paste(subs_data$Date, subs_data$Time, sep = ' '), 
                                   format = '%Y-%m-%d %H:%M:%S')
  
  ##
  ## Add 'WeekDay' variable
  # subs_data$WeekDay <- weekdays(subs_data$Date)
  
  ##
  ## Setup plotting params
  ##
  par(mfrow=c(2,2))
  par(cex = .45)
  
  ##
  ## Plot 1
  ##
  plot(subs_data$DateTime, subs_data$Global_active_power, type = 'l', xlab = '', 
       ylab = 'Global Active Power (kilowatts)')
  
  
  ##
  ## Plot 2
  ##
  plot(subs_data$DateTime, subs_data$Voltage, type = 'l', xlab = 'datetime', 
       ylab = 'Voltage')
  
  
  ##
  ## Plot 3
  ##
  print("Plotting the 'Global Active Power' histogram...")
  plot (range(subs_data$DateTime),range(subs_data$Sub_metering_1),type="n", 
        xlab = '', ylab = 'Energy sub metering', pch = 1)
  lines(subs_data$DateTime, subs_data$Sub_metering_1, type = 'l', col = 'black')
  lines(subs_data$DateTime, subs_data$Sub_metering_2, type = 'l', col = 'red')
  lines(subs_data$DateTime, subs_data$Sub_metering_3, type = 'l', col = 'blue')
  
  ## 
  ## Add legend
  ##
  legend('topright', col = c('black', 'red', 'blue'), 
         legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'), 
         lty = c(1, 1, 1), bty = 'n')
  
  
  
  
  ##
  ## Plot 4
  ##
  plot(subs_data$DateTime, subs_data$Global_reactive_power, type = 'l', xlab = 'datetime', 
       ylab = 'Global_reactive_power')
  
  
  print("Done.")
  
  ##
  ## Save the PNG file
  ##
  png_filename = paste(getwd(), "/figure/plot4.png", sep = '/')
  print(paste("Saving PNG file to ", png_filename, sep = ' '))
  dev.print(png, file = png_filename, width = 480, height = 480)
  print('Done.')
}