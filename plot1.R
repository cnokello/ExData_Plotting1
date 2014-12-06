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

##
## This is the method to call 
## It loads data and plots 'plot1'
##
plot1 <- function(fileName = paste(getwd(), 
                                   'household_power_consumption.txt', 
                                   sep = '/')) {
  
  ##
  ## Set the custom date format
  ##
  setAs('character', 'consDate', as.ConsDate)
  
  ##
  ## Register the custom date class to avoid warnings
  setClass('consDate')
  
  ##
  ## Set colum classes
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
  ## Plot the 'plot'
  ##
  print("Plotting the 'Global Active Power' histogram...")
  par(mfrow=c(1,1))
  hist(subs_data$Global_active_power, xlab = 'Global Active Power (kilowatts)', 
       ylab = 'Frequency', main = 'Global Active Power', col = 'red')
  print("Done.")
  
  ##
  ## Save the PNG file
  ##
  png_filename = paste(getwd(), "/figure/plot1.png", sep = '/')
  print(paste("Saving PNG file to ", png_filename, sep = ' '))
  dev.print(png, file = png_filename, width = 480, height = 480)
  print('Done.')
}