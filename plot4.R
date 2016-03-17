source("data.R")

plot2 <- function(d) {
  plot(d$DateTime, d$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  lines(d$DateTime, d$Global_active_power)
}

plot3 <- function(d) {
  plot(d$DateTime, d$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
  lines(d$DateTime, d$Sub_metering_1)
  lines(d$DateTime, d$Sub_metering_2, col = "red")
  lines(d$DateTime, d$Sub_metering_3, col = "blue")
  
  legend("topright", lty = c(1,1), pt.cex = 1, cex = 0.4, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
}

plot4c <- function(d) {
  plot(d$DateTime, d$Voltage, type = "n", xlab = "datetime", ylab = "Voltage")
  lines(d$DateTime, d$Voltage)
}

plot4d <- function(d) {
  plot(d$DateTime, d$Global_reactive_power, type = "n", xlab = "datetime", ylab = "Global_reactive_power")
  lines(d$DateTime, d$Global_reactive_power)
}

plot4 <- function(d) {
  png(filename = "plot4.png", width = 480, height = 480)
  par(mfcol = c(2,2))
  plot2(d)
  plot3(d)
  plot4c(d)
  plot4d(d)
  
  dev.off()
}

plot4(read_data(paste(getwd(), "/data/household_power_consumption.txt", sep = "")))
