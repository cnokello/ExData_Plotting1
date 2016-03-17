source("data.R")

plot3 <- function(d) {
  png(filename = "plot3.png", width = 480, height = 480)
  par(mfcol = c(1, 1))
  
  plot(d$DateTime, d$Sub_metering_1, type = "n", xlab = "", ylab = "Energy sub metering")
  lines(d$DateTime, d$Sub_metering_1)
  lines(d$DateTime, d$Sub_metering_2, col = "red")
  lines(d$DateTime, d$Sub_metering_3, col = "blue")
  legend("topright", lty = c(1,1), cex = 0.75, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  
  dev.off()
}

plot3(read_data(paste(getwd(), "/data/household_power_consumption.txt", sep = "")))