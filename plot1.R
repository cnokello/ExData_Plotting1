source("data.R")

plot1 <- function(d) {
  png(filename = "plot1.png", width = 480, height = 480)
  par(mfcol = c(1,1))
  hist(d, breaks = 12, col = "red", mfrow = c(1,1), main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency")
  dev.off()
}

plot1(read_data(paste(getwd(), "/data/household_power_consumption.txt", sep = ""))$Global_active_power)