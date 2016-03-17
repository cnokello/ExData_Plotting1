source("data.R")

plot2 <- function(d) {
  png(filename = "plot2.png", width = 480, height = 480)
  par(mfcol = c(1, 1))
  plot(d$DateTime, d$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  lines(d$DateTime, d$Global_active_power)
  dev.off()
}

plot2(read_data(paste(getwd(), "/data/household_power_consumption.txt", sep = "")))