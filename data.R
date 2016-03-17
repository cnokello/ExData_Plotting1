read_data <- function(file_path) {
  
  conn <- file(file_path, open = "r")
  records <- data.frame()
  header <- ""
  processed <- 0
  while (length(record <- readLines(conn, n = 1, warn = FALSE)) > 0) {
    processed <- processed + 1
    if(header == "") {
      header <- record
    } else {
      text_conn <- textConnection(paste(header, record, sep = "\n"), open = "r")
      record_v <- read.csv(text_conn, header = T, sep = ";")
      record_v$DateTime <- strptime(paste(record_v$Date, record_v$Time, sep = " "), "%d/%m/%Y %H:%M:%S")
      record_v$Date <- as.Date(record_v$Date, "%d/%m/%Y")
      
      start_date <- record_v$Date - as.Date("2007-02-01", "%Y-%m-%d")
      end_date <- record_v$Date - as.Date("2007-02-02", "%Y-%m-%d")
      
      if(nrow(records) == 0 && (start_date == 0 || end_date == 0)) { 
        records <- record_v
      } else if(start_date == 0 || end_date == 0) {
        records <- rbind(records, record_v)
      }
      
      print(paste("Matched Records: ", nrow(records), sep = ""))
      print(paste("Processed: ", processed, sep = ""))
      close(text_conn)
      
      if(end_date > 0) {
        break
      }
    }
  }
  
  print(records)
  str(records)
  close(conn)
  
  records
}