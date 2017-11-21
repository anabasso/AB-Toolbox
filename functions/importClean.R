importClean <- function(path){
  dt <- read.csv(path, sep = ";", colClasses = "character")
  
  # Fix Column names
  names(dt) <- tolower(names(dt))
  names(dt) <- gsub("booking_id", "bookingId", names(dt))
  names(dt) <- gsub("^partition\\w+", "partition", names(dt))
  names(dt) <- gsub("ins.commissions.orders", "insCommissions", names(dt))
  names(dt) <- gsub("fee.amount.baggage", "bagFees", names(dt))
  names(dt) <- gsub("fee.amount.seat", "seatFees", names(dt))
  names(dt) <- gsub("expected.revenue.margin..excluded.advertising.and.other.revenue..orders", "rm", names(dt))
  
  dt <- fixNumeric(dt)
  
  # Tag all bookings and not-bookings
  dt$isBooking <- NA
  dt$isBooking[is.na(dt$bookingId)] <- "not-booking"
  dt$isBooking[!is.na(dt$bookingId)] <- "booking"  
  
  # Create issued
  dt$issued <- NA
  dt$issued[dt$status == "CONTRACT" & dt$isBooking == "booking"] <- 1
  dt$rm[dt$status != "CONTRACT"] <- 0
  dt$status <- NULL
  
  #Exclude first line
  dt <- dt[-1, ]
  
  # Change value of session from 0 to 1 in bookings
  dt$sessions[dt$isBooking == "booking"] <- 1
  
  # Reduce the number of bookings in each partition from the number of sessions in 3 partitions
  bookingsA <- sum(dt$orders[dt$partition == 1], na.rm = TRUE)
  dt$sessions[dt$isBooking == "not-booking" & dt$partition == 1] <- dt$sessions[dt$isBooking == "not-booking" & dt$partition == 1] - bookingsA
  
  bookingsB <- sum(dt$orders[dt$partition == 2], na.rm = TRUE)
  dt$sessions[dt$isBooking == "not-booking" & dt$partition == 2] <- dt$sessions[dt$isBooking == "not-booking" & dt$partition == 2] - bookingsB
  
  bookingsC <- sum(dt$orders[dt$partition == 3], na.rm = TRUE)
  dt$sessions[dt$isBooking == "not-booking" & dt$partition == 3] <- dt$sessions[dt$isBooking == "not-booking" & dt$partition == 3] - bookingsC
  
  # Return DT
  return(dt)
}