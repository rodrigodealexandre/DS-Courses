plot3 <- function() {
    hc <- read.table("household_power_consumption.txt", sep=";",header=T,na.strings ="?")
    hc<- na.omit(hc)
    df <- within(hc, datetime <- as.POSIXlt(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    
    sdf <- subset(df, as.Date(df$Date, "%d/%m/%Y") == "2007-02-01" | as.Date(df$Date, "%d/%m/%Y") == "2007-02-02")
    
    png(filename = "plot3.png", width = 480, height = 480)
    par(mfcol = c(1,1))
    
    plot(sdf$datetime, sdf$Sub_metering_1, type="l", ylab = "Energy sub metering", xlab = "")
    lines(sdf$datetime, as.numeric(as.character(sdf$Sub_metering_2)), type = "l", col = "red")
    lines(sdf$datetime, as.numeric(as.character(sdf$Sub_metering_3)), type = "l", col = "blue")
    
    legend("topright", pch = "_", col =  c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    
    dev.off()
    
}