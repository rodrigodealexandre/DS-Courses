plot2 <- function() {
    hc <- read.table("household_power_consumption.txt", sep=";",header=T,na.strings ="?")
    hc<- na.omit(hc)
    df <- within(hc, datetime <- as.POSIXlt(paste(Date, Time), format="%d/%m/%Y %H:%M:%S"))
    
    sdf <- subset(df, as.Date(df$Date, "%d/%m/%Y") == "2007-02-01" | as.Date(df$Date, "%d/%m/%Y") == "2007-02-02")
    
    par(mfcol = c(1,1))
    
    plot(sdf$datetime, sdf$Global_active_power, type="l", ylab = "Global Active Power (KiloWatts)", xlab = "" )
    
    dev.copy(png, file = "plot2.png")
    dev.off()
    
}