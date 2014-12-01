NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



# -------------------------------------------------------------------------------------
# Question 1

NEI <- readRDS("summarySCC_PM25.rds")
totalperyear <- tapply(NEI$Emissions, NEI$year, sum)
Year <- c(1999, 2002, 2005, 2008)
mtx <- data.frame(totalperyear, Year)
plot(mtx$Year, mtx$totalperyear, type="l", ylab = "Sum of emission from all sources of each year", 
     xlab = "Year",
     main = "Total emissions from PM2.5 decreased in the United States from 1999 to 2008.")


Plot1 <- qplot(Year, totalperyear, data =mtx , geom = "line") + 
    ylab("Sum of emission from all sources of each year") + 
    ggtitle("Total emissions from PM2.5 decreased in the United States from 1999 to 2008.")

# -------------------------------------------------------------------------------------
# Question 2

NEI <- readRDS("summarySCC_PM25.rds")
Baltimore <- subset(NEI, NEI$fips == "24510")
baltperyear <- tapply(Baltimore$Emissions, Baltimore$year, sum)
Year <- c(1999, 2002, 2005, 2008)
baltperyear <- data.frame(baltperyear, Year)

plot(baltperyear$Year, baltperyear$baltperyear, type="l", ylab = "Sum of emission from all sources of each year", 
     xlab = "Year",
     main = "Total emissions from PM2.5 decreased in Baltimore City from 1999 to 2008." )

# -------------------------------------------------------------------------------------
# Question 3

NEI <- readRDS("summarySCC_PM25.rds")
library(ggplot2)
library(plyr)
Baltimore <- subset(NEI, NEI$fips == "24510")
baltpertype <- ddply(Baltimore, .(year, type), function(x) sum(x$Emissions))
colnames(baltpertype)[3] <- "totalpertype"

Plot3 <- qplot(year, totalpertype, data =baltpertype , geom = "line", color = type) + 
    ylab("Sum of emission from each types for each year") + xlab("Year") + 
    ggtitle("Total emissions from PM2.5 decreased in Baltimore City from 1999 to 2008.") + 
    scale_colour_discrete(name="Type of\nemission")

# -------------------------------------------------------------------------------------
# Question 4

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")
allcoal <- subset(SCC, grepl(".*Coal.*", Short.Name) | grepl(".*Coal.*", EI.Sector))
coalemission <- subset(NEI, SCC %in% allcoal$SCC)
coalperyear <- tapply(coalemission$Emissions, coalemission$year, sum)
Year <- c(1999, 2002, 2005, 2008)
coalperyear <- data.frame(coalperyear, Year)

plot(coalperyear$Year, coalperyear$coalperyear, type="l", ylab = "Sum of emission of coal for each year", 
     xlab = "Year",
     main = "Coal emissions from PM2.5 decreased in the United States from 1999 to 2008.")

# -------------------------------------------------------------------------------------
# Question 5

NEI <- readRDS("summarySCC_PM25.rds")
Baltimore2 <- subset(NEI, NEI$fips == "24510" & type=="ON-ROAD")
baltmv <- tapply(Baltimore2$Emissions, Baltimore2$year, sum)
Year <- c(1999, 2002, 2005, 2008)
baltmv <- data.frame(baltmv, Year)

plot(baltmv$Year, baltmv$baltmv, type="l", ylab = "Sum of emission from  motor vehicle sources for each year", 
     xlab = "Year",
     main = "Total emissions from PM2.5 of  motor vehicle sourcesin Baltimore City from 1999 to 2008." )


# -------------------------------------------------------------------------------------
# Question 6

NEI <- readRDS("summarySCC_PM25.rds")
baltvslosmv <- subset(NEI, NEI$fips == "24510" & type=="ON-ROAD" |
                          NEI$fips == "06037" & type=="ON-ROAD")
library(plyr)
baltvslosmvperyear <- ddply(baltvslosmv, .(year, fips), function(x) sum(x$Emissions))
colnames(baltvslosmvperyear)[3] <- "totalpercity"

Plot6 <- qplot(year, totalpercity, data =baltvslosmvperyear , geom = "line", color = fips) + 
    ylab("Sum of emission from  motor vehicle sources for each year") + xlab("Year") + 
    ggtitle("Comparison of PM2.5 emission of Baltimore City and Los Angeles County from 1999 to 2008.") + 
    scale_colour_discrete(name="City of emission",  breaks=c("06037", "24510"),
                          labels=c("Los Angeles County", "Baltimore City"))