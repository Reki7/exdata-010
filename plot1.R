# Download and read data
if(!file.exists("./data")) {dir.create("./data")}
if(!file.exists("./plots")) {dir.create("./plots")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
fileName <- "./data/NEI_data.zip"
if(!file.exists(fileName)) {
  download.file(url = fileUrl, destfile = fileName, method = "curl", mode = "wb")
}
unzip(fileName, exdir = "./data")
NEI <- readRDS("./data/summarySCC_PM25.rds")
SCC <- readRDS("./data/Source_Classification_Code.rds")


# Aggregating emission values
em_by_year <- aggregate(Emissions ~ year, NEI, sum)
# Open PNG device
png("./plots/plot1.png", width = 600, height = 600, units = "px")
# Make a plot, a trend line and a title
with(em_by_year, plot(year, Emissions, xlab="Year", ylab="Emission (tons)"))
model <- lm(Emissions ~ year, em_by_year)
abline(model, lwd=1, col=3)
title("Total PM2.5 emissions from all sources")
# Close PNG device
dev.off()
