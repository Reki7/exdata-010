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


# Filter and aggregate emission data for the Baltimore City
em_by_year_balt <- aggregate(Emissions ~ year, NEI[NEI$fips=="24510",], sum)
# Open PNG device
png("./plots/plot2.png", width = 600, height = 600, units = "px")
# Make a plot, a trend line and a title
with(em_by_year_balt, plot(year, Emissions, xlab="Year", ylab="Emissions (tons)", pch=20))
title("Total emissions from PM2.5 in the Baltimore City")
model_balt <- lm(Emissions ~ year, em_by_year_balt)
abline(model_balt, lwd=1, col=3)
# Close PNG device
dev.off()