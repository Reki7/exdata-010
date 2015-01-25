library(ggplot2)
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


# Let's assume that "Motor Vehicle" are the sources that contain "vehicle" in their SCC.Level.Two variable
SCC_motor_vehicle <- SCC[grep("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE),]
NEI_motor_vehicle <- NEI[NEI$SCC %in% SCC_motor_vehicle$SCC,]
# Filter Motor Vehicle sources for the Baltimore City and then aggregate emissions data
em_motor_by_year_balt <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="24510",], sum)
em_motor_by_year_balt$region <- as.factor("Baltimore City")
# Filter Motor Vehicle sources for Los Angeles County and then aggregate emissions data
em_motor_by_year_la <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="06037",], sum)
em_motor_by_year_la$region <- as.factor("Los Angeles County")
# In order to see and compare trends clearer we plot not the raw value of emissions but its change to baseline of 1999 (100%)
em_motor_by_year_balt$EmissionsChange <- em_motor_by_year_balt$Emissions /  em_motor_by_year_balt[em_motor_by_year_balt$year == 1999 & em_motor_by_year_balt$region=="Baltimore City", "Emissions"]
em_motor_by_year_la$EmissionsChange <- em_motor_by_year_la$Emissions /  em_motor_by_year_la[em_motor_by_year_la$year == 1999 & em_motor_by_year_la$region=="Los Angeles County", "Emissions"]
# Merge datasets of Baltimore and Los Angeles
mv_em_by_year <- rbind(em_motor_by_year_balt, em_motor_by_year_la)
g6 <- qplot(year, EmissionsChange * 100, data=mv_em_by_year, color=region, geom=c("point", "smooth"), method="lm")
g6 <- g6 + 
      labs(title="Change of emissions from motor vehicle sources", x="Year", y="Change of emissions, %") + 
      theme_bw() + 
      theme(plot.title=element_text(size=14,face="bold"))
# Save plot to file
ggsave("./plots/plot6.png", g6, width=25, units="cm", dpi=150)