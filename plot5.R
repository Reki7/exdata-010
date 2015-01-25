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
#unique(SCC_motor_vehicle[,"SCC.Level.Two"])
NEI_motor_vehicle <- NEI[NEI$SCC %in% SCC_motor_vehicle$SCC,]
# Filter Motor Vehicle sources for the Baltimore City and then aggregate emissions data
em_motor_by_year_balt <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="24510",], sum)
g5 <- ggplot(em_motor_by_year_balt, aes(year, Emissions))
g5 <- g5 + 
      geom_point() + 
      geom_smooth(method="lm") + 
      labs(title="Emissions from  motor vehicle sources in the Baltimore City", x="Year", y="Emissions (tons)") + 
      theme_bw() + 
      theme(plot.title=element_text(size=14,face="bold"))
# Save plot to file
ggsave("./plots/plot5.png", g5, width=25, units="cm", dpi=150)