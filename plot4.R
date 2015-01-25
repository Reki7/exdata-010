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


# Let's assume that "coal combustion-related sources" are the sources that contain "Comb" and "Coal" in their Short.Name variable
SCC_coal_combustion <- SCC[grep("Comb(.*)Coal", SCC$Short.Name, ignore.case=TRUE),]
# Filter coal combustion-related sources and then aggregate emissions data
em_coal_by_year <- aggregate(Emissions ~ year, NEI[NEI$SCC %in% SCC_coal_combustion$SCC,], sum)
g4 <- ggplot(em_coal_by_year, aes(year, Emissions))
g4 <- g4 + 
      geom_point() + 
      geom_smooth(method="lm") + 
      labs(title="Emissions from coal combustion-related sources", x="Year", y="Emissions (tons)") + 
      theme_bw() + 
      theme(plot.title=element_text(size=14,face="bold"))
# Save plot to file
ggsave("./plots/plot4.png", g4, width=25, units="cm", dpi=150)