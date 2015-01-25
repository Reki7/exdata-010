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


# Filter and aggregate emission data for the Baltimore City
em_by_type_and_year_balt <- aggregate(Emissions ~ type + year, NEI[NEI$fips=="24510",], sum)
#g3 <- ggplot(em_by_type_and_year_balt, aes(year, log(Emissions)))
g3 <- ggplot(em_by_type_and_year_balt, aes(year, Emissions))
g3 <- g3 + 
      geom_point() + 
      facet_grid(. ~ type) + 
      geom_smooth(method="lm") + 
      labs(title="Emissions by type in the Baltimore City", x="Year", y="Emissions (tons)") + 
      theme_bw() + 
      theme(plot.title=element_text(size=14,face="bold"))
# Save plot to file
ggsave("./plots/plot3.png", g3, width=25, units="cm", dpi=150)
