# PA 2
# Prepare data
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url = fileUrl, destfile = "NEI_data.zip", method = "curl")
unzip("NEI_data.zip")
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Subsetting sources
# "Motor Vehicle"
SCC_motor_vehicle <- SCC[grep("vehicle", SCC$SCC.Level.Two, ignore.case = TRUE),]
unique(SCC_motor_vehicle[,"SCC.Level.Two"])
NEI_motor_vehicle <- NEI[NEI$SCC %in% SCC_motor_vehicle$SCC,]
# "coal combustion"
SCC_coal_combustion <- SCC[grep("Comb(.*)Coal", SCC$Short.Name, ignore.case=TRUE),]
SCC_coal_combustion1 <- SCC[grep("Comb{1}(.+)Coal{1}", SCC$Short.Name, ignore.case=TRUE),]


# Q1
# 1. Group data by year
# 2. Sum by groups
# 3. plot (hist?)
em_by_year <- aggregate(Emissions ~ year, NEI, sum)
with(em_by_year, plot(year, Emissions, xlab="Year", ylab="Emission (tons)"))
model <- lm(Emissions ~ year, em_by_year)
abline(model, lwd=1, col=3)
title("Total PM2.5 emission from all sources")


# Q2
# 1. Filter by fips == "24510"
# 2. Sum by groups
# 3. plot (hist?)
em_by_year_balt <- aggregate(Emissions ~ year, NEI[NEI$fips=="24510",], sum)
with(em_by_year_balt, plot(year, Emissions, xlab="Year", ylab="Emissions (tons)", pch=20))
title("Total emissions from PM2.5 in the Baltimore City")
model_balt <- lm(Emissions ~ year, em_by_year_balt)
abline(model_balt, lwd=1, col=3)


# Q3
# 1. Group data by type AND then by year
# 2. Filter by fips == "24510"
# 3. Sum by groups (types and years)
# 3. plot (hist?) 2x2
em_by_type_and_year_balt <- aggregate(Emissions ~ type + year, NEI[NEI$fips=="24510",], sum)
#g2 <- ggplot(em_by_type_and_year_balt, aes(year, log(Emissions)))
g2 <- ggplot(em_by_type_and_year_balt, aes(year, Emissions))
g2 + geom_point() + facet_grid(. ~ type) + geom_smooth(method="lm") + labs(title="Emissions by type in the Baltimore City", x="Year") + theme_bw() + theme(plot.title=element_text(size=14,face="bold"))


# Q4
# 1. Group data by year
# 2. Filter by sources (coal combustion-related sources)
# 3. Sum by groups
# 4. plot (hist?)
em_coal_by_year <- aggregate(Emissions ~ year, NEI[NEI$SCC %in% SCC_coal_combustion$SCC,], sum)
g4 <- ggplot(em_coal_by_year, aes(year, Emissions))
g4 + geom_point() + geom_smooth(method="lm") + labs(title="Emissions from coal combustion-related sources", x="Year") + theme_bw() + theme(plot.title=element_text(size=14,face="bold"))



# Q5
# 1. Group data by year
# 2. Filter by fips == "24510" AND sources (motor vehicle)
# 3. Sum by groups
# 3. plot (hist?)
#motor_vech <- grep("motor vehicle", SCC$SCC.Level.Three, ignore.case = TRUE)
#motor_vech_sources <- SCC[grep("motor vehicle", SCC$SCC.Level.Three, ignore.case = TRUE),"SCC"]
#motor_vehicle_emissions <- subset(NEI, SCC %in% motor_vech_sources)
#mv_em_by_year_balt <- aggregate(Emissions ~ year, motor_vehicle_emissions[motor_vehicle_emissions$fips=="24510",], sum)
mv_em_by_year_balt <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="24510",], sum)
with(mv_em_by_year_balt, plot(year, Emissions, xlab="Year", ylab="Emissions (tons)", pch=20))
title("Emissions from motor vehicle sources in the Baltimore City")
model2_balt <- lm(Emissions ~ year, mv_em_by_year_balt)
abline(model2_balt, lwd=1, col=3)
g5 <- ggplot(mv_em_by_year_balt, aes(year, Emissions))
g5 + geom_point() + geom_smooth(method="lm") + theme_bw()



# Q6
mv_em_by_year_la <- aggregate(Emissions ~ year, motor_vehicle_emissions[motor_vehicle_emissions$fips=="06037",], sum)
qplot(year, Emissions, data=mv_em_by_year, color=city, geom=c("point", "smooth"), method="lm")

g <- ggplot(mv_em_by_year, aes(year, Emissions))
g + geom_point(aes(color=city)) + labs(title="Emissions from motor vehicle sources", x="Year") + theme_bw() + theme(plot.title=element_text(size=14,face="bold"))


mv_em_by_year_balt <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="24510",], sum)
mv_em_by_year_la <- aggregate(Emissions ~ year, NEI_motor_vehicle[NEI_motor_vehicle$fips=="06037",], sum)
mv_em_by_year_la$region <- as.factor("Los Angeles County")
mv_em_by_year_balt$region <- as.factor("Baltimore City")
mv_em_by_year <- rbind(mv_em_by_year_balt, mv_em_by_year_la)
g6 <- qplot(year, ScaledEmissions, data=mv_em_by_year, color=region, geom=c("point", "smooth"), method="lm")
g6 + labs(title="Emissions from motor vehicle sources", x="Year") + theme_bw() + theme(plot.title=element_text(size=14,face="bold"))


mv_em_by_year_balt$EmissionsChange <- mv_em_by_year_balt$Emissions /  mv_em_by_year_balt[mv_em_by_year_balt$year == 1999 & mv_em_by_year_balt$region=="Baltimore City", "Emissions"]
mv_em_by_year_la$EmissionsChange <- mv_em_by_year_la$Emissions /  mv_em_by_year_la[mv_em_by_year_la$year == 1999 & mv_em_by_year_la$region=="Los Angeles County", "Emissions"]
mv_em_by_year <- rbind(mv_em_by_year_balt, mv_em_by_year_la)
g6 <- qplot(year, EmissionsChange * 100, data=mv_em_by_year, color=region, geom=c("point", "smooth"), method="lm")
g6 + labs(title="Change of emissions from motor vehicle sources", x="Year", y="Change of emission, %") + theme_bw() + theme(plot.title=element_text(size=14,face="bold"))
ggsave("plot6.png")
