## Total emmission in US
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.
## This first line will likely take a few seconds. Be patient!

loadData <- function(){
    NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
    SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
}


# reshape data to US not county
# group by year
# sum emission
# ignore county
usaSum <- tapply(NEI$Emissions, NEI$year,sum)
usaSum1000 <-  usaSum / 1000
usaSumDf <- as.data.frame(usaSum1000)
plot( row.names(usaSumDf),usaSumDf$usaSum,  xlab="Year", ylab="Emissions (1000 ppm)")
title(main= "pptx 2.5 USA")


# baltimore
# reshape data to US not county
# group by year
# sum emission
# ignore county
baltimore <- NEI[which(fips=="24510"),]
baltimoreSum <- tapply(baltimore$Emissions, baltimore$year,sum)
baltimoreSumDf <- as.data.frame(baltimoreSum)
plot( row.names(baltimoreSumDf),baltimoreSumDf$baltimoreSum, xlab="Year", ylab="Emissions  ppm")
title(main = "ppx 2.5 Baltimore")

  
baltimore$type <- as.factor(baltimore$type)
baltimore$year <- as.factor(baltimore$year)
baltSum <- summaryBy(. ~type+year, data=baltimore,FUN=list(sum), keep.names=TRUE)
qplot(year,Emissions,data=baltSum,color=type)

# create a logical row wise vector for comb and coal sector
isect <- grep("coal",SCC$EI.Sector,ignore.case = TRUE,perl=TRUE)
sector <- SCC[isect,]
coalOnly <- NEI[which(NEI$SCC %in% sector$SCC),]
coalSum <- tapply(coalOnly$Emissions, coalOnly$year,sum)
coalSum1000 <- coalSum / 1000
coalSumDf <- as.data.frame(coalSum1000)
plot( row.names(coalSumDf),coalSumDf$coalSum,  xlab="Year",  ylab="Emissions (1000 ppm)")
title(main= "coal pptx 2.5 USA")


iHighway <- grep("Highway",SCC$SCC.Level.Two,ignore.case = TRUE,perl=TRUE)
highwaySCC <- SCC[iHighway,]

baltimoreMV <- baltimore[which(baltimore$SCC %in% highwaySCC$SCC),]
baltimoreMVSum <- tapply(baltimoreMV$Emissions, baltimoreMV$year,sum)
baltimoreMVSumDf <- as.data.frame(baltimoreMVSum)
plot( row.names(baltimoreMVSumDf),baltimoreMVSumDf$baltimoreMVSum,  xlab="Year",  ylab="Emissions ")
title(main= "Baltimore Motor Vehicle emissions")

losangles <- NEI[which(fips=="06037"),]
losanglesMV <- losangles[which(losangles$SCC %in% highwaySCC$SCC),]
losanglesMVSum <- tapply(losanglesMV$Emissions, losanglesMV$year,sum)
losanglesMVSumDf <- as.data.frame(losanglesMVSum)
plot( row.names(losanglesMVSumDf),losanglesMVSumDf$losanglesMVSum,  xlab="Year",  ylab="Emissions ")
title(main= "Los Angles Motor Vehicle emissions")

