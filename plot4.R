##  Across the United States, how have emissions from coal combustion-related sources
## changed from 1999–2008?

plot4 <- function() {
    # If the data has not been unpacked, try to unpack
    if(!file.exists("exdata-data-NEI_data") )
    {
        if(file.exist("exdata-data-NEI_data.zip")){
            upzip("exdata-data-NEI_data.zip")
        }
        else {
            exit()
        }
    }
    
    # if file available read in data
    if (file.exists("exdata-data-NEI_data/summarySCC_PM25.rds") && file.exists("exdata-data-NEI_data/Source_Classification_Code.rds")){        
        NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
        SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
    }
    else {
        exit()
    }
    
    png(
        "plot4.png",
        width     = 480,
        height    = 480,
        units     = "px",
    )

    # create a logical row wise vector for coal sector
    # all the black and brown (lignite) coal based emissions include
    isect <- grep("coal",SCC$EI.Sector,ignore.case = TRUE,perl=TRUE)
    sector <- SCC[isect,]
    coalOnly <- NEI[which(NEI$SCC %in% sector$SCC),]
    

    coalSum <- tapply(coalOnly$Emissions, coalOnly$year,sum)
    coalSum1000 <- coalSum / 1000
    coalSumDf <- as.data.frame(coalSum1000)
    
    plot( row.names(coalSumDf),coalSumDf$coalSum,  xlab="Year",  ylab="Emissions (1000 tons)")
    title(main= "USA PM2.5 Emission 1999-2000 from Coal")
    
    dev.off()
}
