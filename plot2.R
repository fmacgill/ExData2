## Total Emission in Baltimore
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
## from 1999 to 2008? 

plot2 <- function() {
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
        "plot2.png",
        width     = 480,
        height    = 480,
        units     = "px",
    )
    
    # baltimore
    baltimore <- NEI[which(NEI$fips=="24510"),]
    
    baltimoreSum <- tapply(baltimore$Emissions, baltimore$year,sum)
    baltimoreSumDf <- as.data.frame(baltimoreSum)
    plot( row.names(baltimoreSumDf),baltimoreSumDf$baltimoreSum, xlab="Year", ylab="Emissions (tons)")
    title(main = "Baltimore PM2.5 Emissions 1999-2008")

    
    dev.off()
}
