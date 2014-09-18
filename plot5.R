##  Motor Vehicle Emission  in Baltimore
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

plot5 <- function() {
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
        "plot5.png",
        width     = 480,
        height    = 480,
        units     = "px",
    )
    
    # Motor Vehicle include Light, heavy, petrol and disel 
    # Any source which Level Two source includes Highway are consider motor vehile emission
    iHighway <- grep("Highway",SCC$SCC.Level.Two,ignore.case = TRUE,perl=TRUE)
    highwaySCC <- SCC[iHighway,]
    
    baltimoreMV <- baltimore[which(baltimore$SCC %in% highwaySCC$SCC),]
    baltimoreMVSum <- tapply(baltimoreMV$Emissions, baltimoreMV$year,sum)
    baltimoreMVSumDf <- as.data.frame(baltimoreMVSum)
    plot( row.names(baltimoreMVSumDf),baltimoreMVSumDf$baltimoreMVSum,  xlab="Year",  ylab="Emissions (tons)")
    title(main= "Baltimore Motor Vehicle Emissions 1999-2008")

    dev.off()
}
