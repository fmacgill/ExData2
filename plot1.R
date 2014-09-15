## Total emmission in US
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
## Using the base plotting system, make a plot showing the total PM2.5 emission from 
## all sources for each of the years 1999, 2002, 2005, and 2008.
## This first line will likely take a few seconds. Be patient!

loadData <- function(){
    # If the data has not been unpacked, try to unpack
    if(!file.exist("exdata-data-NEI_data") )
    {
        if(file.exist("exdata-data-NEI_data.zip")){
            upzip("exdata-data-NEI_data.zip")
        }
        else {
            exit()
        }
    }
    
    # if file available read in data
    if (file.exists("exdata-data-NEI_data/summarySCC_PM25.rds") && file.exists("exdata-data-NEI_data/Source_Classification_Code.rds")){        NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
        SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")
    }
    else {
        exit()
    }
}


plot1 <- function() {
    loadData()
    
    png(
        "plot1.png",
        width     = 480,
        height    = 480,
        units     = "px",
    )
    
    # Sum PM2.5 Emission by year for entire USA
    usaSum <- tapply(NEI$Emissions, NEI$year,sum)
    
    # Convert from tons to 1000's of tons, as more readable                  
    usaSum1000 <-  usaSum / 1000
    
    usaSumDf <- as.data.frame(usaSum1000)
    plot( row.names(usaSumDf),usaSumDf$usaSum,  xlab="Year", ylab="Emissions (1000's tons)")
    title(main= "USA PM2.5 Emission 1999-2008") 
}
