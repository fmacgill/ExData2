##  Emission Types in Baltimore
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
## variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? 
## Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.
library(doBy)
library(ggplot2)

plot3 <- function() {
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
    

    
    # baltimore
    baltimore <- NEI[which(NEI$fips=="24510"),]
    
    baltimore$type <- as.factor(baltimore$type)
    baltimore$year <- as.factor(baltimore$year)
    baltSum <- summaryBy(. ~type+year, data=baltimore,FUN=list(sum), keep.names=TRUE)
    qplot(year,Emissions,data=baltSum,color=type,main="Baltimore PM2.5 Emissions 1999-2008 by Type",ylab="Emissions (tons)")
    

    

}
