##  Motor Vehicle Emission  in Baltimore
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

plot6 <- function() {
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
    

    
    # Motor Vehicle include Light, heavy, petrol and disel 
    # Any source which Level Two source includes Highway are consider motor vehile emission
    iHighway <- grep("Highway",SCC$SCC.Level.Two,ignore.case = TRUE,perl=TRUE)
    highwaySCC <- SCC[iHighway,]
    
    MV <- NEI[which(NEI$SCC %in% highwaySCC$SCC),]
    MVsub <- MV[which(MV$fips=="06037" | MV$fips =="24510"),]
    MVsub$year <- as.factor(MVsub$year)
    
    # Provide nice labels
    MVsub$county <- factor(MVsub$fips, levels=c("06037", "24510"), labels=c("LA", "Baltimore"))
    
    MVsubSummary <- summaryBy(. ~county+year, data=MVsub,FUN=list(sum), keep.names=TRUE)
    qplot(year,Emissions,data=MVsubSummary,color=county,main="Baltimore and Los Angles Motor Vehicle PM2.5 Emissions 1999-2008",ylab="Emissions (tons)")
    
    
    

}
