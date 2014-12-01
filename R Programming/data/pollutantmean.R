pollutantmean <- function(directory, pollutant, id = 1:332) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files
        setwd(directory)
        file_list <- list.files(pattern="*.csv")
        
        for (i in file_list[id]){
                
                if (!exists("dataset")){
                        dataset <- read.csv(i, header=TRUE, sep=",")
                        
                        
                }
                
                else if (exists("dataset")){
                        
                        temp_dataset <-read.csv(i, header=TRUE, sep=",")
                        dataset<-rbind(dataset, temp_dataset)
                        rm(temp_dataset)
                        
                }
                
        }
        if (pollutant == "sulfate") {
                setwd("../")
                return(round(mean(dataset$sulfate, na.rm = TRUE),3))
        }
                
        if (pollutant == "nitrate") {
                setwd("../")
                return(round(mean(dataset$nitrate, na.rm = TRUE),3))
        }
        rm(dataset)
        setwd("../")
}
