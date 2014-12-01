complete <- function(directory, id = 1:332) {
        
        setwd(directory)
        file_list <- list.files(pattern="*.csv")
        
        for (i in file_list[id]){
                
                if (!exists("dataset")){
                        set <- read.csv(i, header=TRUE, sep=",")
                        good <- complete.cases(set)
                        dataset<- set[good,]
                        rm(set)
                        rm(good)
                        nobs <- nrow(dataset)
                }
                
                else if (exists("dataset")){
                        
                        temp <-read.csv(i, header=TRUE, sep=",")
                        good <- complete.cases(temp)
                        temp_dataset <- temp[good,]
                        dataset<-rbind(dataset, temp_dataset)
                        
                        obs <- nrow(temp_dataset)
                        nobs <- c(nobs, obs)
                        
                        rm(temp_dataset)
                        rm(temp)
                        rm(good)
                        rm(obs)
                        
                }
                
        }
        setwd("../")
        
        
        frame <- data.frame(id, nobs)
        
        return(frame)
        
}
