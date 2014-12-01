corr <- function(directory, threshold = 0) {
        
        comp1 <- complete(directory)
        comp <- comp1[!comp1$nobs == 0,]
        th <- subset(comp, comp$nobs >= threshold)
        setwd(directory)
        file_list <- list.files(pattern="*.csv")
        
        if (nrow(th) != 0){
                thid <- th$id
                
                
                
                
                for (i in file_list[thid]){
                        
                        if (!exists("corrdataset")){
                                set <- read.csv(i, header=TRUE, sep=",")
                                good <- complete.cases(set)
                                corrdataset<- set[good,]
                                rm(set)
                                rm(good)
                                cor_SN <- round(cor(corrdataset$sulfate, corrdataset$nitrate),4)
                        }
                        
                        else if (exists("corrdataset")){
                                
                                temp <-read.csv(i, header=TRUE, sep=",")
                                good <- complete.cases(temp)
                                temp_corrdataset <- temp[good,]
                                corrdataset<-rbind(corrdataset, temp_corrdataset)
                                
                                SN <- round(cor(temp_corrdataset$sulfate, temp_corrdataset$nitrate),4)
                                cor_SN <- c(cor_SN, SN)
                                
                                rm(temp_corrdataset)
                                rm(temp)
                                rm(good)
                                rm(SN)
                                
                        }            
                }
                setwd("../")
                return(cor_SN)
        }
        else {
                cor_SN<- vector(length =0, mode = "numeric")
                setwd("../")
                return(cor_SN)
        }
}