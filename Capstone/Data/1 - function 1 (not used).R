setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data")


testing <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    
    n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
    ngram = n_words + 1
    badwords <- read.csv("badwords.csv")
    
    
    if(ngram == 3){
        load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
        test <- get(paste(ngram,"-gram_",first_letter, sep=""))
        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
        
        if(length(prediction[,1])>=0){
            if(length(prediction[,1])==0){
                wbw <- NULL
            }
            if(length(prediction[,1])>0){
                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                wbw <- data.frame(cbind(wbw, prediction[,2]))
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw<- as.matrix(wbw,,ngram+1)
                if(length(wbw[,1])>=3){
                    return(as.vector(wbw[1:3,ngram]))
                }
            }
            if(length(wbw[,1])<3){
                result_1 <- as.vector(wbw[1:length(wbw[,1]),ngram])
                word2 <- strsplit(word, split="\\s+")
                word <- paste(word2[[1]][1], substring(word2[[1]][2], 1, nchar(word2[[1]][2])/2))
                prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                
                if(length(prediction[,1])>0){
                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw<- as.matrix(wbw,,ngram+1)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(as.vector(result_1[1:3]))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw<- as.matrix(wbw,,ngram+1)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(as.vector(result_1[1:3]))
                        }
                    }
                    if(length(result_1)<3){
                        word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- cbind(wbw, prediction[,2])
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw<- as.matrix(wbw,,ngram+1)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(as.vector(result_1[1:3]))
                            }
                            else if(length(result_1)>0){
                                return(as.vector(result_1))
                            }
                        }
                        else{
                            return(print("Sorry no prediction for your word"))
                        }
                    }
                }
            }
        }
    }
    if(ngram == 4){
        load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
        test <- get(paste(ngram,"-gram_",first_letter, sep=""))
        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
        
        if(length(prediction[,1])>=0){
            if(length(prediction[,1])==0){
                wbw <- NULL
            }
            if(length(prediction[,1])>0){
                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                wbw <- data.frame(cbind(wbw, prediction[,2]))
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw<- as.matrix(wbw,,ngram+1)
                if(length(wbw[,1])>=3){
                    return(as.vector(wbw[1:3,ngram]))
                }
            }
            if(length(wbw[,1])<3){
                result_1 <- as.vector(wbw[1:length(wbw[,1]),ngram])
                word2 <- strsplit(word, split="\\s+")
                word <- paste(word2[[1]][1], word2[[1]][2], substring(word2[[1]][3], 1, nchar(word2[[1]][3])/2))
                prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                
                if(length(prediction[,1])>0){
                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw<- as.matrix(wbw,,ngram+1)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(as.vector(result_1[1:3]))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], ceiling(nchar(word2[[1]][3])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw<- as.matrix(wbw,,ngram+1)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(as.vector(result_1[1:3]))
                        }
                    }
                    if(length(result_1)<3){    
                        word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], nchar(word2[[1]][3])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw<- as.matrix(wbw,,ngram+1)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(as.vector(result_1[1:3]))
                            }
                        }
                        
                        if(length(result_1)<3){
                            
                            word <- paste(word2[[1]][2], word2[[1]][3])
                            n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                            ngram = n_words + 1
                            first_letter <- substring(word, 1, 1)
                            if(first_letter == "I"){
                                first_letter = "i"
                            }
                            load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
                            test <- get(paste(ngram,"-gram_",first_letter, sep=""))
                            
                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                            
                            if(length(prediction[,1])>=0){
                                if(length(prediction[,1])==0){
                                    wbw <- NULL
                                }
                                if(length(prediction[,1])>0){
                                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                    wbw<- as.matrix(wbw,,ngram+1)
                                    
                                    if(length(wbw[,1])>0){
                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                        result_1 <- result_1[!duplicated(result_1)]
                                    }
                                    if(length(result_1)>=3){
                                        return(as.vector(result_1[1:3]))
                                    }
                                }
                                if(length(result_1)<3){
                                    
                                    word2 <- strsplit(word, split="\\s+")
                                    word <- paste(word2[[1]][1], substring(word2[[1]][2], 1, nchar(word2[[1]][2])/2))
                                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                                    
                                    if(length(prediction[,1])>0){
                                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                        
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                        
                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                        wbw<- as.matrix(wbw,,ngram+1)
                                        if(length(wbw[,1])>0){
                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                            result_1 <- result_1[!duplicated(result_1)]
                                        }
                                        if(length(result_1)>=3){
                                            return(as.vector(result_1[1:3]))
                                        }
                                    }
                                    if(length(result_1)<3){
                                        
                                        word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                        
                                        if(length(prediction[,1])>0){
                                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                            
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                            
                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                            wbw<- as.matrix(wbw,,ngram+1)
                                            if(length(wbw[,1])>0){
                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                result_1 <- result_1[!duplicated(result_1)]
                                            }
                                            if(length(result_1)>=3){
                                                return(as.vector(result_1[1:3]))
                                            }
                                        }
                                        if(length(result_1)<3){
                                            word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                            if(length(prediction[,1])>0){
                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                wbw <- cbind(wbw, prediction[,2])
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                
                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                wbw<- as.matrix(wbw,,ngram+1)
                                                if(length(wbw[,1])>0){
                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                    result_1 <- result_1[!duplicated(result_1)]
                                                }
                                                if(length(result_1)>=3){
                                                    return(as.vector(result_1[1:3]))
                                                }
                                                else if(length(result_1)>0){
                                                    return(as.vector(result_1))
                                                }
                                            }
                                            else{
                                                return(print("Sorry no prediction for your word"))
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if(ngram >= 5){
        
        word2 <- strsplit(word, split="\\s+")
        lenword <- length(word2[[1]])
        word <- paste(word2[[1]][(lenword-3):lenword], collapse=" ")
        n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
        ngram = n_words + 1
        
        load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
        test <- get(paste(ngram,"-gram_",first_letter, sep=""))
        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
        
        if(length(prediction[,1])>=0){
            if(length(prediction[,1])==0){
                wbw <- NULL
            }
            if(length(prediction[,1])>0){
                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                wbw <- data.frame(cbind(wbw, prediction[,2]))
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw<- as.matrix(wbw,,ngram+1)
                if(length(wbw[,1])>=3){
                    return(as.vector(wbw[1:3,ngram]))
                }
            }
            if(length(wbw[,1])<3){
                result_1 <- as.vector(wbw[1:length(wbw[,1]),ngram])
                word2 <- strsplit(word, split="\\s+")
                word <- paste(word2[[1]][1], word2[[1]][2], word2[[1]][3], substring(word2[[1]][4], 1, nchar(word2[[1]][4])/2))
                prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                
                if(length(prediction[,1])>0){
                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw<- as.matrix(wbw,,ngram+1)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(as.vector(result_1[1:3]))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1]," ", word2[[1]][2]," ", word2[[1]][3], " [[:graph:]]*" , substring(word2[[1]][4], ceiling(nchar(word2[[1]][4])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw<- as.matrix(wbw,,ngram+1)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(as.vector(result_1[1:3]))
                        }
                    }
                    if(length(result_1)<3){    
                        word <- paste(word2[[1]][1]," ", word2[[1]][2]," ", word2[[1]][3], " [[:graph:]]*" , substring(word2[[1]][4], nchar(word2[[1]][4])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw<- as.matrix(wbw,,ngram+1)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(as.vector(result_1[1:3]))
                            }
                        }
                        if(length(result_1)<3){
                            
                            word <- paste(word2[[1]][2], word2[[1]][3], word2[[1]][4])
                            n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                            ngram = n_words + 1
                            first_letter <- substring(word, 1, 1)
                            if(first_letter == "I"){
                                first_letter = "i"
                            }
                            load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
                            test <- get(paste(ngram,"-gram_",first_letter, sep=""))
                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                            
                            if(length(prediction[,1])>=0){
                                if(length(prediction[,1])==0){
                                    wbw <- NULL
                                }
                                if(length(prediction[,1])>0){
                                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                    wbw<- as.matrix(wbw,,ngram+1)
                                    
                                    if(length(wbw[,1])>0){
                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                        result_1 <- result_1[!duplicated(result_1)]
                                    }
                                    if(length(result_1)>=3){
                                        return(as.vector(result_1[1:3]))
                                    }
                                }
                                if(length(result_1)<3){
                                    
                                    word2 <- strsplit(word, split="\\s+")
                                    word <- paste(word2[[1]][1], word2[[1]][2], substring(word2[[1]][3], 1, nchar(word2[[1]][3])/2))
                                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                                    
                                    if(length(prediction[,1])>0){
                                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                        
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                        
                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                        wbw<- as.matrix(wbw,,ngram+1)
                                        if(length(wbw[,1])>0){
                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                            result_1 <- result_1[!duplicated(result_1)]
                                        }
                                        if(length(result_1)>=3){
                                            return(as.vector(result_1[1:3]))
                                        }
                                    }
                                    if(length(result_1)<3){
                                        
                                        word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], ceiling(nchar(word2[[1]][3])/2)), sep="")
                                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                        
                                        if(length(prediction[,1])>0){
                                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                            
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                            
                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                            wbw<- as.matrix(wbw,,ngram+1)
                                            if(length(wbw[,1])>0){
                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                result_1 <- result_1[!duplicated(result_1)]
                                            }
                                            if(length(result_1)>=3){
                                                return(as.vector(result_1[1:3]))
                                            }
                                        }
                                        if(length(result_1)<3){
                                            word <- paste(word2[[1]][1], " ", word2[[1]][2]," [[:graph:]]*" , substring(word2[[1]][3], nchar(word2[[1]][3])-2), sep="")
                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                            if(length(prediction[,1])>0){
                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                wbw <- cbind(wbw, prediction[,2])
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                
                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                wbw<- as.matrix(wbw,,ngram+1)
                                                if(length(wbw[,1])>0){
                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                    result_1 <- result_1[!duplicated(result_1)]
                                                }
                                                if(length(result_1)>=3){
                                                    return(as.vector(result_1[1:3]))
                                                }
                                                
                                            }
                                            
                                            if(length(result_1)<3){
                                                
                                                word <- paste(word2[[1]][2], word2[[1]][3])
                                                n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                                                ngram = n_words + 1
                                                first_letter <- substring(word, 1, 1)
                                                if(first_letter == "I"){
                                                    first_letter = "i"
                                                }
                                                load(paste(ngram,"-gram_",first_letter,".RData", sep=""))
                                                test <- get(paste(ngram,"-gram_",first_letter, sep=""))
                                                
                                                prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                                
                                                if(length(prediction[,1])>=0){
                                                    if(length(prediction[,1])==0){
                                                        wbw <- NULL
                                                    }
                                                    if(length(prediction[,1])>0){
                                                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                        wbw<- as.matrix(wbw,,ngram+1)
                                                        
                                                        if(length(wbw[,1])>0){
                                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                            result_1 <- result_1[!duplicated(result_1)]
                                                        }
                                                        if(length(result_1)>=3){
                                                            return(as.vector(result_1[1:3]))
                                                        }
                                                    }
                                                    if(length(result_1)<3){
                                                        
                                                        word2 <- strsplit(word, split="\\s+")
                                                        word <- paste(word2[[1]][1], substring(word2[[1]][2], 1, nchar(word2[[1]][2])/2))
                                                        prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                                                        
                                                        if(length(prediction[,1])>0){
                                                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                            
                                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                            
                                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                            wbw<- as.matrix(wbw,,ngram+1)
                                                            if(length(wbw[,1])>0){
                                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                result_1 <- result_1[!duplicated(result_1)]
                                                            }
                                                            if(length(result_1)>=3){
                                                                return(as.vector(result_1[1:3]))
                                                            }
                                                        }
                                                        if(length(result_1)<3){
                                                            
                                                            word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                                            
                                                            if(length(prediction[,1])>0){
                                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                                wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                                
                                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                                
                                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                                wbw<- as.matrix(wbw,,ngram+1)
                                                                if(length(wbw[,1])>0){
                                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                    result_1 <- result_1[!duplicated(result_1)]
                                                                }
                                                                if(length(result_1)>=3){
                                                                    return(as.vector(result_1[1:3]))
                                                                }
                                                            }
                                                            if(length(result_1)<3){
                                                                word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                                                                prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                                                if(length(prediction[,1])>0){
                                                                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                                    wbw <- cbind(wbw, prediction[,2])
                                                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                                    
                                                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                                    
                                                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                                    wbw<- as.matrix(wbw,,ngram+1)
                                                                    if(length(wbw[,1])>0){
                                                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                        result_1 <- result_1[!duplicated(result_1)]
                                                                    }
                                                                    if(length(result_1)>=3){
                                                                        return(as.vector(result_1[1:3]))
                                                                    }
                                                                    else if(length(result_1)>0){
                                                                        return(as.vector(result_1))
                                                                    }
                                                                }
                                                                else{
                                                                    return(print("Sorry no prediction for your word"))
                                                                }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }        
                            }
                        }
                    }
                }
            }
        }
    }
}



model.require <- function(){
    #not needed, no libraries need to be load
    
}

model.transform <- function(word){
    #no need to transform anything in the data
    word
    
}

model.predict <- function(word){
    testing(word)
    
}

yhat.config  <- c(username="rodrigodealexandre@hotmail.com",
                  apikey="54ddf919bb015a46e7852808b8fcba75",
                  env="http://cloud.yhathq.com/"
)


yhat.deploy("Prediction")
