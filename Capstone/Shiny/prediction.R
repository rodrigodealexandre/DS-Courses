for(l in letters){
    for(n in 3:5){
        load(paste("Data/",n,"-gram_",l, ".RData",sep=""))
    }
}
badwords <- read.csv("Data/badwords.csv")

cleanword <- function(word){
    
    if(word =="--- Wait, loading data ---"){
       
        return(word)
    }
    
    word <- gsub("^[[:punct:][:space:][:cntrl:][:digit:]]+", "", word)
    
    
    word <- tolower(word)
    word <- gsub("^i ", "I ", word)
    word <- gsub("^i'", "I'", word)
    word <- gsub(" i ", " I ", word)
    word <- gsub(" i'", " I'", word)
    word <- gsub("\ni ", "\nI ", word)
    word <- gsub("\ni'", "\nI'", word)
    
    
    
    return(word)
}


prediction_model <- function(word){
    
    if(word =="--- Wait, loading data ---"){
        
        return(cat("!!!TRY ME!!!"))
    }
    
    word <- gsub("^[[:punct:][:space:][:cntrl:][:digit:]]+", "", word)
    word <- gsub("[[:space:]]+", " ", word)
    word <- tolower(word)
    word <- gsub("^i ", "I ", word)
    word <- gsub("^i'", "I'", word)
    word <- gsub(" i ", " I ", word)
    word <- gsub(" i$", " I", word)
    word <- gsub(" i'", " I'", word)
    
    first_letter <- substring(word, 1, 1)
    first_letter <- tolower(first_letter)
    
    n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
    ngram = n_words + 1
    
    if(ngram == 3){
        
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
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                if(length(wbw[,1])>=3){
                    return(cat(as.vector(wbw[1:3,ngram])))
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
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(cat(as.vector(result_1[1:3])))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(cat(as.vector(result_1[1:3])))
                        }
                    }
                    if(length(result_1)<3){
                        word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(cat(as.vector(result_1[1:3])))
                            }
                            else if(length(result_1)>0){
                                return(cat(as.vector(result_1)))
                            }
                        }
                        else{
                            return(cat("Sorry no prediction for your word"))
                        }
                    }
                }
            }
        }
    }
    if(ngram == 4){
        
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
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                if(length(wbw[,1])>=3){
                    return(cat(as.vector(wbw[1:3,ngram])))
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
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(cat(as.vector(result_1[1:3])))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], ceiling(nchar(word2[[1]][3])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(cat(as.vector(result_1[1:3])))
                        }
                    }
                    if(length(result_1)<3){    
                        word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], nchar(word2[[1]][3])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(cat(as.vector(result_1[1:3])))
                            }
                        }
                        
                        if(length(result_1)<3){
                            
                            word <- paste(word2[[1]][2], word2[[1]][3])
                            n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                            ngram = n_words + 1
                            first_letter <- substring(word, 1, 1)
                            first_letter <- tolower(first_letter)
                            
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
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    
                                    if(length(wbw[,1])>0){
                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                        result_1 <- result_1[!duplicated(result_1)]
                                    }
                                    if(length(result_1)>=3){
                                        return(cat(as.vector(result_1[1:3])))
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
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        if(length(wbw[,1])>0){
                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                            result_1 <- result_1[!duplicated(result_1)]
                                        }
                                        if(length(result_1)>=3){
                                            return(cat(as.vector(result_1[1:3])))
                                        }
                                    }
                                    if(length(result_1)<3){
                                        
                                        word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                        
                                        if(length(prediction[,1])>0){
                                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            if(length(wbw[,1])>0){
                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                result_1 <- result_1[!duplicated(result_1)]
                                            }
                                            if(length(result_1)>=3){
                                                return(cat(as.vector(result_1[1:3])))
                                            }
                                        }
                                        if(length(result_1)<3){
                                            word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                            if(length(prediction[,1])>0){
                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                if(length(wbw[,1])>0){
                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                    result_1 <- result_1[!duplicated(result_1)]
                                                }
                                                if(length(result_1)>=3){
                                                    return(cat(as.vector(result_1[1:3])))
                                                }
                                                else if(length(result_1)>0){
                                                    return(cat(as.vector(result_1)))
                                                }
                                            }
                                            else{
                                                return(cat("Sorry no prediction for your word"))
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
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                if(length(wbw[,1])>=3){
                    return(cat(as.vector(wbw[1:3,ngram])))
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
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                    if(length(wbw[,1])>0){
                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                        result_1 <- result_1[!duplicated(result_1)]
                    }
                    if(length(result_1)>=3){
                        return(cat(as.vector(result_1[1:3])))
                    }
                }
                if(length(result_1)<3){
                    
                    word <- paste(word2[[1]][1]," ", word2[[1]][2]," ", word2[[1]][3], " [[:graph:]]*" , substring(word2[[1]][4], ceiling(nchar(word2[[1]][4])/2)), sep="")
                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                    
                    if(length(prediction[,1])>0){
                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                        if(length(wbw[,1])>0){
                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                            result_1 <- result_1[!duplicated(result_1)]
                        }
                        if(length(result_1)>=3){
                            return(cat(as.vector(result_1[1:3])))
                        }
                    }
                    if(length(result_1)<3){    
                        word <- paste(word2[[1]][1]," ", word2[[1]][2]," ", word2[[1]][3], " [[:graph:]]*" , substring(word2[[1]][4], nchar(word2[[1]][4])-2), sep="")
                        prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                        if(length(prediction[,1])>0){
                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                            if(length(wbw[,1])>0){
                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                result_1 <- result_1[!duplicated(result_1)]
                            }
                            if(length(result_1)>=3){
                                return(cat(as.vector(result_1[1:3])))
                            }
                        }
                        if(length(result_1)<3){
                            
                            word <- paste(word2[[1]][2], word2[[1]][3], word2[[1]][4])
                            n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                            ngram = n_words + 1
                            first_letter <- substring(word, 1, 1)
                            first_letter <- tolower(first_letter)
                            
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
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                    
                                    if(length(wbw[,1])>0){
                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                        result_1 <- result_1[!duplicated(result_1)]
                                    }
                                    if(length(result_1)>=3){
                                        return(cat(as.vector(result_1[1:3])))
                                    }
                                }
                                if(length(result_1)<3){
                                    
                                    
                                    word <- paste(word2[[1]][1], word2[[1]][2], substring(word2[[1]][3], 1, nchar(word2[[1]][3])/2))
                                    prediction <- test[grepl(test[,1], pattern=paste("^",word, sep="")),1:2]
                                    
                                    if(length(prediction[,1])>0){
                                        wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                        wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                        wbw <- data.frame(cbind(wbw, prediction[,2]))
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                        if(length(wbw[,1])>0){
                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                            result_1 <- result_1[!duplicated(result_1)]
                                        }
                                        if(length(result_1)>=3){
                                            return(cat(as.vector(result_1[1:3])))
                                        }
                                    }
                                    if(length(result_1)<3){
                                        
                                        word <- paste(word2[[1]][1], " ", word2[[1]][2], " [[:graph:]]*" , substring(word2[[1]][3], ceiling(nchar(word2[[1]][3])/2)), sep="")
                                        prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                        
                                        if(length(prediction[,1])>0){
                                            wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                            wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                            wbw <- data.frame(cbind(wbw, prediction[,2]))
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                            if(length(wbw[,1])>0){
                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                result_1 <- result_1[!duplicated(result_1)]
                                            }
                                            if(length(result_1)>=3){
                                                return(cat(as.vector(result_1[1:3])))
                                            }
                                        }
                                        if(length(result_1)<3){
                                            word <- paste(word2[[1]][1], " ", word2[[1]][2]," [[:graph:]]*" , substring(word2[[1]][3], nchar(word2[[1]][3])-2), sep="")
                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                            if(length(prediction[,1])>0){
                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                if(length(wbw[,1])>0){
                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                    result_1 <- result_1[!duplicated(result_1)]
                                                }
                                                if(length(result_1)>=3){
                                                    return(cat(as.vector(result_1[1:3])))
                                                }
                                                
                                            }
                                            
                                            if(length(result_1)<3){
                                                
                                                word <- paste(word2[[1]][2], word2[[1]][3])
                                                n_words <- length(strsplit(as.matrix(word),'\\s+')[[1]])
                                                ngram = n_words + 1
                                                first_letter <- substring(word, 1, 1)
                                                first_letter <- tolower(first_letter)
                                                
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
                                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                        wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                        wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                        wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                        
                                                        if(length(wbw[,1])>0){
                                                            result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                            result_1 <- result_1[!duplicated(result_1)]
                                                        }
                                                        if(length(result_1)>=3){
                                                            return(cat(as.vector(result_1[1:3])))
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
                                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                            wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                            wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                            wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                            if(length(wbw[,1])>0){
                                                                result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                result_1 <- result_1[!duplicated(result_1)]
                                                            }
                                                            if(length(result_1)>=3){
                                                                return(cat(as.vector(result_1[1:3])))
                                                            }
                                                        }
                                                        if(length(result_1)<3){
                                                            
                                                            word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], ceiling(nchar(word2[[1]][2])/2)), sep="")
                                                            prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                                            
                                                            if(length(prediction[,1])>0){
                                                                wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                                wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                                wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                                wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                if(length(wbw[,1])>0){
                                                                    result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                    result_1 <- result_1[!duplicated(result_1)]
                                                                }
                                                                if(length(result_1)>=3){
                                                                    return(cat(as.vector(result_1[1:3])))
                                                                }
                                                            }
                                                            if(length(result_1)<3){
                                                                word <- paste(word2[[1]][1], " [[:graph:]]*" , substring(word2[[1]][2], nchar(word2[[1]][2])-2), sep="")
                                                                prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),1:2]
                                                                if(length(prediction[,1])>0){
                                                                    wbw <- strsplit(as.matrix(prediction[,1]), split="\\s+")
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram, byrow = TRUE)
                                                                    wbw <- data.frame(cbind(wbw, prediction[,2]))
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",result_1,"$", collapse="|", sep="")),]
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                    wbw <- wbw[!grepl(wbw[,ngram], pattern = paste("^",badwords[,1],"$", collapse="|", sep="")),]
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                    wbw <- wbw[order(as.numeric(wbw[,ngram+1]), decreasing = TRUE),]
                                                                    wbw <- matrix(unlist(wbw), ncol = ngram+1, byrow = F)
                                                                    if(length(wbw[,1])>0){
                                                                        result_1 <- c(result_1, as.vector(wbw[1:length(wbw[,1]),ngram]))
                                                                        result_1 <- result_1[!duplicated(result_1)]
                                                                    }
                                                                    if(length(result_1)>=3){
                                                                        return(cat(as.vector(result_1[1:3])))
                                                                    }
                                                                    else if(length(result_1)>0){
                                                                        return(cat(as.vector(result_1)))
                                                                    }
                                                                }
                                                                else{
                                                                    return(cat("Sorry no prediction for your word"))
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


