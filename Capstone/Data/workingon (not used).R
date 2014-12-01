library("tm")

#reading the whole directory:
directory_source <- DirSource("FILE")
Combination_of_all_files <- Corpus(directory_source,readerControl=list(reader=readPlain))
#Reading encoding = "UTF-8"
directory_source <- DirSource("FILE", encoding = "UTF-8")
Combination_of_all_files <- Corpus(directory_source,readerControl=list(reader=readPlain))

#reading a single file:
twitter <- readLines("en_US.twitter.txt")


#learn how to do regular expressions:
#> glob2rx("i'")
#[1] "^i'$"
#> glob2rx("abc.*")
#[1] "^abc\\."

#checking first 10 lines
as.list(us_files)[[1]][1:10]
#count how many rows
length(as.list(us_files)[[1]])

#--------------

setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data")
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data/final ngrams")

test <- read.csv("ngram3_DF_z.csv", row.names = 1)


test <- read.csv(paste("ngram3_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
colClasses=c("NULL", NA, NA)


testing3 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram_clean_3_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}

testing33 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram3_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:10,])
}


testing4 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram_clean_4_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}

testing44 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram4_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}


testing5 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram_clean_5_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}



testing55 <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- read.csv(paste("ngram5_DF_", first_letter, ".csv", sep=""), colClasses=c("NULL", NA, NA))
    
    prediction <- test[grepl(test[,1], pattern=paste("^",word, " ", sep="")),]
    inorder <- prediction[order(prediction$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}





#separate the lines that stat with an space or a punctuation (not needed)
# for(g in 1:8){
#     test <- get(paste("ngram3_", g, sep=""))
#     if(!exists("With_non_letter")){
#         assign("With_non_letter", test[grepl(test, pattern="^[[:digit:][:punct:][:blank:]]")])
#     }
#     else if(exists("With_non_letter")){
#         assign("With_non_letter", c(With_non_letter, test[grepl(test, pattern="^[[:digit:][:punct:][:blank:]]")]))
#     }
# }



testing <- function(word){
    first_letter <- substring(word, 1, 1)
    if(first_letter == "I"){
        first_letter = "i"
    }
    test <- get(paste("With_", first_letter, sep=""))
    
    
    prediction <- test[grepl(test, pattern=paste("^",word, " ", sep=""))]
    data <- data.frame(table(prediction))
    inorder <- data[order(data$Freq, decreasing = TRUE),]
    return(inorder[1:10,])
}




while(1>0){wait(beep(4));wait(beep(6))}    



















p1 <- proc.time()
ngram3_list1 <- apply(clean_data1, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
proc.time() - p1
while(1>0){wait(beep(4));wait(beep(6))}


apply(x2, 1, function(x) textcnt(x, method = "string", n = 3L, split = " ", tolower=F))



ngram3_list <- apply(clean_data, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
ngram3 <- rapply(ngram3_list, function(x) as.matrix(get.ngrams(x)))

ngram4_list <- apply(clean_data, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
ngram4 <- rapply(ngram4_list, function(x) as.matrix(get.ngrams(x)))












#Loop for converting to multiple matrixes (WAYYY TO SLOW)
# for(num_files in 1:length(us_files)){
#     #converting Corpus file to a list object
#     aslines <- as.list(us_files[[num_files]])
#     #creating an empty data frame
#     newmatrix <- data.frame()
#     #looping each line
#     for(n in aslines){
#         #looping each dot separated vector
#         newmatrix<- rbind(newmatrix, data.frame(n))
#         }
#     assign(paste("newmatrix",num_files,sep=""),newmatrix)
#     rm(newmatrix)
#     rm(num_files)
#     rm(n)
#     rm(aslines)
# }

# #Loop for converting to multiple matrixes and removing phrases with less than 3 words (WAYYY TO SLOW)
# for(num_files in 1:length(us_files)){
#     #converting Corpus file to a list object
#     aslines <- as.list(us_files[[num_files]])
#     #creating an empty data frame
#     newmatrix <- data.frame()
#     #looping each line
#     for(n in aslines){
#         sizing <- data.frame()
#         #looping each dot separated vector
#         for(i in n){
#             #code for selecting and counting the number of words in the phrase to be bigger than 2
#             if(length(strsplit(as.matrix(i),'\\s+')[[1]]) >=3){
#                 sizing<- rbind(sizing, data.frame(i))
#             }
#         }
#         newmatrix<- rbind(newmatrix, sizing)
#         rm(sizing)
#     }
#     assign(paste("newmatrix",num_files,sep=""),newmatrix)
#     rm(newmatrix)
#     rm(num_files)
#     rm(n)
#     rm(i)
# }









assign(paste("aslines",num_files,sep=""),as.list(us_files[[num_files]]))
#creating a matrix with phrases bigger than 2 words
assign(paste("newmatrix",num_files,sep=""),data.frame())



selected <- aslines[grepl(aslines, pattern="will")]

ng3 <- ngram(aslines , n =3)
grams <- get.ngrams(ng3)
grams[grepl(grams, pattern="will")]


#skipping error within a loop
tryCatch({
    #put your loop with error here
}, error=function(e){})



ngram3 <- function(file){
    for (i in 1:length(file)){ 
        if (!exists("ngwords")){
            tryCatch({
                ng3 <- ngram(file[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords <- as.matrix(grams)
            }, error=function(e){})
        }
        else if (exists("ngwords")){
            tryCatch({
                ng3 <- ngram(file[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords_temp <- as.matrix(grams)
                ngwords <- rbind(ngwords, ngwords_temp)
                rm(ngwords_temp)
                rm(ng3)
                rm(grams)
            }, error=function(e){})
        }
    }
    return(ngwords)
}






test2 <- apply(test, 1, function(x) as.matrix(get.ngrams(x)))







t1 <- function(file){
    p1 <- proc.time()
    test <- apply(file, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
    proc.time() - p1
}

t4 <- function(file){
    p1 <- proc.time()
    text <- textcnt(file, method = "string", n = 3L)
    proc.time() - p1
}


t3 <- function(file){
    p1 <- proc.time()
    test2 <- rapply(file, function(x) as.matrix(get.ngrams(x)))
    proc.time() - p1
}

t2 <- function(file){
    p1 <- proc.time()
    for (i in 1:length(file)){ 
        if (!exists("ngwords")){
            tryCatch({
                ng3 <- ngram(file[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords <- as.matrix(grams)
            }, error=function(e){})
        }
        else if (exists("ngwords")){
            tryCatch({
                ng3 <- ngram(file[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords_temp <- as.matrix(grams)
                ngwords <- rbind(ngwords, ngwords_temp)
                rm(ngwords_temp)
                rm(ng3)
                rm(grams)
            }, error=function(e){})
        }
    }
    proc.time() - p1
}









beep(4); Sys.sleep(3); beep(4); Sys.sleep(3); beep(4); Sys.sleep(3); beep(4); Sys.sleep(3); beep(4); Sys.sleep(3); beep(4); Sys.sleep(3);


testing <- function(word){
    selected <- aslines[grepl(aslines, pattern=paste(word," ", sep = ""))]
    for (i in 1:length(selected)){ 
        if (!exists("ngwords")){
            tryCatch({
                ng3 <- ngram(selected[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords <- as.matrix(grams)
            }, error=function(e){})
        }
        else if (exists("ngwords")){
            tryCatch({
                ng3 <- ngram(selected[i] , n =3)
                grams <- get.ngrams(ng3)
                ngwords_temp <- as.matrix(grams)
                ngwords <- rbind(ngwords, ngwords_temp)
                rm(ngwords_temp)
                rm(ng3)
                rm(grams)
            }, error=function(e){})
        }
    }
    prediction <- ngwords[grepl(ngwords, pattern=paste("^",word, sep=""))]
    data <- data.frame(table(prediction))
    inorder <- data[order(data$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}


testing4 <- function(word){
    selected <- aslines[grepl(aslines, pattern=paste(word," ", sep = ""))]
    for (i in 1:length(selected)){ 
        if (!exists("ngwords")){
            tryCatch({
                ng3 <- ngram(selected[i] , n =4)
                grams <- get.ngrams(ng3)
                ngwords <- as.matrix(grams)
            }, error=function(e){})
        }
        else if (exists("ngwords")){
            tryCatch({
                ng3 <- ngram(selected[i] , n =4)
                grams <- get.ngrams(ng3)
                ngwords_temp <- as.matrix(grams)
                ngwords <- rbind(ngwords, ngwords_temp)
                rm(ngwords_temp)
                rm(ng3)
                rm(grams)
            }, error=function(e){})
        }
    }
    prediction <- ngwords[grepl(ngwords, pattern=paste("^",word, sep=""))]
    data <- data.frame(table(prediction))
    inorder <- data[order(data$Freq, decreasing = TRUE),]
    return(inorder[1:3,])
}


#stringi convert general
#test <- tm_map(test, function(x) stri_trans_general(x[[1]], "lower"))
#remove non ASCII
#test <- tm_map(test, function(x) iconv(x, "latin1", "ASCII", sub=""))


#remove English common words like 'the'
# mtest <- tm_map(test, removeWords, stopwords("english"))





#reduce inflected (or sometimes derived) words to their stem, base or root (not sure if needed)
#test <- tm_map(test, stemDocument)
# stem completion
test <- tm_map(test, stemCompletion, dictionary=dictCorpus)


#remove badwords
badwods <- readLines("badwords.txt")
test <- tm_map(test, removeWords, badwords)
#removing lines?
test[[1]][grepl(test[[1]], pattern = "zeke")]


str1 <- as.list(test[1])
