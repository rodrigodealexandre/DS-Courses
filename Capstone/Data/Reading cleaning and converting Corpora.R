#Reading, cleaning Corpora files and converting to a list type

#------------
# install.packages("tm")
# install.packages("stringi")
# install.packages("qdap")

library("tm")
library("stringi")
#library("gsubfn")
library("qdap")
library("beepr")
library("audio")
library("ngram")
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")

#source for Windows
directory_source <- DirSource("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data/final/en_US", encoding = "UTF-8")
us_files <- Corpus(directory_source,readerControl=list(reader=readPlain))

#Check how many documents there is in this file
length(as.list(us_files))

#convert to lower case
us_files <- tm_map(us_files, function(x) stri_trans_tolower(x[[1]]))
#make all lower letters - don't work with special characters (non ASCII)
#us_files <- tm_map(us_files, tolower)

#transform to english_US
us_files <- tm_map(us_files, function(x) stri_trans_general(x, "en_US"))
#transforming i in I again:
us_files <- tm_map(us_files, function(x) gsub("^i ", "I ", x))
us_files <- tm_map(us_files, function(x) gsub("^i'", "I'", x))
us_files <- tm_map(us_files, function(x) gsub(" i ", " I ", x))
us_files <- tm_map(us_files, function(x) gsub(" i'", " I'", x))
us_files <- tm_map(us_files, function(x) gsub(".i ", ". I ", x))
us_files <- tm_map(us_files, function(x) gsub(".i'", ". I'", x))
#multiple transformations (but reallyslow)
#us_files <- tm_map(us_files, function(x) gsubfn(".", list("^i "="I ", "^i'"="I'"), x))

#remove punctuation with exception of . and '
us_files <- tm_map(us_files, function(x) gsub("[!?,.]+", ".", x))
us_files <- tm_map(us_files, function(x) gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', "", x))
#remove ponctuation not gonna use this code because it removes . and '
#us_files <- tm_map(us_files, removePunctuation)

#remove white spaces
us_files <- tm_map(us_files, stripWhitespace)
#remove numbers
us_files <- tm_map(us_files, removeNumbers)
#remove dot spaces for later split.
us_files <- tm_map(us_files, function(x) gsub(" \\.", ".", x))
us_files <- tm_map(us_files, function(x) gsub("\\. ", ".", x))
#splitting by dots
us_files <- tm_map(us_files, function(x) strsplit(x, "\\."))

#how to extrat words from a language
#us_files <- tm_map(us_files, function(x) stri_extract_words(x[[1]], locale = "en"))

#save for later use
save(us_files, file="us_files.RData")

#convert to a list document
aslines <- as.list(us_files[1])
#convert to matrix with 1 column only
data_table <- matrix(unlist(aslines), ncol = 1, byrow = TRUE)
#remove lines that has less than 3 words
clean_data <- matrix(data_table[wc(data_table)>=3])

#save for later use
save(clean_data, file="clean_data.RData")

load("clean_data.RData")

clean_data <- iconv(clean_data, "UTF-8", "UTF-8")
#transforming single double quotes to apostrophe
clean_data <- gsub("[\u2018\u2019\u201A\u201B\u2032\u2035\u0092\u009d\u0096\u0093\u0094\u0099\u0091]", "'", clean_data)
clean_data <- gsub("[\031] ", "'", clean_data)

#removing double quotes and curly double quotes
clean_data <- gsub('[\u201C\u201D\u201E\u201F\u2033\u2036]', '', clean_data)

#removing strange stuffs
clean_data <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201C\u201D\u201E\u201F\u2033\u2036\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", clean_data)
clean_data <- gsub("[\002\020\023\177\003]", "", clean_data)

#removing excessive lines
clean_data <- gsub(" [-]+ ", " ", clean_data)
clean_data <- gsub(" [-]+", " ", clean_data)
clean_data <- gsub("^[-]+", "", clean_data)
clean_data <- gsub("[-]+ ", " ", clean_data)
clean_data <- gsub("[-]+", "-", clean_data)

#converting lower to capital
clean_data <- gsub("^i ", "I ", clean_data)
clean_data <- gsub("^i'", "I'", clean_data)
clean_data <- gsub(" i ", " I ", clean_data)
clean_data <- gsub(" i'", " I'", clean_data)

#fixing "I'm"s
clean_data <- gsub(" I' m ", " I'm ", clean_data)
clean_data <- gsub("^I' m ", "I'm ", clean_data)
clean_data <- gsub(" im ", " I'm ", clean_data)

#removign excessive apostrophes
clean_data <- gsub(" '+ ", " ", clean_data)
clean_data <- gsub(" '+", " ", clean_data)
clean_data <- gsub("'+ ", " ", clean_data)
clean_data <- gsub("^'+", "", clean_data)
clean_data <- gsub("'+$", "", clean_data)

#removing excessive blank spaces
clean_data <- gsub("^[[:blank:]]+", "", clean_data)
clean_data <- gsub("[[:blank:]]+", " ", clean_data)
clean_data <- gsub("[[:blank:]]+$", "", clean_data)

#save for later use
save(clean_data, file="clean_data.RData")


###---------

#creating a not-English words file
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")
library("stringi")


load("clean_data.RData")
token <- stri_extract_words(clean_data, locale = "en_US")
rm(clean_data)

#creating a TOKEN data frame
dic_data <- data.frame(table(matrix(unlist(token), ncol = 1, byrow = TRUE)))
rm(token)


#separating words with frequency used <= 35 times
dic_data_nw <- dic_data[dic_data$Freq <= 35,]

#loading the dictionary
words <- read.csv("words.csv", sep=";")

#couting how many times to work with the loop
y <- round(length(words[,1])/2500)+1

#loop for removing the real words from the TOKEN
for(n in 0:y){
    t = n*2500+1
    b = n + 1
    x = b*2500
    
    dic_data_nw <- dic_data_nw[!grepl(dic_data_nw[,1], pattern = paste("^",words[t:x,],"$", collapse="|",sep="")),]
    print(c("Round",b))
    print(c("Last x value",x))
    print(c("remaining not words",length(dic_data_nw[,1])))
    
}
save(dic_data_nw, file="dic_data_nw.RData")


#removing the words with frequency lower than 2 since we wont keep n-gram with less than 2 times appearance.
dic_data_nw4 <- dic_data_nw[dic_data_nw$Freq > 2,]
not_words <- dic_data_nw4[,1]


###----- after n-graming phase, cleaning of non-English words.
load("not_words.RData")

#loop for removing the not words from the ngram
for(n in 3:4){
    if(n==3){
        cap = 2
    }
    else{
        cap = 1
    }
    #couting how many times to work with the loop
    load("not_words.RData")
    y <- round(length(not_words)/200)+1
    
    for(l in letters){
        ngramL <- read.csv(paste("ngram", n,"_DF_", l , ".csv", sep=""), colClasses=c("NULL", NA, NA))
        ngramL <- ngramL[ngramL$Freq > cap,]
        ngramL$ngram <- paste(" ", ngramL$ngram, " ", sep= "")
        for(n2 in 0:y){
            t = n2*200+1
            b = n2 + 1
            x = b*200
            p1 <- proc.time()
            ngramL <- ngramL[!grepl(ngramL$ngram, pattern = paste(" ", not_words[t:x], " ", collapse="|", sep="")),]
            
            print(c("ngram number", n, "Letter", l))
            print(c("Round",b, "of", y))
            print(c("number of words passed",x))
            print(c("remaining real words",length(ngramL[,1])))
            print(c(proc.time() - p1))
            print("--------------------------------")
            
        }
        ngramL$ngram <- gsub("^[[:blank:]]+", "", ngramL$ngram)
        ngramL$ngram <- gsub("[[:blank:]]+$", "", ngramL$ngram)
        write.csv(ngramL, file = paste("ngram_clean_",n,"_DF_",l,".csv", sep=""))
        print("--------------------------------")
        print(c("saving file",n, "letter", l))
        print("--------------------------------")
        print("--------------------------------")
        rm(ngramL)
    }
    
}


#transforming back to .RData since it they represent faster reads and lower hard drive space.

setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")
for(l in letters){
    for(n in 3:5){
        assign(paste(n,"-gram_",l,sep=""), read.csv(paste("ngram_clean_", n, "_DF_", l, ".csv", sep=""), colClasses=c("NULL",NA,NA)))
        save(list = paste(n,"-gram_",l,sep=""), file = paste(n,"-gram_",l, ".RData",sep=""))
    }
}







