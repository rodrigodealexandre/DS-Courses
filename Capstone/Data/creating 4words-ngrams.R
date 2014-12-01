library("tm")
library("stringi")
library("gsubfn")
library("qdap")
library("beepr")
library("audio")
library("ngram")
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")


#ngram of 4, Clear workspace and restart R for more memory RAM
#diveided the file in 8 parts because it freezes due to low memory RAM.
#every time it opens the clean_data file, creates an shorter version and
#makes an angram of it. Afterwards it convert the ngram to a matrix with
#one column saves it to later use, and delete the rest to free memory.


load("clean_data.RData")
clean_data1 <- matrix(clean_data[1:1000000])
rm(clean_data)
ngram4_list1 <- apply(clean_data1, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data1)
ngram4_1 <- rapply(ngram4_list1, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list1)
save(ngram4_1, file="ngram4_1.RData")
rm(ngram4_1)

load("clean_data.RData")
clean_data2 <- matrix(clean_data[1000001:2000000])
rm(clean_data)
ngram4_list2 <- apply(clean_data2, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data2)
ngram4_2 <- rapply(ngram4_list2, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list2)
save(ngram4_2, file="ngram4_2.RData")
rm(ngram4_2)

load("clean_data.RData")
clean_data3 <- matrix(clean_data[2000001:3000000])
rm(clean_data)
ngram4_list3 <- apply(clean_data3, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data3)
ngram4_3 <- rapply(ngram4_list3, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list3)
save(ngram4_3, file="ngram4_3.RData")
rm(ngram4_3)

load("clean_data.RData")
clean_data4 <- matrix(clean_data[3000001:4000000])
rm(clean_data)
ngram4_list4 <- apply(clean_data4, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data4)
ngram4_4 <- rapply(ngram4_list4, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list4)
save(ngram4_4, file="ngram4_4.RData")
rm(ngram4_4)

load("clean_data.RData")
clean_data5 <- matrix(clean_data[4000001:5000000])
rm(clean_data)
ngram4_list5 <- apply(clean_data5, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data5)
ngram4_5 <- rapply(ngram4_list5, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list5)
save(ngram4_5, file="ngram4_5.RData")
rm(ngram4_5)

load("clean_data.RData")
clean_data6 <- matrix(clean_data[5000001:6000000])
rm(clean_data)
ngram4_list6 <- apply(clean_data6, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data6)
ngram4_6 <- rapply(ngram4_list6, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list6)
save(ngram4_6, file="ngram4_6.RData")
rm(ngram4_6)

load("clean_data.RData")
clean_data7 <- matrix(clean_data[6000001:7000000])
rm(clean_data)
ngram4_list7 <- apply(clean_data7, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data7)
ngram4_7 <- rapply(ngram4_list7, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list7)
save(ngram4_7, file="ngram4_7.RData")
rm(ngram4_7)

load("clean_data.RData")
clean_data8 <- matrix(clean_data[7000001:7644814])
rm(clean_data)
ngram4_list8 <- apply(clean_data8, 1, function(x) tryCatch({ngram(x , n =4)}, error=function(e){}))
rm(clean_data8)
ngram4_8 <- rapply(ngram4_list8, function(x) as.matrix(get.ngrams(x)))
rm(ngram4_list8)
save(ngram4_8, file="ngram4_8.RData")
rm(ngram4_8)

#load all ngram saved files back
load("ngram4_1.RData")
load("ngram4_2.RData")
load("ngram4_3.RData")
load("ngram4_4.RData")
load("ngram4_5.RData")
load("ngram4_6.RData")
load("ngram4_7.RData")
load("ngram4_8.RData")


#removing leftover strange characters 
ngram4_1 <- gsub("^[-]+", "", ngram4_1)
ngram4_1 <- gsub("^[[:blank:]]+", "", ngram4_1)
ngram4_1 <- gsub("[[:blank:]]+", " ", ngram4_1)

ngram4_2 <- gsub("^[-]+", "", ngram4_2)
ngram4_2 <- gsub("^[[:blank:]]+", "", ngram4_2)
ngram4_2 <- gsub("[[:blank:]]+", " ", ngram4_2)

ngram4_3 <- gsub("^[-]+ ", "", ngram4_3)
ngram4_3 <- gsub("^[[:blank:]]+", "", ngram4_3)
ngram4_3 <- gsub("[[:blank:]]+", " ", ngram4_3)

ngram4_4 <- gsub("^[-]+", "", ngram4_4)
ngram4_4 <- gsub("^[[:blank:]]+", "", ngram4_4)
ngram4_4 <- gsub("[[:blank:]]+", " ", ngram4_4)

ngram4_5 <- gsub("^[-]+", "", ngram4_5)
ngram4_5 <- gsub("^[[:blank:]]+", "", ngram4_5)
ngram4_5 <- gsub("[[:blank:]]+", " ", ngram4_5)

ngram4_6 <- gsub("^[-]+", "", ngram4_6)
ngram4_6 <- gsub("^[[:blank:]]+", "", ngram4_6)
ngram4_6 <- gsub("[[:blank:]]+", " ", ngram4_6)

ngram4_7 <- gsub("^[-]+", "", ngram4_7)
ngram4_7 <- gsub("^[[:blank:]]+", "", ngram4_7)
ngram4_7 <- gsub("[[:blank:]]+", " ", ngram4_7)

ngram4_8 <- gsub("^[-]+", "", ngram4_8)
ngram4_8 <- gsub("^[[:blank:]]+", "", ngram4_8)
ngram4_8 <- gsub("[[:blank:]]+", " ", ngram4_8)


#backup
save(ngram4_1, file="ngram4_1.RData")
save(ngram4_2, file="ngram4_2.RData")
save(ngram4_3, file="ngram4_3.RData")
save(ngram4_4, file="ngram4_4.RData")
save(ngram4_5, file="ngram4_5.RData")
save(ngram4_6, file="ngram4_6.RData")
save(ngram4_7, file="ngram4_7.RData")
save(ngram4_8, file="ngram4_8.RData")


#Clean workspace and restart R
rm(list=ls())

#Removing lines with less than 4 words
#Remove one by one because it uses to much memory

load("ngram4_1.RData")
ngram4_1 <- ngram4_1[wc(ngram4_1)==4]
save(ngram4_1, file="ngram4_1.RData")
rm(ngram4_1)

load("ngram4_2.RData")
ngram4_2 <- ngram4_2[wc(ngram4_2)==4]
save(ngram4_2, file="ngram4_2.RData")
rm(ngram4_2)

load("ngram4_3.RData")
ngram4_3 <- ngram4_3[wc(ngram4_3)==4]
save(ngram4_3, file="ngram4_3.RData")
rm(ngram4_3)

load("ngram4_4.RData")
ngram4_4 <- ngram4_4[wc(ngram4_4)==4]
save(ngram4_4, file="ngram4_4.RData")
rm(ngram4_4)

load("ngram4_5.RData")
ngram4_5 <- ngram4_5[wc(ngram4_5)==4]
save(ngram4_5, file="ngram4_5.RData")
rm(ngram4_5)

load("ngram4_6.RData")
ngram4_6 <- ngram4_6[wc(ngram4_6)==4]
save(ngram4_6, file="ngram4_6.RData")
rm(ngram4_6)

load("ngram4_7.RData")
ngram4_7 <- ngram4_7[wc(ngram4_7)==4]
save(ngram4_7, file="ngram4_7.RData")
rm(ngram4_7)

load("ngram4_8.RData")
ngram4_8 <- ngram4_8[wc(ngram4_8)==4]
save(ngram4_8, file="ngram4_8.RData")
rm(ngram4_8)

#open free RAM
rm(list = ls())

#load all ngram saved files back
load("ngram4_1.RData")
load("ngram4_2.RData")
load("ngram4_3.RData")
load("ngram4_4.RData")
load("ngram4_5.RData")
load("ngram4_6.RData")
load("ngram4_7.RData")
load("ngram4_8.RData")


#separate by alphabetical order in different files
for(i in letters){
    for(n in 1:8){
        test <- get(paste("ngram4_", n, sep=""))
        #if the file does not exist create the file
        if(!exists(paste("With4_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With4_",i,sep=""), test[grepl(test, pattern=paste("^[","iI","]", sep=""))])
            }
            #all other letters are lowercased
            else{
                assign(paste("With4_",i,sep=""), test[grepl(test, pattern=paste("^[",i,"]", sep=""))])
            }
        }
        #if the file already exists join them toguether
        else if(exists(paste("With4_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With4_",i,sep=""), c(get(paste("With4_",i,sep="")), test[grepl(test, pattern=paste("^[","iI","]", sep=""))]))
            }
            #all other letters are lowercased
            else{
                assign(paste("With4_",i,sep=""), c(get(paste("With4_",i,sep="")), test[grepl(test, pattern=paste("^[",i,"]", sep=""))]))
            }
        }
    }
}


save(With4_a, file="With4_a.RData")
save(With4_b, file="With4_b.RData")
save(With4_c, file="With4_c.RData")
save(With4_d, file="With4_d.RData")
save(With4_e, file="With4_e.RData")
save(With4_f, file="With4_f.RData")
save(With4_g, file="With4_g.RData")
save(With4_h, file="With4_h.RData")
save(With4_i, file="With4_i.RData")
save(With4_j, file="With4_j.RData")
save(With4_k, file="With4_k.RData")
save(With4_l, file="With4_l.RData")
save(With4_m, file="With4_m.RData")
save(With4_n, file="With4_n.RData")
save(With4_o, file="With4_o.RData")
save(With4_p, file="With4_p.RData")
save(With4_q, file="With4_q.RData")
save(With4_r, file="With4_r.RData")
save(With4_s, file="With4_s.RData")
save(With4_t, file="With4_t.RData")
save(With4_u, file="With4_u.RData")
save(With4_v, file="With4_v.RData")
save(With4_w, file="With4_w.RData")
save(With4_x, file="With4_x.RData")
save(With4_y, file="With4_y.RData")
save(With4_z, file="With4_z.RData")

#open free RAM
rm(list = ls())

#warning beep to tell the work is finished
while(1>0){wait(beep(4));wait(beep(6))}  

#only run this last step after executing the same previously steps for 3-gram and 5-gram
for(n in 3:5){
    if(n == 3){
        t = ""
    }
    else{
        t = n
    }
    for(l in letters){
        load(paste("With",t,"_",l,".RData", sep=""))
        ngram <- get(paste("With",t,"_",l, sep=""))
        ngram <- data.frame(table(ngram))
        ngram <- ngram[character_count(ngram$ngram) > n,]
        write.csv(ngram, file = paste("ngram",n,"_DF_",l,".csv", sep=""))
        rm(ngram)
        rm(list = paste("With",t,"_",l, sep=""))
        print(c("ngram n:",n))
        print(c("letter:",l))
    }
}