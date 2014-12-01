library("tm")
library("stringi")
library("gsubfn")
library("qdap")
library("beepr")
library("audio")
library("ngram")
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")


#ngram of 5, Clear workspace and restart R for more memory RAM
#diveided the file in 8 parts because it freezes due to low memory RAM.
#every time it opens the clean_data file, creates an shorter version and
#makes an angram of it. Afterwards it convert the ngram to a matrix with
#one column saves it to later use, and delete the rest to free memory.


load("clean_data.RData")
clean_data1 <- matrix(clean_data[1:1000000])
rm(clean_data)
ngram5_list1 <- apply(clean_data1, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data1)
ngram5_1 <- rapply(ngram5_list1, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list1)
save(ngram5_1, file="ngram5_1.RData")
rm(ngram5_1)

load("clean_data.RData")
clean_data2 <- matrix(clean_data[1000001:2000000])
rm(clean_data)
ngram5_list2 <- apply(clean_data2, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data2)
ngram5_2 <- rapply(ngram5_list2, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list2)
save(ngram5_2, file="ngram5_2.RData")
rm(ngram5_2)

load("clean_data.RData")
clean_data3 <- matrix(clean_data[2000001:3000000])
rm(clean_data)
ngram5_list3 <- apply(clean_data3, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data3)
ngram5_3 <- rapply(ngram5_list3, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list3)
save(ngram5_3, file="ngram5_3.RData")
rm(ngram5_3)

load("clean_data.RData")
clean_data4 <- matrix(clean_data[3000001:4000000])
rm(clean_data)
ngram5_list4 <- apply(clean_data4, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data4)
ngram5_4 <- rapply(ngram5_list4, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list4)
save(ngram5_4, file="ngram5_4.RData")
rm(ngram5_4)

load("clean_data.RData")
clean_data5 <- matrix(clean_data[4000001:5000000])
rm(clean_data)
ngram5_list5 <- apply(clean_data5, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data5)
ngram5_5 <- rapply(ngram5_list5, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list5)
save(ngram5_5, file="ngram5_5.RData")
rm(ngram5_5)

load("clean_data.RData")
clean_data6 <- matrix(clean_data[5000001:6000000])
rm(clean_data)
ngram5_list6 <- apply(clean_data6, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data6)
ngram5_6 <- rapply(ngram5_list6, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list6)
save(ngram5_6, file="ngram5_6.RData")
rm(ngram5_6)

load("clean_data.RData")
clean_data7 <- matrix(clean_data[6000001:7000000])
rm(clean_data)
ngram5_list7 <- apply(clean_data7, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data7)
ngram5_7 <- rapply(ngram5_list7, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list7)
save(ngram5_7, file="ngram5_7.RData")
rm(ngram5_7)

load("clean_data.RData")
clean_data8 <- matrix(clean_data[7000001:7644814])
rm(clean_data)
ngram5_list8 <- apply(clean_data8, 1, function(x) tryCatch({ngram(x , n =5)}, error=function(e){}))
rm(clean_data8)
ngram5_8 <- rapply(ngram5_list8, function(x) as.matrix(get.ngrams(x)))
rm(ngram5_list8)
save(ngram5_8, file="ngram5_8.RData")
rm(ngram5_8)

#load all ngram saved files back
load("ngram5_1.RData")
load("ngram5_2.RData")
load("ngram5_3.RData")
load("ngram5_4.RData")
load("ngram5_5.RData")
load("ngram5_6.RData")
load("ngram5_7.RData")
load("ngram5_8.RData")


#removing leftover strange characters 
ngram5_1 <- gsub("^[-]+", "", ngram5_1)
ngram5_1 <- gsub("^[[:blank:]]+", "", ngram5_1)
ngram5_1 <- gsub("[[:blank:]]+", " ", ngram5_1)

ngram5_2 <- gsub("^[-]+", "", ngram5_2)
ngram5_2 <- gsub("^[[:blank:]]+", "", ngram5_2)
ngram5_2 <- gsub("[[:blank:]]+", " ", ngram5_2)

ngram5_3 <- gsub("^[-]+ ", "", ngram5_3)
ngram5_3 <- gsub("^[[:blank:]]+", "", ngram5_3)
ngram5_3 <- gsub("[[:blank:]]+", " ", ngram5_3)

ngram5_4 <- gsub("^[-]+", "", ngram5_4)
ngram5_4 <- gsub("^[[:blank:]]+", "", ngram5_4)
ngram5_4 <- gsub("[[:blank:]]+", " ", ngram5_4)

ngram5_5 <- gsub("^[-]+", "", ngram5_5)
ngram5_5 <- gsub("^[[:blank:]]+", "", ngram5_5)
ngram5_5 <- gsub("[[:blank:]]+", " ", ngram5_5)

ngram5_6 <- gsub("^[-]+", "", ngram5_6)
ngram5_6 <- gsub("^[[:blank:]]+", "", ngram5_6)
ngram5_6 <- gsub("[[:blank:]]+", " ", ngram5_6)

ngram5_7 <- gsub("^[-]+", "", ngram5_7)
ngram5_7 <- gsub("^[[:blank:]]+", "", ngram5_7)
ngram5_7 <- gsub("[[:blank:]]+", " ", ngram5_7)

ngram5_8 <- gsub("^[-]+", "", ngram5_8)
ngram5_8 <- gsub("^[[:blank:]]+", "", ngram5_8)
ngram5_8 <- gsub("[[:blank:]]+", " ", ngram5_8)


#backup
save(ngram5_1, file="ngram5_1.RData")
save(ngram5_2, file="ngram5_2.RData")
save(ngram5_3, file="ngram5_3.RData")
save(ngram5_4, file="ngram5_4.RData")
save(ngram5_5, file="ngram5_5.RData")
save(ngram5_6, file="ngram5_6.RData")
save(ngram5_7, file="ngram5_7.RData")
save(ngram5_8, file="ngram5_8.RData")


#Clean workspace and restart R
rm(list=ls())

#Removing lines with less than 4 words
#Remove one by one because it uses to much memory

load("ngram5_1.RData")
ngram5_1 <- ngram5_1[wc(ngram5_1)==5]
save(ngram5_1, file="ngram5_1.RData")
rm(ngram5_1)

load("ngram5_2.RData")
ngram5_2 <- ngram5_2[wc(ngram5_2)==5]
save(ngram5_2, file="ngram5_2.RData")
rm(ngram5_2)

load("ngram5_3.RData")
ngram5_3 <- ngram5_3[wc(ngram5_3)==5]
save(ngram5_3, file="ngram5_3.RData")
rm(ngram5_3)

load("ngram5_4.RData")
ngram5_4 <- ngram5_4[wc(ngram5_4)==5]
save(ngram5_4, file="ngram5_4.RData")
rm(ngram5_4)

load("ngram5_5.RData")
ngram5_5 <- ngram5_5[wc(ngram5_5)==5]
save(ngram5_5, file="ngram5_5.RData")
rm(ngram5_5)

load("ngram5_6.RData")
ngram5_6 <- ngram5_6[wc(ngram5_6)==5]
save(ngram5_6, file="ngram5_6.RData")
rm(ngram5_6)

load("ngram5_7.RData")
ngram5_7 <- ngram5_7[wc(ngram5_7)==5]
save(ngram5_7, file="ngram5_7.RData")
rm(ngram5_7)

load("ngram5_8.RData")
ngram5_8 <- ngram5_8[wc(ngram5_8)==5]
save(ngram5_8, file="ngram5_8.RData")
rm(ngram5_8)

#open free RAM
rm(list = ls())

#load all ngram saved files back
load("ngram5_1.RData")
load("ngram5_2.RData")
load("ngram5_3.RData")
load("ngram5_4.RData")
load("ngram5_5.RData")
load("ngram5_6.RData")
load("ngram5_7.RData")
load("ngram5_8.RData")


#separate by alphabetical order in different files
for(i in letters){
    for(n in 1:8){
        test <- get(paste("ngram5_", n, sep=""))
        #if the file does not exist create the file
        if(!exists(paste("With5_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With5_",i,sep=""), test[grepl(test, pattern=paste("^[","iI","]", sep=""))])
            }
            #all other letters are lowercased
            else{
                assign(paste("With5_",i,sep=""), test[grepl(test, pattern=paste("^[",i,"]", sep=""))])
            }
        }
        #if the file already exists join them toguether
        else if(exists(paste("With5_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With5_",i,sep=""), c(get(paste("With5_",i,sep="")), test[grepl(test, pattern=paste("^[","iI","]", sep=""))]))
            }
            #all other letters are lowercased
            else{
                assign(paste("With5_",i,sep=""), c(get(paste("With5_",i,sep="")), test[grepl(test, pattern=paste("^[",i,"]", sep=""))]))
            }
        }
    }
}


save(With5_a, file="With5_a.RData")
save(With5_b, file="With5_b.RData")
save(With5_c, file="With5_c.RData")
save(With5_d, file="With5_d.RData")
save(With5_e, file="With5_e.RData")
save(With5_f, file="With5_f.RData")
save(With5_g, file="With5_g.RData")
save(With5_h, file="With5_h.RData")
save(With5_i, file="With5_i.RData")
save(With5_j, file="With5_j.RData")
save(With5_k, file="With5_k.RData")
save(With5_l, file="With5_l.RData")
save(With5_m, file="With5_m.RData")
save(With5_n, file="With5_n.RData")
save(With5_o, file="With5_o.RData")
save(With5_p, file="With5_p.RData")
save(With5_q, file="With5_q.RData")
save(With5_r, file="With5_r.RData")
save(With5_s, file="With5_s.RData")
save(With5_t, file="With5_t.RData")
save(With5_u, file="With5_u.RData")
save(With5_v, file="With5_v.RData")
save(With5_w, file="With5_w.RData")
save(With5_x, file="With5_x.RData")
save(With5_y, file="With5_y.RData")
save(With5_z, file="With5_z.RData")

#open free RAM
rm(list = ls())

#warning beep to tell the work is finished
while(1>0){wait(beep(4));wait(beep(6))}  

#only run this last step after executing the same previously steps for 3-gram and 4-gram
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