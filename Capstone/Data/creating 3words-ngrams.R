library("tm")
library("stringi")
library("gsubfn")
library("qdap")
library("beepr")
library("audio")
library("ngram")
setwd("D:/Dropbox/Courses/Coursera Courses/Data Science Capstone on Coursera/Data-Science-Capstone-Project/Data")


#ngram of 3, Clear workspace and restart R for more memory RAM
#diveided the file in 8 parts because it freezes due to low memory RAM.
#every time it opens the clean_data file, creates an shorter version and
#makes an angram of it. Afterwards it convert the ngram to a matrix with
#one column saves it to later use, and delete the rest to free memory.


load("clean_data.RData")
clean_data1 <- matrix(clean_data[1:1000000])
rm(clean_data)
ngram3_list1 <- apply(clean_data1, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data1)
ngram3_1 <- rapply(ngram3_list1, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list1)
save(ngram3_1, file="ngram3_1.RData")
rm(ngram3_1)

load("clean_data.RData")
clean_data2 <- matrix(clean_data[1000001:2000000])
rm(clean_data)
ngram3_list2 <- apply(clean_data2, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data2)
ngram3_2 <- rapply(ngram3_list2, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list2)
save(ngram3_2, file="ngram3_2.RData")
rm(ngram3_2)

load("clean_data.RData")
clean_data3 <- matrix(clean_data[2000001:3000000])
rm(clean_data)
ngram3_list3 <- apply(clean_data3, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data3)
ngram3_3 <- rapply(ngram3_list3, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list3)
save(ngram3_3, file="ngram3_3.RData")
rm(ngram3_3)

load("clean_data.RData")
clean_data4 <- matrix(clean_data[3000001:4000000])
rm(clean_data)
ngram3_list4 <- apply(clean_data4, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data4)
ngram3_4 <- rapply(ngram3_list4, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list4)
save(ngram3_4, file="ngram3_4.RData")
rm(ngram3_4)

load("clean_data.RData")
clean_data5 <- matrix(clean_data[4000001:5000000])
rm(clean_data)
ngram3_list5 <- apply(clean_data5, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data5)
ngram3_5 <- rapply(ngram3_list5, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list5)
save(ngram3_5, file="ngram3_5.RData")
rm(ngram3_5)

load("clean_data.RData")
clean_data6 <- matrix(clean_data[5000001:6000000])
rm(clean_data)
ngram3_list6 <- apply(clean_data6, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data6)
ngram3_6 <- rapply(ngram3_list6, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list6)
save(ngram3_6, file="ngram3_6.RData")
rm(ngram3_6)

load("clean_data.RData")
clean_data7 <- matrix(clean_data[6000001:7000000])
rm(clean_data)
ngram3_list7 <- apply(clean_data7, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data7)
ngram3_7 <- rapply(ngram3_list7, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list7)
save(ngram3_7, file="ngram3_7.RData")
rm(ngram3_7)

load("clean_data.RData")
clean_data8 <- matrix(clean_data[7000001:7644814])
rm(clean_data)
ngram3_list8 <- apply(clean_data8, 1, function(x) tryCatch({ngram(x , n =3)}, error=function(e){}))
rm(clean_data8)
ngram3_8 <- rapply(ngram3_list8, function(x) as.matrix(get.ngrams(x)))
rm(ngram3_list8)
save(ngram3_8, file="ngram3_8.RData")
rm(ngram3_8)

#load all ngram saved files back
load("ngram3_1.RData")
load("ngram3_2.RData")
load("ngram3_3.RData")
load("ngram3_4.RData")
load("ngram3_5.RData")
load("ngram3_6.RData")
load("ngram3_7.RData")
load("ngram3_8.RData")


#removing leftover strange characters 
ngram3_1 <- gsub("^[-]+", "", ngram3_1)
ngram3_1 <- gsub("^[[:blank:]]+", "", ngram3_1)
ngram3_1 <- gsub("[[:blank:]]+", " ", ngram3_1)

ngram3_2 <- gsub("^[-]+", "", ngram3_2)
ngram3_2 <- gsub("^[[:blank:]]+", "", ngram3_2)
ngram3_2 <- gsub("[[:blank:]]+", " ", ngram3_2)

ngram3_3 <- gsub("^[-]+ ", "", ngram3_3)
ngram3_3 <- gsub("^[[:blank:]]+", "", ngram3_3)
ngram3_3 <- gsub("[[:blank:]]+", " ", ngram3_3)

ngram3_4 <- gsub("^[-]+", "", ngram3_4)
ngram3_4 <- gsub("^[[:blank:]]+", "", ngram3_4)
ngram3_4 <- gsub("[[:blank:]]+", " ", ngram3_4)

ngram3_5 <- gsub("^[-]+", "", ngram3_5)
ngram3_5 <- gsub("^[[:blank:]]+", "", ngram3_5)
ngram3_5 <- gsub("[[:blank:]]+", " ", ngram3_5)

ngram3_6 <- gsub("^[-]+", "", ngram3_6)
ngram3_6 <- gsub("^[[:blank:]]+", "", ngram3_6)
ngram3_6 <- gsub("[[:blank:]]+", " ", ngram3_6)

ngram3_7 <- gsub("^[-]+", "", ngram3_7)
ngram3_7 <- gsub("^[[:blank:]]+", "", ngram3_7)
ngram3_7 <- gsub("[[:blank:]]+", " ", ngram3_7)

ngram3_8 <- gsub("^[-]+", "", ngram3_8)
ngram3_8 <- gsub("^[[:blank:]]+", "", ngram3_8)
ngram3_8 <- gsub("[[:blank:]]+", " ", ngram3_8)


#backup
save(ngram3_1, file="ngram3_1.RData")
save(ngram3_2, file="ngram3_2.RData")
save(ngram3_3, file="ngram3_3.RData")
save(ngram3_4, file="ngram3_4.RData")
save(ngram3_5, file="ngram3_5.RData")
save(ngram3_6, file="ngram3_6.RData")
save(ngram3_7, file="ngram3_7.RData")
save(ngram3_8, file="ngram3_8.RData")


#Clean workspace and restart R
rm(list=ls())

#Removing lines with less than 3 words
#Remove one by one because it uses to much memory

load("ngram3_1.RData")
ngram3_1 <- ngram3_1[wc(ngram3_1)==3]
save(ngram3_1, file="ngram3_1.RData")
rm(ngram3_1)

load("ngram3_2.RData")
ngram3_2 <- ngram3_2[wc(ngram3_2)==3]
save(ngram3_2, file="ngram3_2.RData")
rm(ngram3_2)

load("ngram3_3.RData")
ngram3_3 <- ngram3_3[wc(ngram3_3)==3]
save(ngram3_3, file="ngram3_3.RData")
rm(ngram3_3)

load("ngram3_4.RData")
ngram3_4 <- ngram3_4[wc(ngram3_4)==3]
save(ngram3_4, file="ngram3_4.RData")
rm(ngram3_4)

load("ngram3_5.RData")
ngram3_5 <- ngram3_5[wc(ngram3_5)==3]
save(ngram3_5, file="ngram3_5.RData")
rm(ngram3_5)

load("ngram3_6.RData")
ngram3_6 <- ngram3_6[wc(ngram3_6)==3]
save(ngram3_6, file="ngram3_6.RData")
rm(ngram3_6)

load("ngram3_7.RData")
ngram3_7 <- ngram3_7[wc(ngram3_7)==3]
save(ngram3_7, file="ngram3_7.RData")
rm(ngram3_7)

load("ngram3_8.RData")
ngram3_8 <- ngram3_8[wc(ngram3_8)==3]
save(ngram3_8, file="ngram3_8.RData")
rm(ngram3_8)

#open free RAM
rm(list = ls())

#load all ngram saved files back
load("ngram3_1.RData")
load("ngram3_2.RData")
load("ngram3_3.RData")
load("ngram3_4.RData")
load("ngram3_5.RData")
load("ngram3_6.RData")
load("ngram3_7.RData")
load("ngram3_8.RData")


#separate by alphabetical order in different files
for(i in letters){
    for(n in 1:8){
        test <- get(paste("ngram3_", n, sep=""))
        #if the file does not exist create the file
        if(!exists(paste("With_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With_",i,sep=""), test[grepl(test, pattern=paste("^[","iI","]", sep=""))])
            }
            #all other letters are lowercased
            else{
                assign(paste("With_",i,sep=""), test[grepl(test, pattern=paste("^[",i,"]", sep=""))])
            }
        }
        #if the file already exists join them toguether
        else if(exists(paste("With_",i,sep=""))){
            #if it starts with i and I
            if(i == "i"){
                assign(paste("With_",i,sep=""), c(get(paste("With_",i,sep="")), test[grepl(test, pattern=paste("^[","iI","]", sep=""))]))
            }
            #all other letters are lowercased
            else{
                assign(paste("With_",i,sep=""), c(get(paste("With_",i,sep="")), test[grepl(test, pattern=paste("^[",i,"]", sep=""))]))
            }
        }
    }
}


save(With_a, file="With_a.RData")
save(With_b, file="With_b.RData")
save(With_c, file="With_c.RData")
save(With_d, file="With_d.RData")
save(With_e, file="With_e.RData")
save(With_f, file="With_f.RData")
save(With_g, file="With_g.RData")
save(With_h, file="With_h.RData")
save(With_i, file="With_i.RData")
save(With_j, file="With_j.RData")
save(With_k, file="With_k.RData")
save(With_l, file="With_l.RData")
save(With_m, file="With_m.RData")
save(With_n, file="With_n.RData")
save(With_o, file="With_o.RData")
save(With_p, file="With_p.RData")
save(With_q, file="With_q.RData")
save(With_r, file="With_r.RData")
save(With_s, file="With_s.RData")
save(With_t, file="With_t.RData")
save(With_u, file="With_u.RData")
save(With_v, file="With_v.RData")
save(With_w, file="With_w.RData")
save(With_x, file="With_x.RData")
save(With_y, file="With_y.RData")
save(With_z, file="With_z.RData")

#open free RAM
rm(list = ls())

#warning beep to tell the work is finished
while(1>0){wait(beep(4));wait(beep(6))}  

#only run this last step after executing the same previously steps for 4-gram and 5-gram
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