run_analysis<- function(directory1, directory2, headerfilename, filename, variant1, variant2) {
        
        ## 'directory' is a character vector of length 1 indicating
        ## the location of the TXT files ex. test or train.
        
        ## 'filetype' is a character vector of length 1 indicating
        ## the file type ex: .txt; .csv and etc.
        
        ## 'headerfilename' is the complete name of the file containing the headers.
        ## ex: features.txt.
        
        ## 'filename' is a character vector of length 1 indicating
        ## name of the file to the data to be exported into a .csv file. 
        
        ## 'variant1' and 'variant2' are characters vector of length 1 indicating
        ## which variants to use ex: mean and std.
        
        setwd("UCI HAR Dataset")
        setwd(directory1)
        
        # Creating a list of all files of the selected type in the folder.
        file_list <- list.files(pattern= ".txt")
        
        # Reading files
        d1 <- read.table(file_list[1], header=FALSE)
        d2 <- read.table(file_list[2], header=FALSE)
        d3 <- read.table(file_list[3], header=FALSE)
        
        # Getting back to the previews folder.
        setwd("../")
        setwd(directory2)
        
        # Creating a list of all files of the selected type in the folder.
        file_list2 <- list.files(pattern= ".txt")
        
        # Reading files
        d4 <- read.table(file_list2[1], header=FALSE)
        d5 <- read.table(file_list2[2], header=FALSE)
        d6 <- read.table(file_list2[3], header=FALSE)
        
        # Getting back to the previews folder.
        setwd("../")
        
        
        
        # merge all files of the file_list toguether.
        dataset1 <- cbind(d1, d3, d2)
        dataset2 <- cbind(d4, d6, d5)
        dataset3 <- rbind(dataset1, dataset2)
        
        # Adding header.
        hdf <- read.table("features.txt", sep=" ")
        colnames(dataset3)[1:2] <- c("ID", "Type")
        x <- dataset3[3:563] #doing this becouse R is messing up the data and no one knows why.
        colnames(x) <- hdf[,2]
        dataset<- cbind(dataset3[,1:2], x)
        
        # Selecting which variants to use ex: mean and std
        v1<- paste(".*",variant1,".*", sep= "")
        v2<- paste(".*",variant2,".*", sep= "")
        v1_v2 <- c(v1, v2)
        v1_v2_hd <- unique(grep(paste(v1_v2,collapse="|"),hdf$V2, value=TRUE))
        finaltable <- cbind(dataset[,1:2], dataset[v1_v2_hd])
        
        
        
        # Getting back to the previews folder.
        setwd("../")
        
        
        # Saving as a txt file according to the given name. 
        write.table(finaltable, file = paste(filename,".txt", sep=""),row.names=FALSE,quote=FALSE,col.names=names(finaltable))
        
}