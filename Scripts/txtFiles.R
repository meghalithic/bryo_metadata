## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code:
## 1) extracts file names from the zip file of Steginoporella images
## 2) creates a csv file with the information parsed
## 3) compares the files to the bryozoan metadata file

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)
require(splitstackshape)
require(data.table)

#### EXTRACT FILE NAMES ----
list = list.files(path = "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images",
                  full.names = TRUE,
                  recursive = TRUE)
## remove abstract book
list.rm <- list[!grepl("/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images/Stegs/ISME 14 ABSTRACT BOOK",
                       list)]

#### TXT ONLY ----

list.trim <- list.rm[!grepl("*.tif",
                            list.rm)]

length(list.trim) #1889

##unlist
txtPath <- unlist(list.trim)

#### READ TXT FILES ----

##practice with one file
f <- read.table("/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images/Sara/002_CV_1_15v_x30.txt",
                sep = "^",
                fileEncoding="UTF-16",
                skip = 1)

## now make two columns, using "=" as deliminator

ff <- cSplit(f, 'V1',
             sep = "=",
             stripWhite = TRUE,
             type.convert = FALSE)

#seems Condition is multiple "="
condition <- str_split(ff[ff$V1_1 == "Condition",],
                       pattern = "\ ")

av <- c("AV", gsub(".0kV", "v",condition[[3]][1]))
mag <- c(condition[[3]][2], condition[[4]][1])
wd <- c(condition[[4]][2], condition[[5]][1])
lensMode <- c(condition[[5]][2], condition[[6]][1])
path <- c("path", "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images/Sara/002_CV_1_15v_x30.txt")

cond.paste <- paste(ff$V1_2[ff$V1_1 == "Condition"], 
                    ff$V1_3[ff$V1_1 == "Condition"],
                    ff$V1_4[ff$V1_1 == "Condition"], 
                    ff$V1_5[ff$V1_1 == "Condition"],
                    ff$V1_6[ff$V1_1 == "Condition"], 
                    sep = " ")

ff2 <- ff
ff2$V1_2[ff2$V1_1 == "Condition"] <- cond.paste

ff3 <- ff2[,1:2]

ff4 <- rbind(path, as.data.frame(ff3), av, mag, wd, lensMode)

ff4$V1_1 #need to transpose

## now for all!

txt.df <- data.frame()

for(i in 1:length(txtPath)){
  f <- read.table(txtPath[i],
                  sep = "^",
                  fileEncoding = "UTF-16",
                  skip = 1)
  
  ## now make two columns, using "=" as deliminator
  
  ff <- cSplit(f, 'V1',
               sep = "=",
               stripWhite = TRUE,
               type.convert = FALSE)
  
  #seems Condition is multiple "="
  condition <- str_split(ff[ff$V1_1 == "Condition",],
                         pattern = "\ ")
  
  av <- c(condition[[2]][1],condition[[3]][1])
  mag <- c(condition[[3]][2], condition[[4]][1])
  wd <- c(condition[[4]][2], condition[[5]][1])
  lensMode <- c(condition[[5]][2], condition[[6]][1])
  path <- c("path", txtPath[i])
   
  cond.paste <- paste(ff$V1_2[ff$V1_1 == "Condition"], 
                      ff$V1_3[ff$V1_1 == "Condition"],
                      ff$V1_4[ff$V1_1 == "Condition"], 
                      ff$V1_5[ff$V1_1 == "Condition"],
                      ff$V1_6[ff$V1_1 == "Condition"], 
                      sep = " ")
  
  ff2 <- ff
  
  ff2$V1_2[ff2$V1_1 == "Condition"] <- cond.paste
  
  ff3 <- ff2[,1:2]
  
  ff4 <- rbind(path, as.data.frame(ff3), av, mag, wd, lensMode)

  names <- ff4$V1_1
  ff5 <- as.data.frame(t(ff4[,-1]))
  colnames(ff5) <- names
  
  txt.df <- rbind(txt.df, ff5)
  
}

nrow(txt.df) #1889

txt.df$fileName <- basename(txt.df$path)
txt.df$image <- str_extract(txt.df$fileName, pattern = "[^.]+")


image.parse <- str_split(txt.df$image,
                         pattern = "_")

txt.df$specimenNR <- ""

for(i in 1:nrow(txt.df)){
  txt.df$specimenNR[i] <- paste0(image.parse[[i]][1], image.parse[[i]][2])
}

#write.csv(txt.df,
#          "./Data/txt_metadata.csv",
#          row.names = FALSE)
