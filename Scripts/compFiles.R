## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code:
## 1) extracts file names from the folder of Steginoporella images from the lab computer
## 2) creates a csv file with the information parsed

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)

#### EXTRACT FILE NAMES ----

##https://stackoverflow.com/questions/54510134/getting-list-of-file-names-in-a-directory
# To list all files of a folder in a list variable including files 
# from sub-folders. The code below gets the full path of files not just names.
#list = list.files(path = full_path_to_directory ,full.names=TRUE,recursive=TRUE)
# To get names of all files from their corresponding paths in all_names variable.
#all_names = basename(list)
# To write all_names variable to a CSV file.
#write.csv(all_names, "test.csv")

## get folder names
list = list.files(path = "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/from lab computer",
                  full.names = TRUE,
                  recursive = TRUE)
length(list) #3809

#### CREATE CSV ----

path <- unlist(list)
length(path) #3809

##### PARSE FILE NAMES -----

#first folder is either Sara (folder name Sara) or Mali (folder names Stegs and Stegs2)
#subfolder folder, when given, is the formation or grouping

list.trim <- gsub(list,
                  pattern = "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/from lab computer/",
                  replacement = "")

list.parse <- str_split(list.trim,
                        pattern = "/")

folder <- c()
subfolder <- c()
sub.subfolder <- c()
fileName <- c()
ext <- c()

for(i in 1:length(list.parse)){
  folder[i] <- list.parse[[i]][1]
  if(isTRUE(endsWith(list.parse[[i]][2], ".txt"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
    sub.subfolder[i] <- "NONE"
  }
  else if(isTRUE(endsWith(list.parse[[i]][2], ".tif"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
    sub.subfolder[i] <- "NONE"
  }
  else{
    subfolder[i] <- list.parse[[i]][2]
  }
  if(isTRUE(endsWith(list.parse[[i]][3], ".txt"))){
    fileName[i] <- list.parse[[i]][3]
    sub.subfolder[i] <- "NONE"
  }
  else if(isTRUE(endsWith(list.parse[[i]][3], ".tif"))){
    fileName[i] <- list.parse[[i]][3]
    sub.subfolder[i] <- "NONE"
  }
  else{
    sub.subfolder[i] <- list.parse[[i]][3]
  }
  if(isTRUE(endsWith(list.parse[[i]][4], ".txt"))){
    fileName[i] <- list.parse[[i]][4]
  }
  else if(isTRUE(endsWith(list.parse[[i]][4], ".tif"))){
    fileName[i] <- list.parse[[i]][4]
  }
  if(isTRUE(endsWith(fileName[i], ".txt"))){
    ext[i] <- "txt"
  }
  else{
    ext[i] <- "tif"
  }
}

##### PARSE IMAGE NAME -----

image <- str_extract(fileName, pattern = "[^.]+")

image.list <- str_split(image,
                        pattern = "_")

specimenNR <- c()

for(i in 1:length(image.list)){
  specimenNR[i] <- paste0(image.list[[i]][1], image.list[[i]][2])
}

##### COMBINE & WRITE CSV ----

df.list <- data.frame(folder = folder,
                      subfolder = subfolder,
                      sub.subfolder = sub.subfolder,
                      image = image,
                      ext = ext,
                      fileName = fileName,
                      specimenNR = specimenNR,
                      stringsAsFactors = FALSE)

nrow(df.list) #3809
nrow(df.list[df.list$ext == "tif",]) #1906

#write.csv(df.list,
#          "./Data/computerImageList.csv",
#          row.names = FALSE)
