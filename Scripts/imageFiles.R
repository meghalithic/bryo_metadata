## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code:
## 1) extracts file names from the zip file of Steginoporella images
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
list = list.files(path = "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images",
                  full.names = TRUE,
                  recursive = TRUE)

#get rid of abstract book, I think
list.rm <- list[!grepl("/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images/Stegs/ISME 14 ABSTRACT BOOK", list)]

#### CREATE CSV ----

listPath <- unlist(list.rm)
length(listPath) #3779

##### PARSE FILE NAMES -----

list.trim <- gsub(list.rm,
                  pattern = "/Users/mab/Library/CloudStorage/Dropbox/Rocks-Paradox/Bryozoans/Stegino images/",
                  replacement = "")

list.parse <- str_split(list.trim,
                        pattern = "/")

#first folder is either Sara (folder name Sara) or Mali (folder names Stegs and Stegs2)
#subfolder folder, when given, is the formation or grouping

folder <- c()
subfolder <- c()
fileName <- c()
ext <- c()

for(i in 1:length(list.parse)){
  folder[i] <- list.parse[[i]][1]
  if(isTRUE(endsWith(list.parse[[i]][2], ".txt"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
  }
  else if(isTRUE(endsWith(list.parse[[i]][2], ".tif"))){
    fileName[i] <- list.parse[[i]][2]
    subfolder[i] <- "NONE"
  }
  else{
    subfolder[i] <- list.parse[[i]][2]
    fileName[i] <- list.parse[[i]][3]
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

df.list <- data.frame(path = listPath,
                      folder = folder,
                      subfolder = subfolder,
                      image = image,
                      ext = ext,
                      fileName = fileName,
                      specimenNR = specimenNR,
                      stringsAsFactors = FALSE)

nrow(df.list) #3779
nrow(df.list[df.list$ext == "tif",]) #1890, an extra image

#DO IT THIS WAY:
output.fossil2$form.no <- as.numeric(str_extract(output.fossil2$specimenNR,
                                                 "[0-9]+"))
output.fossil2$formation.manual <- ""

output.fossil2$formation.manual[output.fossil2$form.no <= 399] <- "NKBS"
output.fossil2$formation.manual[output.fossil2$form.no >= 400 & 
                                    output.fossil2$form.no <= 599] <- "NKLS"
output.fossil2$formation.manual[output.fossil2$form.no >= 600 & 
                                    output.fossil2$form.no <= 699] <- "Tewkesbury"
output.fossil2$formation.manual[output.fossil2$form.no >= 700 & 
                                    output.fossil2$form.no <= 799] <- "SHCSBSB"
output.fossil2$formation.manual[output.fossil2$form.no >= 800 & 
                                    output.fossil2$form.no <= 899] <- "Tainui"
output.fossil2$formation.manual[output.fossil2$form.no >= 1000 & 
                                    output.fossil2$form.no <= 1099] <- "Upper Kai-Iwi"
output.fossil2$formation.manual[output.fossil2$form.no >= 1100 & 
                                    output.fossil2$form.no <= 1199] <- "Waipuru" #this is actually upper part of Tewkesbury

#write.csv(df.list,
#          "./Data/imageList.csv",
#          row.names = FALSE)

