## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code adds the output from txtFiles to the output from imageFiles

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)

#### LOAD DATA ----

df.list <- read.table("./Data/imageList.csv",
                      header = TRUE,
                      sep = ";")

txt.df <- read.table("./Data/txt_metadata.csv",
                     header = TRUE,
                     sep = ";")

#### COMBINE DATA ----

##### CREATE SHARED FILE NAME -----

##match tif file to txt file
##assume all tif have associated txt file

##fileNames should match
#to test see how many dupes
nrow(df.list) #3779
nrow(df.list[!duplicated(df.list$image),]) #1889; about half, may be one mismatch

nrow(txt.df) #1889
nrow(df.list[df.list$ext == "tif",]) #1890; seems one extra image

## make df for just images
df.images <- df.list[df.list$ext == "tif",]

length(setdiff(df.images$fileName, txt.df$ImageName)) #49
length(setdiff(txt.df$ImageName, df.images$fileName)) #39

df.image.meta <- merge(df.images, txt.df,
                      by = "image",
                      all.x = TRUE, all.y = TRUE) #1892

colnames(df.image.meta)[colnames(df.image.meta) == 'specimenNR.x'] <- 'specimenNR.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'specimenNR.y'] <- 'specimenNR.txt'
colnames(df.image.meta)[colnames(df.image.meta) == 'fileName.x'] <- 'fileName.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'fileName.y'] <- 'fileName.txt'
colnames(df.image.meta)[colnames(df.image.meta) == 'path.x'] <- 'path.tif'
colnames(df.image.meta)[colnames(df.image.meta) == 'path.y'] <- 'path.txt'


## make check in ImageName matches fileName
df.image.meta$ImageNameCheck <- df.image.meta$fileName.tif == df.image.meta$ImageName
#check for false

## make check for AV and mag
# extract numbers only from AV and mag

image.list <- str_split(df.image.meta$image,
                        pattern = "_")

df.image.meta$AV.fileName <- ""
df.image.meta$mag.fileName <- ""

for(i in 1:length(image.list)){
  df.image.meta$AV.fileName[i] <- as.numeric(gsub("\\D", "", image.list[[i]][4]))
  df.image.meta$mag.fileName[i] <- as.numeric(gsub("\\D", "", image.list[[i]][5]))
}

df.image.meta$magCheck <- df.image.meta$mag.fileName == as.numeric(gsub("\\D", "", df.image.meta$Mag))
#check for false

df.image.meta$AVCheck <- as.integer(df.image.meta$AV.fileName) == (as.numeric(gsub("\\D", "", df.image.meta$Vacc))/10)
#check for false

##double check no differences in txt file names
df.list.txt <- df.list[df.list$ext == "txt",]
setdiff(df.list.txt$fileName, txt.df$fileName)
setdiff(txt.df$fileName, df.list.txt$fileName)
# no difference in txt files

#write.csv(df.image.meta,
#          "./Data/image_merge_txt.csv",
#          row.names = FALSE)

## RECONCILE MANUALLY ##
# standardize "v" in AV to be lowercase
# standardize four numbers
# standardize everything before ext is separated by "_"
# standardize all AV have 2 digits
# standardize all fileName to have BSE
