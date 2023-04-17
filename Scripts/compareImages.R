## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code: compares the outputs from the compFiles.R to the outputs from imageFiles.R

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)

#### LOAD DATA ----
df.images <- read.table("./Data/imageList.csv",
                        sep = ";",
                        header = TRUE) #3779

df.comp <- read.table("./Data/computerImageList.csv",
                      sep = ";",
                      header = TRUE) #3809

#### COMPARE IMAGE NAMES ----

##create subset df of only images

df.images.tif <- df.images[df.images$ext == "tif",] #1890
df.comp.tif <- df.comp[df.comp$ext == "tif",] #1906

##get rid of dupes

df.images.trim <- df.images.tif[!duplicated(df.images.tif$image),] #1889
df.comp.trim <- df.comp.tif[!duplicated(df.comp.tif$image),] #1890

length(setdiff(df.images.trim$image, df.comp.trim$image)) #0
length(setdiff(df.comp.trim$image, df.images.trim$image)) #1
setdiff(df.comp.trim$image, df.images.trim$image) #MHR
df.comp.trim[df.comp.trim$image == "MHR",]

