## Meghan A. Balk
## meghan.balk@nhm.uio.no

## This code extracts formation info from fileNames

#### LOAD PACKAGES ----
require(stringr)
require(dplyr)

#### LOAD IMGAE LIST ----
image.list <- read.csv("./Data/imageList.csv", header = TRUE)

bryo.meta <- read.csv("./Data/Imaged Steginoporella magnifica specimens.csv", header = TRUE)

##### SPECIMEN NR -----

bryo.meta$SPECIMEN.NR <- gsub(bryo.meta$SPECIMEN.NR,
                              pattern = " ", 
                              replacement = "")

#### COMPARE FORMATION NAMES ----
#don't have the same formations
bryo.forms <- unique(bryo.meta$FORMATION)
image.forms <- unique(image.list$formation)
#non-standardized names
inDataset <- c(rep("imageFiles", length(image.forms)), rep("metadata", length(bryo.forms)))
formations <- c(image.forms, bryo.forms)

formations.df <- cbind(inDataset, formations)

#write.csv(formations.df,
#          "./Data/formations.csv",
#          row.names = FALSE)

## RECONCILE MANUALLY