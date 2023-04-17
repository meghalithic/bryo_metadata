#compare output from imageFilesMetadata.R to the metadata file "Imaged Ste...

#### COMPARE TO METADATA FILE ----

##### LOAD DATA -----
bryo.meta <- read.csv("./Data/Imaged Steginoporella magnifica specimens.csv", header = TRUE)
bryo.meta$SPECIMEN.NR <- gsub(bryo.meta$SPECIMEN.NR,
                              pattern = " ", 
                              replacement = "")
nrow(bryo.meta) #880
nrow(bryo.meta[!duplicated(bryo.meta$SPECIMEN.NR),]) #777

##### COMPARE TOTALS -----

tots <- df.list %>%
  group_by(specimenNR, ext) %>%
  summarise(N = n()) %>%
  as.data.frame()

nrow(tots) #1810
#1033 more images

length(setdiff(tots$specimenNR, bryo.meta$SPECIMEN.NR)) #136 in df.list that are not in bryo.meta
length(setdiff(bryo.meta$SPECIMEN.NR, tots$specimenNR)) #8 in bryo.meta that are not in df.list

## look at 8 in bryo.meta that are not in df.list
setdiff(bryo.meta$SPECIMEN.NR, tots$specimenNR) 
#e.g., bryo.meta has 204CV, df.list has 204CC
#232CV in bryo.meta is 232CC in df.list
#005CC in bryo.meta is 005CV in df.list
#0367iCV does not exist in df.list
#435CC does not exist in df.list
#466CV in bryo.meta is 466CC in df.list
#568CC in bryo.meta is 568CV in df.list
#717iCV does not exist in df.list, but 717CV does

#NR OF PICS in bryo.meta don't match the total number of images taken
max(bryo.meta$NR.OF.PICS) #4
max(tots$N) #5
tots$specimenNR[which.max(tots$N)] #237CC
df.list[df.list$specimenNR == "237CC",]
#should be only 3 images
#has 237_CC_2_15v_x40_BSE replicated
#has 237_CC_1_15v_x30_BSE_BTK2

## look at 136 in df.list that are not in bryo.meta
setdiff(tots$specimenNR, bryo.meta$SPECIMEN.NR)

##### CREATE CSV -----
extra.df <- setdiff(tots$specimenNR, bryo.meta$SPECIMEN.NR)
extra.bryo <- setdiff(bryo.meta$SPECIMEN.NR, tots$specimenNR)

df.trim <- df.list[df.list$specimenNR %in% extra.df &
                     df.list$ext == "tif",]
df.trimmed <- df.trim[!duplicated(df.trim[c('specimenNR')]), ]

dates <- c(rep("NA", length(extra.df)), bryo.meta$DATE[bryo.meta$SPECIMEN.NR %in% extra.bryo])
folder <- c(df.trimmed$folder, rep("NA", length(extra.bryo)))
formation <- c(df.trimmed$newFormation, rep("NA", length(extra.bryo)))
fromDataset <- c(rep("imageFiles", length(extra.df)), rep("metadata", length(extra.bryo)))
imageDiff <- c(extra.df, extra.bryo)
errors.df <- cbind(fromDataset, imageDiff, folder, formation, dates)

#write.csv(errors.df,
#          "./Results/errors.csv",
#          row.names = FALSE)

# ADD NOTES MANUALLY