# Homework 11
# Script using barracudar functions
# Franny F Oppenheimer
# University of Vermont
# 10 April 2024
#
# load packages ----
library(log4r)
library(TeachingDemos)
library(tidyverse)
library(pracma)
library(ggmosaic)
# load any additional packages here...


# source function files ----

setwd("~/Desktop/GitHub/ComputationalBiology/OppenheimerBio6100/Homework11/barracudar")

source("barracudar/DataTableTemplate.R")
source("barracudar/AddFolder.R")
source("barracudar/BuildFunction.R")
source("barracudar/MetaDataTemplate.R")
source("barracudar/CreatePaddedLabel.R")
source("barracudar/InitiateSeed.R")
source("barracudar/SetUpLog.R")
source("barracudar/SourceBatch.R")

source("barracudar/QBox.R")
source("barracudar/QCon1.R")
source("barracudar/QCon2.R")
source("barracudar/QHist.R")
source("barracudar/QLogis.R")
source("barracudar/QScat.R")
source("barracudar/QBub.R")
source("barracudar/QContour.R")


library(stringr)


setwd("~/Desktop/GitHub/ComputationalBiology/OppenheimerBio6100/Homework11/OriginalData")

# Iterate through each folder  - go two levels down each time and then extract some file, in this case, pattern="countdata"


# 2. ----
# gathering the file names that we actually want to look at in a vector called filenames

# list.files() will gather character strings of file names

filelist <- list.files("~/Desktop/GitHub/ComputationalBiology/OppenheimerBio6100/Homework11/OriginalData",pattern="BART")

# use a for loop for number of files that we're concerned with, pull out files

# paste() or paste0() function concatenates strings
# paste0("Here is ","the ","filepath: ", filelist[1]) # example

# make an empty vector
filenames <- c()

# make matrix of file names
for (i in 1:10) {
  setwd(paste0("~/Desktop/GitHub/ComputationalBiology/OppenheimerBio6100/Homework11/OriginalData","/", filelist[i])) # create new file path to go to

  filenames[i] <- list.files(pattern="countdata") # any time the file has "countdata" in it, it pulls it out
}

filenames

# 3 + 4 ----
# generating functions to get rid of empty/missing cases, extract the year from the file name, calculate total number of individuals found, and calculate number of unique species found


getinfo <- function(filelist,filenames) {

  metadata <- matrix(0,length(filelist),4)
  colnames(metadata) <- c("File","Year","Total # Individuals", "Species Richness")

  for (i in 1:10) { # for every folder (there is only one good csv per folder)

# go into the folder listed at filelist[i]
setwd(paste0("~/Desktop/GitHub/ComputationalBiology/OppenheimerBio6100/Homework11/OriginalData","/", filelist[i]))

# get the csv data from filename
csvdata <- read.csv(filenames[i]) # read in the csv from this folder, off of the list of good csvs!
csvdata

dfcsv <- data.frame(csvdata)
dfcsv

# GETTING INFORMATION

# get rid of empty/missing cases
dfcsv <- na.omit(dfcsv[,1:20]) # rows after 20 are either all NA or no NA, so this seems prudent if I still want to have stuff to work with
dfcsv

# extract the year from the file name
year <- str_sub(filenames[i],43,46)


# calculate total individuals found
total_individuals <- nrow(dfcsv)

# calculate total number of unique species
species_richness <- length(unique(dfcsv[,12]))

 # fill in the empty vector for each csv
metadata[i,1:4] <- c(filelist[i], year, total_individuals, species_richness)
# number of items to replace is not a multiple of replacement length

  }

return(metadata)

  }

getinfo(filelist,filenames)











