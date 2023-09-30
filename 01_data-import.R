# 01_data-import

# Description ----

# This script imports and organizes all the data about the photos I took.
# 
# All my photos are stored as scans in an external hard drive, with one folder per "shooting session". 
# Each folder is names as: "mm-yyyy_sessionname_nn", where:
# "mm-yyyy" is month and year when the photos were taken,
# "sessionname" is just the "theme" of the photos (in general a place or an occasion)
# "nn" is a numerical counter (from "01"), in case more than one folder has the same date and session name
#
# Inside each folder, photos are all names as: "mm-yyyy_id_subject_nn". Same logic as for folder name, but "subject" indicates the subject in the photo, 
# and "id" is a numeric identifier simply going from "01" to the number of scans in a folder.
# Moreover, each folder also contains a "REAME" file. This contains the following lines:
#
#   README
# 
#   Month yyyy
#   Session name
#   Place
#   Country
#   Camera
#   Lens
#   Film

# Since at the moment I only have one camera (Canon A1), and one lens (50mm), those two lines are not that informative.  
# I'm going to use the rest of the lines, as well as the file names of the photos to build the infographic.

# Libraries ----
library(tidyverse)
library(lubridate)

# Import data ----

in_dir <- "/Volumes/Fra HD/film-photos/2023/" # Dir where all photos are stored (at the moment only photos from 2023)

in_dir_names <- list.files(path = in_dir, full.names = TRUE)  # Read all scan folder names

## Function to read photo file names and REAMDE files within a folder ----
read_photo_info <- function(path, roll_id){
  # ("roll_id" it's a digit to identify different rolls)
  
  # Read photo file names
  photo_list <- list.files(path = path, pattern = ".tif")  # list files (photos are all ".tif" format)
  
  # Split the file name into separate columns of a df
  df <- tibble(filename = photo_list) %>% 
  mutate(
    month = month(as.numeric(str_extract(filename, "^[0-9]{2}")), label = TRUE),  # extract month
    year = str_extract(filename, "(?<=-)[0-9]{4}"),  # extract year
    photo_id = str_extract(filename, "(?<=[0-9]{4}_)[0-9]{2}"),  # extract photo id
    subject = str_extract(filename, "(?<=[0-9]{4}_[0-9]{2}_)[^_.]+"),  # extract subject
    roll_id = roll_id
  )
  
  # Read REAMDE info:
  readme_file <- readLines(paste(path, "README.txt",sep = "/"), warn = FALSE)
  
  # Extract session name, place, country, and film type
  # (we don't need date since it's already extracted from photo file names)
  df <- df %>% 
    mutate(
      session = readme_file[4],
      place = readme_file[5],
      country = readme_file[6],
      film = readme_file[9]
    )
  
  return(df)
}

## Import photo info ----
df_all <- in_dir_names %>% 
  imap_dfr(~read_photo_info(.x, .y))  # use "imap" to include the folder index in the function input

# Save photo info ----
saveRDS(df_all, file = "all_photo_info.RDS")