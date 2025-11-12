#install and import required packages
install.packages("tidyverse")
library("tidyverse")
#stop installing/importing required packages

#load the csv file
housing <- read.csv("housing.csv",header=TRUE)
#clean the data frame
housing |> filter(!is.na(bedrooms),!is.na(bathrooms)) |> select(-1)
