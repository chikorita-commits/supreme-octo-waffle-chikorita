library("tidyverse")
housing |> filter(!is.na(bedrooms),!is.na(bathrooms))
