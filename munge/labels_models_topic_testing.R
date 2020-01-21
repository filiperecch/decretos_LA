##############################
####
####
####
####  Structural Topic Modeling - topic labels testing 
####
####
####
###############################

# loading packages 
library(stm)
library(SnowballC)
library(tidyverse)
library(stminsights)
library(tidytext)
library(ggridges)
library(RColorBrewer)
library(dotwhisker)
library(lubridate)


###########################

# setting paths

root_decretos_LA <- here::here()
raw_decretos_LA <- paste(root_decretos_LA,
                         "/decretos_LA_cloud_dir/data/raw",
                         sep = "")

data_decretos_LA <- paste(root_decretos_LA,
                          "/decretos_LA_cloud_dir/data",
                          sep = "")

images_decretos_LA <- paste(root_decretos_LA,
                            "/analysis/images/",
                            sep = "")

for (r in 4:7) {
  
  topics = r  

  labels <- read_rds(paste(root_decretos_LA,
                  "/R/",
                  "topic_labels_n_",
                  r,
                  sep = ""))

  effects <- read_rds(paste(root_decretos_LA,
                  "/R/",
                  "effects_topics_n_",
                  r,
                  sep = ""))
  
  assign(paste("topic_labels_n_",
               r,
               sep = ""),
         labels)
  
  assign(paste("effects_topics_n_",
               r,
               sep = ""),
         effects)

}
