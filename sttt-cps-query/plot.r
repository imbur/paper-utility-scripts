library(tidyverse)
library(plyr)
library(scales)

#install.packages("tidyverse")

load_results = function(filename) {
  print(filename)
  df <- read_delim(filename, delim="|")
  df <- df[c("query_id", "model_size", "time_seconds")]
  df <- rename(df, qi = query_id, ts = time_seconds)
  df
}

