library(tidyverse)
library(plyr)
library(scales)

#install.packages("tidyverse")

load_query_results = function(filename) {
  print(filename)
  df <- read_delim(filename, delim="|")
  df <- df[c("query_id", "model_size", "time_seconds","allocation_type")]
  df <- select(df, qi = query_id, ms = model_size, ts = time_seconds, at = allocation_type)
  df
}


load_model_results = function(filename) {
  print(filename)
  df <- read_delim(filename, delim="|")
  df <- df[c("model_size", "update_type", "timestamp_seconds", "delta_seconds", "object_count", "object_delta_count", "remote_reference_sent_ack_count", "remote_reference_sent_ack_delta_count", "remote_reference_recv_ack_count", "remote_reference_recv_ack_delta_count")]
  df <- select(df, ms = model_size, ut = update_type, ts = timestamp_seconds, ds = delta_seconds, oc = object_count, odc = object_delta_count)
  df
}

prefix <- "results/"
files <- list.files(path = prefix,pattern = ".csv")
df <- data.frame()
for (filename in files) {
  df <- bind_rows(df, 
    load_query_results(paste(prefix,filename, sep = ""))
  )
}

aggregated <- ddply(
  .data = df,
  .variables = c("ms", "qi")
)

p <- ggplot(aggregated, aes_string(x="qi", y="ts")) #set variables here

p <- p + geom_point(alpha = 1/5, size = 2.0, stroke = 0)

p <- p + facet_grid(ms ~ at, drop=FALSE, scales="free") + #to match all diagram scales, set scales="fixed"
  xlab("Query") +
  ylab("Execution time [s]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(0.1, "lines"),
    plot.margin=unit(c(1,1,1,1), "mm")
  )

ggsave(file="results.pdf", width=150, height=250, units="mm")





prefix <- "model-results/"
files <- list.files(path = prefix ,pattern = ".csv")
df <- data.frame()
for (filename in files) {
  df <- bind_rows(df, 
    load_model_results(paste(prefix,filename, sep = ""))
  )
}

objects = df$odc
breaks = df$ts

modelData <- select(df, "ms", "odc")

#splitdata <- ddply(modelData, .variables = "ms", .fun = cumsum)
splitdata <- ddply(df, .variables = "ms", summarise, cummulated = cumsum(odc), time = ts)

splitdata[ms] <- sapply(splitdata$ms, as.character)

ggplot(splitdata, aes(time, cummulated, colour=ms)) + geom_line(aes(group = ms))

plot(splitdata$timestamp, splitdata$total_objects,            # plot the data 
          main="Number of model objects",  # main title 
          xlab="Duration minutes",        # x−axis label 
          ylab="Cumulative eruptions")   # y−axis label 
lines(breaks, cumobj)           # join the points 
#lines(breaks, cumfreq0)           # join the points 

