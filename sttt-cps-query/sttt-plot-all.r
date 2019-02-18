library(tidyverse)
library(plyr)
library(scales)
library(RColorBrewer)

#install.packages("RColorBrewer")
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
  df <- df[c("model_size", "update_type", "sample_id","timestamp_seconds", "delta_seconds", "object_count", "object_delta_count", "remote_reference_sent_ack_count", "remote_reference_sent_ack_delta_count", "remote_reference_recv_ack_count", "remote_reference_recv_ack_delta_count")]
  df <- select(df, ms = model_size, ut = update_type, si = sample_id, ts = timestamp_seconds, ds = delta_seconds, oc = object_count, odc = object_delta_count)
  df
}

savepathFileName <- "savepath.csv"

saveprefix <- if (file.exists(savepathFileName)) (read.csv(gsub(" ","", savepathFileName, fixed=TRUE), header = FALSE, sep = ";",stringsAsFactors=FALSE))$V1 else ""

prefix <- "query-logs/"
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

#set variables here
ggplot(aggregated, aes_string(x="qi", y="ts")) +
  geom_boxplot() +
  facet_grid(ms ~ at, drop=FALSE, scales="free") + #to match all diagram scales, set scales="fixed"
  xlab("Query") +
  ylab("Execution time [s]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(0.1, "lines"),
    plot.margin=unit(c(1,1,1,1), "mm")
  )

ggsave(file=paste(saveprefix,"plot-query-execution-times.pdf", sep = ""), width=65, height=200, units="mm")



prefix <- "model-logs/"
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

splitdata <- ddply(df, .variables = "ms", summarise, cummulated = oc, time = ts)

splitdata["ms"] <- sapply(splitdata$ms, as.character)

lastTimestamp <- max(splitdata$time)

modelSizes <- unique(select(splitdata,"ms"))
modelSizes$cummulated = as.integer(modelSizes$ms)
modelSizes["time"] <- c(lastTimestamp)

splitdata <- bind_rows(splitdata, modelSizes)

splitdata <- setNames(average_value <- aggregate(x=splitdata$cummulated,
                           by=list(splitdata$ms,splitdata$time),
                           FUN=median), c("ms", "time", "cummulated"))

splitdata["time"] <- splitdata$time / 1000;

ggplot(splitdata, aes(time, cummulated, colour=ms)) + 
  #scale_y_continuous(trans='log10') +
  #scale_x_continuous(trans='log10') +
  geom_line(aes(group = ms)) +
  labs(colour = "Complete model size") +
  xlab("Elapsed time (s)") +
  ylab("Model objects") +
  theme_bw() +
  theme(legend.position = 'bottom') +
  scale_colour_brewer(palette = "Set1")

ggsave(file=paste(saveprefix,"plot-model-throughput.pdf", sep = ""), width=100, height=100, units="mm")

