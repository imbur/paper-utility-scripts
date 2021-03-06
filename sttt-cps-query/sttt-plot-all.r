library(tidyverse)
library(plyr)
library(scales)
library(RColorBrewer)

#install.packages("RColorBrewer")
#install.packages("tidyverse")

# Function definitions ###############################

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

# Global config ###############################

savepathFileName <- "savepath.csv"
saveprefix <- if (file.exists(savepathFileName)) (read.csv(gsub(" ","", savepathFileName, fixed=TRUE), header = FALSE, sep = ";",stringsAsFactors=FALSE))$V1 else ""

# MoDeS3 query plot ###############################

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

aggregated$at_ordered = factor(aggregated$at, levels=c('local','standard','alternative'))

#set variables here
ggplot(aggregated, aes_string(x="qi", y="ts")) +
  geom_boxplot() +
  facet_grid(ms ~ at_ordered, drop=FALSE, scales="free") + #to match all diagram scales, set scales="fixed"
  xlab("Query") +
  ylab("Execution time [s]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(0.1, "lines"),
    plot.margin=unit(c(1,1,1,1), "mm")
  )

ggsave(file=paste(saveprefix,"plot-query-execution-times.pdf", sep = ""), width=80, height=120, units="mm")

# MoDeS3 model creation plot ###############################

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

splitdata <- bind_rows(splitdata, "ms")

splitdata <- setNames(average_value <- aggregate(x=splitdata$cummulated,
                           by=list(splitdata$ms,splitdata$time),
                           FUN=median), c("ms", "time", "cummulated"))

splitdata["time"] <- splitdata$time / 1000;


#Force to show values at time 0
splitdata$cummulated <- ifelse(splitdata$time < .025, 0, splitdata$cummulated)

#Compensate late arrivals because they distort the average
#TODO

ggplot(splitdata, aes(time, cummulated, colour=ms)) + 
  #scale_y_continuous(trans='log10') +
  #scale_x_continuous(trans='log10') +
  geom_line() +
  labs(colour = "Total size") +
  xlab("Elapsed time (s)") +
  ylab("Model object count") +
  theme_bw() +
  theme(legend.position = 'none') +
  scale_colour_brewer(palette = "Set1") +
  facet_wrap(~ms , drop=FALSE, scales="free", nrow = 1)

ggsave(file=paste(saveprefix,"plot-model-throughput.pdf", sep = ""), width=200, height=50, units="mm")


# Trainbenchmark query plot ###############################

prefix <- "query-tb-logs/"
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

aggregated$at_f = factor(aggregated$at, levels=c('2 participants','5 participants','10 participants','20 participants'))

#set variables here
ggplot(aggregated, aes_string(x="qi", y="ts")) +
  geom_boxplot() +
  facet_grid(ms ~ at_f, drop=FALSE, scales="free") + #to match all diagram scales, set scales="fixed"
  xlab("Query") +
  ylab("Execution time [s]") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
    panel.spacing = unit(0.1, "lines"),
    plot.margin=unit(c(1,1,1,1), "mm")
  )

ggsave(file=paste(saveprefix,"plot-query-tb-execution-times.pdf", sep = ""), width=140, height=180, units="mm")
