library(tidyverse)
library(plyr)
library(scales)

#install.packages("tidyverse")

load_results = function(filename) {
  print(filename)
  df <- read_delim(filename, delim="|")
  df <- df[c("query_id", "model_size", "time_seconds","allocation_type")]
  df <- select(df, qi = query_id, ms = model_size, ts = time_seconds, at = allocation_type)
  df
}

files <- list.files(path = "results/",pattern = ".csv")
for (filename in files) {
  df <- bind_rows(df, 
    load_results(paste("results/",filename, sep = ""))
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
