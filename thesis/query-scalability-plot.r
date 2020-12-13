library(tidyverse)
library(plyr)
library(scales)
library(RColorBrewer)
library(ggplot2)

load_query_results = function(filename, sf, uc) {
  print(filename)
  df <- read_delim(filename, delim = ",")
  df <-
    df[c(
      "iteration",
      " measurement stage",
      " model",
      " query",
      " baseline clock ticks",
      " clock ticks",
      " execution time",
      " dimension"
    )]
  df["scale factor"] = sf
  df["microcontroller"] = uc
  df <-
    select(
      df,
      it = `iteration`,
      ms = ` measurement stage`,
      model = ` model`,
      query = ` query`,
      baseline = ` baseline clock ticks`,
      ticks = ` clock ticks`,
      exectime = ` execution time`,
      exectime_dimension = ` dimension`,
      sf = `scale factor`,
      uc = `microcontroller`
    )
  df$exectime <- as.numeric(df$exectime)
  df$uc <- toupper(df$uc)
  df
}

scale_factors = c(1, 2, 5, 10, 20)
microcontroller_types = c("ATSAMD21G18A", "XMC4500", "STM32f767ZI")

prefix <- "data/"
files <- list.files(path = prefix , pattern = ".csv")
df <- data.frame()
for (sf in scale_factors) {
  for (uc in microcontroller_types) {
    df <-
      bind_rows(df, load_query_results(
        paste(prefix, "modes3-", tolower(uc), "-query-eval-sf-", sf, ".csv", sep = ""),
        sf,
        uc
      ))
  }
}

ggplot(data = df, aes(x = sf, y = exectime), as.table = T) +
  geom_line(aes(color = query)) +
  geom_point(aes(color = query)) +
  xlab("Model Size") +
  ylab("Execution Time (microseconds)") +
  scale_x_continuous(trans='log10', breaks = c(1,2,5,10,20), labels = c(24, 48, 120, 240, 480)) +
  scale_y_continuous(trans='log10') +
  scale_fill_discrete(name = "Dose", labels = c("A", "B", "C", "D"))+
  facet_wrap( ~ uc, ncol = 3, scales = "free_y") +
  scale_color_discrete(name = "Query")
