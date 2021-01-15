library(tidyverse)
library(plyr)
library(scales)
library(RColorBrewer)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(data.table)

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
      Query = ` query`,
      baseline = ` baseline clock ticks`,
      ticks = ` clock ticks`,
      exectime = ` execution time`,
      exectime_dimension = ` dimension`,
      sf = `scale factor`,
      uc = `microcontroller`
    )
  df$exectime <- as.numeric(df$exectime)
  df$uc <- toupper(df$uc)
  df$uc <- factor(df$uc)
  #convert us to ms
  df$exectime <- df$exectime / 1000
  `df`
}

scale_factors = c(1, 2, 5, 10, 20)
microcontroller_types = c("ATSAMD21G18A", "XMC4500", "STM32f767ZI")

prefix <- "data/"
df <- data.frame()
for (sf in scale_factors) {
  for (uc in microcontroller_types) {
    df <-
      bind_rows(df, load_query_results(
        paste(
          prefix,
          "modes3-",
          tolower(uc),
          "-query-eval-sf-",
          sf,
          ".csv",
          sep = ""
        ),
        sf,
        uc
      ))
  }
}

df$Implementation = "Optimized"

forced_levels = rev(levels(df$uc))
tmp = forced_levels[3]
forced_levels[3] = forced_levels[1]
forced_levels[1] = tmp

exectime_plot = ggplot(data = transform(df,
                                        uc = factor(uc, levels = forced_levels)),
                       aes(x = sf, y = exectime, color = Query, shape = Query),
                       as.table = T) +
  geom_line() +
  geom_point(size=2) +
  xlab("Model Size") +
  ylab("Execution Time [ms]") +
  scale_x_continuous(
    trans = 'log10',
    breaks = c(1, 2, 5, 10, 20),
    labels = c(24, 48, 120, 240, 480)
  ) +
  scale_y_continuous(trans = 'log10') +
  facet_grid(. ~ uc)

exectime_plot

ggsave(
  file = "~/git/phd/chapters/queries-at-runtime/figures/embedded-queries-scalability.pdf",
  plot = exectime_plot,
  width = 200,
  height = 80,
  units = "mm"
)


dfcompare <- data.frame()
for (sf in scale_factors) {
  for (uc in microcontroller_types) {
    dfcompare <-
      bind_rows(dfcompare, load_query_results(
        paste(
          prefix,
          "modes3-",
          tolower(uc),
          "-unoptimized-query-eval-sf-",
          sf,
          ".csv",
          sep = ""
        ),
        sf,
        uc
      ))
  }
}
dfcompare$Implementation <- "Subquery calls"

jointdataset <- rbind(df[df$Query==" Misaligned turnout",],df[ df$Query==" End of siding",], dfcompare)

#jointdataset$Query <- levels(jointdataset$Query)

exectime_compare_plot = ggplot(data = transform(jointdataset,
                                        uc = factor(uc, levels = forced_levels)),
                       aes(x = sf, y = exectime, color = Implementation, shape = Implementation),
                       as.table = T) +
  geom_line() +
  geom_point(size=2) +
  xlab("Model Size") +
  ylab("Execution Time [ms]") +
  scale_x_continuous(
    trans = 'log10',
    breaks = c(1, 2, 5, 10, 20),
    labels = c(24, 48, 120, 240, 480)
  ) +
  scale_y_continuous(trans = 'log10') +
  facet_grid(Query ~ uc)#,
             #scales = "free") 
#+
#  theme(axis.text = element_text(size = 7),
#        axis.text.x = element_text(angle = 90))


exectime_compare_plot

ggsave(
  file = "~/git/phd/chapters/queries-at-runtime/figures/embedded-queries-comparison.pdf",
  plot = exectime_compare_plot,
  width = 200,
  height = 140,
  units = "mm"
)
