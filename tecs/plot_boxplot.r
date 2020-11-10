library(ggplot2)
library(RColorBrewer)
library(reshape2)

measurements = read.csv("~/git/paper-rt-queries-modelgen/tables/random-model-runtimes.csv", header=T)

measurements$Query = gsub(" ","\n",measurements$Query)
measurements$Query = factor(gsub("\nof"," of",measurements$Query))

boxplot_exectimes <- ggplot(measurements, aes(factor(Query), Exectime)) + 
  geom_boxplot() +
  annotate("point", x = 'Close\ntrains', y = 22.36, col = "red", size = 2) +
  annotate("point", x = 'End of\nsiding', y = 15.29, col = "red", size = 2) + 
  annotate("point", x = 'Misaligned\nturnout', y = 7.83, col = "red", size = 2) +
  annotate("point", x = 'Train\nlocations', y = 4.25, col = "red", size = 2) +
  annotate("point", x = 'Close\ntrains', y = 11.54, col = "blue", size = 2) +
  annotate("point", x = 'End of\nsiding', y = 7.75, col = "blue", size = 2) + 
  annotate("point", x = 'Misaligned\nturnout', y = 5.45, col = "blue", size = 2) +
  annotate("point", x = 'Train\nlocations', y = 2.85, col = "blue", size = 2) +
  xlab("Query") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  ylab("Execution Times (microseconds)")

boxplot_exectimes

ggsave(file="~/git/paper-rt-queries-modelgen/figs/boxplot.pdf", plot = boxplot_exectimes, width = 100, height = 86, units="mm")


min(subset(measurements, Query == "Close\ntrains")$Exectime)
min(subset(measurements, Query == "End of\nsiding")$Exectime)
min(subset(measurements, Query == "Misaligned\nturnout")$Exectime)
min(subset(measurements, Query == "Train\nlocations")$Exectime)

mean(subset(measurements, Query == "Close\ntrains")$Exectime)
mean(subset(measurements, Query == "End of\nsiding")$Exectime)
mean(subset(measurements, Query == "Misaligned\nturnout")$Exectime)
mean(subset(measurements, Query == "Train\nlocations")$Exectime)

max(subset(measurements, Query == "Close\ntrains")$Exectime)
max(subset(measurements, Query == "End of\nsiding")$Exectime)
max(subset(measurements, Query == "Misaligned\nturnout")$Exectime)
max(subset(measurements, Query == "Train\nlocations")$Exectime)
