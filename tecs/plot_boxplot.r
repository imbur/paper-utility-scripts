library(ggplot2)
library(RColorBrewer)
library(reshape2)

measurements = read.csv("~/git/paper-rt-queries-modelgen/tables/random-model-runtimes.csv", header=T)

boxplot_exectimes <- ggplot(measurements, aes(factor(Query), Exectime)) + 
  geom_boxplot() +
  annotate("point", x = 'Close trains', y = 22.36, col = "red", size = 2.5) +
  annotate("point", x = 'End of siding', y = 15.29, col = "red", size = 2.5) + 
  annotate("point", x = 'Misaligned turnout', y = 7.83, col = "red", size = 2.5) +
  annotate("point", x = 'Train locations', y = 4.25, col = "red", size = 2.5) +
  xlab("Query") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  ylab("Execution Times (microseconds)")

boxplot_exectimes

ggsave(file="~/git/paper-rt-queries-modelgen/figs/boxplot.pdf", plot = boxplot_exectimes, width = 100, height = 100, units="mm")


min(subset(measurements, Query == "Close trains")$Exectime)
min(subset(measurements, Query == "End of siding")$Exectime)
min(subset(measurements, Query == "Misaligned turnout")$Exectime)
min(subset(measurements, Query == "Train locations")$Exectime)

mean(subset(measurements, Query == "Close trains")$Exectime)
mean(subset(measurements, Query == "End of siding")$Exectime)
mean(subset(measurements, Query == "Misaligned turnout")$Exectime)
mean(subset(measurements, Query == "Train locations")$Exectime)

max(subset(measurements, Query == "Close trains")$Exectime)
max(subset(measurements, Query == "End of siding")$Exectime)
max(subset(measurements, Query == "Misaligned turnout")$Exectime)
max(subset(measurements, Query == "Train locations")$Exectime)
