library(ggplot2)
library(RColorBrewer)
library(reshape2)

measurements = read.csv("~/git/paper-rt-queries-modelgen/tables/random-model-runtimes.csv", header=T)

maxvals <- data.frame(variable=levels(measurements$Query), 
                max=c(18.6,15,10,3.68))

histogram_exectimes <- ggplot(measurements, aes(Exectime)) + 
  geom_histogram(aes(y = ..count.., fill = ..count..), bins = 10) +
  scale_fill_gradient(low= "#007BFF", high = "#89EBFF") + 
  geom_vline(data=maxvals, aes(xintercept = max), colour="red") + 
  xlab("Execution time (micro seconds)") + 
  ggtitle("Histogram of Execution Time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Query, scales = "free_y")+
  theme(legend.position = "none")

histogram_exectimes

ggsave(file="~/git/paper-rt-queries-modelgen/figs/histogram.pdf", plot = histogram_exectimes, width = 65, height = 65, units="mm")
