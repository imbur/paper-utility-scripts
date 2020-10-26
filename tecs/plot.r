library(ggplot2)
library(RColorBrewer)

measurements <- read.csv("~/git/paper-rt-queries-modelgen/tables/runtimes.csv", header=T)
print(measurements[])

str(measurements)

Query <- measurements[[1]]
exec_time <- measurements[[3]]
Model <- measurements[[2]]

ggplot(measurements, aes(fill=Model, y=exec_time, x=Query)) +
  scale_fill_brewer(palette = "Dark2") +
  xlab("Query") + 
  ylab("Execution time (systicks)") +
  geom_bar(position="dodge", stat="identity")


