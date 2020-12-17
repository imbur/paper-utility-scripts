library(tidyverse)

wide <- read_csv("local-bbb-runtimes.csv")
long <-
  gather(wide, run, time,-c(query, allocation, size, elements))

long$query = factor(long$query,
                    levels = c("Train Locations", "Close Trains", "Derailment", "End of Siding"))

exectimes = ggplot(long) +
  geom_boxplot(aes_string(x = "query", y = "time")) +
  facet_wrap(facets = long$elements,
             scale = "free_y",
             ncol = 4) +
  xlab("Query") +
  ylab("Execution time [s]") +
  theme_bw() +
  theme(axis.text = element_text(size = 7),
        axis.text.x = element_text(angle = 90))

exectimes

ggsave(
  plot = exectimes,
  file = "modes-execution-times-plot.pdf",
  width = 200,
  height = 70,
  units = "mm"
)
