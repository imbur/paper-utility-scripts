library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)
library(data.table) 

measurements = read.csv("~/git/paper-tecs-2021-rt-queries-modelgen/tables/heatmap-data.csv", header=T)

# Create a new scale for each query (column)
measurements<-data.table(measurements)
measurements[,value_norm:=exectime/max(exectime),by=.(query)]
measurements[,min_by_query:=min(exectime),by=.(query)]
measurements[,value_norm_corrected:=(exectime - min_by_query)/max(exectime - min_by_query),by=.(query)]

# Add linebreaks for long model names
measurements$model = gsub("for ","for\n",measurements$model)
measurements$model = factor(gsub("snapshot ","snapshot\n",measurements$model))

measurements$query = gsub(" ","\n",measurements$query)
measurements$query = factor(gsub("\nof"," of",measurements$query))


forced_levels = rev(levels(measurements$model))
tmp = forced_levels[5]
forced_levels = shift(forced_levels)
forced_levels[1] = tmp

heatmap_plot<-ggplot(measurements, aes(query, model)) +
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_gradient2(high = c("limegreen", "gold", "coral"), midpoint = 0) +
  geom_tile(aes(fill = value_norm_corrected)) +
  geom_text(aes(label = exectime)) +
  theme(text = element_text(size=14)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
  theme(axis.text.y = element_text(hjust = 0.5, vjust = 0.5)) +
  ylab("Model Variant") + 
  xlab("Query") +
  scale_y_discrete(limits = forced_levels)

heatmap_plot

ggsave(file="~/git/paper-tecs-2021-rt-queries-modelgen/figs/heatmap.pdf", plot = heatmap_plot,width = 120, height = 100, units="mm")
