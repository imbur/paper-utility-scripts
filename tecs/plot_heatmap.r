library(ggplot2)
library(RColorBrewer)
library(reshape2)
library(gridExtra)

measurements = read.csv("~/git/paper-rt-queries-modelgen/tables/heatmap-data.csv", header=T)
melted_meas = melt(measurements)
melted_meas$value = round(melted_meas$value / 120, digits=1)
heatmap_plot<-ggplot(melted_meas, aes(melted_meas$variable, 
                                      factor(melted_meas$query,
                                             levels = c("Train locations","Misaligned turnout", "End of siding",  "Close trains")))) +
  guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) +
  scale_fill_gradient2(high = c("limegreen","gold","coral"), midpoint = 1) +
  geom_tile(aes(fill = melted_meas$value)) +
  geom_text(aes(label = melted_meas$value)) +
  theme(text = element_text(size=14)) +
  #theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Model variant") + 
  #theme(axis.title.y = ytitle)+
  ylab("Query") +
  facet_wrap(~melted_meas$model, scales = "fixed", nrow = 1)
heatmap_plot

ggsave(file="~/git/paper-rt-queries-modelgen/figs/heatmap.pdf", plot = heatmap_plot,width = 380, height = 70, units="mm")








##########################################












measurements = read.csv("~/git/paper-rt-queries-modelgen/tables/heatmap-data.csv", header=T)
melted_meas = melt(measurements)
levels(melted_meas$model)

melted_meas$value = round(melted_meas$value / 120, digits=1)
heatmap_plot<-ggplot(melted_meas, aes(melted_meas$variable,
                                     factor(melted_meas$model
                                            #,
                                            #levels = c("Train locations", "End of siding",#"Misaligned turnout",  "Close trains")
                                     ))) +
 guides(fill = FALSE, color = FALSE, linetype = FALSE, shape = FALSE) +
 scale_fill_gradient2(high = c("limegreen","gold","coral")) +
 geom_tile(aes(fill = melted_meas$value)) +
 geom_text(aes(label = melted_meas$value)) +
 theme(text = element_text(size=14)) +
 #theme(legend.position = "none") +
 theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
 xlab("Model variant") +
 #theme(axis.title.y = ytitle)+
 ylab("Query") +
 facet_wrap(~melted_meas$query, scales = "free", nrow = 1)
heatmap_plot
#
#ggsave(file="~/git/paper-rt-queries-modelgen/figs/heatmap.pdf", plot = heatmap_plot,width = 380, height #= 70, units="mm")
#