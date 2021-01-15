# install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
# OR
# install.packages("devtools")
# devtools::install_github( 'daqana/tikzDevice' )
require(tikzDevice)

# TODO autmate this, but at least insert different filenames

# figure ratios differ below

# sttt model update throughput
tikz('normal.tex', standAlone = FALSE, width = 6, height = 1.5)

# sttt tb queries
tikz('normal.tex', standAlone = FALSE, width = 5, height = 8)

# sttt modes3 queries
tikz('normal.tex', standAlone = FALSE, width = 5, height = 5)

# tecs boxplot -- FIXME
tikz('normal.tex', standAlone = FALSE, width = 2, height = 3)

# tecs heatmap -- FIXME
tikz('normal.tex', standAlone = FALSE, width = 2, height = 4)

# thesis embedded query scalability plot
tikz('normal.tex', standAlone = FALSE, width = 6, height = 2.5)

# thesis embedded query comparison plot
tikz('normal.tex', standAlone = FALSE, width = 6, height = 4.4)

# thesis BBB query scalability plot
tikz('normal.tex', standAlone = FALSE, width = 5, height = 3)

# do plotting then terminate session by dev.off()
dev.off()

