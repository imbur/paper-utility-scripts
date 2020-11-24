# install.packages("tikzDevice", repos="http://R-Forge.R-project.org")
# OR
# install.packages("devtools")
# devtools::install_github( 'daqana/tikzDevice' )
require(tikzDevice)

tikz('normal.tex', standAlone = FALSE, width=3, height=3)

# do plotting then terminate session by dev.off()

dev.off()
