# Brian Stock
# 4.20.15
# Make pairs plots (Fig 4)

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/10_habitat_sourcebyhab_mixsir_conc")
load("finished.RData")
library(R2jags)
require(ggplot2)
require(MASS)
require(RColorBrewer)
require(reshape)
require(lattice)

mcmc.chains <- jags.1$BUGSoutput$n.chains
N <- mix$N
n.re <- mix$n.re
n.effects <- mix$n.effects
random_effects <- mix$random_effects
n.sources <- source$n.sources
source_names <- source_names <- c("Alph/Worm","Brittle Star", "Clam", "Crab","Fish","Snail")
attach.jags(jags.1)
jags1.mcmc <- as.mcmc(jags.1)

  panel.hist <- function(x, ...){   
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col='blue', xlim=c(0,1),...)
  }
  # Function: panel.cor (from http://personality-project.org/r/r.graphics.html)
  # Purpose: prints correlation coefficients in the lower panel, 
  #          scales text sizes to the correlation coefficient magnitudes
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor){
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r = (cor(x, y,use="pairwise"))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * abs(r))
  }
  # Function: panel.contour (inspired by http://stats.stackexchange.com/questions/31726/scatterplot-with-contour-heat-overlay)
  # Purpose: replaces scatterplots with colored contour plots
  panel.contour <- function(x,y){
    n.lines <- 4  # number of contour lines
    my.cols <- rev(brewer.pal(n.lines, "RdYlBu"))   # gets some pretty colors
    z <- kde2d(x,y)   # calculates the 2D kernel density that the contour function needs
    contour(z, drawlabels=FALSE, nlevels=n.lines, col=my.cols, add=TRUE)
  }
  
# First pairs plot - Coral
dev.new()
pairs(p.fac1[,1,], labels=source_names, diag.panel=panel.hist, lower.panel=panel.cor, upper.panel=panel.contour)

# mypath <- file.path(paste(getwd(),"/fig4_pairs_coral.pdf",sep=""))  # svalue(plot_pairs_name)
dev.copy2pdf(file="/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_pairs_coral.pdf")
dev.off()

# Second pairs plot - Seagrass
dev.new()
pairs(p.fac1[,2,], labels=source_names, diag.panel=panel.hist, lower.panel=panel.cor, upper.panel=panel.contour)

# mypath <- file.path(paste(getwd(),"/fig4_pairs_seagrass.pdf",sep=""))  # svalue(plot_pairs_name)
dev.copy2pdf(file="/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_pairs_seagrass.pdf")
dev.off()
