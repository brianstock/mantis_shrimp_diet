# Mantis shrimp
# Figure 3
# Boxplots by habitat - uninformative prior
# 1.7.15
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")

# Load packages
require(ggplot2)
require(R2jags)
require(MASS)
require(RColorBrewer)
require(lattice)
require(dplyr)
require(tidyr)
require(grid)

# ---------------------------------------------------------------
# Diet boxplot for results in one folder only (hard & uninformative priors)
# ------------------------------------------------------------------
# Load data
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/15_uninf_prior")
load("finished.RData")

# Get posterior chains into one tidy data frame
attach.jags(jags.1)
post.grass <- data.frame(habitat = "Seagrass",aw = p.fac1[,1,1], bs = p.fac1[,1,2], clam = p.fac1[,1,3], crab = p.fac1[,1,4], fish = p.fac1[,1,5], snail = p.fac1[,1,6])
post.coral <- data.frame(habitat = "Coral",aw = p.fac1[,2,1], bs = p.fac1[,2,2], clam = p.fac1[,2,3], crab = p.fac1[,2,4], fish = p.fac1[,2,5], snail = p.fac1[,2,6])
grass <- post.grass %>% gather(source,value,2:7)
coral <- post.coral %>% gather(source,value,2:7)
all <- rbind(grass,coral)

# Boxplot
cairo_pdf("/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig3_diet_boxplot_uninf.pdf")
plot.new()
ggplot(aes(y = value, x = source, fill = habitat), data = all) + 
	geom_boxplot(outlier.colour = NA) +
	coord_cartesian(ylim = c(0,0.85)) +
	theme_bw() +
	xlab("Source") +
	ylab("Diet proportion") +
	scale_fill_manual(values=c("black","white"), name="") + # Seagrass is black, coral is white
	theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
    		panel.background = element_blank(), axis.line = element_line(colour = "black"),
    		legend.position=c(.2, .8), legend.text=element_text(size=16), legend.key.size = unit(1, "cm"),
    		axis.title=element_text(size=16), axis.text.x=element_blank(), 
    		plot.margin = unit(c(0.3, 0.3, 1.5, 0.3), "cm"), axis.title.x = element_text(vjust=-4))
dev.off()

