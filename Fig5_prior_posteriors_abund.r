# Brian Stock, adapted from Eric Ward
# Figure 5
# 4/15/15
# Plot alternative priors

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")
library(tidyr)
library(dplyr)
library(ggplot2)

alpha.hard <- c(1,1,4,4,1,4)
n.sources <- length(alpha.hard)
alpha.hard <- alpha.hard*n.sources/sum(alpha.hard)

alpha.unif <- rep(1,n.sources)

alpha.abund.grass <- c(0.37,1.67,0.46,(53.51+0.28),0.29,43.4)*6/100
alpha.abund.coral <- c((25.88+14.97),0.001,16.2,(14.45+4.92),4.22,19.37)*6/100

library(gplots)
library(fields)
library(compositions)
p.grass = rDirichlet.rcomp(100000, alpha = alpha.abund.grass)
p.coral = rDirichlet.rcomp(100000, alpha = alpha.abund.coral)

# Make hard/soft aggregated prior/posterior figure
grass.prior <- data.frame(hard = p.grass[,3]+p.grass[,4]+p.grass[,6], soft = p.grass[,1]+p.grass[,2]+p.grass[,5])
grass.prior.df <- grass.prior %>% gather(variable,value,1:2)

coral.prior <- data.frame(hard = p.coral[,3]+p.coral[,4]+p.coral[,6], soft = p.coral[,1]+p.coral[,2]+p.coral[,5])
coral.prior.df <- coral.prior %>% gather(variable,value,1:2)

grass.prior.plot <- ggplot(data=grass.prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
				geom_density(alpha=.3) +
				# guides(colour=FALSE) +
				guides(colour = guide_legend(override.aes = list(size=2))) +
				scale_colour_discrete(name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				scale_fill_discrete(name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				theme(legend.key.width=unit(8,"line")) +
				theme_bw() +
				xlab("Diet proportion") +
				ylab("Prior density") +
				coord_cartesian(xlim = c(0,1)) +
				labs(title = "Seagrass Prior")

grass.prior.plot <- grass.prior.plot + theme(legend.position="bottom", legend.text = element_text(size = 16))

coral.prior.plot <- ggplot(data=coral.prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
				geom_density(alpha=.3) +
				# guides(colour=FALSE) +
				guides(colour = guide_legend(override.aes = list(size=2))) +
				scale_colour_discrete(name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				scale_fill_discrete(name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				theme(legend.key.width=unit(8,"line")) +
				theme_bw() +
				xlab("Diet proportion") +
				ylab("Prior density") +
				coord_cartesian(xlim = c(0,1)) +
				labs(title = "Coral Prior")

# Get posteriors from model #13 (abundance posterior seagrass)
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/13_abundprior_seagrass")
load("finished.RData")
library(R2jags)
attach.jags(jags.1)
post.grass <- data.frame(hard = p.global[,3]+p.global[,4]+p.global[,6], soft = p.global[,1]+p.global[,2]+p.global[,5])
post.grass.df <- post.grass %>% gather(variable,value,1:2)

# Get posteriors from model #14 (abundance posterior coral)
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/14_abundprior_coral")
load("finished.RData")
library(R2jags)
attach.jags(jags.1)
post.coral <- data.frame(hard = p.global[,3]+p.global[,4]+p.global[,6], soft = p.global[,1]+p.global[,2]+p.global[,5])
post.coral.df <- post.coral %>% gather(variable,value,1:2)


print_CI <- function(vec){
	med <- median(vec)
	low <- quantile(vec,.025)
	high <- quantile(vec,.975)
	return(round(c(low,med,high),3))
}
apply(post.coral,2,print_CI)
apply(post.grass,2,print_CI)

# Calculate specialization index (Newsome et al. 2012), hard vs. soft
n.sources <- 2
gam <<- rep(1/n.sources,n.sources)
phi <<- rep(0,n.sources)
phi[1] <- 1
calc_eps <- function(f)	sqrt(sum((f-gam)^2))/sqrt(sum((phi-gam)^2))
eps.grass <- apply(post.grass,1,calc_eps)
eps.coral <- apply(post.coral,1,calc_eps)
# eps.prior <- apply(prior,1,calc_eps)

post.grass.plot <- ggplot(data=post.grass.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Seagrass Posterior") +
				    theme(legend.position="none")

post.coral.plot <- ggplot(data=post.coral.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Coral Posterior") +
				    theme(legend.position="none")

# Plot all 3 together
library(gridExtra)
# grid.arrange(prior.plot,post.grass.plot,post.coral.plot, ncol=1)
# dev.copy(pdf,"prior_posteriors.pdf")
# dev.off()

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(grass.prior.plot)

# p3 <- grid.arrange(arrangeGrob(prior.plot + theme(legend.position="none"),
#                                post.grass.plot + theme(legend.position="none"),
#                                post.coral.plot + theme(legend.position="none"),ncol=1),
#              mylegend, nrow=4,heights=c(3,3,3,1))

grass.prior.plot <- grass.prior.plot + theme(legend.position="none")
coral.prior.plot <- coral.prior.plot + theme(legend.position="none")
p3 <- grid.arrange(arrangeGrob(grass.prior.plot, coral.prior.plot, post.grass.plot, post.coral.plot,ncol=2,nrow=2), mylegend,ncol=1,nrow=2,heights=c(6.3,0.7))
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig5_prior_posteriors_abund_color.pdf")
dev.off()

# --------------------------------------------------------------------------------
# Same but in black & white
# ---------------------------------------------------------------------------------
grass.prior.plot <- ggplot(data=grass.prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
				geom_density(alpha=.3) +
				# guides(colour=FALSE) +
				guides(colour = guide_legend(override.aes = list(size=2))) +
				scale_colour_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				scale_fill_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				theme(legend.key.width=unit(8,"line")) +
				theme_bw() +
				xlab("Diet proportion") +
				ylab("Prior density") +
				coord_cartesian(xlim = c(0,1)) +
				labs(title = "Seagrass Prior")

grass.prior.plot <- grass.prior.plot + theme(legend.position="bottom", legend.text = element_text(size = 16))

coral.prior.plot <- ggplot(data=coral.prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
				geom_density(alpha=.3) +
				# guides(colour=FALSE) +
				guides(colour = guide_legend(override.aes = list(size=2))) +
				scale_colour_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				scale_fill_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				theme(legend.key.width=unit(8,"line")) +
				theme_bw() +
				xlab("Diet proportion") +
				ylab("Prior density") +
				coord_cartesian(xlim = c(0,1)) +
				labs(title = "Coral Prior")

# Get posteriors from model #13 (hardprior, hab, mixsir, sources by habitat)
post.grass.plot <- ggplot(data=post.grass.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					scale_fill_grey(start = 0.2, end = .6) +
					scale_colour_grey(start = 0.2, end = .6) +	
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Seagrass Posterior") +
				    theme(legend.position="none")

post.coral.plot <- ggplot(data=post.coral.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					scale_fill_grey(start = 0.2, end = .6) +
					scale_colour_grey(start = 0.2, end = .6) +	
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Coral Posterior") +
				    theme(legend.position="none")

# Plot all 3 together
library(gridExtra)
# grid.arrange(prior.plot,post.grass.plot,post.coral.plot, ncol=1)
# dev.copy(pdf,"prior_posteriors.pdf")
# dev.off()

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(grass.prior.plot)

# p3 <- grid.arrange(arrangeGrob(prior.plot + theme(legend.position="none"),
#                                post.grass.plot + theme(legend.position="none"),
#                                post.coral.plot + theme(legend.position="none"),ncol=1),
#              mylegend, nrow=4,heights=c(3,3,3,1))

grass.prior.plot <- grass.prior.plot + theme(legend.position="none")
coral.prior.plot <- coral.prior.plot + theme(legend.position="none")
p3 <- grid.arrange(arrangeGrob(grass.prior.plot, coral.prior.plot, post.grass.plot, post.coral.plot,ncol=2,nrow=2), mylegend,ncol=1,nrow=2,heights=c(6.3,0.7))
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig5_prior_posteriors_abund_bw.pdf")
dev.off()
