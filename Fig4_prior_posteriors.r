# Brian Stock
# 4/15/15
# Figure 4
# Plot priors and posteriors for hard prior

library(gplots)
library(fields)
library(compositions)

alpha.hard <- c(1,1,4,4,1,4)
n.sources <- length(alpha.hard)
alpha.hard <- alpha.hard*n.sources/sum(alpha.hard)

alpha.unif <- rep(1,n.sources)

alpha.abund.grass <- c(0.35,1.61,0.43,(51.65+0.26),5.18,40.5)*6/100
alpha.abund.coral <- c((14.31+24.74),0.01,15.48,(13.81+4.71),8.44,18.51)*6/100

pb = rDirichlet.rcomp(10000, alpha = alpha.hard)

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")

# Make hard/soft aggregated prior/posterior figure
library(tidyr)
library(dplyr)
library(ggplot2)
prior <- data.frame(hard = pb[,3]+pb[,4]+pb[,6], soft = pb[,1]+pb[,2]+pb[,5])
prior.df <- prior %>% gather(variable,value,1:2)

prior.plot <- ggplot(data=prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
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
				labs(title = "Aggregated prior density")

prior.plot <- prior.plot + theme(legend.position="bottom", legend.text = element_text(size = 16))

# Get posteriors from best fit model #10 (hardprior, hab, mixsir, sources by habitat, conc dep)
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/10_habitat_sourcebyhab_mixsir_conc")
load("finished.RData")
library(R2jags)
attach.jags(jags.1)
post.grass <- data.frame(hard = p.fac1[,1,3]+p.fac1[,1,4]+p.fac1[,1,6], soft = p.fac1[,1,1]+p.fac1[,1,2]+p.fac1[,1,5])
post.coral <- data.frame(hard = p.fac1[,2,3]+p.fac1[,2,4]+p.fac1[,2,6], soft = p.fac1[,2,1]+p.fac1[,2,2]+p.fac1[,2,5])

# Get median +/- 95% CI proportions for hard vs. soft
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
eps.prior <- apply(prior,1,calc_eps)

post.grass.df <- post.grass %>% gather(variable,value,1:2)
post.coral.df <- post.coral %>% gather(variable,value,1:2)

post.grass.plot <- ggplot(data=post.grass.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Aggregated posterior density: Seagrass") +
				    theme(legend.position="none")

post.coral.plot <- ggplot(data=post.coral.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Aggregated posterior density: Coral") +
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

mylegend<-g_legend(prior.plot)

# p3 <- grid.arrange(arrangeGrob(prior.plot + theme(legend.position="none"),
#                                post.grass.plot + theme(legend.position="none"),
#                                post.coral.plot + theme(legend.position="none"),ncol=1),
#              mylegend, nrow=4,heights=c(3,3,3,1))

prior.plot <- prior.plot + theme(legend.position="none")
p3 <- grid.arrange(prior.plot, post.grass.plot, post.coral.plot, mylegend,ncol=1,nrow=4,heights=c(3.1,3.1,3.1,0.7))
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_prior_posteriors_hard_color.pdf")
dev.off()

# --------------------------------------------------------------------------------
# Same but in black & white
# ---------------------------------------------------------------------------------
prior.plot <- ggplot(data=prior.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
				geom_density(alpha=.3) +
				# guides(colour=FALSE) +
				guides(colour = guide_legend(override.aes = list(size=2))) +
				scale_colour_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				scale_fill_grey(start = 0.2, end = .6, name="",
                         breaks=c("hard", "soft"),
                         labels=c(" Hard-shelled          ", " Soft-bodied")) +
				# scale_fill_grey(start = 0.2, end = .6) +
				# scale_colour_grey(start = 0.2, end = .6) +
				theme(legend.key.width=unit(8,"line")) +
				theme_bw() +
				xlab("Diet proportion") +
				ylab("Prior density") +
				coord_cartesian(xlim = c(0,1)) +
				labs(title = "Aggregated prior density")

prior.plot <- prior.plot + theme(legend.position="bottom", legend.text = element_text(size = 16))

post.grass.plot <- ggplot(data=post.grass.df, aes(x=value, fill=as.factor(variable), colour=as.factor(variable))) +
					geom_density(alpha=.3) +
					# guides(fill=FALSE,colour=FALSE) +
					scale_fill_grey(start = 0.2, end = .6) +
					scale_colour_grey(start = 0.2, end = .6) +					
					theme_bw() +
					xlab("Diet proportion") +
				    ylab("Posterior density") +
				    coord_cartesian(xlim = c(0,1)) +
				    labs(title = "Aggregated posterior density: Seagrass") +
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
				    labs(title = "Aggregated posterior density: Coral") +
				    theme(legend.position="none")

# Plot all 3 together
library(gridExtra)

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(prior.plot)

prior.plot <- prior.plot + theme(legend.position="none")
p3 <- grid.arrange(prior.plot, post.grass.plot, post.coral.plot, mylegend,ncol=1,nrow=4,heights=c(3.1,3.1,3.1,0.7))
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_prior_posteriors_hard_bw.pdf")
dev.off()
