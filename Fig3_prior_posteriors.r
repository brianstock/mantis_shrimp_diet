# Brian Stock
# 4/15/15
# Plot priors and posteriors for hard prior and abundance priors

alpha.hard <- c(1,1,4,4,1,4)
n.sources <- length(alpha.hard)
alpha.hard <- alpha.hard*n.sources/sum(alpha.hard)

alpha.unif <- rep(1,n.sources)

alpha.abund.grass <- c(0.35,1.61,0.43,(51.65+0.26),5.18,40.5)*6/100
alpha.abund.coral <- c((14.31+24.74),0.01,15.48,(13.81+4.71),8.44,18.51)*6/100

# Plot prior distributions:
#   1) generalist: uninformative (alpha.unif)
#   2) generalist: prey abundance (alpha.abund.grass)
#   3) generalist: prey abundance (alpha.abund.coral)
#   4) specialist: hard-shell prey 4x likely (alpha.hard)
library(gplots)
library(fields)
library(compositions)
# p = rDirichlet.rcomp(10000, alpha = alpha.unif)
# p1 = c(p[,1], seq(0,1,0.01))
# p5 = c(p[,5], seq(0,1,0.01))
# p4 = c(p[,4], seq(0,1,0.01))
# p_jeff = rDirichlet.rcomp(10000, alpha = alpha.abund.grass)
# p1_jeff = c(p_jeff[,1], seq(0,1,0.01))
# p5_jeff = c(p_jeff[,5], seq(0,1,0.01))
# p4_jeff = c(p_jeff[,4], seq(0,1,0.01))
# pa = rDirichlet.rcomp(10000, alpha = alpha.abund.coral)
# p1a = c(pa[,1], seq(0,1,0.01))
# p5a = c(pa[,5], seq(0,1,0.01))
# p4a = c(pa[,4], seq(0,1,0.01))
pb = rDirichlet.rcomp(10000, alpha = alpha.hard)
# p1b = c(pb[,1], seq(0,1,0.01))
# p5b = c(pb[,5], seq(0,1,0.01))
# p4b = c(pb[,4], seq(0,1,0.01))
# par(mfrow = c(3,4), mgp = c(2,1,0),mai=c(0.5,0.5,0.4,0.1))
# hist(p1, breaks = seq(0,1,length.out=40),col="darkgrey", main = "Source 1: Uniform",xlab=expression(p[1]),xlim=c(0,1))
# hist(p1_jeff, breaks = seq(0,1,length.out=40),col="lightgrey", main = "Source 1: Seagrass abundance",xlab=expression(p[1]),xlim=c(0,1))
# hist(p1a, breaks = seq(0,1,length.out=40),col="blue", main = "Source 1: Coral abundance",xlab=expression(p[1]),xlim=c(0,1))
# hist(p1b, breaks = seq(0,1,length.out=40),col="red", main = "Source 1: Hard-shell",xlab=expression(p[1]),xlim=c(0,1))
# hist(p5, breaks = seq(0,1,length.out=40),col="darkgrey", main = "Source 5: Uniform",xlab=expression(p[2]),xlim=c(0,1))
# hist(p5_jeff, breaks = seq(0,1,length.out=40),col="lightgrey", main = "Source 5: Seagrass abundance",xlab=expression(p[1]),xlim=c(0,1))
# hist(p5a, breaks = seq(0,1,length.out=40),col="blue", main = "Source 5: Coral abundance",xlab=expression(p[2]),xlim=c(0,1))
# hist(p5b, breaks = seq(0,1,length.out=40),col="red", main = "Source 5: Hard-shell",xlab=expression(p[2]),xlim=c(0,1))
# hist(p4, breaks = seq(0,1,length.out=40),col="darkgrey", main = "Source 4: Uniform",xlab=expression(p[4]),xlim=c(0,1))
# hist(p4_jeff, breaks = seq(0,1,length.out=40),col="lightgrey", main = "Source 4: Seagrass abundance",xlab=expression(p[1]),xlim=c(0,1))
# hist(p4a, breaks = seq(0,1,length.out=40),col="blue", main = "Source 4: Coral abundance",xlab=expression(p[4]),xlim=c(0,1))
# hist(p4b, breaks = seq(0,1,length.out=40),col="red", main = "Source 4: Hard-shell",xlab=expression(p[4]),xlim=c(0,1))

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")
# dev.copy(png,"priors.png", width=980, height=683)
# dev.off()

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
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig3_prior_posteriors_hard_color.pdf")
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
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig3_prior_posteriors_hard_bw.pdf")
dev.off()
