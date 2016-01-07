# Brian Stock
# 1/7/16
# Figure 4
# Plot priors and posteriors for uninformative prior

library(gplots)
library(fields)
library(compositions)

# alpha.hard <- c(1,1,4,4,1,4)
# n.sources <- length(alpha.hard)
# alpha.hard <- alpha.hard*n.sources/sum(alpha.hard)

n.sources <- 6
alpha.unif <- rep(1,n.sources)

# alpha.abund.grass <- c(0.35,1.61,0.43,(51.65+0.26),5.18,40.5)*6/100
# alpha.abund.coral <- c((14.31+24.74),0.01,15.48,(13.81+4.71),8.44,18.51)*6/100

pb = rDirichlet.rcomp(10000, alpha = alpha.unif)

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

# Get posteriors from Model #15: uninformative prior, hab, mixsir, sources by habitat, conc dep
setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet/15_uninf_prior")
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
	return(round(c(med,low,high),3))
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
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_prior_posteriors_uninf_color.pdf")
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
dev.copy(pdf,"/home/brian/Documents/Isotopes/mantis_shrimp_diet/Fig4_prior_posteriors_uninf_bw.pdf")
dev.off()

# -------------------------------------------------------------------------------
# Table 3: summary of mixing model diet estimates (median and 95% CI)
# -------------------------------------------------------------------------------

# SG med, SG low, SG high, C med, C low, C high
# Soft agg, AW, BS, Fish, Hard agg, Clam, Crab, Snail
table3 <- matrix(NA,nrow=8,ncol=6)
rownames(table3) <- c("Soft-bodied", "Alpheid/worm", "Brittle star", "Fish", "Hard-shelled","Clam","Crab","Snail")
colnames(table3) <- c("SG 50%","SG 2.5%","SG 97.5%","Coral 50%","Coral 2.5%","Coral 97.5%")
table3[1,1:3] <- t(apply(post.grass,2,print_CI)[,2]) # SG soft agg
table3[2,1:3] <- print_CI(p.fac1[,1,1]) # SG AW
table3[3,1:3] <- print_CI(p.fac1[,1,2]) # SG BS
table3[4,1:3] <- print_CI(p.fac1[,1,5]) # SG fish
table3[5,1:3] <- t(apply(post.grass,2,print_CI)[,1]) # SG hard agg
table3[6,1:3] <- print_CI(p.fac1[,1,3]) # SG Clam
table3[7,1:3] <- print_CI(p.fac1[,1,4]) # SG Crab
table3[8,1:3] <- print_CI(p.fac1[,1,6]) # SG Snail
table3[1,4:6] <- t(apply(post.coral,2,print_CI)[,2]) # Coral soft agg
table3[2,4:6] <- print_CI(p.fac1[,2,1]) # Coral AW
table3[3,4:6] <- print_CI(p.fac1[,2,2]) # Coral BS
table3[4,4:6] <- print_CI(p.fac1[,2,5]) # Coral fish
table3[5,4:6] <- t(apply(post.coral,2,print_CI)[,1]) # Coral hard agg
table3[6,4:6] <- print_CI(p.fac1[,2,3]) # Coral Clam
table3[7,4:6] <- print_CI(p.fac1[,2,4]) # Coral Crab
table3[8,4:6] <- print_CI(p.fac1[,2,6]) # Coral Snail
write.csv(table3, file = "/home/brian/Documents/Isotopes/mantis_shrimp_diet/Table3_uninf.csv", row.names = TRUE)
table3
