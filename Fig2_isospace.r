# Mantis shrimp
# Figure 2
# Isospace plot
# 4.16.15

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")

# First load the package dependencies (note that 'gWidgetsRGtk2' is NOT loaded - we're not making the GUI)
require(ggplot2)
require(R2jags)
require(MASS)
require(RColorBrewer)
require(reshape)
require(lattice)

# Load all MixSIAR functions into the workspace
source("load_mix_data.r")
source("load_source_data.r")
source("load_discr_data.r")
source("plot_data.r")
source("plot_data_opencirc.r")

# -----------------------------------------------------
# Coral isospace
# --------------------------------------------------------
mix <- load_mix_data(filename="mantis_consumer_coral.csv", iso_names=c("d13C","d15N"), random_effects=NULL, cont_effects=NULL, fixed_effects=NULL)
source <- load_source_data(filename="mantis_source_coral.csv", source_factors=NULL, conc_dep=FALSE, data_type="means", mix)    
discr <- load_discr_data(filename="mantis_discrimination.csv", mix)
plot_data_opencirc(filename="Fig2a_isospace_coral", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

# -----------------------------------------------------
# Seagrass isospace
# --------------------------------------------------------
mix <- load_mix_data(filename="mantis_consumer_seagrass.csv", iso_names=c("d13C","d15N"), random_effects=NULL, cont_effects=NULL, fixed_effects=NULL)
source <- load_source_data(filename="mantis_source_seagrass.csv", source_factors=NULL, conc_dep=FALSE, data_type="means", mix)    
discr <- load_discr_data(filename="mantis_discrimination.csv", mix)
plot_data(filename="Fig2b_isospace_seagrass", plot_save_pdf=TRUE, plot_save_png=FALSE, mix,source,discr)

