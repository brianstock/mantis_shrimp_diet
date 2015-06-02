# Brian Stock
# 4.6.15

# Mantis shrimp diet analysis #7
#  prior: hard-shelled prey 4x more likely
#  data:  Habitat + source(Habitat)
#  error: MixSIR

setwd("/home/brian/Documents/Isotopes/mantis_shrimp_diet")

# First load the package dependencies (note that 'gWidgetsRGtk2' is NOT loaded - we're not making the GUI)
require(ggplot2)
require(R2jags)
require(MASS)
require(RColorBrewer)
require(reshape)
require(lattice)

# Next we clean up the workspace
rm(list=ls()) 	# deletes everything previously in the workspace
runif(1)		# generates one random number (else JAGS can complain)

# Load all MixSIAR functions into the workspace
source("load_mix_data.r")
source("load_source_data.r")
source("load_discr_data.r")
source("plot_data.r")
source("write_JAGS_model.r")
source("run_model.r")
source("output_JAGS_sourcebyhab.r")
source("plot_continuous_var.r")

#####################################################################################
# Load mixture data, i.e. your:
#	Consumer isotope values (trophic ecology / diet)
#	Mixed sediment/water tracer values (sediment/hydrology fingerprinting)

mix <- load_mix_data(filename="mantis_consumer.csv", iso_names=c("d13C","d15N"), random_effects=NULL, cont_effects=NULL, fixed_effects="Habitat")

#####################################################################################
# Load source data, i.e. your:
#	Source isotope values (trophic ecology / diet)
#	Sediment/water source tracer values (sediment/hydrology fingerprinting)

source <- load_source_data(filename="mantis_source_habitat.csv", source_factors="Habitat", conc_dep=FALSE, data_type="means", mix)    

#####################################################################################
# Load discrimination data, i.e. your:
#	Trophic Enrichment Factor (TEF) / fractionation values (trophic ecology / diet)
#	xxxxxxxx (sediment/hydrology fingerprinting)

discr <- load_discr_data(filename="mantis_discrimination.csv", mix)

# Change into output folder
folder <- "/7_habitat_sourcebyhab_mixsir"
new.dir <- paste(getwd(),folder,sep="")
setwd(new.dir)

#####################################################################################
# Make isospace plot
# Are the data loaded correctly?
# Is your mixture data in the source polygon?
# Are one or more of your sources confounded/hidden?
# pdf.options(encoding='ISOLatin2.enc')
# postscript(encoding="WinAnsi.enc")
plot_data(filename="isospace_plot", plot_save_pdf=TRUE, plot_save_png=TRUE, mix,source,discr)

#####################################################################################
# Write JAGS model file
# Model will be saved as 'model_filename' ("MixSIAR_model.txt" is default, but may want to change if in a loop)

# Wolves example
model_filename <- "MixSIAR_model.txt"   # Name of the JAGS model file
indiv_effect <- FALSE	               # Include Individual as a random effect in the model?
nested <- NULL                          # If there are 2 random effects, is the 2nd nested in the 1st (hierarchical)?
write_JAGS_model(model_filename, indiv_effect, nested, resid_err=FALSE, mix,source)

#####################################################################################
# Run model
# JAGS output will be saved as 'jags.1'

# MCMC run options:
# run <- "test"       	# list(chainLength=1000, burn=500, thin=1, chains=3, calcDIC=TRUE)
# run <- "very short" 	# list(chainLength=10000, burn=5000, thin=5, chains=3, calcDIC=TRUE)
# run <- "short"     	# list(chainLength=50000, burn=25000, thin=25, chains=3, calcDIC=TRUE)
# run <- "normal"      	# list(chainLength=100000, burn=50000, thin=50, chains=3, calcDIC=TRUE)
# run <- "long"  		# list(chainLength=300000, burn=200000, thin=100, chains=3, calcDIC=TRUE)
# run <- "very long" 	# list(chainLength=1000000, burn=700000, thin=300, chains=3, calcDIC=TRUE)
# run <- "extreme"    	# list(chainLength=3000000, burn=2700000, thin=300, chains=3, calcDIC=TRUE)

# Can also set custom MCMC parameters
# run <- list(chainLength=500000, burn=300000, thin=100, chains=3, calcDIC=TRUE)

# jags.1 <- run_model(run="normal", indiv_effect,mix,source,discr,model_filename)
alpha.hard <- c(1,1,4,4,1,4)
n.sources <- length(alpha.hard)
alpha.hard <- alpha.hard*n.sources/sum(alpha.hard)
jags.1 <- run_model(run="very long", indiv_effect,mix,source,discr,model_filename, alpha.prior = alpha.hard) # informative prior

#####################################################################################
# Process JAGS output

# All examples
output_options <- list(summary_save = TRUE,                 # Save the summary statistics as a txt file?
                    summary_name = "summary_statistics",    # If yes, specify the base file name (.txt will be appended later)
                    sup_post = FALSE,                       # Suppress posterior density plot output in R?
                    plot_post_save_pdf = TRUE,              # Save posterior density plots as pdfs?
                    plot_post_name = "posterior_density",   # If yes, specify the base file name(s) (.pdf/.png will be appended later)
                    sup_pairs = FALSE,                      # Suppress pairs plot output in R?
                    plot_pairs_save_pdf = TRUE,             # Save pairs plot as pdf?
                    plot_pairs_name = "pairs_plot",         # If yes, specify the base file name (.pdf/.png will be appended later)
                    sup_xy = FALSE,                         # Suppress xy/trace plot output in R?
                    plot_xy_save_pdf = TRUE,                # Save xy/trace plot as pdf?
                    plot_xy_name = "xy_plot",               # If yes, specify the base file name (.pdf/.png will be appended later)
                    gelman = TRUE,                          # Calculate Gelman-Rubin diagnostic test?
                    heidel = FALSE,                          # Calculate Heidelberg-Welch diagnostic test?
                    geweke = TRUE,                          # Calculate Geweke diagnostic test?
                    diag_save = TRUE,                       # Save the diagnostics as a txt file?
                    diag_name = "diagnostics",              # If yes, specify the base file name (.txt will be appended later)
                    indiv_effect = indiv_effect,            # Is Individual a random effect in the model? (already specified)
                    plot_post_save_png = FALSE,             # Save posterior density plots as pngs?
                    plot_pairs_save_png = FALSE,            # Save pairs plot as png?
                    plot_xy_save_png = FALSE)               # Save xy/trace plot as png?
output_JAGS(jags.1, mix, source, output_options)
save.image("finished.RData")
