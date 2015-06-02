# Mantis Shrimp Diet

Data and code associated with:
**Specialized morphology corresponds to a generalist diet: linking form and function in mantis shrimp crustaceans**

Maya S. deVries, Brian C. Stock, John H. Christy, Gregory R. Goldsmith, & Todd E. Dawson

Manuscript in prep.

All mantis shrimp isotope data and R-scripts used to perform the analyses and create all figures are here. The best fit model (Habitat + source(Habitat) + concentration dependence) with an informative prior (hard-shelled prey 4x more likely than soft-bodied) is model #10, see `mixsiar_script_10.r`. The models using informative priors based on the prey abundance surveys are #s 13 (seagrass) and 14 (coral), see `mixsiar_script_13.r` and `mixsiar_script_14.r`.

The R code is based on MixSIAR (https://github.com/brianstock/MixSIAR), which requires you to first install JAGS (http://mcmc-jags.sourceforge.net/). The code also depends on several R packages. Many thanks to the authors of:

 - ggplot2
 - R2jags
 - MASS
 - RColorBrewer
 - reshape
 - lattice


