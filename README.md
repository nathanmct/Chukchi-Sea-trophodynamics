# Chukchi-Sea-trophodynamics

Readme for McTigue and Dunton (2017) Trophodynamics of the Hanna Shoal Ecosystem (Chukchi Sea, Alaska): connecting multiple end-members to a rich food web


In RStudio, open Hanna_Shoal_SIA.r to run analysis. Be sure all files are in one folder and your working directory is set to that folder.
JAGS must be installed on your computer to run some of these scripts. Install it here: https://sourceforge.net/projects/mcmc-jags/files/


Some of the analysis scripts have been borrowed from other sources: 
1) Passoti et al. (2015) Benthic trophic interactions in an Antarctic shallow water ecosystem affected by recent glacial retreat. PLoS ONE 10(11):e0141742. doi:10.1371/journal.pone.0141742
2) The vignette for the package simmr available at https://cran.r-project.org/web/packages/simmr/vignettes/simmr.html
3) From Andrew Parnell's GitHub for simmr (https://github.com/andrewcparnell/simmr/tree/master/R). The functionality is the same, but I have altered some of the ggplot code to change the output visualization.

Much of it has been modified for our own purposes, but we acknowledge and thank these sources for their open-source code.
The script does not produce every figure and table in the order they appear in the manuscript, but rather follows our logic for data analysis. However, each product's label corresponds with the manuscript.

# File descriptions:

HS_isotope_data.csv is a file containing all stable isotope data for Hanna Shoal study.

source_averages.csv contains the end-members for the stable isotope mixing model.

corrections.csv contains the trophic enrichment factors for each end-member.

corrections_higherTL.csv contains the trophic enrichment factors for each end-member for higher trophic level organisms.
