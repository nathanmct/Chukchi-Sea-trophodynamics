#Supplemental Information for McTigue & Dunton (2017) Trophodynamics of the Hanna Shoal Ecosystem (Chukchi Sea, Alaska): connecting multiple end-members to a rich food web. Deep-Sea Research II: Topical Studies in Oceanography.

#Some of the below script has been borrowed from two other sources: 
#1) Passoti et al. (2015) Benthic trophic interactions in an Antarctic shallow water ecosystem affected by recent glacial retreat. PLoS ONE 10(11):e0141742. doi:10.1371/journal.pone.0141742
#2) The vignette for the package simmr available at https://cran.r-project.org/web/packages/simmr/vignettes/simmr.html
#3) From Andrew Parnell's GitHub for simmr (https://github.com/andrewcparnell/simmr/tree/master/R). The functionality is the same, but I have altered some of the ggplot code to change the output visualization.

#Much of it has been modified for our own purposes, but we acknowledge and thank these sources for their open-source code.
#The script does not produce every figure and table in the order they appear in the manuscript, but rather follows our logic for data analysis. However, each product's label corresponds with the manuscript.

#JAGS must be installed on your computer to run some of these scripts. Install it here: https://sourceforge.net/projects/mcmc-jags/files/
#It does not need to be open when running R

#For the publication, I used the package 'egg' to grid panels together. Since then it appears 'egg' has not been updated or possibly is no longer avialable. I have commented out the code that uses 'egg' and instead left the code so that every single panel individually will save into the working directory.

rm(list=ls())
dev.off()

#set to your own wd
setwd("P://My Documents//Hanna Shoal food webs//R data analysis")

######################################################
#Be sure helper functions at put in working directory#
######################################################

#These are from Pasotti et al. (2015)
source("SEA_fitting.r")
source("BUGS_models.r")
source("comp_SEAb.r")
source("extract_SEAc.r")

#These have been modified from Andrew Parnell's GitHub for simmr (https://github.com/andrewcparnell/simmr/tree/master/R). The functionality is the same, but I have altered some of the ggplot code to change the output visualization.
source("susp_compare_groups.r")
source("depo_compare_groups.r")
source("pred_compare_groups.r")

#used for graphing, from the 'egg' package
source("gtable_frame.r")

#install.packages(c("siar", "simmr", "SIBER", "plyr", "R2jags", "pander", "reshape2", "mcmcplots", "tidyverse", "xlsx", "grid", "gridExtra", "gtable"))

#Load necessary packages
library(siar) #simmr and SIBER dependency
library(simmr) #stable isotope mixing models in R, used for dietary proportions
library(SIBER) #stable isotope bayesian ellipses in R, used for SEA calculation
library(plyr) #some code is antiquated and still uses plyr
library(R2jags) 
library(rjags)
library(pander)
library(reshape2)
library(mcmcplots)
library(tidyverse)
library(xlsx) #creates .xlsx files for table output
library(grid) #used in graphing
library(gridExtra) #used in graphing
library(gtable) #used in graphing

#I used the package 'egg' for some plotting, but it may no longer be available.
#library(devtools)
#devtools::install_github("baptiste/egg")
#library(egg)

#we used this color palette, modified from http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbpalette <- (c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#999999", "#F781BF", "#32CD32", "#00008B", "#A65628", "#FFD900", "#8B6969"))

##################################

#import data with a priori trophic guilds assigned
iso <- read.csv("HS_isotope_data.csv") #import as sorted by species name alphabetically

Species.count <- iso %>% dplyr::count(Species)
iso <- iso %>% dplyr::mutate(idcount = (rep(Species.count$n, Species.count$n))) #determine n value

#########
#Table 2#
#########

iso.table <- dplyr::group_by(iso, Species) %>%
  summarise(avg.d15N = mean(d15N),
            sd.d15N = sd(d15N),
            avg.d13C = mean(d13C),
            sd.13C = sd(d13C),
            n = max(idcount)
  )

write.xlsx(iso.table, file="Table 2.xlsx")

##########
#Figure 2#
##########

#Visualize distribution of SI data with Jitterplots for d13C and d15N
d13C.jitter <- ggplot(data=iso, aes(x=Trophic.Descriptor, y=d13C, color=Trophic.Descriptor))+
  geom_jitter(size=2, alpha=0.6)+
  scale_color_manual(values = cbpalette)+
  ylab(expression(paste(delta^{13},'C (\u2030)')))+
  xlab("")+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=18),
        legend.position="none",
        #legend.text = element_text(size=20),
        #legend.title = element_text(size=20),
        panel.grid.major = element_blank(),#removes panel grids
        panel.grid.minor = element_blank())+
  #axis.text.x = element_text(angle=45, hjust=1))+
  coord_flip()+
  scale_x_discrete(limits = c("end-member", "suspension feeder", "surface deposit feeder", "subsurface deposit feeder", "scavenger/omnivore", "predator"))+
                   #labels = rev(levels(iso$Trophic.Descriptor)))+
  scale_y_continuous(breaks = seq(-26, -14, 4))+
  annotate("text", x = 6.2, y = -26, label = "a", size = 8)

d13C.jitter

#ggsave(filename="Figure_2a.png", plot = d13C.jitter, device = "png", width = 8, height = 5, units = "in", dpi=330)

d15N.jitter <- ggplot(data=iso, aes(x=Trophic.Descriptor, y=d15N, color=Trophic.Descriptor))+
  geom_jitter(size=2, alpha=0.6)+
  scale_color_manual(values = cbpalette)+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab("")+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=18),
        legend.position="none",
        #legend.text = element_text(size=20),
        #legend.title = element_text(size=20),
        panel.grid.major = element_blank(),#removes panel grids
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank())+
  coord_flip()+
  scale_x_discrete(limits = c("end-member", "suspension feeder", "surface deposit feeder", "subsurface deposit feeder", "scavenger/omnivore", "predator"))+
  scale_y_continuous(breaks = seq(6, 18, 4))+
  annotate("text", x = 6.2, y = 6, label = "b", size = 8)

d15N.jitter

#ggsave(filename = "Figure 2b.png", plot = d15N.jitter, device = "png", width = 4, height = 5, units = "in", dpi=330)

#Grid previous two plots together for plotting together
g1 <- ggplotGrob(d13C.jitter)
g2 <- ggplotGrob(d15N.jitter)

source("gtable_frame.r")
fg1 <- gtable_frame(g1)
fg2 <- gtable_frame(g2)

grid.newpage()
combined <- cbind(fg1, fg2)
grid.draw(combined)

#Make Figure 2 with d15N x-axis off
#library(egg)
#grid.newpage()
#grid.draw(ggarrange(d13C.jitter, d15N.jitter, ncol = 2))

#To save gridded figure as high resolution png
#First, open device
tiff("P:/My Documents/Hanna Shoal food webs/R data analysis/Figure 2.tiff", height = 4, width = 10, units="in", res=600)

## Create a graphical object here
grid.draw(combined) # print it

## Stop writing to the PDF file
dev.off()


#######################
#Supplemental Figure 1#
#######################

iso.consumers <- dplyr::filter(iso, Trophic.Descriptor!="end-member")

ggplot(data = iso.consumers, aes(x = d13C, y = d15N, color = Trophic.Descriptor))+
  geom_point(size = 3)+
  scale_color_manual(values = cbpalette,
                     name = "Trophic Guild")+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(axis.text = element_text(size=14, color = "black"),
        legend.text = element_text(size=16),
        legend.title = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.background = element_rect(color="black"),
        panel.grid.major = element_blank(),#removes panel grids
        panel.grid.minor = element_blank(),
        axis.title = element_text(size=20),
        strip.text = element_text(size=18)
  )+
  scale_y_continuous(breaks = seq(10, 18, 4))+
  facet_wrap(~Station, ncol = 10)

ggsave("Supplemental_Figure_1.tiff", plot = last_plot(), device = "tiff", width = 13, height = 8, units = "in", dpi = 300)

ggsave("Supplemental_Figure_1.png", plot = last_plot(), device = "png", width = 13, height = 8, units = "in", dpi = 600)


#Does not appear in manuscript, but helpful for data visualization
ggplot(data = iso.consumers, aes(x = d13C, y = d15N))+
  geom_point(size = 2)+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(text = element_text(size = 18),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 7)
  )+
  scale_y_continuous(breaks = seq(9, 18, 3))+
  facet_wrap(~Species)

###########
#Run SIBER#
###########

#Parameters and priors for SIBER model
parms <- list()
parms$n.iter <- 2 * 10^5
parms$n.burning <- 1 * 10^4
parms$n.thin <- 10
parms$n.chains <- 2

priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

#Run SIBER for all species in dataset, subset by trophic guild
SEA.Jags.Trophic <- with(iso, siber.ellipses.test(d13C, d15N, Trophic.Descriptor, method = "IWJAGS", parms, priors))

#Find standard ellipse paths for Trophic guilds
ell.Trophic <-dlply(iso,.(Trophic.Descriptor),function(x) standard.ellipse(x$d13C,x$d15N,steps = 1))

e2.Trophic <- ldply(ell.Trophic, extract_SEAc) #this is object read by ggplot for SEA paths
names(e2.Trophic)[2:4] <-c("SEAc", "xSEAc", "ySEAc")

##########
#Figure 3#
##########

#plot ALL species, but only Trophic Guild SEAs
ggplot(data = iso, aes(x = d13C, y = d15N))+
  geom_point(size = 1.8, alpha = 0.6, aes(color = Trophic.Descriptor))+
  geom_path(data = e2.Trophic, aes(x = xSEAc, y = ySEAc, color = Trophic.Descriptor), linetype = 1, size = 2, alpha = 1)+
  scale_color_manual(values = cbpalette,
                     name = "Trophic Guild",
                     guide = guide_legend(override.aes = list(shape = NA)))+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(text = element_text(size = 12),
        axis.text = element_text(size = 12, color="black"),
        axis.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  coord_fixed(ratio = 1)+
  scale_y_continuous(limits = c(5,20))+
  scale_x_continuous(limits=c(-27, -13))

ggsave(filename = "Figure 3.tiff", plot = last_plot(), device = "tiff", width = 7, height = 5, units = "in", dpi=600)

#########
#Table 4#
#########

#compare probability that g1 > g2 in isospace
comp_SEAb(SEA.Jags.Trophic$SEA.B, iso$Trophic.Descriptor)

#make table with statistics for trophic guilds
met2 <- data.frame(SEA.Jags.Trophic$SEA)
names(met2) < -levels(iso$Trophic.Descriptor)
met2 <- melt(met2) #throws warning about no id variables; proceed anyway
names(met2) <- c("Trophic.Descriptor", "value")
hh <- function(x){
  h <- hdr(x$value,h = bw.nrd0(x$value))
  data.frame(mean = mean(x$value), sd = sd(x$value), mode = h$mode, lo95 = h$hdr[2,1],hi95 = h$hdr[2,2])
}
tSEAb <- ddply(met2,.(Trophic.Descriptor), hh)

tSEAb <- cbind(tSEAb, ddply(e2.Trophic,.(Trophic.Descriptor), summarize, unique(SEAc))[,2])
names(tSEAb)[7] <- "SEAc"

knitr::kable(tSEAb)

write.xlsx(tSEAb, file = "Table_3.xlsx")

#Make table of overlapping SEAc isospace
s <- as.character(levels(iso$Trophic.Descriptor))

ovrlpSEAc <- function(x) {
  ov1 <- iso[iso$Trophic.Descriptor==s[x[1]],]
  ov2 <- iso[iso$Trophic.Descriptor==s[x[2]],]
  data.frame(g1=s[x[1]], g2=s[x[2]],overlap(ov1$d13C,ov1$d15N,ov2$d13C,ov2$d15N,steps=1))
}

comp1 <- do.call(rbind, apply(combn(seq_along(s), 2), 2, ovrlpSEAc))

knitr::kable(comp1)

write.xlsx(comp1, file="Table_4_overlap.xlsx")

######################################################################################
#Need similar table with statistics for all species that will be analyzed (n > 9)
#Will be Table 5
#Re-do above procedure, but now: 
#(1) subset n > 9, and  (2) do not divide by trophic descriptor
######################################################################################

iso<-dplyr::filter(iso, idcount > 9) %>% droplevels()

SEA.Jags.iso <- with(iso, siber.ellipses.test(d13C, d15N, Species, method = "IWJAGS", parms, priors))

#Find standard ellipse paths for all trophic guilds and save as Excel sheet
ell.iso <-dlply(iso,.(Species), function(x) standard.ellipse(x$d13C, x$d15N, steps=1))

extract_SEAc <- function(e) {
  data.frame(e$SEAc,e$xSEAc, e$ySEAc)
}

e2.iso <- ldply(ell.iso, extract_SEAc)
names(e2.iso)[2:4] <-c("SEAc", "xSEAc", "ySEAc")

met2 <- data.frame(SEA.Jags.iso$SEA)
names(met2) <- levels(iso$Species)
met2 <- melt(met2)
names(met2) <- c("Species", "value")
hh <- function(x){
  h <- hdr(x$value,h = bw.nrd0(x$value))
  data.frame(mean = mean(x$value), sd=sd(x$value), mode = h$mode,lo95 = h$hdr[2,1], hi95 = h$hdr[2,2])
}
tSEAb <- ddply(met2,.(Species), hh)

tSEAb <- cbind(tSEAb, ddply(e2.iso,.(Species), summarize, unique(SEAc))[,2])
names(tSEAb)[7]<-"SEAc"

knitr::kable(tSEAb)

write.xlsx(tSEAb, file = "Table 5.xlsx")

#Since [pred & scav] and [SSDF & surface deposit] are overlapping, combine for graphing in Figure 4.

susp <- dplyr::filter(iso, Trophic.Descriptor=="suspension feeder")
susp$Species <- factor(susp$Species)

pred <- dplyr::filter(iso, Trophic.Descriptor=="predator" | Trophic.Descriptor=="scavenger/omnivore")
pred$Species <- factor(pred$Species)

depo <- dplyr::filter(iso, Trophic.Descriptor=="surface deposit feeder" | Trophic.Descriptor=="subsurface deposit feeder")
depo$Species <- factor(depo$Species)

####################
#SUSPENSION FEEDERS#
####################
#Figure 4a
ell.susp <- dlply(susp,.(Species), function(x) standard.ellipse(x$d13C, x$d15N, steps=1))

e2.susp <- ldply(ell.susp, extract_SEAc)
names(e2.susp)[2:4] <-c("SEAc", "xSEAc", "ySEAc")

#plot suspension feeders and SEAs
susp.SEA <- ggplot(data = susp, aes(x = d13C, y = d15N))+
  geom_path(data = e2.susp, aes(x = xSEAc, y = ySEAc, color = Species), linetype = 1, size = 2)+
  scale_color_manual(values = cbpalette,
                     name = "Suspension Feeders")+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  #xlab(expression(paste(delta^{13},'C (\u2030)')))+ #turned off for gridding
  xlab(expression(""))+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text = element_text(size=16, color="black"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),#removes panel grids
        panel.grid.minor = element_blank())+
  coord_fixed(ratio = 1)+
  scale_y_continuous(limits = c(8, 18), breaks = seq(8, 18, 2))+
  scale_x_continuous(limits = c(-26, -14))+
  annotate("text", x = -25.5, y = 17.5, label = "a", size = 7)

susp.SEA

ggsave(filename = "Figure 4a.tiff", plot = susp.SEA, device = "tiff", width = 7, height = 6, units = "in", dpi=600)


#########################
#DEPOSIT FEEDERS#
#########################
#Figure 4b
ell.depo <- dlply(depo,.(Species), function(x) standard.ellipse(x$d13C, x$d15N, steps=1))

extract_SEAc <- function(e) {
  data.frame(e$SEAc,e$xSEAc, e$ySEAc)
}

e2.depo <- ldply(ell.depo, extract_SEAc) #this is object fed into ggplot
names(e2.depo)[2:4] <-c("SEAc","xSEAc","ySEAc")

#plot deposit feeders and SEAs
depo.SEA <- ggplot(data = depo, aes(x = d13C, y = d15N))+
  geom_path(data = e2.depo, aes(x = xSEAc, y = ySEAc, color = Species), linetype = 1, size = 2)+
  scale_color_manual(values = cbpalette, 
                     name="Deposit Feeders")+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  #xlab(expression(paste(delta^{13},'C (\u2030)')))+ #turned off for gridding
  xlab(expression(""))+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text = element_text(size=16, color="black"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major=element_blank(),#removes panel grids
        panel.grid.minor=element_blank())+
  coord_fixed(ratio=1)+
  scale_y_continuous(limits=c(8, 18), breaks=seq(8, 18, 2))+
  scale_x_continuous(limits=c(-26, -14))+
  annotate("text", x=-25.5, y=17.5, label="b", size=7)

depo.SEA

ggsave(filename="Figure 4b.tiff", plot = depo.SEA, device = "tiff", width = 7, height = 5, units = "in", dpi=600)


###########
#PREDATORS#
###########
#Figure 4c

ell.pred <- dlply(pred,.(Species), function(x) standard.ellipse(x$d13C, x$d15N, steps=1))

extract_SEAc <- function(e) {
  data.frame(e$SEAc,e$xSEAc, e$ySEAc)
}

e2.pred <- ldply(ell.pred, extract_SEAc)
names(e2.pred)[2:4] <-c("SEAc","xSEAc","ySEAc")

#plot predators and  scavenger feeders
pred.SEA <- ggplot(data = pred, aes(x = d13C, y = d15N))+
  geom_path(data = e2.pred, aes(x = xSEAc, y = ySEAc, color = Species), linetype = 1, size = 2)+
  scale_color_manual(values = cbpalette,
                     name = "Predator/Scavengers")+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(text = element_text(size=16),
        axis.text = element_text(size=16, color="black"),
        legend.text = element_text(size=14),
        legend.title = element_text(size=16),
        panel.grid.major = element_blank(),#removes panel grids
        panel.grid.minor = element_blank())+
  coord_fixed(ratio = 1)+
  scale_y_continuous(limits = c(8, 18), breaks = seq(8, 18, 2))+
  scale_x_continuous(limits = c(-26, -14))+
  annotate("text", x = -25.5, y = 17.5, label = "c", size = 7)

pred.SEA

ggsave(filename = "Figure 4c.tiff", plot = pred.SEA, device = "tiff", width = 7, height = 5, units = "in", dpi=600)


susp.g <- ggplotGrob(susp.SEA)
depo.g <- ggplotGrob(depo.SEA)
pred.g <- ggplotGrob(pred.SEA)

fg3 <- gtable_frame(susp.g)
fg4 <- gtable_frame(depo.g)
fg5 <- gtable_frame(pred.g)

grid.newpage()
combined.Fig4 <- rbind(fg3, fg4, fg5)
grid.draw(combined.Fig4)

#Make Figure 4 with 'egg'
library(egg)
grid.newpage()
grid.draw(ggarrange(susp.SEA, depo.SEA, pred.SEA, ncol = 1))

#To save gridded figure as high resolution png
#First, open device
tiff("P:/My Documents/Hanna Shoal food webs/R data analysis/Figure 4.tiff", height = 10, width = 8, units="in", res=600)

## Create a graphical object g here
#grid.draw(ggarrange(susp_SEA, depo_SEA, pred_SEA, ncol = 1)) #nice function from 'egg' library, if available. Otherwise, have to use gtable. Works the same but doesn't align legends

grid.draw(combined.Fig4)

## Stop writing to the PDF file
dev.off()


###########
#Run simmr#
###########

#simmr requires n>4. Since we already filtered for n>9 for SIBER but simply overwrote the 'iso' object, we'll have to reload the original dataset and overwrite 'iso' again.

iso <- read.csv("HS_isotope_data.csv") #import as sorted by species name alphabetically

Species.count <- iso %>% dplyr::count(Species)
iso <- iso %>% dplyr::mutate(idcount = (rep(Species.count$n, Species.count$n)))
simmriso <- dplyr::filter(iso, idcount > 4) %>% droplevels()

susp.simmr <- dplyr::filter(simmriso, Trophic.Descriptor=="suspension feeder")
susp.simmr <- droplevels(susp.simmr)

pred.simmr <- dplyr::filter(simmriso, Trophic.Descriptor=="predator" | Trophic.Descriptor=="scavenger/omnivore")
pred.simmr <- droplevels(pred.simmr)

depo.simmr <- dplyr::filter(simmriso, Trophic.Descriptor=="surface deposit feeder" | Trophic.Descriptor=="subsurface deposit feeder")
depo.simmr <- droplevels(depo.simmr)

#simmr requires a column called "Code" for operation
#counts the number of each genus. Will be used in rep() as the number of times to repeat the code.
Species.count.susp <- susp.simmr %>% dplyr::count(Species) 
susp <- susp.simmr %>% droplevels() %>% dplyr::mutate(Code = (rep(1:length(unique(Species)), Species.count.susp$n))) #use mutate() to add a column called 'Code'

Species.count.depo <- depo.simmr %>% dplyr::count(Species)
depo <- depo.simmr %>% droplevels() %>%
  dplyr::mutate(Code = (rep(1:length(unique(Species)), Species.count.depo$n)))

Species.count.pred <- pred.simmr %>% dplyr::count(Species)
pred <- pred.simmr %>% droplevels() %>%
  dplyr::mutate(Code = (rep(1:length(unique(Species)), Species.count.pred$n)))

#Import other csv files for simmr. These should be altered for each specific study.
corrections <- read.csv("corrections.csv", header=T) #trophic enrichment factors
corr.higherTL <- read.csv("corrections_higherTL.csv", header=T) #trophic enrichment factors for higher trophic level organisms
sources <- read.csv("source_averages.csv",header=T) #end-member means and standard deviations

#preen data for use with simmr
mix.susp <- as.matrix.data.frame(susp[,c(10:9)], ncol=2, nrow=length(susp$d13C))
mix.depo <- as.matrix.data.frame(depo[,c(10:9)], ncol=2, nrow=length(depo$d13C))
mix.pred <- as.matrix.data.frame(pred[,c(10:9)], ncol=2, nrow=length(pred$d13C))
s_names <- levels(sources$Source)
s_means <- as.matrix.data.frame(sources[,c(2,4)], ncol=2, nrow=3)
s_sds <- as.matrix.data.frame(sources[,c(3,5)], ncol=2, nrow=3)
c_means <- as.matrix.data.frame(corrections[,c(2,4)], ncol=2, nrow=3)
c_sds <- as.matrix.data.frame(corrections[,c(3,5)], ncol=2, nrow=3)
c_means_HTL <- as.matrix.data.frame(corr.higherTL[,c(2,4)], ncol=2, nrow=3) #use HTL (higher trophic level) notation for pred group correction (i.e., TEF is ...)
c_sds_HTL <- as.matrix.data.frame(corr.higherTL[,c(3,5)], ncol=2, nrow=3)
grp.susp <- as.integer(as.matrix(susp[,13]))
grp.depo <- as.integer(as.matrix(depo[,13]))
grp.pred <- as.integer(as.matrix(pred[,13]))


#load data so that simmr can use it

simmr_groups_susp = simmr_load(mixtures = mix.susp,
                               source_names = s_names,
                               source_means = s_means,
                               source_sds = s_sds,
                               correction_means = c_means,
                               correction_sds = c_sds,
                               group = grp.susp)

simmr_groups_depo = simmr_load(mixtures = mix.depo,
                               source_names = s_names,
                               source_means = s_means,
                               source_sds = s_sds,
                               correction_means = c_means,
                               correction_sds = c_sds,
                               group = grp.depo)

simmr_groups_pred = simmr_load(mixtures = mix.pred,
                               source_names = s_names,
                               source_means = s_means,
                               source_sds = s_sds,
                               correction_means = c_means_HTL,
                               correction_sds = c_sds_HTL,
                               group = grp.pred)


#run MCMC for each group
source("simmrmcmc.r")
susp_out = simmr_mcmc(simmr_groups_susp)
depo_out = simmr_mcmc(simmr_groups_depo)
pred_out = simmr_mcmc(simmr_groups_pred)

######################
#Supplemental Table 2#
######################

#obtain 95% credible intervals...
susp.summ <- summary.simmr_output(susp_out, type=c("quantiles", "statistics"), group=c(1:max(susp$Code)))
write.xlsx(susp.summ, "Supp Table 2_susp_summary.xlsx")

depo.summ <- summary.simmr_output(depo_out, type=c("quantiles", "statistics"), group=c(1:max(depo$Code)))
write.xlsx(depo.summ, "Supp Table 2_depo_summary.xlsx")

pred.summ <- summary.simmr_output(pred_out, type=c("quantiles", "statistics"), group=c(1:max(pred$Code)))
write.xlsx(pred.summ, "Supp Table 2_pred_summary.xlsx")

#Visually assess convergence of chains and make note of Gelman diagnostics

plot(susp_out, type = 'convergence')
plot(depo_out, type = 'convergence')
plot(pred_out, type = 'convergence')
summary(susp_out, type = "diagnostics")
summary(depo_out, type = "diagnostics")
summary(pred_out, type = "diagnostics")

######################
#Supplemental Table 1#
######################

#end-member discrimination
#There is probably an elegant solution to extract all this information from 'xxxx_out' output lists. For time sake, I printed all the graphs for each species and manually recorded correlations.
#will iteratively print max(xxxx$Code) graphs. Use the 'back' arrow in RStudio plot viewer to see them all.
plot(susp_out, type = 'matrix', group = 1:max(susp$Code))
plot(depo_out, type = 'matrix', group = 1:max(depo$Code))
plot(pred_out, type = 'matrix', group = 1:max(pred$Code))

##########
#Figure 6#
##########

#plot group comparisons as boxplots. uses simmr output.
#Graph aesthetics can be edited from referenced graphing scripts.
#Can't figure out a simple way to grid these nine graphs, so I did it manually.
#Predator/Scavenger plot script has x-axis label turned on; it is muted in other plots

source("susp_compare_groups.r")
susp_compare_groups(susp_out, source='MPB', groups=1:max(grp.susp))
ggsave("susp_MPB.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("susp_MPB.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

susp_compare_groups(susp_out, source='Ice algae', groups=1:max(grp.susp))
ggsave("susp_IceAlgae.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("susp_IceAlgae.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

susp_compare_groups(susp_out, source='PSOM', groups=1:max(grp.susp))
ggsave("susp_PSOM.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("susp_PSOM.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

###########################

source("depo_compare_groups.r")
depo_compare_groups(depo_out, source='MPB', groups=1:max(grp.depo))
ggsave("depo_MPB.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("depo_MPB.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

depo_compare_groups(depo_out, source='Ice algae', groups=1:max(grp.depo))
ggsave("depo_IceAlgae.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("depo_IceAlgae.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

depo_compare_groups(depo_out, source='PSOM', groups=1:max(grp.depo))
ggsave("depo_PSOM.png", plot = last_plot(), device = "png", width = 3.75, height = 2.5, dpi = 600)
ggsave("depo_PSOM.eps", plot = last_plot(), device = "eps", width = 3.75, height = 2.5)

#############################
source("pred_compare_groups.r")
pred_compare_groups(pred_out, source="MPB", groups=1:max(grp.pred))
ggsave("pred_MPB.png", plot = last_plot(), device = "png", width = 3.875, height = 4, dpi = 600)
ggsave("pred_MPB.eps", plot = last_plot(), device = "eps", width = 3.875, height = 4)

pred_compare_groups(pred_out, source="Ice algae", groups=1:max(grp.pred))
ggsave("pred_IceAlgae.png", plot = last_plot(), device = "png", width = 3.875, height = 4, dpi = 600)
ggsave("pred_IceAlgae.eps", plot = last_plot(), device = "eps", width = 3.875, height = 4)

pred_compare_groups(pred_out, source="PSOM", groups=1:max(grp.pred))
ggsave("pred_PSOM.png", plot = last_plot(), device = "png", width = 3.875, height = 4, dpi = 600)
ggsave("pred_PSOM.eps", plot = last_plot(), device = "eps", width = 3.875, height = 4)


##########
#Figure 5#
##########

#Stable isotope biplots below do not incorporate simmr output. They simply are visualize the raw data distributions with the end-member polygons.
#Species facet solves the problem of congested plots with lots of colors
#End-member polygons are mean +/- SD, then + TEF min and max
#Visualizes the "geometric" TEF correction that is used in simmr

#Figure 5a
ggplot(data = susp, aes(x = d13C, y = d15N))+
  annotate("rect", xmin = -21.3, xmax = -18.9, ymin = 11.3, ymax = 13.9, alpha = 0.2, color = "black", fill = "skyblue")+
  annotate("text", x = -20.1, y = 13, label = "Ice \n Algae", size = 4)+
  annotate("rect", xmin = -17.5, xmax = -14.7, ymin = 7.8, ymax = 12.0, alpha = 0.2, color = "black", fill = "grey")+
  annotate("text", x = -16.2, y = 8.5, label = "MPB", size = 4)+
  annotate("rect", xmin = -23.5, xmax = -21.1, ymin = 9, ymax = 13, alpha = 0.2, color = "black", fill = "forestgreen")+
  annotate("text", x = -22.3, y = 9.4, label = "PSOM", size = 4)+
  geom_point(size = 3)+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 14)
  )+
  coord_fixed(ratio = 1)+
  scale_y_continuous(breaks = seq(8, 16, 2))+
  scale_x_continuous(breaks = seq(-24, -16, 4))+
  facet_wrap(~Species, ncol = 3)

ggsave("Figure 5a.png", plot = last_plot(), device = "png", width = 10, height = 10, units = "in", dpi = 330)

ggsave("Figure 5a.tiff", plot = last_plot(), device = "tiff", width = 10, height = 10, units = "in", dpi = 330)

#Figure 5b
ggplot(data = depo, aes(x = d13C, y = d15N))+
  annotate("rect", xmin = -21.3, xmax = -18.9, ymin = 11.3, ymax = 13.9, alpha = 0.2, color = "black", fill = "skyblue")+
  annotate("text", x = -20.1, y = 13, label = "Ice \n Algae", size = 4)+
  annotate("rect", xmin = -17.5, xmax = -14.7, ymin = 7.8, ymax = 12.0, alpha = 0.2, color = "black", fill = "grey")+
  annotate("text", x = -16.2, y = 8.5, label = "MPB", size = 4)+
  annotate("rect", xmin = -23.5, xmax = -21.1, ymin = 9, ymax = 13, alpha = 0.2, color = "black", fill = "forestgreen")+
  annotate("text", x = -22.3, y = 9.4, label = "PSOM", size = 4)+
  geom_point(size = 3)+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 14)
  )+
  coord_fixed(ratio = 1)+
  scale_y_continuous(breaks = seq(8, 16, 2))+
  scale_x_continuous(breaks = seq(-24, -16, 4))+
  facet_wrap(~Species)

ggsave("Figure 5b.png", plot = last_plot(), device = "png", width = 10, height = 10, units = "in", dpi = 330)

ggsave("Figure 5b.tiff", plot = last_plot(), device = "tiff", width = 10, height = 10, units = "in", dpi = 330)

#Figure 5c
#NB. For preds, the end-member polygons are larger to incorporate the propagated TEFs for higher trophic levels
ggplot(data = pred, aes(x = d13C, y = d15N))+
  annotate("rect", 
           xmin = -21.0, xmax = -18.2, 
           ymin = 13.2, ymax = 18, 
           alpha = 0.2, color = "black", fill = "skyblue"
  )+
  annotate("text", x = -19.6, y = 17, label = "Ice \n Algae", size = 4)+
  annotate("rect", xmin = -17.2, xmax = -14, ymin = 9.7, ymax = 16.1, 
           alpha = 0.2, color = "black", fill = "grey")+
  annotate("text", x = -15.9, y = 10.5, label = "MPB", size = 4)+
  annotate("rect", xmin = -23.2, xmax = -20.3, ymin = 10.9, ymax = 17.1, 
           alpha = 0.2, color = "black", fill = "forestgreen")+
  annotate("text", x = -21.8, y = 11.5, label = "PSOM", size = 4)+
  geom_point(size = 2)+
  ylab(expression(paste(delta^{15},'N (\u2030)')))+
  xlab(expression(paste(delta^{13},'C (\u2030)')))+
  theme_bw()+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14, color = "black"),
        panel.grid.minor = element_blank(),
        strip.text.x = element_text(size = 14)
  )+
  scale_x_continuous(breaks = seq(-24, -16, 4))+
  coord_fixed(ratio = 1)+
  facet_wrap(~Species)

ggsave("Figure 5c.png", plot = last_plot(), device = "png", width = 10, height = 10, units = "in", dpi = 600)

ggsave("Figure 5c.tiff", plot = last_plot(), device = "tiff", width = 10, height = 10, units = "in", dpi = 330)
 
