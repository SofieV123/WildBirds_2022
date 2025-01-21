#NMDS Wild Birds 2023
library(cluster)
library(dplyr)
library(ggplot2)
library(indicspecies)
library(tidyr)
library(vegan)

#Bird NMDS #################################################################
birds = read.table("./bird_nmds.txt", sep = "\t", header = T)  

birds$Sum = rowSums(birds[2:48])
birds = birds[birds$Sum > 0,]
birds$Sum = NULL
birds = birds[-29,]

bmat = as.data.frame(lapply(birds[,2:48], as.numeric))
bmat = as.matrix(bmat)

set.seed(192)
bnmds = metaMDS(bmat, distance = "bray", trymax = 250, k = 2)
stressplot(bnmds)

#Indicator Species Stuff #################################################
farms_clus = as.data.frame(birds$Farm)
names(farms_clus)[names(farms_clus) == "birds$Farm"] = "Farm"
farms_clus = merge(farms_clus, farms_sub, by = "Farm")
farms_clus = farms_clus$Cluster

birds_indic_clus = multipatt(birds[,-1], farms_clus, func="IndVal.g", 
                        control = how(nperm = 9999))
summary(birds_indic_clus, alpha = 1)

farms_eco = farms_ano[,14]

birds_indic_eco = multipatt(birds[,-1], farms_eco, func="IndVal.g", 
                        control = how(nperm = 9999))
summary(birds_indic_eco, alpha = 1)

#NMDS Final ##############################################################
farm_coords = as.data.frame(scores(bnmds)$sites)
names(farm_coords)[c(1,2)] = c("x", "y")
farm_coords$z = NA
farm_coords$Farm = as.factor(birds$Farm)
farm_coords = merge(farm_coords, farms_sub, by = "Farm")

bird_specs = as.data.frame(scores(bnmds)$species)
names(bird_specs)[c(1,2)] = c("x", "y")
bird_specs$z = NA
bird_specs$Code = row.names(bird_specs)

poop = read.table("./poopy_birds.txt", sep = "\t", header = T)
bird_specs = merge(bird_specs, poop, by = "Code", all.x = T)
bird_specs$Field = NULL
bird_specs = bird_specs %>%
  mutate(Feces = ifelse(
    Feces != "Y", "N", "Y"))
bird_specs$Feces[is.na(bird_specs$Feces)] = "N"

lc = read.table("./land_cover.txt", sep = "\t", header = T)
lc = lc[-c(11,22,41),]
wet_prop = ordisurf(bnmds ~ lc$Wet, plot = F)
extract.xyz <- function(obj) {
  xy <- expand.grid(x = obj$grid$x, y = obj$grid$y)
  xyz <- cbind(xy, c(obj$grid$z))
  names(xyz) <- c("x", "y", "z")
  return(xyz)
}
wet_prop_vals <- extract.xyz(obj = wet_prop)

#Figure 2 ############################################################
ggplot(data = farm_coords, aes(x = x, y = y)) + 
  labs(x = "NMDS1", y = "NMDS2") +
  geom_point(aes(color = EcoReg), size = 3, alpha = 0.7) +
  stat_ellipse(data = farm_coords, 
               aes(group = EcoReg, color = EcoReg, linetype = EcoReg), 
               linewidth = 1.5) +
  scale_color_manual(name = "Ecoregion",
                     labels = c("SE Coastal Plains", "Appalachian Mtns",
                                     "Piedmont"),
                       values = c("#D55E00", "#0072B2", "#009E73")) +
  scale_linetype_manual(name = "Ecoregion",
                          labels = c("SE Coastal Plains", "Appalachian Mtns",
                                  "Piedmont"),
                          values = c("dashed", "dotted", "solid")) +
  geom_text(data = bird_specs[bird_specs$Feces == "Y",], aes(label = Code), 
            size = 3, color = "red") +
  geom_text(data = bird_specs[bird_specs$Feces == "N",], aes(label = Code), 
            size = 3, color = "gray20") +
  labs(color = "Ecoregion", linetype = "Ecoregion") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
        legend.key = element_blank()) +
  annotate("text", x = -2.15, y = c(1.15, 1.30), size = 5,
           label = c("stress == 0.19", "r ^ 2 == 0.96"),
           parse = T)

#ANOSIM and Mantel Stuff #################################################
farms_ano = farms[-c(2,30,35),]
farms_ano = farms_ano %>%
  mutate(SizeF = ifelse(Size > 35, "2", "1"))

ano_eco.size = anosim(bmat, farms_ano$EcoReg, distance = "bray", permutations = 999,
                 strata = farms_ano$SizeF)
ano_eco.live = anosim(bmat, farms_ano$EcoReg, distance = "bray", permutations = 999,
                      strata = farms_ano$Live)

bird_man = birds[,2:48]
dist.abund = vegdist(bird_man, method = "bray")

lc = read.table("./land_cover.txt", sep = "\t", header = T)
birds_lc = merge(birds, lc, by = "Farm", all.x = T)
birds_lc = merge(birds_lc, farms_ano[,c(1:13,36)], by = "Farm", all.x = T)

dev.lc = birds_lc$Develop
dev_dist.lc = vegdist(dev.lc, method = "euclidean")
man_dev.sz = mantel(dist.abund, dev_dist.lc, method = "spearman", permutations = 999,
                 strata = birds_lc$SixeF)
man_dev.lv = mantel(dist.abund, dev_dist.lc, method = "spearman", permutations = 999,
                 strata = birds_lc$Live)

nat.lc = birds_lc$Nat
nat_dist.lc = vegdist(nat.lc, method = "euclidean")
man_nat.sz = mantel(dist.abund, nat_dist.lc, method = "spearman", permutations = 999,
                 strata = birds_lc$SizeF)
man_nat.lv = mantel(dist.abund, nat_dist.lc, method = "spearman", permutations = 999,
                 strata = birds_lc$Live)

agr.lc = birds_lc$Ag
agr_dist.lc = vegdist(agr.lc, method = "euclidean")
man_agr.sz = mantel(dist.abund, agr_dist.lc, method = "spearman", permutations = 999,
                    strata = birds_lc$SizeF)
man_agr.lv = mantel(dist.abund, agr_dist.lc, method = "spearman", permutations = 999,
                    strata = birds_lc$Live)

wet.lc = birds_lc$Wet
wet_dist.lc = vegdist(wet.lc, method = "euclidean")
man_wet.sz = mantel(dist.abund, wet_dist.lc, method = "spearman", permutations = 999,
                    strata = birds_lc$SizeF)
man_wet.lv = mantel(dist.abund, wet_dist.lc, method = "spearman", permutations = 999,
                    strata = birds_lc$Live)