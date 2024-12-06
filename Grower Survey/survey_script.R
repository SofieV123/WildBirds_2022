#Survey Regression Analysis
library(AICcmodavg)
library(car)
library(caret)
library(dplyr)
library(effects)
library(ggplot2)
library(MASS)
library(patchwork)
library(reshape)
library(tidyverse)

surv_df = read.table("./survey_response.txt", header = T, sep = "\t")

#Add mammalian livestock together
surv_df$TotalM = rowSums(surv_df[,c(22,24,26,28,30,32)])

#Formatting ####################################################################
fs_df = surv_df[,c("Concern", "SizeInt", "ProdInt", "ProduceN", "LivestockS", "BirdObsN", 
                   "BirdManAtt", "Prevent", "TotalM")]
fs_df_num = data.frame(fs_df)

fact_cols = c("Concern", "SizeInt", "ProdInt", "BirdManAtt", "Prevent")
fs_df[fact_cols] = lapply(fs_df[fact_cols], factor)

fs_df = fs_df[-26,] #dropping an incomplete response
fs_df_num = fs_df_num[-26,]

fs_df_coll = fs_df
fs_df_coll$Concern[fs_df_coll$Concern == 5] = 4

#Setting Up Models #######################################################
lvl5_m0 = polr(Concern ~ 1, data = fs_df_coll, Hess = T) #null model

lvl5_m1 = polr(Concern ~ ProduceN, data = fs_df_coll, Hess = T)
lvl5_m2 = polr(Concern ~ LivestockS, data = fs_df_coll, Hess = T)
lvl5_m3 = polr(Concern ~ BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m4 = polr(Concern ~ ProdInt, data = fs_df_coll, Hess = T)
lvl5_m5 = polr(Concern ~ TotalM, data = fs_df_coll, Hess = T)

lvl5_m6 = polr(Concern ~ BirdObsN + ProduceN, data = fs_df_coll, Hess = T)
lvl5_m7 = polr(Concern ~ BirdObsN + LivestockS, data = fs_df_coll, Hess = T)
lvl5_m8 = polr(Concern ~ BirdObsN + TotalM, data = fs_df_coll, Hess = T)
lvl5_m9 = polr(Concern ~ BirdObsN + ProdInt, data = fs_df_coll, Hess = T)
lvl5_m10 = polr(Concern ~ BirdObsN * ProduceN, data = fs_df_coll, Hess = T)
lvl5_m11 = polr(Concern ~ BirdObsN * LivestockS, data = fs_df_coll, Hess = T)
lvl5_m12 = polr(Concern ~ BirdObsN * TotalM, data = fs_df_coll, Hess = T)
lvl5_m13 = polr(Concern ~ BirdObsN * ProdInt, data = fs_df_coll, Hess = T)

lvl5_m14 = polr(Concern ~ ProduceN + LivestockS, data = fs_df_coll, Hess = T)
lvl5_m15 = polr(Concern ~ ProduceN + TotalM, data = fs_df_coll, Hess = T)
lvl5_m16 = polr(Concern ~ ProduceN + ProdInt, data = fs_df_coll, Hess = T)
lvl5_m17 = polr(Concern ~ ProduceN * LivestockS, data = fs_df_coll, Hess = T)
lvl5_m18 = polr(Concern ~ ProduceN * TotalM, data = fs_df_coll, Hess = T)
lvl5_m19 = polr(Concern ~ ProduceN * ProdInt, data = fs_df_coll, Hess = T)

lvl5_m20 = polr(Concern ~ ProdInt + LivestockS, data = fs_df_coll, Hess = T)
lvl5_m21 = polr(Concern ~ ProdInt + TotalM, data = fs_df_coll, Hess = T)
lvl5_m22 = polr(Concern ~ ProdInt * LivestockS, data = fs_df_coll, Hess = T)
lvl5_m23 = polr(Concern ~ ProdInt * TotalM, data = fs_df_coll, Hess = T)

lvl5_m24 = polr(Concern ~ ProdInt + LivestockS + ProduceN, data = fs_df_coll, Hess = T)
lvl5_m25 = polr(Concern ~ ProdInt + TotalM + ProduceN, data = fs_df_coll, Hess = T)
lvl5_m26 = polr(Concern ~ ProdInt + LivestockS + BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m27 = polr(Concern ~ ProdInt + TotalM + BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m28 = polr(Concern ~ ProdInt + ProduceN + BirdObsN, data = fs_df_coll, Hess = T)

lvl5_m29 = polr(Concern ~ ProdInt * LivestockS + ProduceN, data = fs_df_coll, Hess = T)
lvl5_m30 = polr(Concern ~ ProdInt + LivestockS * ProduceN, data = fs_df_coll, Hess = T)

lvl5_m31 = polr(Concern ~ ProdInt * TotalM + ProduceN, data = fs_df_coll, Hess = T)
lvl5_m32 = polr(Concern ~ ProdInt + TotalM * ProduceN, data = fs_df_coll, Hess = T)

lvl5_m33 = polr(Concern ~ ProdInt * LivestockS + BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m34 = polr(Concern ~ ProdInt + LivestockS * BirdObsN, data = fs_df_coll, Hess = T)

lvl5_m35 = polr(Concern ~ ProdInt * TotalM + BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m36 = polr(Concern ~ ProdInt + TotalM * BirdObsN, data = fs_df_coll, Hess = T)

lvl5_m37 = polr(Concern ~ ProdInt * ProduceN + BirdObsN, data = fs_df_coll, Hess = T)
lvl5_m38 = polr(Concern ~ ProdInt + ProduceN * BirdObsN, data = fs_df_coll, Hess = T)

lvl5_m39 = polr(Concern ~ ProdInt + ProduceN + BirdObsN + LivestockS, data = fs_df_coll, Hess = T)
lvl5_m40 = polr(Concern ~ ProdInt + ProduceN + BirdObsN + TotalM, data = fs_df_coll, Hess = T)

#Looking at Assumptions #####################################################
#Assumption 1: Proportional Odds (Brant test)
car::poTest(lvl5_m5)

#SizeInt was originally included as an IV but does not meet PO assumption
#LivestockS*BirdObsN also doesn't meet PO assumptions

#Assumption 2: No Multicollinearity (VIF)
#First test for pairwise correlation with all IVs
fs_df_cor = fs_df_num[,c(2:6, 9)]
fs_df_cor$Live = as.numeric(fs_df_cor$Live)
cor(fs_df_cor, method = "spearman")

#Then look at GVIF
fs_df_dv = data.frame(predict(dummyVars(~., fs_df), fs_df))
fs_df_dv = cbind(fs_df_dv, "ConcernT" = as.numeric(fs_df_coll$Concern))

lvl5_dm1 = lm(ConcernT ~ SizeInt.2 + SizeInt.3 + SizeInt.4 + SizeInt.5 + SizeInt.7 +
                SizeInt.8 + ProdInt.2 + ProdInt.3 + ProduceN + LivestockS + BirdObsN
              + TotalM,
              data = fs_df_dv)
car::vif(lvl5_dm1, type = "predictor")

#Model Comparison and Interpretation ########################################
lvl5_m_list = list(lvl5_m1, lvl5_m2, lvl5_m3, lvl5_m4, lvl5_m5, lvl5_m6, lvl5_m7,
                   lvl5_m8, lvl5_m9, lvl5_m10, lvl5_m11, lvl5_m12, lvl5_m13, lvl5_m14,
                   lvl5_m15, lvl5_m16, lvl5_m17, lvl5_m18, lvl5_m19, lvl5_m20, lvl5_m21,
                   lvl5_m22, lvl5_m23, lvl5_m24, lvl5_m25, lvl5_m26, lvl5_m27, lvl5_m28,
                   lvl5_m29, lvl5_m30, lvl5_m31, lvl5_m32, lvl5_m33, lvl5_m34, lvl5_m35,
                   lvl5_m36, lvl5_m37, lvl5_m38, lvl5_m39, lvl5_m40, lvl5_m0)
aic_lvl5_m = aictab(lvl5_m_list, second.ord = T, sort = T)

#coefficients and p-values
cp_tab5 = coef(summary(lvl5_m5))
pv_vec5 = pnorm(abs(cp_tab5[, "t value"]), lower.tail = F) * 2
cp_tab5 = cbind(cp_tab5, "p value" = pv_vec5)

#CIs (97.5%) and odds ratios
ci_tab5 = confint(lvl5_m5)
ci_tab5 = cbind(OR = coef(lvl5_m5), ci_tab5)

#Figure 4 ############################################
surv_df.gg = read.table("./surv_gg.txt", sep = "\t", header = T)
surv_df.gg = surv_df.gg %>%
  pivot_longer(cols = "Organic":"PreventY",
               names_to = "Chars",
               values_to = "Perc")
surv_df.gg$Chars = factor(surv_df.gg$Chars, levels = c("Organic", "Mix", "Convent",
                                                       "Produce1", "Produce2", "Produce3",
                                                       "Concern1", "Concern2", "Concern3",
                                                       "BMA1", "BMA2", "BMA3",
                                                       "PreventY", "PreventN"),
                          ordered = T)

ggplot(surv_df.gg, aes(fill = Live, x = Chars, y = Perc)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_discrete(limits = rev(levels(surv_df.gg$Chars)),
                   labels = c("No", "Yes",
                              "Encourage", "Neither encourage \n nor discourage", "Discourage",
                              "Concerned", "Neither concerned \n nor unconcerned", "Unconcerned",
                              "11+", "6-10", "1-5",
                              "Conventional", "Mixed", "Organic"),
                   expand= c(0,0)) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0", "25", "50", "75", "100"),
                     expand = c(0,0)) +
  scale_fill_manual(labels = c("No", "Yes"), values = c("#D55E00", "#0072B2")) +
  coord_flip() +
  labs(x = element_blank(), y = "Percentage of Growers (%)", 
       fill = "Livestock \n Present?") +
  theme(panel.background = element_blank(),
        axis.line = element_line("black"))

#Supplementary Material Figure 2 #######################################
prev_eff = read.table("./prevent_eff.txt", header = T, 
                      sep = "\t")
prev_freq = read.table("./prevent_freq.txt", header = T,
                       sep = "\t")

#Effects Grid (Color)
longeff = melt(prev_eff, id = "Taxonomy")
prev_eff2 = dcast(longeff, variable ~ Taxonomy)
prev_eff2 = prev_eff2[,c(1,5,9,4,8,10,2,3,6,7,11,12)]

num_vec = c(11,10,9,8,7,6,5,4,3,2,1)
prev_eff2$ID = num_vec
names(prev_eff2)[names(prev_eff2) == "variable"] = "PM" 

prev_eff_long = melt(prev_eff2, id = c("ID", "PM"))
names(prev_eff_long)[names(prev_eff_long) == "variable"] = "Taxonomy"
names(prev_eff_long)[names(prev_eff_long) == "value"] = "Ef"

#Freq Matrix (Dots)
prev_freq2 = prev_freq[1:11,]
longfreq = melt(prev_freq2, id = "Taxonomy")
prev_freq2 = dcast(longfreq, variable ~ Taxonomy)
prev_freq2 = prev_freq2[,c(1,5,9,4,8,10,2,3,6,7,11,12)]

prev_freq2$ID = num_vec
names(prev_freq2)[names(prev_freq2) == "variable"] = "PM"

prev_freq_long = melt(prev_freq2, id = c("ID", "PM"))
names(prev_freq_long)[names(prev_freq_long) == "variable"] = "Taxonomy"
names(prev_freq_long)[names(prev_freq_long) == "value"] = "Frq"

#Matrix Formatting
mbird_plot = ggplot() +
  labs(x = "Targeted Birds", y = element_blank(), tag = "B") +
  geom_tile(data = prev_eff_long[1:55,], aes(x = Taxonomy, y = reorder(PM, ID), fill = Ef), 
            color = "grey95") +
  scale_fill_gradient(low = "white", high = "orangered3", breaks = c(1:5), 
                      name = "Reported \n Efficacy", na.value = "gray78") +
  geom_point(data = prev_freq_long[1:55,], aes(x = Taxonomy, y = reorder(PM, ID), size = Frq)) +
  scale_size_continuous(limits = c(1,5), name = "Frequency") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(axis.text.x = element_text(angle = 28, hjust = 1),
        axis.text.y = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.3))

#PM Barplot
pm_list = c("Other", "Lethal control", "Habitat removal", "Spikes", "Rotating laser",
            "Reflective surfaces", "Air cannon", "Ultrasonic device", "Bird/predator noises",
            "Decoys", "Netting")

prev_freq3 = prev_freq[12:13,]

longfreq3 = melt(prev_freq3, id = "Taxonomy")
prev_freq3 = dcast(longfreq3, variable ~ Taxonomy)
prev_freq3$ID = num_vec
prev_freq_long3 = melt(prev_freq3, id = c("variable", "ID"))

mbar_plot = ggplot(prev_freq3, aes(x = TotalProp, y = reorder(variable, ID))) +
  labs(x = "Proportion of Farms (%)", y = element_blank(), tag = "A") +
  scale_y_discrete(labels = stringr::str_wrap(pm_list, width = 13)) +
  scale_x_continuous(expand = c(0,0)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(color = "black")) +
  geom_bar(stat = "identity")

#Putting Together
pm_matrix = mbar_plot + mbird_plot + plot_layout(width = c(1.2, 1.8))



