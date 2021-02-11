#prePrint data
rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(ggplot2)
library("ggpubr")
library('ppcor')

# IMPORTING Data ###########################################################

library("readxl")
#data <- read_excel("C:\\Users\\mount\\OneDrive\\Documents\\GPN\\KLU_APC2_Master_2020_12_16.xlsx");
data <- import("/Users/jinghangli/Desktop/Pitt Fall 2020/GPN/KLU_APC2_Master_2020_12_16.xlsx")
data <- data[is.na(data$FaceNames_Exclude),] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1,] # Comment out for longitudinal studies
data <- data[!is.na(data$FaceNames_GoodCoverage),]
data$PiB_Median_Split <- NA
data$Sex[data$Sex == "NaN"] <- NA
PiB_Median = median(data$PiB_SUVR_GTM_FS_Global, na.rm = 'True');
Pred_STRCW <- (data$STRCOL*data$STRWRD) / (data$STRCOL+data$STRWRD)
data$STRINTERFERENCE <- data$STRCW - Pred_STRCW
data$LETTER_FLUENCY <- (data$FLUENA + data$FLUENF+ data$FLUENS) / 3
data$WREC_TOT <- (data$WREC + data$WREC2 + data$WREC3)

# PiB Median Split ###############################################################
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global > PiB_Median)] <- "high_PiB"
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global <= PiB_Median)] <- "low_PiB"

# Cognitive Domain Scores ##########################################################

# Cognitive Domain - Z Transform 
#Negative z value means that lower value = higher performance
# doi:10.1016/j.jalz.2017.12.003 - method of composite calculation
#doi/ 10.1136/jnnp.2004.045567 - standard deviations from normative data
CLOCKD_Z <- (data$CLOCKD - mean(data$CLOCKD, na.rm = TRUE)) / sd(data$CLOCKD, na.rm = TRUE)
BLOCKDES_Z <- (data$BLOCKDES - mean(data$BLOCKDES, na.rm = TRUE)) / sd(data$BLOCKDES, na.rm = TRUE)
BNT60TOT_Z <- (data$BNT60TOT - mean(data$BNT60TOT, na.rm = TRUE)) / sd(data$BNT60TOT, na.rm = TRUE)
REYCO_Z <- (data$REYCO - mean(data$REYCO, na.rm = TRUE)) / sd (data$REYCO, na.rm = TRUE)
REYIM_Z <- (data$REYIM-mean(data$REYIM, na.rm = TRUE)) / sd(data$REYIM, na.rm = TRUE)
REYDE_Z <- (data$REYDE - mean(data$REYDE, na.rm = TRUE)) / sd(data$REYDE, na.rm = TRUE)
FLUEN_Z <- (data$FLUEN - mean(data$FLUEN, na.rm = TRUE)) / sd(data$FLUEN, na.rm = TRUE)
LETTER_FLUENCY_Z <- (data$LETTER_FLUENCY - mean(data$LETTER_FLUENCY, na.rm = TRUE)) / sd(data$LETTER_FLUENCY, na.rm=TRUE)
WREC_TOT_Z <- (data$WREC_TOT - mean(data$WREC_TOT, na.rm = TRUE)) / sd(data$WREC_TOT, na.rm =TRUE)
WRECDE_Z <- (data$WRECDE - mean(data$WRECDE, na.rm = TRUE)) / sd(data$WRECDE, na.rm = TRUE)
SPANSF_Z <- (data$SPANSF - mean(data$SPANSF, na.rm = TRUE)) / sd(data$SPANSF, na.rm = TRUE)
SPANSB_Z <- (data$SPANSB - mean(data$SPANSB, na.rm = TRUE)) / sd(data$SPANSB, na.rm = TRUE)
TRAILAS_Z <- (data$TRAILAS - mean(data$TRAILAS, na.rm = TRUE)) / sd(data$TRAILAS, na.rm = TRUE)
TRAILBS_Z <- (data$TRAILBS - mean(data$TRAILBS, na.rm = TRUE)) / sd(data$TRAILBS, na.rm= TRUE)
LMIAIMM_Z <- (data$LMIAIMM - mean(data$LMIAIMM, na.rm = TRUE)) /sd(data$LMIAIMM, na.rm = TRUE)
LMIIADEL_Z <- (data$LMIIADEL - mean(data$LMIIADEL, na.rm = TRUE))/sd(data$LMIIADEL, na.rm = TRUE)
DIGSYMWR_Z <- (data$DIGSYMWR - mean(data$DIGSYMWR, na.rm = TRUE))/sd(data$DIGSYMWR, na.rm = TRUE)
STRINTERFERENCE_Z <- (data$STRINTERFERENCE - mean(data$STRINTERFERENCE, na.rm = TRUE))/ sd(data$STRINTERFERENCE, na.rm = TRUE)
TRAILAS_Z_INV <- -1 * TRAILAS_Z
TRAILBS_Z_INV <- -1 * TRAILBS_Z
# Domain Scores #########################################################################
#doi:10.1016/j.jalz.2017.12.003., doi:10.1080/13607860903071014. (Both Beth Snitz articles), https://www.ncbi.nlm.nih.gov/books/NBK285344/ - for SPANSB in Executive
data$memory_learning <- (LMIAIMM_Z + REYIM_Z + WREC_TOT_Z) / 3
data$memory_retrieval <- (LMIIADEL_Z + REYDE_Z + WRECDE_Z) / 3
data$visuospatial <- (BLOCKDES_Z + REYCO_Z) / 2
data$language <- (FLUEN_Z + LETTER_FLUENCY_Z + BNT60TOT_Z) / 3
data$executive_attention <- (TRAILAS_Z_INV + TRAILBS_Z_INV + CLOCKD_Z + DIGSYMWR_Z + STRINTERFERENCE_Z + SPANSF_Z + SPANSB_Z) / 7


#Function for p test from linear model ###########################################
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

# Memory_Learning Cognitive Domain Scores and Asymmetry ##################################################################
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$memory_learning, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_mem_learn_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_mem_learn_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_mem_learn_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_mem_learn_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_mem_learn_adjust_p <- p.adjust(asy_mem_learn_p, method = 'fdr')
absasy_mem_learn_adjust_p <- p.adjust(absasy_mem_learn_p, method = 'fdr')

# Memory_Retrieval Cognitive Domain Scores and Asymmetry ##################################################################
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR,data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$memory_retrieval, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_mem_retrieval_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_mem_retrieval_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_mem_retrieval_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_mem_retrieval_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_mem_retrieval_adjust_p <- p.adjust(asy_mem_retrieval_p, method = 'fdr')
absasy_mem_retrieval_adjust_p <- p.adjust(absasy_mem_retrieval_p, method = 'fdr')

# Visuospatial and Asymmetry ##################################################################
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$visuospatial, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_visuospatial_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_visuospatial_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_visuospatial_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_visuospatial_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_visuospatial_adjust_p <- p.adjust(asy_visuospatial_p, method = 'fdr')
absasy_visuospatial_adjust_p <- p.adjust(absasy_visuospatial_p, method = 'fdr')

# Language Cognitive Domain Scores and Asymmetry ##################################################################
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$language, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$language, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_language_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_language_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_language_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_language_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_language_adjust_p <- p.adjust(asy_language_p, method = 'fdr')
absasy_language_adjust_p <- p.adjust(absasy_language_p, method = 'fdr')

# Executive_Attention Cognitive Domain Scores and Asymmetry ##################################################################
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$executive_attention, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_executive_attention_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_executive_attention_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_executive_attention_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_executive_attention_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_executive_attention_adjust_p <- p.adjust(asy_executive_attention_p, method = 'fdr')
absasy_executive_attention_adjust_p <- p.adjust(absasy_executive_attention_p, method = 'fdr')


# AI WITH WMHI ################################################################
complete_data <- data[!is.na(data$WMH_Volume_mm3),] #WMHI and Total Brain Volume variables have missing data for the same participants
#All asymmetry columns have complete data
which(is.na(data$Total_Brain_Volume_mm3)) == which(is.na(data$WMH_Volume_mm3))

mdl1 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl11 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$WMH_Volume_mm3)
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$WMH_Volume_mm3)

mdl2 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl22 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$WMH_Volume_mm3)
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$WMH_Volume_mm3)

mdl3 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl33 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$WMH_Volume_mm3)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$WMH_Volume_mm3)

mdl4 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl44 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
reg <- lm(WMH_Volume_mm3 ~  FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data = data)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$WMH_Volume_mm3)
abline(reg, col = 'blue')

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$WMH_Volume_mm3)

mdl5 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl55 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");

mdl6 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");
mdl66 <- pcor.test(complete_data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, complete_data$WMH_Volume_mm3, complete_data$Total_Brain_Volume_mm3, method = "pearson");

#P ADJUST ######################################################################

asy_WMH_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_WMH_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_WMH_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_WMH_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_WMH_adjust_p <- p.adjust(asy_WMH_p, method = 'fdr')
absasy_WMH_adjust_p <- p.adjust(absasy_WMH_p, method = 'fdr')



# AI WITH FDG ##################################################################
#Putamen 
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
reg <- lm( FDG_SUVR_GTM_FS_Global ~  FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data = data)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global)
abline(reg, col = 'blue')

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global)

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$FDG_SUVR_GTM_FS_Global, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_FDG_p<- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_FDG_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_FDG_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_FDG_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_FDG_adjust_p <- p.adjust(asy_FDG_p, method = 'fdr')
absasy_FDG_adjust_p <- p.adjust(absasy_FDG_p, method = 'fdr')


#Independent T Test ###############################################################
#Two sided = mean difference can be less than/greater than 0
#Variances are assumed to NOT be equal (false)
asy_PiB_p <- c(0,0,0,0,0,0)
absasy_PiB_p <- c(0,0,0,0,0,0)

a<- t.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[1] <- a$p.value
aa <- t.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[1] <- aa$p.value

b<- t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[2] <- b$p.value
bb <- t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[2] <- bb$p.value

c <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[3] <- c$p.value
cc <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[3] <- cc$p.value

d <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[4] <- d$p.value
dd <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[4] <- dd$p.value

e<-t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[5] <- e$p.value
ee <- t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[5] <- ee$p.value

f <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[6]<- f$p.value
ff <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
absasy_PiB_p[6]<- ff$p.value

#t values
asy_PiB_t <- c(a$statistic,b$statistic,c$statistic,d$statistic,e$statistic,f$statistic)
absasy_PiB_t <- c(aa$statistic,bb$statistic,cc$statistic,dd$statistic,ee$statistic,ff$statistic)

#p values
asy_PiB_p
absasy_PiB_p

#p adjustments
asy_PiB_adjust_p <- p.adjust(asy_PiB_p,method = "fdr")
absasy_PiB_adjust_p <- p.adjust(absasy_PiB_p,method = "fdr")

#Asymmetry and Task Performance
library("irr")
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA

frontal_med_orb_asy_accuracy <-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
frontal_med_orb_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

frontal_mid_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
frontal_mid_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

frontal_sup_medial_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
frontal_sup_medial_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

putamen_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
putamen_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

supp_motor_area_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
supp_motor_area_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

thal_VPL_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
thal_VPL_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

#correlation
asy_accuracy_c <- c(putamen_asy_accuracy$estimate,  thal_VPL_asy_accuracy$estimate, 
                    frontal_sup_medial_asy_accuracy$estimate, frontal_mid_asy_accuracy$estimate, 
                    supp_motor_area_asy_accuracy$estimate, frontal_med_orb_asy_accuracy$estimate 
                  )
absasy_accuracy_c <- c(putamen_absasy_accuracy$estimate, thal_VPL_absasy_accuracy$estimate,
                        frontal_sup_medial_absasy_accuracy$estimate,frontal_mid_absasy_accuracy$estimate, 
                      supp_motor_area_absasy_accuracy$estimate,frontal_med_orb_absasy_accuracy$estimate  
                      
                       )

#p-values
asy_accuracy_p <- c(putamen_asy_accuracy$p.value,  thal_VPL_asy_accuracy$p.value,
                   frontal_sup_medial_asy_accuracy$p.value, frontal_mid_asy_accuracy$p.value,
                   supp_motor_area_asy_accuracy$p.value,frontal_med_orb_asy_accuracy$p.value
                   
                    )
absasy_accuracy_p <- c(putamen_absasy_accuracy$p.value,thal_VPL_absasy_accuracy$p.value,
                       frontal_sup_medial_absasy_accuracy$p.value, frontal_mid_absasy_accuracy$p.value, 
                           supp_motor_area_absasy_accuracy$p.value, frontal_med_orb_absasy_accuracy$p.value
                       )

asy_accuracy_adjust_p <- p.adjust(asy_accuracy_p,method = "fdr")
absasy_accuracy_adjust_p <- p.adjust(absasy_accuracy_p,method = "fdr")

# AI with Age
mdl1 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl11 <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl2 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl22 <- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl3 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl33 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl4 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl44 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Age_CurrentVisit)
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Age_CurrentVisit)

mdl5 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl55 <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")

mdl6 <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Age_CurrentVisit, method = "pearson", use = "complete.obs")
mdl66 <- cor.test(data$Age_CurrentVisit, data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, method = "pearson", use = "complete.obs")
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
correlation1 <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
correlation2 <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
#corr
asy_age_c <- correlation1
absasy_age_c <- correlation2
#p-values
asy_age_p <- pvalList1
absasy_age_p <- pvalList2
asy_age_adjust_p <- p.adjust(pvalList1, method = 'fdr')
absasy_age_adjust_p <- p.adjust(pvalList2, method = 'fdr')


# AI WITH SEX DIFFERENCE #######################################################
#Putamen **
mdl1 <- t.test(FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ Sex, data = data)
mdl11 <- t.test(FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR~Sex, data = data)
#Thalumus
mdl2 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR~Sex, data = data)
mdl22 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Med
mdl3 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR~Sex, data = data)
mdl33 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Mid **
mdl4 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR~Sex, data = data)
mdl44 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR~Sex, data = data)

#Supp_motor_Area
mdl5 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR~Sex, data = data)
mdl55 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR~Sex, data = data)

#Sup_Medial
mdl6 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR~Sex, data = data)
mdl66 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR~Sex, data = data)

#t values
asy_Sex_t <- c(mdl1$statistic, mdl2$statistic, mdl3$statistic, mdl4$statistic, mdl5$statistic, mdl6$statistic)
absasy_Sex_t <- c(mdl11$statistic, mdl22$statistic, mdl33$statistic, mdl44$statistic, mdl55$statistic, mdl66$statistic)

#P values ######################################################################
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)

asy_sex_p <- pvalList1
absasy_sex_p <- pvalList2
asy_sex_adjust_p <- p.adjust(pvalList1, method = 'fdr')
absasy_sex_adjust_p <- p.adjust(pvalList2, method = 'fdr')


#Asymmetry and Education
frontal_med_orb_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Education, use = "complete.obs")
frontal_med_orb_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Education, use = "complete.obs")

frontal_mid_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Education, use = "complete.obs")
frontal_mid_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Education, use = "complete.obs")

frontal_sup_medial_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Education, use = "complete.obs")
frontal_sup_medial_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Education, use = "complete.obs")

putamen_asy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Education, use = "complete.obs")
putamen_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Education, use = "complete.obs")

supp_motor_area_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Education, use = "complete.obs")
supp_motor_area_absasy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Education, use = "complete.obs")

thal_VPL_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Education, use = "complete.obs")
thal_VPL_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Education, use = "complete.obs")
#c-values
asy_education_c <- c(putamen_asy_education$estimate, thal_VPL_asy_education$estimate, 
                     frontal_sup_medial_asy_education$estimate, frontal_mid_asy_education$estimate,
                     supp_motor_area_asy_education$estimate, frontal_med_orb_asy_education$estimate)

absasy_education_c <- c(putamen_absasy_education$estimate, thal_VPL_absasy_education$estimate, 
                     frontal_sup_medial_absasy_education$estimate, frontal_mid_absasy_education$estimate,
                     supp_motor_area_absasy_education$estimate, frontal_med_orb_absasy_education$estimate)


#p adjustments
asy_education_p <- c(putamen_asy_education$p.value, thal_VPL_asy_education$p.value, 
                     frontal_sup_medial_asy_education$p.value, frontal_mid_asy_education$p.value,
                     supp_motor_area_asy_education$p.value, frontal_med_orb_asy_education$p.value)

absasy_education_p <- c(putamen_absasy_education$p.value, thal_VPL_absasy_education$p.value, 
                        frontal_sup_medial_absasy_education$p.value, frontal_mid_absasy_education$p.value,
                        supp_motor_area_absasy_education$p.value, frontal_med_orb_absasy_education$p.value)

asy_education_adjust_p <- p.adjust(asy_education_p,method = "fdr")
absasy_education_adjust_p <- p.adjust(absasy_education_p,method = "fdr",n=length(absasy_education_p))


## Report ##
#Cognitive Domains
#Memory_Learning
asy_mem_learn_c 
absasy_mem_learn_c

asy_mem_learn_p 
absasy_mem_learn_p 

asy_mem_learn_adjust_p 
absasy_mem_learn_adjust_p 

#Memory_Retrieval
asy_mem_retrieval_c 
absasy_mem_retrieval_c

asy_mem_retrieval_p 
absasy_mem_retrieval_p 

asy_mem_retrieval_adjust_p 
absasy_mem_retrieval_adjust_p 

#Visuospatial
asy_visuospatial_c 
absasy_visuospatial_c

asy_visuospatial_p 
absasy_visuospatial_p 

asy_visuospatial_adjust_p 
absasy_visuospatial_adjust_p 

#Language
asy_language_c 
absasy_language_c

asy_language_p 
absasy_language_p 

asy_language_adjust_p 
absasy_language_adjust_p 

#Executive_Attention
asy_executive_attention_c 
absasy_executive_attention_c

asy_executive_attention_p 
absasy_executive_attention_p 

asy_executive_attention_adjust_p 
absasy_executive_attention_adjust_p 

#WMH
asy_WMH_c 
absasy_WMH_c

asy_WMH_p 
absasy_WMH_p 

asy_WMH_adjust_p 
absasy_WMH_adjust_p 

#FDG
asy_FDG_c 
absasy_FDG_c

asy_FDG_p 
absasy_FDG_p 
 
asy_FDG_adjust_p 
absasy_FDG_adjust_p 

#PiB
asy_PiB_t 
absasy_PiB_t

asy_PiB_p
absasy_PiB_p

asy_PiB_adjust_p
absasy_PiB_adjust_p


#Performance
asy_accuracy_c 
absasy_accuracy_c 

asy_accuracy_p 
absasy_accuracy_p 

asy_accuracy_adjust_p 
absasy_accuracy_adjust_p 

#Age
asy_age_c
absasy_age_c

asy_age_p 
absasy_age_p 

asy_age_adjust_p 
absasy_age_adjust_p 

#Sex
asy_Sex_t
absasy_Sex_t

asy_sex_p
absasy_sex_p

asy_sex_adjust_p
absasy_sex_adjust_p

#Education
asy_education_c
absasy_education_c

asy_education_p
absasy_education_p

asy_education_adjust_p
absasy_education_adjust_p

# BoxPlots ####################################################3

boxplot(data$FaceNames_Pos_Novel_Control_Putamen_L, data$FaceNames_Pos_Novel_Control_Putamen_R, 
        main = "Putamen Activation", col = c("blue","yellow"), 
        xlab = c("Left Putamen", "Right Putamen"), ylab("Putamen Activation"))

# Scatterplots ##################################################################
data$asymmetry_direction[ data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR > 0] <- 'Left'
data$asymmetry_direction[ data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR < 0] <- 'Right'
h <- ggplot(data = data, aes(x=memory_learning, y=FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR))
h + geom_jitter(aes(colour = asymmetry_direction)) + 
  geom_smooth(method = "lm", se = FALSE, color = 'black') + labs(title='a. Absolute Asymmetry in Thalamus Ventral Posterolateral Nucleus and Memory Learning', x = 'Memory Learning Cognitive Domain', y = 'Absolute Asymmetry in Thalamus Ventral Posterolateral') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(color = 'black', size = 10, face = 'bold')) 


h <- ggplot(data = data, aes(x=executive_attention, y=FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR))
h + geom_jitter(aes(colour = asymmetry_direction)) + 
  geom_smooth(method = "lm", se = FALSE, color = 'black') + labs(title='b. Absolute Asymmetry in Thalamus Ventral Posterolateral Nucleus and Executive Attention', x = 'Executive Attention Cognitive Domain', y = 'Absolute Asymmetry in Thalamus Ventral Posterolateral') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(color = 'black', size = 10, face = 'bold')) 

h <- ggplot(data = data, aes(x=visuospatial, y=FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR))
h + geom_jitter(aes(colour = asymmetry_direction)) + 
  geom_smooth(method = "lm", se = FALSE, color = 'black') + labs(title='c. Absolute Asymmetry in Putamen and Visuospatial', x = 'Visuospatial Cognitive Domain', y = 'Absolute Asymmetry in Putamen') +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(color = 'black', size = 10, face = 'bold'),
        axis.title.y = element_text(color = 'black', size = 10, face = 'bold')) 


plot(data$memory_learning, data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, main = "a. Absolute Asymmetry in Thalamus Ventral Posterolateral Nucleus and Memory Learning", xlab = "Memory Learning Cognitive Domain", ylab = "Absolute Asymmetry in Thalamus Ventral Posterolateral");
abline(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$memory_learning));

plot(data$executive_attention, data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, main = "b. Absolute Asymmetry in Thalamus Ventral Posterolateral Nucleus and Executive Attention", xlab = "Executive Attention Cognitive Domain", ylab = "Absolute Asymmetry in Thalamus Ventral Posterolateral Nucleus");
abline(lm(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$executive_attention));

plot(data$visuospatial, data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, main = "c. Absolute Asymmetry in Putamen and Visuospatial", xlab = "Visuospatial Cognitive Domain", ylab = "Absolute Asymmetry in Putamen");
abline(lm(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ data$visuospatial));

