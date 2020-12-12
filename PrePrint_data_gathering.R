#prePrint data
rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)
library(ggplot2)
library("ggpubr")

# IMPORTING Data ###########################################################
data <- import("/Users/jinghangli/Desktop/Pitt Fall 2020/GPN/KLU_APC2_Master_2020_09_18.xlsx")
data <- data[is.na(data$FaceNames_Exclude),] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1,] # Comment out for longitudinal studies
data <- data[!is.na(data$FaceNames_GoodCoverage),]
data$PiB_Median_Split <- NA
data$Sex[data$Sex == "NaN"] <- NA
PiB_Median = median(data$PiB_SUVR_GTM_FS_Global, na.rm = 'True');

# PiB Median Split ###############################################################
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global > PiB_Median)] <- "high_PiB"
data$PiB_Median_Split[which(data$PiB_SUVR_GTM_FS_Global <= PiB_Median)] <- "low_PiB"

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
mdl66 <- cor.test(data$FDG_SUVR_GTM_FS_Global, data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, method = "pearson", use = "complete.obs")

#P ADJUST ######################################################################

asy_FDG_p <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
absasy_FDG_p <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
asy_FDG_c <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
absasy_FDG_c <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
asy_FDG_adjust_p <- p.adjust(asy_FDG_p, method = 'fdr')
absasy_FDG_adjust_p <- p.adjust(absasy_FDG_p, method = 'fdr')


#Independent T Test ###############################################################
#Two sided = mean difference can be less than/greater than 0
#Variances are assumed to NOT be equal (false)
asy_PiB_p <- c(0,0,0,0,0,0)
a <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[1]<- a$p.value
aa <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

b <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[2] <- b$p.value
bb <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

c <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[3] <- c$p.value
cc <- t.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

d<- t.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[4] <- d$p.value
dd <- t.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

e<-t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[5] <- e$p.value
ee <- t.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

f<- t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)
asy_PiB_p[6] <- f$p.value
ff <- t.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR ~ data$PiB_Median_Split, mu = 0, alt="two.sided", conf= 0.95, var.eq=F,paired=F)

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}
#t values
asy_PiB_t <- c(a$statistic,b$statistic,c$statistic,d$statistic,e$statistic,f$statistic)
absasy_PiB_t <- c(aa$statistic,bb$statistic,cc$statistic,dd$statistic,ee$statistic,ff$statistic)
#p adjustments
asy_PiB_adjust_p <- p.adjust(asy_PiB_p,method = "fdr",n=6)

#Asymmetry and Task Performance
library("irr")
data$FaceName_PostScanAccuracy[data$FaceName_PostScanAccuracy == "NA"] <- NA
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_med_orb_asy_accuracy <-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_med_orb_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_mid_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_mid_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
reg<-lm(as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE) ~ data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR)
coeff=coefficients(reg)
abline(reg)
frontal_sup_medial_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
frontal_sup_medial_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
putamen_asy_accuracy <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
putamen_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
supp_motor_area_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
supp_motor_area_absasy_accuracy<-cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
reg<-lm(as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE) ~ data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR)
coeff=coefficients(reg)
abline(reg)
thal_VPL_asy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE))
thal_VPL_absasy_accuracy<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, as.numeric(data$FaceName_PostScanAccuracy, na.rm = TRUE), use = "complete.obs")
#correlation
asy_accuracy_c <- c(frontal_med_orb_asy_accuracy$estimate,  frontal_mid_asy_accuracy$estimate, 
                    frontal_sup_medial_asy_accuracy$estimate, 
                    putamen_asy_accuracy$estimate,  supp_motor_area_asy_accuracy$estimate,
                    thal_VPL_asy_accuracy$estimate)
absasy_accuracy_c <- c(frontal_med_orb_absasy_accuracy$estimate,
                       frontal_mid_absasy_accuracy$estimate, frontal_sup_medial_absasy_accuracy$estimate, 
                       putamen_absasy_accuracy$estimate, supp_motor_area_absasy_accuracy$estimate,
                       thal_VPL_absasy_accuracy$estimate)

#p-values
asy_accuracy_p <- c(frontal_med_orb_asy_accuracy$p.value,frontal_mid_asy_accuracy$p.value,frontal_sup_medial_asy_accuracy$p.value, putamen_asy_accuracy$p.value,supp_motor_area_asy_accuracy$p.value,thal_VPL_asy_accuracy$p.value)
absasy_accuracy_p <- c(frontal_med_orb_absasy_accuracy$p.value,frontal_mid_absasy_accuracy$p.value,frontal_sup_medial_absasy_accuracy$p.value, putamen_absasy_accuracy$p.value,supp_motor_area_absasy_accuracy$p.value,thal_VPL_absasy_accuracy$p.value)
asy_accuracy_adjust_p <- p.adjust(asy_accuracy_p,method = "fdr",n=length(asy_accuracy_p))
absasy_accuracy_adjust_p <- p.adjust(absasy_accuracy_p,method = "fdr",n=length(absasy_accuracy_p))

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
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Education)
frontal_med_orb_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Education)
frontal_med_orb_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Education)
frontal_mid_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Education)
frontal_mid_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Education)
frontal_sup_medial_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Education)
frontal_sup_medial_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Education)
putamen_asy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Education)
putamen_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR, data$Education, use = "complete.obs")

plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Education)
supp_motor_area_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Education)
reg<-lm(data$Education ~ data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR)
coeff=coefficients(reg)
abline(reg)
supp_motor_area_absasy_education <- cor.test(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Education)
thal_VPL_asy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR, data$Education, use = "complete.obs")
plot(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Education)
thal_VPL_absasy_education<- cor.test(data$FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR, data$Education, use = "complete.obs")

#p adjustments
asy_education_p <- c(putamen_asy_education$p.value, thal_VPL_asy_education$p.value, 
                     frontal_sup_medial_asy_education$p.value, frontal_mid_asy_education$p.value,
                     supp_motor_area_asy_education$p.value, frontal_med_orb_asy_education$p.value)

absasy_education_p <- c(putamen_absasy_education$p.value, thal_VPL_absasy_education$p.value, 
                        frontal_sup_medial_absasy_education$p.value, frontal_mid_absasy_education$p.value,
                        supp_motor_area_absasy_education$p.value, frontal_med_orb_absasy_education$p.value)

asy_education_adjust_p <- p.adjust(asy_education_p,method = "fdr",n=length(asy_education_p))
absasy_education_adjust_p <- p.adjust(absasy_education_p,method = "fdr",n=length(absasy_education_p))


## Report ##
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
asy_PiB_adjust_p <- p.adjust(asy_PiB_p,method = "fdr",n=6)

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
pvalList1
pvalList2

asy_sex_p
absasy_sex_p
asy_sex_adjust_p
absasy_sex_adjust_p

#Education
asy_education_p
absasy_education_p

asy_education_adjust_p
absasy_education_adjust_p

