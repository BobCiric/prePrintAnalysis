#independent t-test
rm(list=ls())
dev.off()
pacman::p_load(pacman, rio)

library("ggpubr")

# IMPORTING Data ###########################################################
data <- import("~/Desktop/GPN/KLU_APC2_Master_2020_12_16.xlsx")
#data <- import("/Users/jinghangli/Desktop/Pitt Fall 2020/GPN/KLU_APC2_Master_2020_09_18.xlsx")
data <- data[is.na(data$FaceNames_Exclude), ] #Issues with face name data and only 1 scan/subject - 87 observations
data <- data[data$Visit_Relative == 1, ] # Comment out for longitudinal studies
data <- data[!is.na(data$FaceNames_GoodCoverage), ] # Comment out for longitudinal studies
data$PiBStatus_SUVR_GTM_FS_Global[data$PiBStatus_SUVR_GTM_FS_Global == "NaN"] = NA
data$Sex[data$Sex == "NaN"] = NA
PiB_m <- median(data$PiB_SUVR_GTM_FS_Global[!is.na(data$PiB_SUVR_GTM_FS_Global)])

data$PiB_status <- NA #creating a variable PiB status
data$PiB_status[(data$PiB_SUVR_GTM_FS_Global > PiB_m)] <- "Amyloid Burdened" # 1 if PiB score over threshold
data$PiB_status[(data$PiB_SUVR_GTM_FS_Global < PiB_m)] <- "non Amyloid Burdened" # 0 if PiB score under threshold

boxData <- data.frame('Putamen' = c(data$FaceNames_Pos_Novel_Control_Putamen_L, 
                                              data$FaceNames_Pos_Novel_Control_Putamen_R),
                      'PutamenCat'= c('Putamen_L'),
                      'Thalamus' = c(data$FaceNames_Pos_Novel_Control_Thal_VPL_L,
                                     data$FaceNames_Pos_Novel_Control_Thal_VPL_R),
                      'ThalamusCat'= c('Thalamus_L'),
                      'Frontal_Mid' = c(data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_L,
                                        data$FaceNames_Pos_Novel_Control_Frontal_Mid_2_R),
                      'Frontal_MidCat'= c('Frontal_Mid_L'),
                      'Frontal_Sup_Medial' = c(data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_L,
                                               data$FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_R),
                      'Frontal_Sup_MedialCat'= c('Frontal_Sup_Medial_L'),
                      'Frontal_Med_Orb' = c(data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_L,
                                            data$FaceNames_Pos_Novel_Control_Frontal_Med_Orb_R),
                      'Frontal_Med_OrbCat'= c('Frontal_Med_Orb_L'),
                      'Supp_Motor_Area' = c(data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_L,
                                            data$FaceNames_Pos_Novel_Control_Supp_Motor_Area_R),
                      'Supp_Motor_AreaCat'= c('Supp_Motor_Area_L'))

boxData$PutamenCat[71:140] <- "Putamen_R"
boxData$ThalamusCat[71:140] <- "Thalamus_R"
boxData$Frontal_MidCat[71:140] <- "Frontal_Mid_R"
boxData$Frontal_Sup_MedialCat[71:140] <- "Frontal_Sup_Medial_R"
boxData$Frontal_Med_OrbCat[71:140] <- "Frontal_Med_Orb_R"
boxData$Supp_Motor_AreaCat[71:140] <- "Supp_Motor_Area_R"

ggboxplot(boxData, x = "PutamenCat", y = "Putamen", 
          title = "a. Putamen",
          color = "PutamenCat", legend = "None", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen Activation", xlab = "Left & Right Hemsiphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)


ggboxplot(boxData, x = "ThalamusCat", y = "Thalamus", 
          main = "b. Thal_VPL",
          color = "ThalamusCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus Activation", xlab = "Left & Right Hemesphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)

ggboxplot(boxData, x = "Frontal_MidCat", y = "Frontal_Mid", 
          main = "d. Frontal_Mid", legend = "None",
          color = "Frontal_MidCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Middle Frontal Gyrus Activation", xlab = "Left & Right Hemisphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)

ggboxplot(boxData, x = "Frontal_Sup_MedialCat", y = "Frontal_Sup_Medial", 
          main = "c.  Frontal_Sup_Medial", legend = "None",
          color = "Frontal_Sup_MedialCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Superior Frontal Gyrus Activation", xlab = "Left & Right Hemisphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)

ggboxplot(boxData, x = "Frontal_Med_OrbCat", y = "Frontal_Med_Orb", 
          main = "f. Frontal_Med_Orb", legend = "None",
          color = "Frontal_Med_OrbCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Medial Orbitofrontal Cortex Activation", xlab = "Left & Right Hemisphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)

ggboxplot(boxData, x = "Supp_Motor_AreaCat", y = "Supp_Motor_Area", 
          main = "e. Supp_Motor_Area", legend = "None",
          color = "Supp_Motor_AreaCat", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supplement Motor Area Activation", xlab = "Left & Right Hemisphere") + geom_jitter(color="black", size=0.4, alpha=0.9) +
          font("title", size = 20, color = "black", face = "bold")+
          font("xlab", size = 20, color = "black")+
          font("ylab", size = 20, color = "black")+
          font("x.text", size = 20)

# AI WITH SEX DIFFERENCE #######################################################
#Putamen **
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen AI", xlab = "Sex")
mdl1 <- t.test(FaceNames_Pos_Novel_Control_Putamen_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Putamen Abs AI", xlab = "Sex")
mdl11 <- t.test(FaceNames_Pos_Novel_Control_Putamen_AbsAsymmetry_LR~Sex, data = data)

#Thalumus
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus AI", xlab = "Sex")
mdl2 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_Asymmetry_LR~Sex, data = data)


ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Thalamus Abs AI", xlab = "Sex")

mdl22 <- t.test(FaceNames_Pos_Novel_Control_Thal_VPL_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Med
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal_Med AI", xlab = "Sex")
mdl3 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal_Med Abs AI", xlab = "Sex")
mdl33 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Sup_Medial_AbsAsymmetry_LR~Sex, data = data)

#Frontal_Mid **
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal Mid AI", xlab = "Sex")
mdl4 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Frontal Mid abs AI", xlab = "Sex")
mdl44 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Mid_2_AbsAsymmetry_LR~Sex, data = data)

#Supp_motor_Area
ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supp_motor Area AI", xlab = "Sex")
mdl5 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Supp_motor Area abs AI", xlab = "Sex")
mdl55 <- t.test(FaceNames_Pos_Novel_Control_Supp_Motor_Area_AbsAsymmetry_LR~Sex, data = data)

#Sup_Medial

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Sup_Medial AI", xlab = "Sex")
mdl6 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_Asymmetry_LR~Sex, data = data)

ggboxplot(data, x = "Sex", y = "FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR", 
          color = "Sex", palette = c("#00AFBB", "#E7B800"),
          ylab = "Sup_Medial Area abs AI", xlab = "Sex")
mdl66 <- t.test(FaceNames_Pos_Novel_Control_Frontal_Med_Orb_AbsAsymmetry_LR~Sex, data = data)


#P ADJUST ######################################################################
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
addadjustedP2 <- p.adjust(pvalList2, method = 'fdr')

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
pvalList1 <- c(mdl1$p.value, mdl2$p.value, mdl3$p.value, mdl4$p.value, mdl5$p.value, mdl6$p.value)
pvalList2 <- c(mdl11$p.value, mdl22$p.value, mdl33$p.value, mdl44$p.value, mdl55$p.value, mdl66$p.value)
correlation1 <- c(mdl1$estimate, mdl2$estimate, mdl3$estimate, mdl4$estimate, mdl5$estimate, mdl6$estimate)
correlation2 <- c(mdl11$estimate, mdl22$estimate, mdl33$estimate, mdl44$estimate, mdl55$estimate, mdl66$estimate)
adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
adjustedP2 <- p.adjust(pvalList2, method = 'fdr')

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

adjustedP1 <- p.adjust(pvalList1, method = 'fdr')
adjustedP2 <- p.adjust(pvalList2, method = 'fdr')



