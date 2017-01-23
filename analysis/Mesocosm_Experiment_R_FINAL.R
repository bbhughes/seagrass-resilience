mesocosm <- read.csv("data/Mesocosm_ExperimentalData_R.csv")
options(na.action = "na.fail")
mesocosm$NO3_ambient <- as.numeric(as.character(mesocosm$NO3_ambient))
mesocosm$NO3_added <- as.numeric(as.character(mesocosm$NO3_added))
nitrate<-t.test(mesocosm$NO3_ambient, mesocosm$NO3_added,var.equal=TRUE, paired=FALSE) 
nitrate
mesocosm$PO4_ambient <- as.numeric(as.character(mesocosm$PO4_ambient))
mesocosm$PO4_added <- as.numeric(as.character(mesocosm$PO4_added))
phosphate<-t.test(mesocosm$PO4_ambient, mesocosm$PO4_added,var.equal=TRUE, paired=FALSE) 
phosphate

#LMs and GLMs, using a stepwise approach with pH, nutrients and pH$nutrients ####
library(lme4) #this is for glms
library(MASS) #this is for the stepAIC model selection
library(ggplot2)
library(plotly)


#Epiphyte loading####
str(mesocosm)
#Compare linear, quadratic and exponential regression using the greatest R2 values
#linear
epi_linear<- lm(Epiphyte_cm_shoot ~ 1 + pH*Nutrients, mesocosm, 
            na.action=na.omit, family = "gaussian")
summary(epi_linear)

# 3rd order polynomial
epi_poly3<- lm(Epiphyte_cm_shoot ~ 1 + ((poly(pH,3))*Nutrients), mesocosm, 
            na.action=na.omit, family = "gaussian")
summary(epi_poly3)

#exponential
epi_exp<- lm(Epiphyte_cm_shoot ~ 1+(log(pH)*Nutrients), 
             mesocosm, na.action=na.omit)
summary(epi_exp)

#2nd order polynomial
epi_poly2<- lm(Epiphyte_cm_shoot ~ 1 + ((poly(pH,2))*Nutrients), mesocosm, 
               na.action=na.omit, family = "gaussian")
summary(epi_poly2)
#use 3rd order polynomial based on best R2

epi_lm<- lm(Epiphyte_cm_shoot ~ 1 + ((poly(pH,3))*Nutrients), mesocosm, 
            na.action=na.omit, family = "gaussian")
summary(epi_lm)
step_epi_lm<-stepAIC(epi_lm)
summary(step_epi_lm)
step_epi_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
epi_plot<-ggplot(data=mesocosm, aes(x=pH, y=Epiphyte_cm_shoot))+theme_bw(base_size=20)+ 
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +geom_point(aes(colour = Nutrients), size=4)+
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  theme(plot.title=element_text(hjust=0)) + scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle('a')+theme(plot.title=element_text(hjust=0))+
  labs(y = "Epiphyte loading (mg DW/cm shoot)\n", x = "\n")
epi_plot

#Ulva biomass####
#Compare linear, quadratic and exponential regression using the greatest R2
#linear
ulva_linear<- lm(Algal_mass_g_DW ~ 1 + (pH*Nutrients), 
               mesocosm, na.action=na.omit, family = "gaussian")
summary(ulva_linear)

#exponential
ulva_exp<- lm(Algal_mass_g_DW ~ 1+(log(pH)*Nutrients), 
             mesocosm, na.action=na.omit)
summary(ulva_exp)

#3rd order polynomial
ulva_poly3<- lm(Algal_mass_g_DW ~ 1+((poly(pH,3))*Nutrients), 
             mesocosm, na.action=na.omit)
summary(ulva_poly3)

#2nd order polynomial
ulva_poly2<- lm(Algal_mass_g_DW ~ 1+((poly(pH,2))*Nutrients), 
                mesocosm, na.action=na.omit)
summary(ulva_poly2)
#use exponential based on best R2

#Ulva exponential regression
ulva_lm<- lm(Algal_mass_g_DW ~ 1+(log(pH)*Nutrients), 
             mesocosm, na.action=na.omit)
summary(ulva_lm)
step_ulva_lm<-stepAIC(ulva_lm)
summary(step_ulva_lm)
step_ulva_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Algal_mass_g_DW)
max_y<-max(mesocosm$Algal_mass_g_DW)
ulva_mass_plot<-ggplot(data=mesocosm, aes(x=pH, y=Algal_mass_g_DW))+
  theme_bw(base_size=20)+geom_point(alpha = 1.0, position = position_jitter(w=0, h=0.02)) +
  theme(legend.position="none")+
  geom_point(aes(colour = Nutrients), size = 4) +
  geom_smooth(method = "lm",formula = y~splines::bs(x,3), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x))+ theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle('b')+theme(plot.title=element_text(hjust=0)) +
  labs(y = "Macroalgal mass (g DW)\n", x = "\n")
ulva_mass_plot


#shoot mortality####
#Compare linear, quadratic and exponential regression using the greatest Rsq
#linear
sht_mortal_linear<- lm(Dead_shoots ~ 1 + (pH*Nutrients), 
                 mesocosm, na.action=na.omit, family = "poisson")
summary(sht_mortal_linear)

#exponential
sht_mortal_exp<- glm(Dead_shoots ~ 1+(log(pH)*Nutrients), 
              mesocosm, na.action=na.omit, family = "poisson")
summary(sht_mortal_exp)
step_sht_mortal_exp<-stepAIC(sht_mortal_exp)
summary(step_sht_mortal_exp)
step_sht_mortal_exp$anova

#3rd order polynomial
sht_mortal_poly3<- glm(Dead_shoots ~ 1+((poly(pH,3))*Nutrients), 
                mesocosm, na.action=na.omit, family = "poisson")
summary(sht_mortal_poly3)
step_sht_mortal_poly3<-stepAIC(sht_mortal_poly3)
summary(step_sht_mortal_poly3)
step_sht_mortal_poly3$anova

#2nd order polynomial
sht_mortal_poly2<- glm(Dead_shoots ~ 1+((poly(pH,2))*Nutrients), 
                mesocosm, na.action=na.omit, family = "poisson")
summary(sht_mortal_poly2)
step_sht_mortal_poly2<-stepAIC(sht_mortal_poly2)
summary(step_sht_mortal_poly2)
step_sht_mortal_poly2$anova
#use exponential function

#shoot mortaility plot
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
dead_shoot_plot<-ggplot(data=mesocosm, 
                        aes(x=pH, y=Dead_shoots, group = Nutrients, colour = Nutrients))+
  theme_bw(base_size=20)+geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
  theme(legend.position="none")+
  geom_point(aes(colour = Nutrients), size =4)+
  geom_smooth(aes(colour = Nutrients), method = "glm", method.args = list(family = "poisson"), se = FALSE, size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  ggtitle('d')+theme(plot.title=element_text(hjust=0))+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Shoot mortality\n", x = "\npH")
dead_shoot_plot


#Shoot mass####
#Compare linear, quadratic and exponential regression using the greatest Rsq
#linear
sht_mass_linear<- lm(Sht_g_DW_MEAN ~ 1 + (pH*Nutrients), 
                 mesocosm, na.action=na.omit, family = "gaussian")
summary(sht_mass_linear)

#3rd order polynomial
sht_mass_poly3<- lm(Sht_g_DW_MEAN ~ 1 + ((poly(pH,3))*Nutrients), 
                 mesocosm, na.action=na.omit, family = "gaussian")
summary(sht_mass_poly3)

#2nd order polynomial
sht_mass_poly2<- lm(Sht_g_DW_MEAN ~ 1 + ((poly(pH,2))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(sht_mass_poly2)

#exponential
sht_mass_exp<- lm(Sht_g_DW_MEAN ~ 1+(log(pH)*Nutrients), 
             mesocosm, na.action=na.omit)
summary(sht_mass_exp)
#use linear model

sht_mass_lm<- lm(Sht_g_DW_MEAN ~ 1 + (pH*Nutrients), 
                 mesocosm, na.action=na.omit)
summary(sht_mass_lm)
step_sht_mass_lm<-stepAIC(sht_mass_lm)
summary(step_sht_mass_lm)
step_sht_mass_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Sht_g_DW_MEAN)
max_y<-max(mesocosm$Sht_g_DW_MEAN)
sht_mass_plot<-ggplot(data=mesocosm, aes(x=pH, y=Sht_g_DW_MEAN))+theme_bw(base_size=20) +
  geom_point(aes(colour=Nutrients), size=4) +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"),se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  ggtitle('a')+theme(plot.title=element_text(hjust=0))+
  labs(y = "Shoot mass (g DW)\n", x = "\n")
sht_mass_plot


#Rhizome mass####
#Compare linear, quadratic and exponential regression using the greatest Rsq
#linear
rhi_mass_linear<- lm(Rhi_mass_g_DW_MEAN ~ 1 + (pH*Nutrients), 
                     mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_mass_linear)

#3rd order polynomial
rhi_mass_poly3<- lm(Rhi_mass_g_DW_MEAN ~ 1 + ((poly(pH,3))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_mass_poly3)

#2nd order polynomial
rhi_mass_poly2<- lm(Rhi_mass_g_DW_MEAN ~ 1 + ((poly(pH,2))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_mass_poly2)

#exponential
rhi_mass_exp<- lm(Rhi_mass_g_DW_MEAN ~ 1+(log(pH)*Nutrients), 
                  mesocosm, na.action=na.omit)
summary(rhi_mass_exp)
#use 3rd order polynomial

rhi_mass_lm<- lm(Rhi_mass_g_DW_MEAN ~ 1 + ((poly(pH,3))*Nutrients), 
                 mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_mass_lm)
step_rhi_mass_lm<-stepAIC(rhi_mass_lm)
summary(step_rhi_mass_lm)
step_rhi_mass_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Rhi_mass_g_DW_MEAN)
max_y<-max(mesocosm$Rhi_mass_g_DW_MEAN)
rhi_mass_plot<-ggplot(data=mesocosm, aes(x=pH, y=Rhi_mass_g_DW_MEAN))+
  theme_bw(base_size=20)+geom_point(aes(colour=Nutrients), size=4)+
  theme(legend.position="none")+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle('b')+theme(plot.title=element_text(hjust=0))+labs(y = "Rhizome mass (g DW/cm rhizome)\n", x = "\npH")
rhi_mass_plot

#Rhizome elongation####
#Compare linear, quadratic and exponential regression using the greatest Rsq
#linear
rhi_elong_linear<- lm(Rhi_Elong_cm ~ 1 + (pH*Nutrients), 
                     mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_elong_linear)

#3rd order polynomial
rhi_elong_poly3<- lm(Rhi_Elong_cm ~ 1 + ((poly(pH,3))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_elong_poly3)

#2nd order polynomial
rhi_elong_poly2<- lm(Rhi_Elong_cm ~ 1 + ((poly(pH,2))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_elong_poly2)

#exponential
rhi_elong_exp<- lm(Rhi_Elong_cm ~ 1+(log(pH)*Nutrients), 
                  mesocosm, na.action=na.omit)
summary(rhi_elong_exp)
#use linear


rhi_elong_lm<- lm(Rhi_Elong_cm ~ 1 + (pH*Nutrients), 
                  mesocosm, na.action=na.omit, family = "gaussian")
summary(rhi_elong_lm)
step_rhi_elong_lm<-stepAIC(rhi_elong_lm)
summary(step_rhi_elong_lm)
step_rhi_elong_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Rhi_Elong_cm)
max_y<-max(mesocosm$Rhi_Elong_cm)
rhi_elong_plot<-ggplot(data=mesocosm, aes(x=pH, y=Rhi_Elong_cm))+
  theme_bw(base_size=20)+geom_point(aes(colour=Nutrients), size=4)+ 
  theme(legend.position="none")+
  geom_smooth(method = "glm",method.args = list(family = "gaussian"), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle('c')+theme(plot.title=element_text(hjust=0))+
  labs(y = "Rhizome elongation (cm)\n", x = "\n")
rhi_elong_plot

#Idotea mortaility####
#Compare linear, quadratic and exponential regression using the lowest AIC
#linear
ido_mort_linear<- glm(Ido_mortality ~ 1 + (pH*Nutrients), 
                      mesocosm, na.action=na.omit, family = "poisson")
summary(ido_mort_linear)

#3rd order polynomial
ido_mort_poly3<- glm(Ido_mortality ~ 1 + ((poly(pH,3))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "poisson")
summary(ido_mort_poly3)

#2nd order polynomial
ido_mort_poly2<- glm(Ido_mortality ~ 1 + ((poly(pH,2))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "poisson")
summary(ido_mort_poly2)

#exponential
ido_mort_exp<- glm(Ido_mortality ~ 1+(log(pH)*Nutrients), 
                   mesocosm, na.action=na.omit, family = "poisson")
summary(ido_mort_exp)
#use linear

ido_mort_glm<- glm(Ido_mortality ~ 1 + (pH*Nutrients), 
                   mesocosm, na.action=na.omit, family = "poisson")
summary(ido_mort_glm)
step_ido_mort_glm<-step(ido_mort_glm)
summary(step_ido_mort_glm)
stepAIC_ido_mort_glm<-stepAIC(ido_mort_glm)
stepAIC_ido_mort_glm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Ido_mortality)
max_y<-max(mesocosm$Ido_mortality)
ido_dense_plot<-ggplot(data=mesocosm, aes(x=pH, y=Ido_mortality))+
  theme_bw(base_size=16)+geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
  geom_smooth(method = "glm",method.args = list(family = "poisson"), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Idotea mortality\n", x = "\npH")
ido_dense_plot

#Change in Idotea mass####
#Compare linear, quadratic and exponential regression using the greatest R2
#linear
ido_mass_linear<- lm(Change_Idotea_mass_g_FW ~ 1 + (pH*Nutrients), 
                      mesocosm, na.action=na.omit, family = "gaussian")
summary(ido_mass_linear)

#3rd order polynomial
ido_mass_poly3<- lm(Change_Idotea_mass_g_FW ~ 1 + ((poly(pH,3))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "gaussian")
summary(ido_mass_poly3)

#2nd order polynomial
ido_mass_poly2<- lm(Change_Idotea_mass_g_FW ~ 1 + ((poly(pH,2))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "gaussian")
summary(ido_mass_poly2)

#exponential
ido_mass_exp<- lm(Change_Idotea_mass_g_FW ~ 1+(log(pH)*Nutrients), 
                   mesocosm, na.action=na.omit)
summary(ido_mass_exp)
#use 2nd order polynomial


ido_mass_lm<- lm(Change_Idotea_mass_g_FW ~ 1+((poly(pH,2))*Nutrients), 
                 mesocosm, na.action=na.omit, family = "gaussian")
summary(ido_mass_lm)
step_ido_mass_lm<-step(ido_mass_lm)
summary(step_ido_mass_lm)
step_ido_mass_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$Change_Idotea_mass_g_FW)
max_y<-max(mesocosm$Change_Idotea_mass_g_FW)
ido_dense_plot<-ggplot(data=mesocosm, aes(x=pH, y=Change_Idotea_mass_g_FW))+
  theme_bw(base_size=20)+geom_point(aes(colour=Nutrients), size=4) +
  theme(legend.position="none")+
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  ggtitle('b')+theme(plot.title=element_text(hjust=0))+
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Change in isopod mass (g FW)\n", x = "\n")
ido_dense_plot

#Seahare mortaility####
#Compare linear, quadratic and exponential regression using the lowest AIC
#linear
sh_mort_linear<- glm(SH_mortality ~ 1 + (pH*Nutrients), 
                      mesocosm, na.action=na.omit, family = "poisson")
summary(sh_mort_linear)

#3rd order polynomial
sh_mort_poly3<- glm(SH_mortality ~ 1 + ((poly(pH,3))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "poisson")
summary(sh_mort_poly3)

#2nd order polynomial
sh_mort_poly2<- glm(SH_mortality ~ 1 + ((poly(pH,2))*Nutrients), 
                     mesocosm, na.action=na.omit, family = "poisson")
summary(sh_mort_poly2)

#exponential
sh_mort_exp<- glm(SH_mortality ~ 1+(log(pH)*Nutrients), 
                   mesocosm, na.action=na.omit, family = "poisson")
summary(sh_mort_exp)
#use 2nd order polynomial

sh_mort_glm<- glm(SH_mortality ~ 1 + ((poly(pH,2))*Nutrients), 
                  mesocosm, na.action=na.omit, family = "poisson")
summary(sh_mort_glm)
step_sh_mort_glm<-step(sh_mort_glm)
summary(step_sh_mort_glm)
step_sh_mort_glm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(mesocosm$SH_mortality)
max_y<-max(mesocosm$SH_mortality)
sh_mort_plot<-ggplot(data=mesocosm, aes(x=pH, y=SH_mortality))+
  theme_bw(base_size=20)+geom_point(aes(colour=Nutrients), size=4) +
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2),method.args = list(family = "poisson"), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Seahare mortality\n", x = "\npH")
sh_mort_plot

#Change in Seahare mass####
#Compare linear, quadratic and exponential regression using the greatest R2
#linear
sh_mass_linear<- lm(Change_Seahare_mass_g_FW ~ 1 + (pH*Nutrients), 
                     mesocosm, na.action=na.omit, family = "gaussian")
summary(sh_mass_linear)

#3rd order polynomial
sh_mass_poly3<- lm(Change_Seahare_mass_g_FW ~ 1 + ((poly(pH,3))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(sh_mass_poly3)

#2nd order polynomial
sh_mass_poly2<- lm(Change_Seahare_mass_g_FW ~ 1 + ((poly(pH,2))*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(sh_mass_poly2)

#exponential
sh_mass_exp<- lm(Change_Seahare_mass_g_FW ~ 1+(log(pH)*Nutrients), 
                  mesocosm, na.action=na.omit)
summary(sh_mass_exp)
#use 2nd order polynomial

sh_mass_lm<- lm(Change_Seahare_mass_g_FW ~ 1 + ((poly(pH,2))*Nutrients), 
                mesocosm, na.action=na.omit)
summary(sh_mass_lm)
step_sh_mass_lm<-stepAIC(sh_mass_lm)
summary(step_sh_mass_lm)
step_sh_mass_lm$anova
min_x <- min(mesocosm$pH)
max_x <- max(mesocosm$pH)
min_y<-min(0)
max_y<-max(mesocosm$Change_Seahare_mass_g_FW)
sh_mass_plot<-ggplot(data=mesocosm, aes(x=pH, y=Change_Seahare_mass_g_FW))+
  theme_bw(base_size=20)+geom_point(alpha = 1.0, position = position_jitter(w=0, h=0.02)) + 
  theme(legend.justification=c(1,1), legend.position=c(1,1)) +
  geom_point(aes(colour = Nutrients), size=4) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  ggtitle('a')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Change in seahare mass (g FW)\n", x = "\n")
sh_mass_plot


#Epiphyte Grazer consumption#### 
mesocosm$SH_Grazing <- as.numeric(as.character(mesocosm$SH_Grazing))
mesocosm$SH_Grazing_ES <- as.numeric(as.character(mesocosm$SH_Grazing_ES))
mesocosm$pH_EffectSize2 <- as.numeric(as.character(mesocosm$pH_EffectSize2))

#Compare linear, quadratic and exponential regression using the greatest R2
#linear
SH_Grazing_linear<- lm(SH_Grazing ~ 1 + (pH*Nutrients), 
                    mesocosm, na.action=na.omit, family = "gaussian")
summary(SH_Grazing_linear)

#3rd order polynomial
SH_Grazing_poly3<- lm(SH_Grazing ~ 1 + ((poly(pH,3))*Nutrients), 
                   mesocosm, na.action=na.omit, family = "gaussian")
summary(SH_Grazing_poly3)

#2nd order polynomial
SH_Grazing_poly2<- lm(SH_Grazing ~ 1 + ((poly(pH,2))*Nutrients), 
                   mesocosm, na.action=na.omit, family = "gaussian")
summary(SH_Grazing_poly2)

#exponential
SH_Grazing_exp<- lm(SH_Grazing ~ 1+(log(pH)*Nutrients), 
                 mesocosm, na.action=na.omit)
summary(SH_Grazing_exp)
#use 3rd order polynomial

SH_Grazing_lm<- lm(SH_Grazing ~ 1 + ((poly(pH,3))*Nutrients), 
                   mesocosm, na.action=na.omit, family="gaussian")
summary(SH_Grazing_lm)
step_SH_Grazing_lm<-stepAIC(SH_Grazing_lm)
summary(step_SH_Grazing_lm)
step_SH_Grazing_lm$anova

#Grazing plot of ambient and enriched nutrients on one plot to show nutrient interaction
sh_grazing_raw_plot<-ggplot(data=mesocosm, aes(x=pH, y=SH_Grazing, group = Nutrients, colour = Nutrients))+
  theme_bw(base_size=20)+geom_point(alpha = 1.0, position = position_jitter(w=0, h=0.02)) +
  theme(legend.position="none")+geom_point(aes(colour = Nutrients), size =4) +
  geom_smooth(aes(colour = Nutrients), method = "lm", formula = y ~ poly(x, 2), se = FALSE, size=1.5, alpha = 0.3) +
  ggtitle('c')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Algal epiphyte consumed (mg DW/g DW seahare/hr)\n", x = "\npH")
sh_grazing_raw_plot

#### Epiphyte Grazer consumption, ln RR ####
str(mesocosm)
mesocosm$pH_EffectSize2 <- as.numeric(as.character(mesocosm$pH_EffectSize2))
mesocosm$SH_Grazing_ES <- as.numeric(as.character(mesocosm$SH_Grazing_ES))
SH_Grazing_lnRR<-lm(SH_Grazing_ES~pH_EffectSize2 + I(pH_EffectSize2^2), 
                    mesocosm, na.action=na.omit)
summary(SH_Grazing_lnRR)
min_x <- min(mesocosm$pH_EffectSize2)
max_x <- max(mesocosm$pH_EffectSize2)
min_y<-min(mesocosm$SH_Grazing_ES)
max_y<-max(mesocosm$SH_Grazing_ES)
SH_Grazing_lnRR_plot<-ggplot(data=mesocosm, aes(x=pH_EffectSize2, y=SH_Grazing_ES))+
  theme_bw(base_size=20)+geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02), size=4) +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2),se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +scale_y_continuous(limits=c(-4,4))+
  ggtitle('d')+theme(plot.title=element_text(hjust=0))+
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Nutrient effect on seahare herbivory (ln RR)\n", x = "\npH")
SH_Grazing_lnRR_plot

#Ulva recruit cover #### 
mesocosm$Ulva_grazing <- as.numeric(as.character(mesocosm$Ulva_grazing))
mesocosm$Ulva_Grazing_ES <- as.numeric(as.character(mesocosm$Ulva_Grazing_ES))
mesocosm$pH_EffectSize2 <- as.numeric(as.character(mesocosm$pH_EffectSize2))
mesocosm$lnUlva_grazing <- as.numeric(as.character(mesocosm$lnUlva_grazing))

#Compare linear, quadratic and exponential regression using the greatest R2
#linear
Ulva_Grazing_linear<- lm(Ulva_grazing ~ 1 + (pH*Nutrients), 
                       mesocosm, na.action=na.omit, family = "gaussian")
summary(Ulva_Grazing_linear)

#3rd order polynomial
Ulva_Grazing_poly3<- lm(Ulva_grazing ~ 1 + ((poly(pH,3))*Nutrients), 
                      mesocosm, na.action=na.omit, family = "gaussian")
summary(Ulva_Grazing_poly3)

#2nd order polynomial
Ulva_Grazing_poly2<- lm(Ulva_grazing ~ 1 + ((poly(pH,2))*Nutrients), 
                      mesocosm, na.action=na.omit, family = "gaussian")
summary(Ulva_Grazing_poly2)

#exponential
Ulva_Grazing_exp<- lm(Ulva_grazing ~ 1+(log(pH)*Nutrients), 
                    mesocosm, na.action=na.omit)
summary(Ulva_Grazing_exp)
#use 3rd order polynomial

Ulva_Grazing_lm<- lm(Ulva_grazing ~ 1 + ((poly(pH,3))*Nutrients), 
                     mesocosm, na.action=na.omit, family="gaussian")
summary(Ulva_Grazing_lm)
step_Ulva_Grazing_lm<-stepAIC(Ulva_Grazing_lm)
summary(step_Ulva_Grazing_lm)
step_Ulva_Grazing_lm$anova

#Ulva recruit plot of ambient and enriched nutrients on one plot to show nutrient interaction
Ulva_recruit_raw_plot<-ggplot(data=mesocosm, aes(x=pH, y=Ulva_grazing, group = Nutrients, colour = Nutrients))+
  theme_bw(base_size=20)+geom_point(alpha = 1.0, position = position_jitter(w=0, h=0.02)) +
  theme(legend.position="none")+geom_point(aes(colour = Nutrients), size =4) +
  geom_smooth(aes(colour = Nutrients), method = "lm", formula = y ~ poly(x, 3), se = FALSE, size=1.5, alpha = 0.3) +
  ggtitle('c')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(x = "\npH") +ylab(expression(paste(italic("Ulva "),"recruitment (% cover)")))
Ulva_recruit_raw_plot

#### Ulva recruit cover, ln RR ####
Ulva_recruit_lnRR<-lm(Ulva_Grazing_ES~pH_EffectSize2 + I(pH_EffectSize2^2), 
                      mesocosm, na.action=na.omit)
summary(Ulva_recruit_lnRR)
min_x <- min(mesocosm$pH_EffectSize2)
max_x <- max(mesocosm$pH_EffectSize2)
min_y<-min(mesocosm$Ulva_Grazing_ES)
max_y<-max(mesocosm$Ulva_Grazing_ES)
Ulva_recruit_lnRR_plot<-ggplot(data=mesocosm, aes(x=pH_EffectSize2, y=Ulva_Grazing_ES))+
  theme_bw(base_size=20)+geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02), size=4) +
  geom_smooth(method = "lm",formula = y ~ poly(x, 2),se = TRUE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle('d')+theme(plot.title=element_text(hjust=0))+
  labs(x = "\npH")+ylab(expression(paste("Nutrient effect on ",italic("Ulva "),"recruitment (ln RR)")))
Ulva_recruit_lnRR_plot

#Hypotheses for Fig. 1 ####
#Figure 1A ####
#Grazers
mesocosm$hyp1_grazers <- as.numeric(as.character(mesocosm$hyp1_grazers))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp1_grazers)
hyp1_grazers_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp1_grazers))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  ggtitle('A')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Grazer consumption\n", x = "")
hyp1_grazers_plot

#algae
mesocosm$hyp_1_algae <- as.numeric(as.character(mesocosm$hyp_1_algae))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp_1_algae)
hyp1_algae_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp_1_algae))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Algal biomass\n", x = "")
hyp1_algae_plot

#Seagrass
mesocosm$hyp1_eelgrass <- as.numeric(as.character(mesocosm$hyp1_eelgrass))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp1_eelgrass)
hyp1_seagrass_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp1_eelgrass))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.text.x = element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "Seagrass biomass\n", x = "\npH")
hyp1_seagrass_plot

#Figure 1B ####
#Grazers
mesocosm$hyp2_grazers <- as.numeric(as.character(mesocosm$hyp2_grazers))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp2_grazers)
hyp2_grazers_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp2_grazers))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  ggtitle('B')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "")
hyp2_grazers_plot

#algae
mesocosm$hyp_2_algae <- as.numeric(as.character(mesocosm$hyp_2_algae))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp_2_algae)
hyp2_algae_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp_2_algae))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "")
hyp2_algae_plot

#Seagrass
mesocosm$hyp2_eelgrass <- as.numeric(as.character(mesocosm$hyp2_eelgrass))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp2_eelgrass)
hyp2_seagrass_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp2_eelgrass))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~x, se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.text.x = element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "\npH")
hyp2_seagrass_plot

#Figure 1C ####
#Grazers
mesocosm$hyp3_grazers <- as.numeric(as.character(mesocosm$hyp3_grazers))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp3_grazers)
hyp3_grazers_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp3_grazers))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  ggtitle('C')+theme(plot.title=element_text(hjust=0)) + 
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "")
hyp3_grazers_plot

#algae
mesocosm$hyp_3_algae <- as.numeric(as.character(mesocosm$hyp_3_algae))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp_3_algae)
hyp3_algae_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp_3_algae))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.x=element_blank(),axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "")
hyp3_algae_plot

#Seagrass
mesocosm$hyp3_eelgrass <- as.numeric(as.character(mesocosm$hyp3_eelgrass))
min_x <- min(7)
max_x <- max(8)
min_y<-min(0)
max_y<-max(mesocosm$hyp3_eelgrass)
hyp3_seagrass_plot<-ggplot(data=mesocosm, aes(x=pH, y=hyp3_eelgrass))+
  theme_bw(base_size=30)+ 
  geom_smooth(method = "lm", formula = y ~x, se = FALSE,colour='black', size=1.5, alpha = 0.3) +
  scale_x_continuous(limits=c(min_x,max_x)) +
  theme(axis.ticks.y=element_blank(),axis.text.y = element_blank(), axis.ticks.x=element_blank(),axis.text.x = element_blank(), panel.grid.major = element_blank(),panel.grid.minor=element_blank(),panel.background = element_blank(), panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  labs(y = "", x = "\npH")
hyp3_seagrass_plot

grid.newpage()
grid.draw(rbind(ggplotGrob(hyp1_grazers_plot), ggplotGrob(hyp1_algae_plot), ggplotGrob(hyp1_seagrass_plot),size = "last"))

# ggsave(p, filename = "hypothesis.pdf", width=20, height=18)#### Hypothesis figure
pdf("hypothesis.pdf", width=20, height=18)
gridExtra::grid.arrange(hyp1_grazers_plot, hyp2_grazers_plot, hyp3_grazers_plot, hyp1_algae_plot, hyp2_algae_plot, hyp3_algae_plot, hyp1_seagrass_plot, hyp2_seagrass_plot, hyp3_seagrass_plot,ncol = 3, nrow =3)
dev.off()

# ggsave(p, filename = "grazer.pdf", width=12, height=14)#### Grazer figure
pdf("grazer.pdf", width=12, height=14)
gridExtra::grid.arrange(sh_mass_plot,ido_dense_plot, sh_grazing_raw_plot, SH_Grazing_lnRR_plot,ncol = 2, nrow =2)
dev.off()

# ggsave(p, filename = "algae.pdf", width=12, height=14)#### Algae figure
pdf("algae.pdf", width=12, height=14)
gridExtra::grid.arrange(epi_plot, ulva_mass_plot, Ulva_recruit_raw_plot, Ulva_recruit_lnRR_plot, ncol = 2, nrow =2)
dev.off()

# ggsave(p, filename = "eelgrass.pdf", width=18, height=7)#### Eelgrass figure
pdf("eelgrass.pdf", width=18, height=7)
gridExtra::grid.arrange(sht_mass_plot, rhi_mass_plot, rhi_elong_plot, ncol = 3, nrow =1)
dev.off()

#THE END####