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