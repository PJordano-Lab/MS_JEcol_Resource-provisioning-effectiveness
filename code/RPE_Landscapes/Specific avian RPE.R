library(readr)
#install effect_landscap from commit where ggrepel option was not modified (nudge_y=0.5)
devtools::install_github ("pakillo/effectiveness_pckg", ref='55d3183') 
#otherwise install from last commit and specify nudge_y=0.5, and then include the black countour
library(effect.lndscp)
library(devtools)
library(ggplot2)
library(ggrepel)


###CRACIDAE
Cracidae <- read_delim("code/RPE_Landscapes/Cracidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)
str(Cracidae)
names(Cracidae)

##Effectiveness landscape - using fruits/visit
Cracids <-effectiveness_plot(q1=Cracidae$imp_fruit_visit, q2=Cracidae$energy_fruit, 
                          q1.error=Cracidae$imp_fruit_visit_SE,q2.error = NULL,italic=T,
                          pts.color=Cracidae$species, pts.shape=Cracidae$species, label=Cracidae$plant, nlines = 7, 
                          lines.breaks = c(20, 80, 200, 354, 551, 801, 1130, 1608),
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/visit)", myylab= "QLY component - energy (KJ/fruit)")
Cracids+scale_colour_manual(values=c("gray43","gray83")) + geom_point(shape=Cracidae$species, colour = "black", size=3) #Specifiying colours and including black countour to points
#ggsave("MS/figures/Fig SM2.1.A. CRACIDAE .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


###RAMPHASTIDAE
#Toucans
Toucansbig <- read_delim("code/RPE_Landscapes/big_toucans.csv", ";", escape_double = FALSE, 
                      col_types = cols(species = col_factor(levels = c()), 
                                       plant_family = col_factor(levels = c()), 
                                       plant = col_factor(levels = c())), trim_ws = TRUE)
big <-effectiveness_plot(q1=Toucansbig$QTY, q2=Toucansbig$energy_fruit, 
                           q1.error=Toucansbig$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Toucansbig$species, pts.color=Toucansbig$species, label=Toucansbig$plant, nlines = 8,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", 
                          myylab= "QLY component - energy (KJ/fruit)")
big+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Toucansbig$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.B. TOUCANS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)

#Toucanets
Toucanets <- read_delim("code/RPE_Landscapes/small_toucans.csv", ";", escape_double = FALSE, 
                         col_types = cols(species = col_factor(levels = c()), 
                                          plant_family = col_factor(levels = c()), 
                                          plant = col_factor(levels = c())), trim_ws = TRUE)
small <-effectiveness_plot(q1=Toucanets$QTY, q2=Toucanets$energy_fruit, 
                           q1.error=Toucanets$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Toucanets$species, pts.color=Toucanets$species, label=Toucanets$plant, nlines = 8,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
small+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Toucanets$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.C. TOUCANETS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


##TROGONIDAE
Trogon <- read_delim("code/RPE_Landscapes/Trogonidae.csv", ";", escape_double = FALSE, 
                     col_types = cols(plant = col_factor(levels = c()), 
                                      plant_family = col_factor(levels = c()), 
                                      species = col_factor(levels = c())), trim_ws = TRUE)
Trogon <- subset(Trogon, QTY>0)#Remove NA
Trogon<- subset(Trogon, energy_fruit>0)#Remove NA

#Trogon spp.
Trogons <-effectiveness_plot(q1=Trogon$QTY, q2=Trogon$energy_fruit, 
                           q1.error=Trogon$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Trogon$species, pts.color=Trogon$species, label=Trogon$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Trogons+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Trogon$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.D. TROGONS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


###COTINGIDAE
Cotingidae <- read_delim("code/RPE_Landscapes/Cotingidae.csv", ";", escape_double = FALSE, 
                         col_types = cols(plant = col_factor(levels = c()), 
                                          plant_family = col_factor(levels = c()), 
                                          species = col_factor(levels = c())), trim_ws = TRUE)
#Removed Cabralea camjerana as outlier and both Cotingids together
Cotingidae <- Cotingidae [2:12,]
Cotingids <-effectiveness_plot(q1=Cotingidae$QTY, q2=Cotingidae$energy_fruit, 
                            q1.error=Cotingidae$QTY_SE,q2.error = NULL,italic=T, 
                            pts.shape = Cotingidae$species, pts.color = Cotingidae$species,
                            label=Cotingidae$plant, nlines = 7,
                            lines.color = "light grey", pts.size=3,
                            myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Cotingids+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Cotingidae$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.E. COTINGIDS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


###TURDIDAE
Turdus <- read_delim("code/RPE_Landscapes/Turdidae.csv", ";", escape_double = FALSE, 
                     col_types = cols(plant = col_factor(levels = c()), 
                                      plant_family = col_factor(levels = c()), 
                                      species = col_factor(levels = c())), trim_ws = TRUE)
#Turdus rufiventris
T_rufiventris <- subset(Turdus, species == "Turdus rufiventris")
T.ruf <-effectiveness_plot(q1=T_rufiventris$QTY, q2=T_rufiventris$energy_fruit, 
                           q1.error=T_rufiventris$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_rufiventris$species, label=T_rufiventris$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.ruf+geom_point(shape=21, fill="grey83",colour = "black", size=3) #Including black countour to points
#ggsave("MS/figures/Fig SM2.1.F. T.RUFIVENTRIS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)

#Turdus albicollis
T_albicollis <- subset(Turdus, species == "Turdus albicollis") 
T_albicollis <- T_albicollis[-c(11),]#remove myrsine umbellata as outlier
T.alb <-effectiveness_plot(q1=T_albicollis$QTY, q2=T_albicollis$energy_fruit, 
                           q1.error=T_albicollis$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_albicollis$species, label=T_albicollis$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.alb+geom_point(shape=21, fill="grey83", colour = "black", size=3) #Including black countour to points
#ggsave("MS/figures/Fig SM2.1.G. T.ALBICOLLIS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


###THRAUPIDAE
Thraupid <- read_delim("code/RPE_Landscapes/Thraupidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)

T_sayaca <- subset(Thraupid, species == "Thraupis sayaca")
T_palmarum <- subset(Thraupid, species == "Thraupis palmarum")

#Thraupis sayaca
T.say <-effectiveness_plot(q1=T_sayaca$QTY, q2=T_sayaca$energy_fruit, 
                           q1.error=T_sayaca$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_sayaca$species, label=T_sayaca$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.say+geom_point(shape=21, fill="grey83", colour = "black", size=3) #Including black countour to points
#ggsave("MS/figures/Fig SM2.1.H. T.SAYACA .pdf", plot=last_plot(), units = "in", width = 10, height = 7)

#Thraupis palmarum
T.pal <-effectiveness_plot(q1=T_palmarum$QTY, q2=T_palmarum$energy_fruit, 
                           q1.error=T_palmarum$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_palmarum$species, label=T_palmarum$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.pal+geom_point(shape=21, fill="grey83", colour = "black", size=3) #Including black countour to points
#ggsave("MS/figures/Fig SM2.1.I. T.PALMARUM .pdf", plot=last_plot(), units = "in", width = 10, height = 7)

#Tangara spp.
Tangara <- read_delim("code/RPE_Landscapes/Tangaras.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                                       plant_family = col_factor(levels = c()), 
                                       species = col_factor(levels = c())), trim_ws = TRUE)
T_seledon <- subset(Tangara, species == "Tangara seledon")
T_cyanocephala <- subset(Tangara, species == "Tangara cyanocephala")

Tang <-effectiveness_plot(q1=Tangara$QTY, q2=Tangara$energy_fruit, 
                          q1.error=Tangara$QTY_SE,q2.error = NULL,italic=T,
                          pts.shape=Tangara$species, pts.color = Tangara$species, label=Tangara$plant, nlines = 7,
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Tang+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Tangara$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.J. TANGARAS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)


###FRINGILLIDAE
Euphonia <- read_delim("code/RPE_Landscapes/Fringillidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)
E_viola <- subset(Euphonia, species == "Euphonia violacea")
E_chloro <- subset(Euphonia, species == "Euphonia chlorotica")

Eupho <-effectiveness_plot(q1=Euphonia$QTY, q2=Euphonia$energy_fruit, 
                          q1.error=Euphonia$QTY_SE,q2.error = NULL,italic=T,
                          pts.shape=Euphonia$species, pts.color=Euphonia$species, label=Euphonia$plant, nlines = 9,
                          lines.breaks = c(3,7,29,43,61,81,106,136,182,231),
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Eupho+scale_colour_manual(values=c("gray43","gray83"))+geom_point(shape=Euphonia$species, colour = "black", size=3)
#ggsave("MS/figures/Fig SM2.1.K. EUPHONIAS .pdf", plot=last_plot(), units = "in", width = 10, height = 7)



###ALL SPECIES TOGETHER
frug <- read.csv("code/RPE_Landscapes/all_frug.csv", sep=";", header = TRUE)
names(frug)
#Spp not taken: 1,7,12,15,16,18,19,21,22,25,27,32,37,38 - because few data (low n), or not fidable
#Acnistus arborescens, Cinnamomum triplinerve, Eugenia umbelliflora,Ficus benjamina, Ficus insipida
#Miconia prasina, Miconia pusilliflora, Myrsine gardneriana, Myrsine umbellata, Phoradendron affine,
#Phorandendron crassifolium, Sorocea ilicifolia, Virola sebifera,Vitex polygamia
myspp <- c(2,3,4,5,6,8,9,10,11,13,14,17,20,23,24,27,28,29,30,31,33,34,35,36)
frug2<- frug[myspp,]

All <-effectiveness_plot(q1=frug2$QTY_mean, q2=frug2$energy_fruit, 
                           q1.error=frug2$QTY_SE,q2.error = NULL,italic=T,
                           label=frug2$plant_species, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
All+geom_point(shape=21, fill="grey83", colour = "black", size=3) #Including black countour to points
#ggsave("MS/figures/Fig 3. RPE LANDSCAPE FOR ALL .pdf", plot=last_plot(), units = "in", width = 10, height = 7)       

###DIGESTIBILITY
digest <- read.csv("code/RPE_Landscapes/Digestibility.csv", sep=";", header=T)

dg <-effectiveness_plot(q1=digest$QTY, q2=digest$QLY_digest, 
                         q1.error=digest$QTY_SE,q2.error = digest$QLY_SE,italic=T,
                         pts.shape=digest$species, label=digest$plant, nlines = 7,
                         lines.color = "light grey", pts.size=3,
                         myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
dg+geom_point(aes(shape = factor(digest$species), color=factor(digest$plant_family)), size=2.2)

#Dotplot
ggplot(digest, aes(x=digest_data, y=TE, fill=species, shape=plant))+geom_dotplot(binaxis = "y", stackdir = "center", binwidth =0.03, show.legend=F)+ 
        scale_fill_manual(values=c('#ffffcc','#a1dab4','#41b6c4','#225ea8')) + 
        geom_text(data=subset (digest, digest_data=="YES"), aes(label=paste(species,"-",plant)), hjust=-0.05, size=3.3)+
        theme_minimal() + theme (panel.border = element_rect(colour = "black", fill=NA), 
                                 axis.text.y = element_text(size=12),
                                 axis.text.x = element_text(size=12),         
                                 axis.title.y = element_text(size=12),
                                 axis.title.x = element_text(size=12)) + xlab("Digestibility data") + ylab("RPE")
#ggsave("MS/figures/Fig 4. DIGESTIBLITY2.pdf", plot=last_plot(), units = "in", width = 10, height = 7)            
 
#TRIAL Including fruit size (PDM or FFM) in the dot of the plot + QLY error bars
frug_S <- read.csv("code/RPE_Landscapes/all_frug_size.csv", sep=";", header = TRUE)
names(frug_S)
#Spp not taken: 1,7,12,15,16,18,19,21,22,25,27,32,37,38 - because few data (low n), or not fidable
#Acnistus arborescens, Cinnamomum triplinerve, Eugenia umbelliflora,Ficus benjamina, Ficus insipida
#Miconia prasina, Miconia pusilliflora, Myrsine gardneriana, Myrsine umbellata, Phoradendron affine,
#Phorandendron crassifolium, Sorocea ilicifolia, Virola sebifera,Vitex polygamia
myspp <- c(2,3,4,5,6,8,9,10,11,13,14,17,20,23,24,27,28,29,30,31,33,34,35,36)
frug2_S<- frug_S[myspp,]

All <-effectiveness_plot(q1=frug2_S$QTY_mean, q2=frug2_S$energy_fruit, 
                         q1.error=frug2_S$QTY_SE,q2.error =frug2_S$QLY_SE,italic=T,
                         label=frug2_S$plant_species, nlines = 7,
                         lines.color = "light grey",pts.size=0.1,
                         myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
All+geom_point(aes(size=frug2_S$FFM), shape=21, colour = "black", fill="grey83") + theme(legend.position = "none")
#Iclugind size and COLOR!
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21, colour = "black") +
  scale_fill_continuous()+
  scale_size_continuous()+
  guides(color=guide_legend(), size = guide_legend())
#choose colors
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21, colour = "black") +scale_fill_gradient(low = "springgreen3", high = "darkblue")

#remove legend
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21, colour = "black") +scale_fill_gradient(low = "springgreen3", high = "darkblue")+
  theme(legend.position = "none")
#Show legend for colour but not for size
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21, colour = "black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue")+guides(size=FALSE)

#merge size and color legend
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21) +
  scale_fill_continuous(type="viridis", guide="legend", limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))+
  scale_size_continuous(guide="legend", limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))

All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21) +
  scale_fill_gradient2(low = "#edf8b1",mid='#7fcdbb', high = "#2c7fb8", midpoint = 0.7, guide="legend",limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))+
  scale_size_continuous(guide="legend", limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))

#Using FFM instead of PDM for the dot size
All+geom_point(aes(size=frug2_S$FFM, fill=frug2_S$FFM), shape=21, colour="black") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", guide="legend", limits=c(0, 15), breaks=seq(0, 15, by=3))+
  scale_size_continuous(guide="legend", limits=c(0, 15), breaks=seq(0, 15, by=3))



#ggsave("MS/figures/Fig 3. RPE LANDSCAPE FOR ALL-PDM4.pdf", plot=last_plot(), units = "in", width = 10, height = 7)
#ggsave("MS/figures/Fig 3. RPE LANDSCAPE FOR ALL-PDM3.pdf", plot=last_plot(), units = "in", width = 11.5, height = 8)  