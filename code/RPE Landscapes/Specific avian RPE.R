library(readr)
#install effect_landscap from commit where ggrepel option was not modified (nudge_y=0.5)
devtools::install_github ("pakillo/effectiveness_pckg", ref='55d3183') 
library(effect.lndscp)
library(devtools)
library(ggplot2)
library(ggrepel)

###CRACIDAE
Cracidae <- read_delim("Cracidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)
str(Cracidae)
names(Cracidae)
#Effectiveness landscape 1
Cracid <- subset(Cracidae, Cracidae$QTY > 0)
Cracid
Crac <-effectiveness_plot(q1=Cracid$QTY, q2=Cracid$energy_fruit, 
                       q1.error=Cracid$QTY_SE,q2.error = NULL,italic=T,
                       pts.color=Cracid$species, label=Cracid$plant, nlines = 5,
                       lines.color = "light grey", pts.size=3,
                       myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Crac+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

##Effectiveness landscape 2 - using fruits/visit
Crac2 <-effectiveness_plot(q1=Cracidae$imp_fruit_visit, q2=Cracidae$energy_fruit, 
                          q1.error=Cracidae$imp_fruit_visit_SE,q2.error = NULL,italic=T,
                          pts.shape=Cracidae$species, label=Cracidae$plant, nlines = 7, lines.breaks = c(20, 80, 200, 354, 551, 801, 1130, 1608),
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/visit)", myylab= "QLY component - energy (KJ/fruit)")
Crac2+geom_point(aes(shape = factor(Cracidae$species), color=factor(Cracidae$plant_family)), size=2.2) 




###RAMPHASTIDAE
Toucans <- read_delim("Toucans.csv", ";", escape_double = FALSE, 
                      col_types = cols(species = col_factor(levels = c()), 
                                       plant_family = col_factor(levels = c()), 
                                       plant = col_factor(levels = c())), trim_ws = TRUE)
#Subset for each toucan species
Baillonus <- subset(Toucans, species == "Baillonius bailloni")
R_dicol <- subset(Toucans, species == "Ramphastos dicolorus")
R_vitel <- subset(Toucans, species == "Ramphastos vitellinus")
Selenidera <- subset(Toucans, species == "Selenidera maculirostris")

#Toucans
Toucansbig <- Toucans[c(2,3,4,6,7,8,10,11,13,14,15,16,18,21,23), ]
Toucansbig <- Toucans[c(2,3,4,6,7,8,10,11,13,14,15,16,18,21), ] #without Vitex polygamia
big <-effectiveness_plot(q1=Toucansbig$QTY, q2=Toucansbig$energy_fruit, 
                           q1.error=Toucansbig$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Toucansbig$species, label=Toucansbig$plant, nlines = 8,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", 
                          myylab= "QLY component - energy (KJ/fruit)")
big+geom_point(aes(shape = factor(Toucansbig$species), color=factor(Toucansbig$plant_family)), size=2.2)

#Toucanets
Toucanets <- Toucans[c(1,5,9,12,17,19,20,22), ]
small <-effectiveness_plot(q1=Toucanets$QTY, q2=Toucanets$energy_fruit, 
                           q1.error=Toucanets$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Toucanets$species, label=Toucanets$plant, nlines = 8,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
small+geom_point(aes(shape = factor(Toucanets$species), color=factor(Toucanets$plant_family)), size=2.2)

#Ramphastos vitellinus
R.vit <-effectiveness_plot(q1=R_vitel$QTY, q2=R_vitel$energy_fruit, 
                           q1.error=R_vitel$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=R_vitel$plant_family, label=R_vitel$plant, nlines = 8, lines.breaks = c(0.5,3,5,8,11,15,21,29,41),
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
R.vit+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

#Ramphastos dicolorus
R.dic <-effectiveness_plot(q1=R_dicol$QTY, q2=R_dicol$energy_fruit, 
                           q1.error=R_dicol$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=R_dicol$plant_family, label=R_dicol$plant, nlines = 6,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
R.dic+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

#Baillonius bailloni
B.bal <-effectiveness_plot(q1=Baillonus$QTY, q2=Baillonus$energy_fruit, 
                            q1.error=Baillonus$QTY_SE,q2.error = NULL,italic=T,
                            pts.color=Baillonus$plant_family, label=Baillonus$plant, nlines = 7,
                            lines.color = "light grey", pts.size=3,
                            myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
B.bal+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

#Selenidera maculirostris
S.mac <-effectiveness_plot(q1=Selenidera$QTY, q2=Selenidera$energy_fruit, 
                           q1.error=Selenidera$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=Selenidera$plant_family, label=Selenidera$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
S.mac+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points




##TROGONIDAE
Trogon <- read_delim("Trogonidae.csv", ";", escape_double = FALSE, 
                     col_types = cols(plant = col_factor(levels = c()), 
                                      plant_family = col_factor(levels = c()), 
                                      species = col_factor(levels = c())), trim_ws = TRUE)
Trogon <- subset(Trogon, QTY>0)#Remove NA
Trogon<- subset(Trogon, energy_fruit>0)#Remove NA

#Trogon spp.
Trogons <-effectiveness_plot(q1=Trogon$QTY, q2=Trogon$energy_fruit, 
                           q1.error=Trogon$QTY_SE,q2.error = NULL,italic=T,
                           pts.shape=Trogon$species, label=Trogon$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Trogons+geom_point(aes(shape = factor(Trogon$species), color=factor(Trogon$plant_family)), size=2.2)

#Trogon surrucura
T_surrucura <- subset(Trogon, species == "Trogon surrucura")
T.sur <-effectiveness_plot(q1=T_surrucura$QTY, q2=T_surrucura$energy_fruit, 
                           q1.error=T_surrucura$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_surrucura$plant_family, label=T_surrucura$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.sur+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

#Trogon viridis
T_viridis <- subset(Trogon, species == "Trogon viridis")
T.vir <-effectiveness_plot(q1=T_viridis$QTY, q2=T_viridis$energy_fruit, 
                           q1.error=T_viridis$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_viridis$plant_family, label=T_viridis$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.vir+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points




###COTINGIDAE
Cotingidae <- read_delim("Cotingidae.csv", ";", escape_double = FALSE, 
                         col_types = cols(plant = col_factor(levels = c()), 
                                          plant_family = col_factor(levels = c()), 
                                          species = col_factor(levels = c())), trim_ws = TRUE)
#Removed Cabralea camjerana as outlier and both Cotingids together
Cotingidae <- Cotingidae [2:12,]
Cotingids <-effectiveness_plot(q1=Cotingidae$QTY, q2=Cotingidae$energy_fruit, 
                            q1.error=Cotingidae$QTY_SE,q2.error = NULL,italic=T, pts.shape = Cotingidae$species,
                            label=Cotingidae$plant, nlines = 7,
                            lines.color = "light grey", pts.size=3,
                            myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Cotingids+geom_point(aes(shape = factor(Cotingidae$species), color=factor(Cotingidae$plant_family)), size=2.2)

#Tityra cayana
Tit_cayana <- subset(Cotingidae, species == "Tityra cayana")
Ti.cay <-effectiveness_plot(q1=Tit_cayana$QTY, q2=Tit_cayana$energy_fruit, 
                           q1.error=Tit_cayana$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=Tit_cayana$plant_family, label=Tit_cayana$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Ti.cay+geom_point(shape = 21, colour = "black", size=3) #Including black countour to points

#Procnias nudicollis
P_nudicollis <- subset(Cotingidae, species == "Procnias nudicollis")
P.nud <-effectiveness_plot(q1=P_nudicollis$QTY, q2=P_nudicollis$energy_fruit, 
                            q1.error=P_nudicollis$QTY_SE,q2.error = NULL,italic=T,
                            pts.color=P_nudicollis$plant_family, label=P_nudicollis$plant, nlines = 7,
                            lines.color = "light grey", pts.size=3,
                            myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
P.nud+geom_point(shape=21, colour = "black", size=3) #Including black countour to points




###TURDIDAE
Turdus <- read_delim("Turdidae.csv", ";", escape_double = FALSE, 
                     col_types = cols(plant = col_factor(levels = c()), 
                                      plant_family = col_factor(levels = c()), 
                                      species = col_factor(levels = c())), trim_ws = TRUE)
#Turdus rufiventris
T_rufiventris <- subset(Turdus, species == "Turdus rufiventris")
T.ruf <-effectiveness_plot(q1=T_rufiventris$QTY, q2=T_rufiventris$energy_fruit, 
                           q1.error=T_rufiventris$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_rufiventris$plant_family, label=T_rufiventris$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.ruf+geom_point(shape=21, colour = "black", size=3) #Including black countour to points

#Turdus albicollis
T_albicollis <- subset(Turdus, species == "Turdus albicollis")
T.alb <-effectiveness_plot(q1=T_albicollis$QTY, q2=T_albicollis$energy_fruit, 
                           q1.error=T_albicollis$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_albicollis$plant_family, label=T_albicollis$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.alb+geom_point(shape=21, colour = "black", size=3) #Including black countour to points



###THRAUPIDAE
Thraupid <- read_delim("Thraupidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)
Tangara <- read_delim("Tangaras.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                                       plant_family = col_factor(levels = c()), 
                                       species = col_factor(levels = c())), trim_ws = TRUE)
T_sayaca <- subset(Thraupid, species == "Thraupis sayaca")
T_palmarum <- subset(Thraupid, species == "Thraupis palmarum")
T_seledon <- subset(Tangara, species == "Tangara seledon")
T_cyanocephala <- subset(Tangara, species == "Tangara cyanocephala")

#Thraupis sayaca
T.say <-effectiveness_plot(q1=T_sayaca$QTY, q2=T_sayaca$energy_fruit, 
                           q1.error=T_sayaca$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_sayaca$plant_family, label=T_sayaca$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.say+geom_point(shape=21, colour = "black", size=3) #Including black countour to points

#Thraupis palmarum
T.pal <-effectiveness_plot(q1=T_palmarum$QTY, q2=T_palmarum$energy_fruit, 
                           q1.error=T_palmarum$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_palmarum$plant_family, label=T_palmarum$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.pal+geom_point(shape=21, colour = "black", size=3) #Including black countour to points

#Tangara spp.
Tang <-effectiveness_plot(q1=Tangara$QTY, q2=Tangara$energy_fruit, 
                          q1.error=Tangara$QTY_SE,q2.error = NULL,italic=T,
                          pts.shape=Tangara$species, label=Tangara$plant, nlines = 7,
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Tang+geom_point(aes(shape = factor(Tangara$species), color=factor(Tangara$plant_family)), size=2.2)

#Tangara cyanocephala
T.cya <-effectiveness_plot(q1=T_cyanocephala$QTY, q2=T_cyanocephala$energy_fruit, 
                           q1.error=T_cyanocephala$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_cyanocephala$plant_family, label=T_cyanocephala$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.cya+geom_point(shape=21, colour = "black", size=3) #Including black countour to points

#Tangara seledon
T.sel <-effectiveness_plot(q1=T_seledon$QTY, q2=T_seledon$energy_fruit, 
                           q1.error=T_seledon$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=T_seledon$plant_family, label=T_seledon$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
T.sel+geom_point(shape=21, colour = "black", size=3) #Including black countour to points




###FRINGILLIDAE
Euphonia <- read_delim("Fringillidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)
E_viola <- subset(Euphonia, species == "Euphonia violacea")
E_chloro <- subset(Euphonia, species == "Euphonia chlorotica")

#Euphonia spp.
Eupho <-effectiveness_plot(q1=Euphonia$QTY, q2=Euphonia$energy_fruit, 
                          q1.error=Euphonia$QTY_SE,q2.error = NULL,italic=T,
                          pts.shape=Euphonia$species, label=Euphonia$plant, nlines = 9,lines.breaks = c(3,7,29,43,61,81,106,136,182,231),
                          lines.color = "light grey", pts.size=3,
                          myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
Eupho+geom_point(aes(shape = factor(Euphonia$species), color=factor(Euphonia$plant_family)), size=2.2)

#Euphonia cholorotica
E.chl <-effectiveness_plot(q1=E_chloro$QTY, q2=E_chloro$energy_fruit, 
                           q1.error=E_chloro$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=E_chloro$plant_family, label=E_chloro$plant, nlines = 9, lines.breaks=c(2,7,29,43,61,81,106,136,182,231),
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
E.chl+geom_point(shape=21, colour = "black", size=3) #Including black countour to points

#Euphonia violacea
E.vio <-effectiveness_plot(q1=E_viola$QTY, q2=E_viola$energy_fruit, 
                           q1.error=E_viola$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=E_viola$plant_family, label=E_viola$plant, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
E.vio+geom_point(shape=21, colour = "black", size=3) #Including black countour to points



###ALL SPECIES TOGETHER
frug <- read.csv("all_frug.csv", sep=";", header = TRUE)
names(frug)
#Spp not taken: 1,7,12,15,16,18,19,21,22,25,27,32,37,38 - because few data (low n), or not fidable
#Acnistus arborescens, Cinnamomum triplinerve, Eugenia umbelliflora,Ficus benjamina, Ficus insipida
#Miconia prasina, Miconia pusilliflora, Myrsine gardneriana, Myrsine umbellata, Phoradendron affine,
#Phorandendron piperoides, Sorocea ilicifolia, Virola sebifera,Vitex polygamia
myspp <- c(2,3,4,5,6,8,9,10,11,13,14,17,20,23,24,26,28,29,30,31,33,34,35,36)
frug2<- frug[myspp,]

All <-effectiveness_plot(q1=frug2$QTY_mean, q2=frug2$energy_fruit, 
                           q1.error=frug2$QTY_SE,q2.error = NULL,italic=T,
                           pts.color=frug2$plant_familiy, label=frug2$plant_species, nlines = 7,
                           lines.color = "light grey", pts.size=3,
                           myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
All+geom_point(shape=21, colour = "black", size=3) #Including black countour to points
        

###DIGESTIBILITY
digest <- read.csv("Digestibility.csv", sep=";", header=T)

dg <-effectiveness_plot(q1=digest$QTY, q2=digest$QLY_digest, 
                         q1.error=digest$QTY_SE,q2.error = digest$QLY_SE,italic=T,
                         pts.shape=digest$species, label=digest$plant, nlines = 7,
                         lines.color = "light grey", pts.size=3,
                         myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
dg+geom_point(aes(shape = factor(digest$species), color=factor(digest$plant_family)), size=2.2)

#Dotplot
ggplot(digest, aes(x=digest_data, y=TE, fill=species))+geom_dotplot(binaxis = "y", stackdir = "center", size=0.8, show.legend=T)
