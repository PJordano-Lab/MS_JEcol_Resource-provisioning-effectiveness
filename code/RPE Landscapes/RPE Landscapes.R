library(readr)
library(effect.lndscp)
library(devtools)
library(ggplot2)
library(ggrepel)

#RPE FOR CRACIDS
Cracidae <- read_delim("Cracidae.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                      plant_family = col_factor(levels = c()), 
                      species = col_factor(levels = c())), trim_ws = TRUE)
str(Cracidae)
names(Cracidae)

#RPE landscape using fruits/visit and energy/visit - MISSING DATA FOR VISIT FREQ
RPE_Cracidae <- effectiveness_plot(Cracidae$fruit_visit, Cracidae$energy_visit, , 
                                   Cracidae$plant, 10, 
                                   myxlab= "Qty component - feeding frequency (fruits/visit)", 
                                   myylab= "Qly component - energy (KJ/visit)")
RPE_Cracidae + geom_errorbar(aes(ymin= Cracidae$energy_visit - Cracidae$energy_visit_SE, ymax=Cracidae$energy_visit + Cracidae$energy_visit_SE), width=.02)+ 
              geom_errorbarh(aes(xmin = Cracidae$fruit_visit - Cracidae$fruit_visit_SE, xmax = Cracidae$fruit_visit + Cracidae$fruit_visit_SE), height=0.3)+ 
              geom_point(aes(colour = factor(Cracidae$species)), size=3)

#RPE landscape with normal definition QTY and QLY - only for Euterpe, Cecropia and Virola
Cracid <- subset(Cracidae, Cracidae$QTY > 0)
Cracid
RPE2_Cracidae <- effectiveness_plot(Cracid$QTY, Cracid$energy_fruit, , 
                                    Cracid$plant, 10, 
                                    myxlab= "Qty component - feeding frequency (fruits/h)", 
                                    myylab= "Qly component - energy (KJ/fruit)")
RPE2_Cracidae + geom_errorbarh(aes(xmin = Cracid$QTY - Cracid$QTY_SE,xmax = Cracid$QTY + Cracid$QTY_SE), height=0.3)+ 
                geom_point(aes(colour = factor(Cracid$species)), size=3)







#RPE FOR TOUCANS
Toucans <- read_delim("Toucans.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                                        plant_family = col_factor(levels = c()), 
                                        species = col_factor(levels = c())), trim_ws = TRUE)

#Subset for each toucan species
Baillonus <- subset(Toucans, species == "Baillonius bailloni")
R_dicol <- subset(Toucans, species == "Ramphastos dicolorus")
R_vitel <- subset(Toucans, species == "Ramphastos vitellinus")
Selenidera <- subset(Toucans, species == "Selenidera maculirostris")

#RPE for all Toucans together
RPE_toucans <- effectiveness_plot(Toucans$QTY, Toucans$energy_fruit,, 
                                  Toucans$plant, 10, 
                                  myxlab= "Qty component - feeding frequency (fruits/h)", 
                                  myylab= "Qly component - energy (KJ/fruit)")
RPE_toucans +   geom_errorbarh(aes(xmin = Toucans$QTY - Toucans$QTY_SE, xmax = Toucans$QTY + Toucans$QTY_SE), height=0.3)+
  geom_point(aes(colour = factor(Toucans$species)), size=3)

#RPE landscape for Baillonius bailloni
RPE_Baillonus <- effectiveness_eq(Baillonus$QTY, Baillonus$energy_fruit, 
                                     Baillonus$plant_family, 
                                     Baillonus$plant, 10, 
                                     myxlab= "Qty component - feeding frequency (fruits/h)", 
                                     myylab= "Qly component - energy (KJ/fruit)")
RPE_Baillonus + geom_errorbarh(aes(xmin = Baillonus$QTY - Baillonus$QTY_SD, 
                               xmax = Baillonus$QTY + Baillonus$QTY_SD), height=0.3) + geom_point(aes(colour = factor(Baillonus$plant_family)), size=3)


#RPE landscape for Ramphastos dicolorus
RPE_R_dicol <- effectiveness_eq(R_dicol$QTY, R_dicol$energy_fruit, , 
                                    R_dicol$plant, 10, 
                                    myxlab= "Qty component - feeding frequency (fruits/h)", 
                                    myylab= "Qly component - energy (KJ/fruit)")
RPE_R_dicol + geom_errorbarh(aes(xmin = R_dicol$QTY - R_dicol$QTY_SD, 
                              xmax = R_dicol$QTY + R_dicol$QTY_SD), height=0.3) + geom_point(aes(colour = factor(R_dicol$plant_family)), size=3)


#RPE landscape for Ramphastos vitellinus
RPE_R_vitel <- effectiveness_eq(R_vitel$QTY, R_vitel$energy_fruit, 
                                  R_vitel$plant_family, 
                                  R_vitel$plant, 10, 
                                  myxlab= "Qty component - feeding frequency (fruits/h)", 
                                  myylab= "Qly component - energy (KJ/fruit)")
RPE_R_vitel + geom_errorbarh(aes(xmin = R_vitel$QTY - R_vitel$QTY_SD, 
                            xmax = R_vitel$QTY + R_vitel$QTY_SD), height=0.3) + geom_point(aes(colour = factor(R_vitel$plant_family)), size=3)



#RPE landscape for Selenidera maculirostris
RPE_Selenidera <- effectiveness_eq(Selenidera$QTY, Selenidera$energy_fruit, 
                                  Selenidera$plant_family, 
                                  Selenidera$plant, 10, 
                                  myxlab= "Qty component - feeding frequency (fruits/h)", 
                                  myylab= "Qly component - energy (KJ/fruit)")
RPE_Selenidera + geom_errorbarh(aes(xmin = Selenidera$QTY - Selenidera$QTY_SD, 
                            xmax = Selenidera$QTY + Selenidera$QTY_SD), height=0.3)+ geom_point(aes(colour = factor(Selenidera$plant_family)), size=3)










#RPE FOR THRAUPIDS
Thraupid <- read_delim("Thraupidae.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                      plant_family = col_factor(levels = c())), trim_ws = TRUE)

#Subset for each species
T_sayaca <- subset(Thraupid, species == "Thraupis sayaca")
T_palmarum <- subset(Thraupid, species == "Thraupis palmarum")

#RPE landscape using QLY (extracted with frug mass capacity)
RPE_T_sayaca <- effectiveness_eq(T_sayaca$QTY, T_sayaca$energy_fruit, 
                                   T_sayaca$plant_family, 
                                   T_sayaca$plant, 10, 
                                   myxlab= "Qty component - feeding frequency (fruits/h)", 
                                   myylab= "Qly component - energy (KJ/fruit)")
RPE_T_sayaca + geom_errorbarh(aes(xmin = T_sayaca$QTY - T_sayaca$QTY_SD, 
                             xmax = T_sayaca$QTY + T_sayaca$QTY_SD), height=0.2)+ 
                geom_point(aes(colour = factor(T_sayaca$plant_family)), size=3)



RPE_T_palmarum <- effectiveness_eq(T_palmarum$QTY, T_palmarum$energy_fruit, 
                                     T_palmarum$plant_family, 
                                     T_palmarum$plant, 10, 
                                     myxlab= "Qty component - feeding frequency (fruits/h)", 
                                     myylab= "Qly component - energy (KJ/fruit)")
RPE_T_palmarum + geom_errorbarh(aes(xmin = T_palmarum$QTY - T_palmarum$QTY_SD, 
                              xmax = T_palmarum$QTY + T_palmarum$QTY_SD), height=0.3)+ geom_point(aes(colour = factor(T_palmarum$plant_family)), size=3)



#RPE FOR TURDIDAE
Turdus <- read_delim("Turdidae.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                      plant_family = col_factor(levels = c()), 
                      species = col_factor(levels = c())), trim_ws = TRUE)

#Turdus rufiventris
T_rufiventris <- subset(Turdus, species == "Turdus rufiventris")
RPE_T_rufiventris<- effectiveness_eq(T_rufiventris$QTY, T_rufiventris$energy_fruit, 
                                     T_rufiventris$plant_family, 
                                     T_rufiventris$plant, 10, 
                                     myxlab= "Qty component - feeding frequency (fruits/h)", 
                                     myylab= "Qly component - energy (KJ/fruit)")
RPE_T_rufiventris + geom_errorbarh(aes(xmin = T_rufiventris$QTY - T_rufiventris$QTY_SD, 
                              xmax = T_rufiventris$QTY + T_rufiventris$QTY_SD), height=0.3)+
                          geom_point(aes(colour = factor(T_rufiventris$plant_family)), size=3)


#Turdus albicollis
T_albicollis <- subset(Turdus, species == "Turdus albicollis")
RPE_T_albicollis<- effectiveness_eq(T_albicollis$QTY, T_albicollis$energy_fruit, 
                                       T_albicollis$plant_family, 
                                       T_albicollis$plant, 10, 
                                       myxlab= "Qty component - feeding frequency (fruits/h)", 
                                       myylab= "Qly component - energy (KJ/fruit)")
RPE_T_albicollis + geom_errorbarh(aes(xmin = T_albicollis$QTY - T_albicollis$QTY_SD, 
                                  xmax = T_albicollis$QTY + T_albicollis$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(T_albicollis$plant_family)), size=3)
#+ geom_errorbar(aes(ymin= T_albicollis$QLY - T_albicollis$QLY_SD, ymax=T_albicollis$QLY + T_albicollis$QLY_SD), width=.02) 


#RPE FOR TANGARAS
Tangara <- read_delim("Tangaras.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                      plant_family = col_factor(levels = c()), 
                      species = col_factor(levels = c())), trim_ws = TRUE)

#Tangara cyanocephala
T_cyanocephala <- subset(Tangara, species == "Tangara cyanocephala")
RPE_T_cyanocephala<- effectiveness_eq(T_cyanocephala$QTY, T_cyanocephala$energy_fruit, 
                                       T_cyanocephala$plant_family, 
                                       T_cyanocephala$plant, 10, 
                                       myxlab= "Qty component - feeding frequency (fruits/h)", 
                                       myylab= "Qly component - energy (KJ/fruit)")
RPE_T_cyanocephala + geom_errorbarh(aes(xmin = T_cyanocephala$QTY - T_cyanocephala$QTY_SD, 
                                  xmax = T_cyanocephala$QTY + T_cyanocephala$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(T_cyanocephala$plant_family)), size=3)


#Tangara seledon
T_seledon <- subset(Tangara, species == "Tangara seledon")
RPE_T_seledon<- effectiveness_eq(T_seledon$QTY, T_seledon$energy_fruit, 
                                      T_seledon$plant_family, 
                                      T_seledon$plant, 10, 
                                      myxlab= "Qty component - feeding frequency (fruits/h)", 
                                      myylab= "Qly component - energy (KJ/fruit)")
RPE_T_seledon + geom_errorbarh(aes(xmin = T_seledon$QTY - T_seledon$QTY_SD, 
                                                                 xmax = T_seledon$QTY + T_seledon$QTY_SD), height=0.3) +
  geom_point(aes(colour = factor(T_seledon$plant_family)), size=3)







#RPE FOR COTINGIDAE
Cotingidae <- read_delim("Cotingidae.csv", ";", escape_double = FALSE, 
                      col_types = cols(plant = col_factor(levels = c()), 
                      plant_family = col_factor(levels = c()), 
                      species = col_factor(levels = c())), trim_ws = TRUE)

#Tityra cayana
Tit_cayana <- subset(Cotingidae, species == "Tityra cayana")
RPE_Tit_cayana<- effectiveness_eq(Tit_cayana$QTY, Tit_cayana$energy_fruit, 
                                        Tit_cayana$plant_family, 
                                        Tit_cayana$plant, 10, 
                                        myxlab= "Qty component - feeding frequency (fruits/h)", 
                                        myylab= "Qly component - energy (KJ/fruit)")
RPE_Tit_cayana + geom_errorbarh(aes(xmin = Tit_cayana$QTY - Tit_cayana$QTY_SD, 
                                    xmax = Tit_cayana$QTY + Tit_cayana$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(Tit_cayana$plant_family)), size=3)


#Procnias nudicollis
P_nudicollis <- subset(Cotingidae, species == "Procnias nudicollis")
RPE_P_nudicollis<- effectiveness_eq(P_nudicollis$QTY, P_nudicollis$energy_fruit, 
                                   P_nudicollis$plant_family, 
                                   P_nudicollis$plant, 10, 
                                   myxlab= "Qty component - feeding frequency (fruits/h)", 
                                   myylab= "Qly component - energy (KJ/fruit)")
RPE_P_nudicollis + geom_errorbarh(aes(xmin = P_nudicollis$QTY - P_nudicollis$QTY_SD, 
                              xmax = P_nudicollis$QTY + P_nudicollis$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(P_nudicollis$plant_family)), size=3)




#RPE FOR EUPHONIAS
Euphonia <- read_delim("Fringillidae.csv", ";", escape_double = FALSE, 
                        col_types = cols(plant = col_factor(levels = c()), 
                        plant_family = col_factor(levels = c()), 
                        species = col_factor(levels = c())), trim_ws = TRUE)

#Euphonia cholorotica
E_chloro <- subset(Euphonia, species == "Euphonia chlorotica")
RPE_E_chloro<- effectiveness_eq(E_chloro$QTY, E_chloro$energy_fruit, 
                                    E_chloro$plant_family, 
                                    E_chloro$plant, 10, 
                                    myxlab= "Qty component - feeding frequency (fruits/h)", 
                                    myylab= "Qly component - energy (KJ/fruit)")
RPE_E_chloro+ geom_errorbarh(aes(xmin = E_chloro$QTY - E_chloro$QTY_SD,xmax = E_chloro$QTY + E_chloro$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(E_chloro$plant_family)), size=3)

#Euphonia violacea
E_viola <- subset(Euphonia, species == "Euphonia violacea")
RPE_E_viola<- effectiveness_eq(E_viola$QTY, E_viola$energy_fruit, 
                                      E_viola$plant_family, 
                                      E_viola$plant, 10, 
                                      myxlab= "Qty component - feeding frequency (fruits/h)", 
                                      myylab= "Qly component - energy (KJ/fruit)")
RPE_E_viola  + geom_errorbarh(aes(xmin = E_viola$QTY - E_viola$QTY_SD,xmax = E_viola$QTY + E_viola$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(E_viola$plant_family)), size=3)




#RPE FOR TROGONS
Trogon <- read_delim("Trogonidae.csv", ";", escape_double = FALSE, 
                       col_types = cols(plant = col_factor(levels = c()), 
                       plant_family = col_factor(levels = c()), 
                       species = col_factor(levels = c())), trim_ws = TRUE)

#Trogon surrucura
T_surrucura <- subset(Trogon, species == "Trogon surrucura")
T_surrucura <- subset(T_surrucura, QTY>0)
RPE_T_surrucura<- effectiveness_eq(T_surrucura$QTY, T_surrucura$energy_fruit, 
                                  T_surrucura$plant_family, 
                                  T_surrucura$plant, 10, 
                                  myxlab= "Qty component - feeding frequency (fruits/h)", 
                                  myylab= "Qly component - energy (KJ/fruit)")
RPE_T_surrucura + geom_errorbarh(aes(xmin = T_surrucura$QTY - T_surrucura$QTY_SD, 
                                                             xmax = T_surrucura$QTY + T_surrucura$QTY_SD), height=0.3)+
  geom_point(aes(colour = factor(T_surrucura$plant_family)), size=3)


#Trogon viridis
T_viridis <- subset(Trogon, species == "Trogon viridis")
RPE_T_viridis<- effectiveness_eq(T_viridis$QTY, T_viridis$energy_fruit, ,
                                 T_viridis$plant, 10, 
                                 myxlab= "Qty component - feeding frequency (fruits/h)", 
                                 myylab= "Qly component - energy (KJ/fruit)")
RPE_T_viridis + geom_errorbarh(aes(xmin = T_viridis$QTY - T_viridis$QTY_SD, 
                                                            xmax = T_viridis$QTY + T_viridis$QTY_SD), height=0.3)+  
      geom_point(aes(colour = factor(T_viridis$plant_family)), size=3)
