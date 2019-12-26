#Here I want to plot a new effectiveness landscape where I redifine QTY and QLY
#Now QTY= visits/h*fruits/visit*PDM=PDM/h
#New QLY=specific_energy/grams of PDM
#RPE values will remain same, however the location in the landscape will change.
library(effect.lndscp) 
library(devtools)
library(ggplot2)
library(ggrepel)

frug_S <- read.csv("code/RPE_Landscapes/all_frug_size_revised.csv", sep=";", header = TRUE)
names(frug_S)
#Spp not taken: 1,7,12,15,16,18,19,21,22,25,27,32,37,38 - because few data (low n), or not fidable
#Acnistus arborescens, Cinnamomum triplinerve, Eugenia umbelliflora,Ficus benjamina, Ficus insipida
#Miconia prasina, Miconia pusilliflora, Myrsine gardneriana, Myrsine umbellata, Phoradendron affine,
#Phorandendron crassifolium, Sorocea ilicifolia, Virola sebifera,Vitex polygamia
myspp <- c(2,3,4,5,6,8,9,10,11,13,14,17,20,23,24,27,28,29,30,31,33,34,35,36)
frug2_S<- frug_S[myspp,]
newqty <- frug2_S$QTY_mean*frug2_S$PDM
newqly <- frug2_S$energy_fruit/frug2_S$PDM
newrpe<- cbind(frug2_S,newqty,newqly)

newlds <-effectiveness_plot(q1=newrpe$newqty, q2=newrpe$newqly, 
                         label=newrpe$plant_species, nlines = 7,
                         lines.color = "light grey",pts.size=0.1,
                         myxlab= "QTY component - feeding frequency (PDM/h)", myylab= "QLY component - energy (KJ/g of PDM)")
newlds+geom_point(aes(size=newrpe$PDM, fill=newrpe$PDM), shape=21) +
  scale_fill_gradient2(low = "#edf8b1",mid='#7fcdbb', high = "#2c7fb8", midpoint = 0.7, guide="legend",limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))+
  scale_size_continuous(guide="legend", limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))


#COMPARISION WITH USED RPE
All <-effectiveness_plot(q1=frug2_S$QTY_mean, q2=frug2_S$energy_fruit, 
                         q1.error=frug2_S$QTY_SE,q2.error =frug2_S$QLY_SE,italic=T,
                         label=frug2_S$plant_species, nlines = 7,
                         lines.color = "light grey",pts.size=0.1,
                         myxlab= "QTY component - feeding frequency (fruits/h)", myylab= "QLY component - energy (KJ/fruit)")
All+geom_point(aes(size=frug2_S$PDM, fill=frug2_S$PDM), shape=21) +
  scale_fill_gradient2(low = "#edf8b1",mid='#7fcdbb', high = "#2c7fb8", midpoint = 0.7, guide="legend",limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))+
  scale_size_continuous(guide="legend", limits=c(0, 1.2), breaks=seq(0, 1.2, by=0.3))

par(mfrow=c(2,2))
