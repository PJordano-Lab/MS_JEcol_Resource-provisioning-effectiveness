#KENDALL TEST
#Include data
toteff <- read.csv("code/Correlation RPEs/Kendall_test.csv", sep=";", header=T )
names(toteff)
#subset data to continuos variables and without 2 species not used
myvars <- names(toteff) %in% c("plant_species", "plant_familiy", "QTY_mean", "energy_fruit", "TE2", "Euphonia_pectoralis", "Tangara_cayana") 
newdata <- toteff[!myvars]
names(newdata)
#correlation between variables
cor(newdata, method="kendall", use= "pairwise.complete.obs")
cor(newdata$TE, newdata[,2:19], use= "pairwise.complete.obs", method = "kendall")


#CORRELATION TEST
#Correlation between total effects
tot<-read.csv("code/Correlation RPEs/Total effects all obs.csv", sep=";", header=T) #Data for all interactions (no means)
str(tot)

#Correlation between subcomponents
cor.test(tot$QTY, tot$energy_fruit, use= "pairwise.complete.obs", method = "pearson")
a <- lm (QTY~energy_fruit, data=tot)
summary(a)

#Remove interactions with no Total effect value [1:238,] +
#Remove species with only one observation: Cinnamomum triplinerve, Ficus insipida, Miconia pusilliflora, 
#Ocotea pulchella, Phoradendron crassifolium, Sorocea ilicifolia and Vitex polygama
spp<-c(1:238,239,243,266,287,304,362,485)
tot <- tot [-spp,]

library(ggplot2)
ggplot(tot, aes(TE_plant, TE) ) + geom_point() + geom_smooth() +
  scale_x_log10() + scale_y_log10()

plot(tot$TE_plant, tot$TE)
cor.test(tot$TE_plant, tot$TE)
cor.test(log(tot$TE_plant), log(tot$TE))

a<-lm(tot$TE_plant~tot$TE)
summary(a)
abline(a)

ggplot(tot, aes(TE_plant, TE)) + geom_point() + geom_smooth(color="red", method="loess") +
  scale_x_log10(breaks=c(0.01, 0.1, 1, 10)) + scale_y_log10(breaks=c(0.01, 0.1, 1, 10))+
  theme(axis.text = element_text(size = rel(0.8)), 
        axis.ticks = element_line(colour = "black"), 
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        panel.grid.major = element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor = element_line(colour = "grey98", size = 0.4), 
        strip.background = element_rect(fill = "grey80", 
                                        colour = "grey50", size = 0.2))+
  labs(x = "Log of mean plant TE", y="Log of species-specific TE")


#Same but with data for mean value of each species-specific interaction (e.g. only one value for Aburria-Euterpe)
tot2<-read.csv("code/Correlation RPEs/Total effects means.csv", sep=";", header=T) 
#Remove species with only one observation: Cinnamomum triplinerve, Ficus insipida, Miconia pusilliflora, 
#Ocotea pulchella, Phoradendron crassifolium, Sorocea ilicifolia and Vitex polygama
spp2<-c(1,28,57,59,81,88,129)
tot2 <- tot2 [-spp2,]
rownames(tot2) <- NULL #restart numeration in the new dataset  

#We did this analysis including two especies that are not being accounted in the manuscript: "Euphonia_pectoralis", "Tangara_cayana"
#Here we remove the 2 frugivore spp and the plants with one replicate.
tot2<-read.csv("code/Correlation RPEs/Total effects means.csv", sep=";", header=T) 
spp3<-c(1,19,24,28,29,33,37,52,57,58,59,68,71,80,81,87,88,123,129,130,161,162)
tot3 <-tot2 [-spp3,]
rownames(tot3) <- NULL

ggplot(tot3, aes(TE_common, TE)) + geom_point() + geom_smooth(color="red", method="loess") +
  scale_x_log10(breaks=c(0.01, 0.1, 1, 10)) + scale_y_log10(breaks=c(0.01, 0.1, 1, 10))+
  theme(axis.text = element_text(size = rel(0.8)), 
        axis.ticks = element_line(colour = "black"), 
        panel.background = element_rect(fill = "white", colour = NA), 
        panel.border = element_rect(fill = NA, colour = "black"), 
        panel.grid.major = element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor = element_line(colour = "grey98", size = 0.4), 
        strip.background = element_rect(fill = "grey80", 
                                        colour = "grey50", size = 0.2))+
  labs(x = "Log of mean plant TE", y="Log of species-specific TE")

cor.test(tot3$TE_common, tot3$TE)
cor.test(log(tot3$TE_common), log(tot3$TE))

#ggsave("MS/revision/Fig 4. Correlation mean plot.pdf", plot=last_plot(), units = "in", width = 10, height = 7)
