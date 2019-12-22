#KENDALL TEST
#Include data
toteff <- read.csv("Kendall_test.csv", sep=";", header=T )
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
tot<-read.csv("Total effects all obs.csv", sep=";", header=T) #Data for all interactions (no means)
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
tot2<-read.csv("Total effects means.csv", sep=";", header=T) 
#Remove species with only one observation: Cinnamomum triplinerve, Ficus insipida, Miconia pusilliflora, 
#Ocotea pulchella, Phoradendron crassifolium, Sorocea ilicifolia and Vitex polygama
spp2<-c(1,28,57,59,81,88,129)
tot2 <- tot2 [-spp2,]

ggplot(tot2, aes(TE_common, TE)) + geom_point() + geom_smooth(color="red", method="loess") +
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

cor.test(tot2$TE_common, tot2$TE)
cor.test(log(tot2$TE_common), log(tot2$TE))

#Boxplot without Cabralea canjerana outlier for Tityra cayana
tot2<-read.csv("Total effects means.csv", sep=";", header=T) 
tot3 <- tot2 [-c(1,28,57,59,81,88,129,145),]
boxplot(tot3$TE~tot3$plant)






#PHYLOGENETIC SIGNAL TEST
library(ape)
library(lattice)
library(permute)
library(nlme)
library(vegan)
library(picante)
library(phylosignal)

#INCLUDE TREE
tree.tr <- read.nexus("treedumm.nex")
plot.phylo(tree.tr, cex=0.75)
#Inlude 3 trees with solved politomies for Myrsine spp. with all the combinations and Virola sebifera and oleifera rerooted as dichomotie
tree.1 <- read.nexus("treedumm_mcoriacea.nex")
tree.2 <- read.nexus("treedumm_mgarneriana.nex")
tree.3 <- read.nexus("treedumm_mumbellata.nex")
plot.phylo(tree.1, cex=0.75)

#calculate branch lenght for the tree (neccessary to run the analysis)
#tree_lenght2<-compute.brlen(tree.tr, 1) #method=giving a branch lenght of 1
tree_lenght <-compute.brlen(tree.tr) #method=Graphen by default
tree_1 <-compute.brlen(tree.1)
tree_2 <-compute.brlen(tree.2)
tree_3 <-compute.brlen(tree.3)

#INCLUDE TRAIT DATA
traits<-read.csv("traits.csv", sep=";", header=T)
head(traits)
#Convert trait data to readable format - create data frame with row names as species
trait.tr <- data.frame(row.names=1, traits)
trait.tr
#trait <- traits[tree.tr$tip.label, ] #Other way
#match.phylo.data(tree_1, trait) #Anotehr way to match the data

#Vector for total effect values and fruit_energy
TE <- df2vec(trait.tr, "TE")
fruit_energy <- df2vec(trait.tr, "energy_fruit")
QTY <- df2vec(trait.tr, "QTY_mean")
s_energy <- df2vec(trait.tr, "s_energy")
PDM <- df2vec(trait.tr, "PDM")
water <- df2vec(trait.tr, "WATER")
lipid <- df2vec(trait.tr, "LIP")
prot <- df2vec(trait.tr, "PRO")


#Calculates Blomberg's K value for each vector
Kcalc(TE, tree_lenght, checkdata=TRUE)
Kcalc(TE, tree_1, checkdata=TRUE)
Kcalc(TE, tree_2, checkdata=TRUE)
Kcalc(TE, tree_3, checkdata=TRUE)
Kcalc(fruit_energy, tree_lenght, checkdata=TRUE)
Kcalc(QTY, tree_lenght, checkdata=TRUE)
Kcalc(s_energy, tree_lenght, checkdata=TRUE)
Kcalc(water, tree_lenght, checkdata=TRUE) #Problem with NAs
Kcalc(lipid, tree_lenght, checkdata=TRUE) #Problem with NAs
Kcalc(prot, tree_lenght, checkdata=TRUE) #Problem with NAs

#Calculate for all factors together - also obtain p-value
traitmulti <- data.frame(row.names=1, traits[,c(1,4:9)]) #Create data frame for the traits withouth plant_family or variables with NAs (water, lip...)
traitmulti <-as.matrix(traitmulti) #Convert to a matrix
multiPhylosignal(traitmulti, tree_1, checkdata=T)
multiPhylosignal(traitmulti, tree_2, checkdata=T)
multiPhylosignal(traitmulti, tree_3, checkdata=T)


#PHYLOSIGNAL - using Phylosignal package which also gives lambda and accepts politomie
#First I read the tree with picante to add branch lenght and create a new nexux file with info for the branch lenghts
phylotree <- read.nexus("myfavtree.nex")#conserved politomy of Myrsine and polytomy Virola solved
phylotree_lenght <-compute.brlen(tree.tr) #method=Graphen by default
plot(phylotree_lenght) 
write.nexus(phylotree_lenght, file="phylotree.nex")


#create a phylo4d (i.e. phylogenetic tree stored in a nexus file and tips data stored from txt file). 
#They must conincide with same number and same species and cannot have NAs data
phylotree <-read.p4d(phylo.file="phylotree.nex", data.file="phylotraits.txt", phylo.format="nexus", data.format = "table")
phylotree
a<-phyloSignal(phylotree)
a

