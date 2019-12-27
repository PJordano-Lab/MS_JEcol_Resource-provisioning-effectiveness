#PHYLOGENETIC SIGNAL TEST
library(ape)
library(lattice)
library(permute)
library(nlme)
library(vegan)
library(picante)
library(phylosignal)

#PHYLOSIGNAL - using Phylosignal package which also gives lambda and accepts politomie
#First I read the tree with picante to add branch lenght and create a new nexux file with info for the branch lenghts

phylotree <- read.nexus("code/Phylogeny/myfavtree.nex")#conserved politomy of Myrsine and polytomy Virola solved
phylotree_lenght <-compute.brlen(phylotree) #method=Graphen by default
plot(phylotree_lenght) 
write.nexus(phylotree_lenght, file="code/Phylogeny/phylotree.nex")


#create a phylo4d (i.e. phylogenetic tree stored in a nexus file and tips data stored from txt file). 
#They must conincide with same number and same species and cannot have NAs data
phylotree <-read.p4d(phylo.file="code/Phylogeny/phylotree.nex", data.file="code/Phylogeny/phylotraits.txt", phylo.format="nexus", data.format = "table")
phylotree
a<-phyloSignal(phylotree)
a

