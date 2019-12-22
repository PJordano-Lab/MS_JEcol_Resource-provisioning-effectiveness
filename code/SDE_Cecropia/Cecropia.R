library(readr)
library(effect.lndscp)
library(devtools)
library(ggplot2)
library(ggrepel)

#Include the data
Cecropia <- read_delim("Cecropia.csv", 
                       ";", escape_double = FALSE, col_types = cols(species = col_factor(levels = c()), 
                       fam = col_factor(levels = c()), ord = col_factor(levels = c())), 
                       trim_ws = TRUE)
str(Cecropia)

#Seed Dispersal effectiveness Landscape with Cecropia Quantity component
rpec <- effectiveness_plot(Cecropia$visit_rate, Cecropia$bic_visit, Cecropia$fam, 
                                   Cecropia$species, 10, 
                                   myxlab= "Visitation frequency", 
                                   myylab= "Bites per visit")
rpec




#Including error bars
rpec_errorbar <- rpec + geom_errorbar(aes(ymin= Cecropia$bic_visit - Cecropia$bic_visit_SE, ymax=Cecropia$bic_visit + Cecropia$bic_visit_SE), 
                             width=.005) + geom_errorbarh(aes(xmin = Cecropia$visit_rate - Cecropia$visit_rate_SE, 
                             xmax = Cecropia$visit_rate + Cecropia$visit_rate_SE), height=0.2)
rpec_errorbar
#Missing error bars when go out of y-axis limit





#Including legend with colours for families
rpec_errorbar_colour <- rpec_errorbar + geom_point(aes(colour = factor(Cecropia$fam)), size=3) 
rpec_errorbar_colour
#OR without error bars and without underlying points
rpec2 <- effectiveness_plot(Cecropia$visit_rate, Cecropia$bic_visit, , 
                           Cecropia$species, 10, 
                           myxlab= "Visitation frequency", 
                           myylab= "Bites per visit")
rpec2 + geom_point(aes(colour = factor(Cecropia$fam)), size=3)

#Inluding shapes for the families
familyshape <- factor(Cecropia$fam) #setting the number of shapes upper than 6 by default
rpec_shape <- rpec2  + geom_point(aes(shape = factor(Cecropia$fam)), size=3) + scale_shape_manual(values=1:nlevels(familyshape))
rpec_shape




#Including number for the families
rpec_errorbar_numbers <- rpec2  + geom_point(aes(shape = factor(Cecropia$fam)), size=3) + scale_shape_manual(values=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13","14"))
rpec_errorbar_numbers #problem after 9, only considers one caracter




#Trying to include number as labels for the points and then include a legend with the species name for each number... UNFINISHED
rpec3 <- effectiveness_plot(Cecropia$visit_rate, Cecropia$bic_visit, , , 10, 
                           myxlab= "Visitation frequency", 
                           myylab= "Bites per visit")
rpec_labels <- rpec3  + geom_point() + geom_text(aes(label=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13","14", "15", "16", "17", "18","19", "20", "21", "22", "23","24","25", "26", "27", "28","29","30","31","32","33", "34", "35", "36","37","38","39","40","41", "42", "43")), hjust=-0.5, vjust=-0.5)
rpec_labels






effectiveness_plot(Cecropia$visit_rate, Cecropia$bic_visit, q1.error = Cecropia$visit_rate_SE, q2.error= Cecropia$bic_visit_SE, 
                   pts.color=Cecropia$fam,label = Cecropia$species, 
                   myxlab= "Visitation frequency", 
                   myylab= "Bites per visit")
