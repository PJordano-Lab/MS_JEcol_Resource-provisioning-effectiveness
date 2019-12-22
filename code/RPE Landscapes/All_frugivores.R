library(readr)
library(effect.lndscp)
library(devtools)
library(ggplot2)
library(ggrepel)

#RPE FOR frug FRUGIVORES
frug <- read_delim("all_frug.csv", ";", escape_double = FALSE, col_types = cols(plant_family = col_factor(levels = c()), 
                                                                               species = col_factor(levels = c())),  trim_ws = TRUE)
#RPE landscape +- Standard error
RPE_frug <- effectiveness_eq(frug$QTY, frug$energy_fruit, ,
                                   frug$species,10, 
                                   myxlab= "Qty component (fruits/hour)", 
                                   myylab= "Qly component (KJ/fruit)")
RPE_frug + geom_errorbarh(aes(xmin = frug$QTY - frug$QTY_SE, xmax = frug$QTY + frug$QTY_SE), height=0.3)+
  geom_point(aes(colour = factor(frug$plant_family)), size=3)

#+ geom_errorbar(aes(ymin= frug$QLY - frug$QLY_SE, ymax=frug$QLY + frug$QLY_SE), width=.02)+ 
