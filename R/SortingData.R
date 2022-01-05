#import data & explore
library(tidyverse)
library(readxl)
TOVE_Points_2006_2020 <- read_excel("data/TOVE_Points_2006_2020.xlsx")
head(TOVE_Points_2006_2020) %>% view()

#convert to sites on left and species on top

TOVE_points<-TOVE_Points_2006_2020 %>% 
  pivot_wider(id_cols=c(RouteID,PointID,YEAR),names_from=ScientificName, values_from=Count) %>% 
  replace(is.na(.), 0)

TOVE_points %>% 
  summarise(across(4:219,sum)) %>% 
  pivot_longer(cols=1:216,names_to = "species", values_to="count") %>% 
  ggplot(aes(reorder(species,count),count))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme(axis.text.y=element_text(size=3))

#Remove species less than 5 times
# Traits data - (need migration status, Species temp index, bodysize, diet, broad habitat classification)
# Phylogeny
# Site covariates - (altitude, temperature, precip, habitat type, year, )
#Phylogeny
# Run for TR first
# See if phylo tree and not distacnce matrix works

#colnames(TOVE_points)[4:219]
# Get the correct name for matching
library(rotl)
colnames(TOVE_points)[193]<-"Schoeniclus rusticus"

taxa <- tnrs_match_names(names = colnames(TOVE_points)[4:219],context_name = "Animals")
taxa

library(ape)
library(phytools)

tree <- tol_induced_subtree(ott_ids = ott_id(taxa))
phydist=cophenetic(tree)
class(phydist)
rnames=rownames(phydist)
cnames=colnames(phydist)
rnames=sub("_[^_]+$", "", rnames)
cnames=sub("_[^_]+$", "", cnames)

rownames(phydist)=rnames
colnames(phydist)=cnames
phydist
plot(tree, cex = .8, label.offset = .1, no.margin = TRUE)


??studies_find_trees

#remotes::install_github("RS-eco/traitdata", build_vignettes = T, force=T)

library(traitdata)
data("elton_birds")
sum(taxa$unique_name%in% paste0(elton_birds$Genus," ",elton_birds$Species))
dim(taxa)

head(elton_birds)
names(elton_birds)
#list_All_data<-list(Y=TOVE_points[,4:219])
