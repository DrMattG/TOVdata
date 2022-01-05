library(Hmsc)
library(corrplot)
library(ape)
library(tidyverse)
library(readxl)
library(readr)

phylo = ape::read.tree("data/Tronderlag/Ctree (1).tre")
Tr = read.csv("data/Tronderlag/traits.csv")
TOVE_Points_2006_2020 <- read_excel("data/TOVE_Points_2006_2020.xlsx")
Habitat_data_TOVE_routes <- read_delim("data/Tronderlag/Habitat_data_TOVE_routes.csv", 
         
                                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)

str(Habitat_data_TOVE_routes)
Habitat_data_TOVE_routes$YEAR=as.numeric(Habitat_data_TOVE_routes$Aar)
Habitat_data_TOVE_routes$PointID=as.numeric(Habitat_data_TOVE_routes$PktNr)
Habitat_data_TOVE_routes$RouteID=as.numeric(Habitat_data_TOVE_routes$RuteNr)
TOVE_Points_2006_2020=TOVE_Points_2006_2020 %>% 
  left_join(.,Habitat_data_TOVE_routes, by=c("RouteID"="RouteID", "PointID"="PointID"))


#convert to sites on left and species on top

dim(TOVE_Points_2006_2020)
TOVE_points<-TOVE_Points_2006_2020 %>% 
    select(RouteID, YEAR.x, PointID, ScientificName, FK_Vegetasjonstype1, FK_Vegetasjonstype2, FK_Vegetasjonstype3) %>% 
    mutate(YEAR=YEAR.x) %>% 
    select(!YEAR.x)
TOVE_points_data=TOVE_points %>% 
  mutate(count=1) %>% 
  group_by(RouteID, PointID, YEAR, ScientificName,FK_Vegetasjonstype1,FK_Vegetasjonstype2,FK_Vegetasjonstype3) %>% 
  summarise(count=sum(count)) %>%
  pivot_wider(id_cols=c(RouteID,PointID, YEAR,FK_Vegetasjonstype1,FK_Vegetasjonstype2,FK_Vegetasjonstype3),
              names_from=ScientificName, 
              values_from=count,
              values_fill = 0)  
  
TOVE_points_data_na=
TOVE_points_data %>% 
  drop_na()


         #species occ
Y = as.matrix(TOVE_points_data_na[,7:222]) 

# Variable selection 
studyDesign = TOVE_points_data_na[,c("RouteID","YEAR", "PointID")]
studyDesign = data.frame(apply(studyDesign,2,as.factor))
str(studyDesign)
studyDesign$RouteID=as.factor(studyDesign$RouteID)
studyDesign$YEAR=as.factor(studyDesign$YEAR)
studyDesign$PointID=as.factor(studyDesign$PointID)
# Variable structuring 
RouteID = HmscRandomLevel(units = studyDesign$RouteID)
YEAR = HmscRandomLevel(units = studyDesign$YEAR)
PointID=HmscRandomLevel(units=studyDesign$PointID)
ranlevels = list(RouteID = RouteID, YEAR = YEAR, PointID=PointID) 
ranlevels

cov = TOVE_points_data_na[,4:6 ]

X = as.data.frame(cov[,c("FK_Vegetasjonstype1","FK_Vegetasjonstype2",
                         "FK_Vegetasjonstype3")])
XFormula = ~FK_Vegetasjonstype1+FK_Vegetasjonstype2+FK_Vegetasjonstype3


## Phylogeny
phylo
phylo$tip.label<-gsub("_"," ", phylo$tip.label)
phylo$tip.label[c(2,15,16,18)]
tips2drop=c("Corvus corone","Parus cristatus",
            "Parus montanus", "Parus caeruleus")


phylo=drop.tip(phylo, tips2drop)
C<-ape::cophenetic.phylo(phylo)




#Traits
colnames(Tr)

Tr$Species=gsub("_", " ", Tr$Species)

splist=Tr$Species[Tr$Species%in% colnames(TOVE_points_data_na[,7:222])]


TOVE_points_data_na %>% 
  ungroup() %>% 
  select(splist)->Y2

#Y2
Y = as.matrix(Y2) 
Y.pa = Y
Y.pa[Y>1] = 1
Tr=Tr %>% 
  filter(Species%in% colnames(Y2))
rownames(Tr)<-Tr[,1]
Tr = as.data.frame(Tr[,-1])

dim(Tr)

TrFormula = ~Migration + 
  Mass +
  Urb +
  Br+
  Co+
  Op+
  Ma+
  We

Tr=Tr[ order(row.names(Tr)), ]
Y=Y[,order(colnames(Y))]

dim(Tr)
dim(Y)
dim(C)

is.ultrametric.phylo(phylo)# TRUE


simul <- Hmsc(Y=Y, 
              XData = X,
              XFormula = XFormula,
              TrData = Tr, 
              TrFormula = TrFormula ,
              phyloTree = phylo,
              studyDesign = studyDesign,
              ranLevels  = ranlevels,
              distr = "poisson")
simul2 <- Hmsc(Y=Y.pa, 
              XData = X,
              XFormula = XFormula,
              TrData = Tr, 
              TrFormula = TrFormula ,
              phyloTree = phylo,
              studyDesign = studyDesign,
              ranLevels  = ranlevels,
              distr = "probit")
simul2

thin = 1
samples = 50
nChains = 2
transient = 50

mod_HMSC = sampleMcmc(simul,
                      samples = samples,
                      thin = thin,
                      transient = transient,
                      nChains = nChains, 
                      nParallel = nChains)

mod2_HMSC = sampleMcmc(simul2,
                      samples = samples,
                      thin = thin,
                      transient = transient,
                      nChains = nChains, 
                      nParallel = nChains)



save(mod_HMSC,file="mod_HMSC.Rdata")

save(mod2_HMSC,file="mod2_HMSC.Rdata")



postBeta = getPostEstimate(mod2_HMSC, parName = "Beta")

par(mar=c(5,11,2.5,0))

plotBeta(mod2_HMSC,
         post = postBeta, 
         plotTree = F,
         spNamesNumbers = c(T,F))

par(mar=c(5,11,2.5,0))

postGamma = getPostEstimate(mod2_HMSC, parName = "Gamma")


plotGamma(mod_HMSC, post = postGamma, supportLevel = 0.2)
