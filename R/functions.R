# functions.R

joint_habitat_2_points=function(Habitat_data_TOVE_routes,TOVE_Points_2006_2020){
  Habitat_data_TOVE_routes$YEAR=as.numeric(Habitat_data_TOVE_routes$Aar)
  Habitat_data_TOVE_routes$PointID=as.numeric(Habitat_data_TOVE_routes$PktNr)
  Habitat_data_TOVE_routes$RouteID=as.numeric(Habitat_data_TOVE_routes$RuteNr)
  TOVE_Points_2006_2020=TOVE_Points_2006_2020 %>% 
    left_join(.,Habitat_data_TOVE_routes, by=c("RouteID"="RouteID", "PointID"="PointID"))
  
}

Pivot_data=function(jn_habitat_points){
  #convert to sites on left and species on top
  
  TOVE_points<-jn_habitat_points %>% 
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
  
  return(TOVE_points_data_na)
}

make_studyDesign=function(pivot_data){
# Variable selection 
studyDesign = pivot_data[,c("RouteID","YEAR", "PointID")]
studyDesign = data.frame(apply(studyDesign,2,as.factor))
studyDesign$RouteID=as.factor(studyDesign$RouteID)
studyDesign$YEAR=as.factor(studyDesign$YEAR)
studyDesign$PointID=as.factor(studyDesign$PointID)
return(studyDesign)
}

make_variables=function(studyDesign){
# Variable structuring 
RouteID = HmscRandomLevel(units = studyDesign$RouteID)
YEAR = HmscRandomLevel(units = studyDesign$YEAR)
PointID=HmscRandomLevel(units=studyDesign$PointID)
ranlevels = list(RouteID = RouteID, YEAR = YEAR, PointID=PointID) 
return(ranlevels)
}

make_covariates=function(pivot_data){
  cov = pivot_data[,4:6 ]
  X = as.data.frame(cov[,c("FK_Vegetasjonstype1","FK_Vegetasjonstype2",
                           "FK_Vegetasjonstype3")])
  
  return(X)
}
make_phylogeny=function(phylo){
  ## Phylogeny
  phylo$tip.label<-gsub("_"," ", phylo$tip.label)
  phylo$tip.label[c(2,15,16,18)]
  tips2drop=c("Corvus corone","Parus cristatus",
              "Parus montanus", "Parus caeruleus")
  
  
  phylo=drop.tip(phylo, tips2drop)
  #C<-ape::cophenetic.phylo(phylo)
  return(phylo)
}

make_splist=function(Tr, pivot_data){
  #Traits
  Tr$Species=gsub("_", " ", Tr$Species)
splist=Tr$Species[Tr$Species%in% colnames(pivot_data[,7:222])]
}
make_Y=function(pivot_data, splist){
  pivot_data %>% 
    ungroup() %>% 
    select(splist)->Y2
  Y = as.matrix(Y2) 
  Y=Y[,order(colnames(Y))]
  Y.pa = Y
  Y.pa[Y>1] = 1
  return(list(Y,
              Y.pa))
}
make_traits_df=function(Tr, Y_obs){
Tr=Tr %>% 
  mutate(Species=gsub("_", " ", Species)) %>% 
  filter(Species%in% colnames(Y_obs[[1]]))
rownames(Tr)<-Tr[,1]
Tr = as.data.frame(Tr[,-1])
Tr=Tr[ order(row.names(Tr)), ]
return(Tr)
}

make_formulas=function(){
  XFormula = ~FK_Vegetasjonstype1+FK_Vegetasjonstype2+FK_Vegetasjonstype3
  TrFormula = ~Migration + 
    Mass +
    Urb +
    Br+
    Co+
    Op+
    Ma+
    We
  
  return(list(XFormula, 
              TrFormula))
}
poissMod=function(Y_obs,covariates, formulas, trait_mod,phylo_mod,
                  studyDesign,variableStr){
                Hmsc(Y=Y_obs[[1]], 
                XData = covariates,
                XFormula = formulas[[1]],
                TrData = trait_mod, 
                TrFormula = formulas[[2]] ,
                phyloTree = phylo_mod,
                studyDesign = studyDesign,
                ranLevels  = variableStr,
                distr = "poisson")
}

run_HMSC =function(poiss_mod_str, modelSettings){
                      sampleMcmc(poiss_mod_str,
                      samples = modelSettings$samples,
                      thin = modelSettings$thin,
                      transient = modelSettings$transient,
                      nChains = modelSettings$nChains, 
                      nParallel = modelSettings$nChains)
}

check_model=function(mod_HMSC){
  ## Convergence tests
  mcoda <- convertToCodaObject(mod_HMSC)
  par(mar = rep(2, 4))
  #Visual chain tests for different coefficients of interest 
  plot(mcoda$Beta)
  plot(mcoda$Rho)
  plot(mcoda$Gamma)
}

modelOut=function(mod_HMSC){
  
}