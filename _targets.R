# _targets.R file
library(targets)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse",
                            "Hmsc",
                            "corrplot",
                            "ape",
                            "tidyverse",
                            "readxl",
                            "readr",
                            "odbc",
                            "DBI"))
list(
  tar_target(
    raw_data_phylo,
    "data/Tronderlag/Ctree (1).tre",
    format = "file"
  ),
  tar_target(
    raw_data_Tr,
    "data/Tronderlag/traits.csv",
    format = "file"
  ),
  #tar_target(
  #  raw_data_TOVE_Points_2006_2020,
  #  "data/TOVE_Points_2006_2020.xlsx",
  #  format = "file"
  #),
  #tar_target(
  #  raw_data_Habitat_data_TOVE_routes,
  #  "data/Tronderlag/Habitat_data_TOVE_routes2.csv",
  #  format = "file"
  #),
  tar_target(
    phylo,
    ape::read.tree(raw_data_phylo)
  ),
  tar_target(
    Tr,
    read.csv(raw_data_Tr)
  ),
  tar_target(
    TOVE_Points,
    download_TOVE_Points()
  ),
  tar_target(
    Habitat_data_TOVE_routes,
    download_TOVE_Habitat()
  ),
  tar_target(
    jn_habitat_points,
    joint_habitat_2_points(Habitat_data_TOVE_routes, TOVE_Points)
  ),
  tar_target(
    pivot_data,
    Pivot_data(jn_habitat_points)
  ),
  tar_target(
    studyDesign,
    make_studyDesign(pivot_data)
  ),
  tar_target(
    variableStr,
    make_variables(studyDesign)
  ),
  tar_target(
    covariates,
    make_covariates(pivot_data)
    ),
  tar_target(
    phylo_mod,
    make_phylogeny(phylo)
  ),
  tar_target(
    splist,
    make_splist(Tr, pivot_data)
  ),
  tar_target(
    Y_obs,
    make_Y(pivot_data, splist)
  ),
  tar_target(
    trait_mod,
    make_traits_df(Tr, Y_obs)
  ),
  tar_target(
    formulas,
    make_formulas()
  ),
  tar_target(
    poiss_mod_str,
    poissMod(Y_obs,covariates, formulas, trait_mod,phylo_mod,
             studyDesign,variableStr)
  ),
  tar_target(
    modelSettings,
    list(thin = 1,
    samples = 50,
    nChains = 3,
    transient = 50)
  ),
  tar_target(
    mod_HMSC,
    run_HMSC(poiss_mod_str, modelSettings)
  ),
  tar_target(
    model_diagnostics,
    check_model(mod_HMSC)
  ),
  tar_target(
    postBeta,
    Hmsc::getPostEstimate(mod_HMSC, parName = "Beta")
  ),
  tar_target(
    postGamma,
    Hmsc::getPostEstimate(mod_HMSC, parName="Gamma")
  ),
  tar_target(
    plotBeta,
    Hmsc::plotBeta(mod_HMSC, post = postBeta, supportLevel = 0.2)
  ),
  tar_target(
    plotGamma,
    Hmsc::plotGamma(mod_HMSC, post = postGamma, supportLevel = 0.2)
  )
)

