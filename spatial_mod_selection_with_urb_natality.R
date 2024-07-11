#!/usr/bin/env Rscript


library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

# get data 
load("migterm_imp.Rdata")

# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)

cpall = merge(cp, migterm_imp, by.x = "GEOID", by.y = "destid", all.y = TRUE)
cpall$ft = as.factor(cpall$year)

cpp = pdata.frame(cpall, index = c("GEOID", "year"))

#########################################################################

#load spatial weights matrix which was created using the following: 
# commented out because queenw was saved and can now be loaded directly from data_processed 
# spall = cpall %>% select(GEOID, geometry) %>% distinct()
# queen = spdep::poly2nb(spall, row.names = "GEOID", queen = T)
# queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

load("queenw.Rdata")





# Define models
models <- list(
  # Original models
  fm = rate_d1 ~ ft + rate_d0 + migterm,
  fnom = rate_d1 ~ ft + rate_d0,
  fff2 = rate_d1 ~ ft + ns(rate_d0, df = 2) + ns(migterm, df = 2),
  fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4),
  fff3 = rate_d1 ~ ft + ns(rate_d0, df = 3) + ns(migterm, df = 3),
  fff5 = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm, df = 5),
  fff2_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2),
  fff4_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4),
  fff3_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3),
  fff5_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5),
  
  # Models with natality
  fm_natality = rate_d1 ~ ft + rate_d0 + migterm_nat,
  #fnom_natality = rate_d1 ~ ft + rate_d0,
  fff2_natality = rate_d1 ~ ft + ns(rate_d0, df = 2) + ns(migterm_nat, df = 2),
  fff4_natality = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm_nat, df = 4),
  fff3_natality = rate_d1 ~ ft + ns(rate_d0, df = 3) + ns(migterm_nat, df = 3),
  fff5_natality = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm_nat, df = 5),
  #fff2_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 2),
  #fff4_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 4),
  #fff3_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 3),
  #fff5_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 5),
  
  # Models with rurality interactions
  fmr = rate_d1 ~ ft + rate_d0*as.factor(rural) + migterm*as.factor(rural) + as.factor(rural),
  fnomr = rate_d1 ~ ft + rate_d0*as.factor(rural) + as.factor(rural),
  fff2r = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + ns(migterm, df = 2)*as.factor(rural) + as.factor(rural),
  fff4r = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + ns(migterm, df = 4)*as.factor(rural) + as.factor(rural),
  fff3r = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + ns(migterm, df = 3)*as.factor(rural) + as.factor(rural),
  fff5r = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + ns(migterm, df = 5)*as.factor(rural) + as.factor(rural),
  fff2r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + as.factor(rural),
  fff4r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + as.factor(rural),
  fff3r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + as.factor(rural),
  fff5r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + as.factor(rural),
  
  # Models with natality and rurality interactions 
  fmr_natality = rate_d1 ~ ft + rate_d0*as.factor(rural) + migterm_nat*as.factor(rural) + as.factor(rural),
  #fnomr = rate_d1 ~ ft + rate_d0*as.factor(rural) + as.factor(rural),
  fff2r_natality = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + ns(migterm_nat, df = 2)*as.factor(rural) + as.factor(rural),
  fff4r_natality = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + ns(migterm_nat, df = 4)*as.factor(rural) + as.factor(rural),
  fff3r_natality = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + ns(migterm_nat, df = 3)*as.factor(rural) + as.factor(rural),
  fff5r_natality = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + ns(migterm_nat, df = 5)*as.factor(rural) + as.factor(rural)
  
)







# AIC and BIC function for splm object #
#copied (and then edited) from this git repository https://github.com/rfsaldanha/ecoespacialunicamp/blob/master/OLD/AICsplm.R
# at the suggestion of this stackoverflow post: https://stackoverflow.com/questions/55838656/extract-aic-from-a-fixed-effect-spatial-panel-model-estimation-result

BICsplm = function(object, k=2){
  #  tryCatch({
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  bic = -2*l+log(N)*np
  names(bic) = "BIC"
  return(bic)
  # }, error = function(e) {
  #  bic = NA
  # names(bic) = NA
  # return(bic)
  # })
}



rel.tol.values= 10^seq(-5, -17, by = -1)



# Define a function to fit the model with different rel.tol values
fit_model_with_reltol <- function(formula, data= cpp, rel.tol.values) {
  results <- list()
  
  for (rel.tol in rel.tol.values) {
    fit <- tryCatch({
      #the commented out model is useful for testing since it has no slow spatial components 
      #splm::spml(formula, data = cpp,listw=queenw, model = "random", lag = FALSE, spatial.error = "none", local = list(parallel = TRUE))
      splm::spml(formula, data = cpp,listw=queenw, model = "random", lag = FALSE, spatial.error = "b", local = list(parallel = TRUE), rel.tol = rel.tol)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(fit)) {
      results[[paste0("rel.tol_", rel.tol)]] <- list(
        fit = fit,
        bic = BICsplm(fit),
        rel.tol = rel.tol
      )
      break
    }
  }
  return(results)
}







# Fit the models and save results
output_file <- "modselection_nat.txt"
write("Model,BIC,rel.tol", file = output_file)

for (model_name in names(models)) {
  model_formula <- models[[model_name]]
  model_results <- fit_model_with_reltol(model_formula, your_data, rel.tol.values)
  
  for (result_name in names(model_results)) {
    result <- model_results[[result_name]]
    write(sprintf("%s,%f,%e", model_name, result$bic, result$rel.tol), file = output_file, append = TRUE)
  }
}