#!/usr/bin/env Rscript


library(tidycensus)
library(tidyverse)
library(splines)
library(splm)
library(plm)

# get data 
load("migterm_imp.RData")

natsub = migterm_imp %>% filter(!is.na(migterm_nat))
#natsub$ft = as.factor(natsub$year)
#natsub= plm::make.pbalanced(natsub, index = c("destid", "year"),balance = "fill")

natsub$ft = as.factor(natsub$year)

library(dplyr)
library(tidyr)
nat_bal = natsub %>% 
  #select(-c(NAME, tpopE, tpopM, ctyname, st_abbrev, code2013, chrr_urb_code)) %>% 
  complete(destid, year) %>%
  arrange(destid, year)

incomplete = cpbal %>%
  filter(if_any(everything(), is.na)) %>%
  pull(GEOID) %>%
  unique()

cpbal_filt = cpbal %>% filter(!(GEOID %in% incomplete))






# add geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, variables = c(tpop = "B01003_001"), survey = "acs5", output = "wide", geometry = TRUE)

cpall = merge(cp, natsub, by.x = "GEOID", by.y = "destid", all.y = TRUE)



cpp = plm::pdata.frame(cpbal_filt, index = c("GEOID", "year"))


# cpp = plm::pdata.frame(cpall, index = c("GEOID", "year"))
# cpg = cpp %>% select(-geometry, NAME,tp 
# cpg = plm::pdata.frame(cpg, index = c("GEOID", "year"))
# cpg = plm::make.pbalanced(cpg, index = c("GEOID", "year"), balance = "fill")


#########################################################################

#create spatial weights matrix for subset 
spall = cpbal_filt %>% select(GEOID, geometry) %>% distinct() 
spall = spall %>% filter(!is.na(GEOID))
spall = spall[!duplicated(spall$GEOID), ]

queen = spdep::poly2nb(spall, row.names = spall$GEOID, queen = T)
queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)








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
  fff5_natality = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm_nat, df = 5)
  #fff2_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 2),
  #fff4_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 4),
  #fff3_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 3),
  #fff5_nomig_natality = rate_d1 ~ ft + ns(rate_d0, df = 5),
  
)


# Define the range of rel.tol values to test
rel.tol.values = 1e-05




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







# Define a function to fit the model with different rel.tol values
fit_model_with_reltol <- function(formula, data, rel.tol.values) {
  results <- list()
  
  for (rel.tol in rel.tol.values) {
    fit <- tryCatch({
      splm::spml(formula, data = data,listw=queenw, model = "random", lag = FALSE, spatial.error = "b", local = list(parallel = TRUE), rel.tol = rel.tol)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(fit)) {
      results[[paste0("rel.tol_", rel.tol)]] <- list(
        fit = fit,
        bic = BICsplm(fit),
        rel.tol = rel.tol
      )
    }
  }
  return(results)
}




# Fit the models and save results
output_file <- "modselection_nat_sensitivity.txt"
write("Model,BIC,rel.tol", file = output_file)

for (model_name in names(models)) {
  model_formula <- models[[model_name]]
  model_results <- fit_model_with_reltol(model_formula, cpp, 1e-05)
  
  for (result_name in names(model_results)) {
    result <- model_results[[result_name]]
    write(sprintf("%s,%f,%e", model_name, result$bic, result$rel.tol), file = output_file, append = TRUE)
  }
}
