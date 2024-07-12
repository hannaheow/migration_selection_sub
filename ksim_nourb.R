#!/usr/bin/env Rscript

#at top of script: 
args <- commandArgs(trailingOnly = TRUE)
i <- args[1]
# j <- args[2]
# k <- args[3]
# l <- args[4]

#this means i can remove the forloop 

library(tidyverse)
library(tidycensus)
library(splines)
library(splm)
library(plm)
library(haven)

load("imputed_for_ksim.RData") 
#this dataset is called natmorturb_imp
# it has natality, death, population, premature mortality rates, migration flows, rural/urb codes but no migterms (yet)
# it is balanced and has no missing rates (except for initial years)

urbcodes = haven::read_sas("nchs_urbanicity_wlabels.sas7bdat") %>% 
  select(fipscode, rural) %>% 
  rename(rural_orig = rural)
#add rural/urb for origin 
nm = natmorturb_imp %>% rename(rural_dest = rural) %>% 
  left_join(urbcodes, by = join_by(origid == fipscode)) %>% 
  mutate(migtype = ifelse(!is.na(rural_dest)& !is.na(rural_orig), paste0(rural_dest, rural_orig), NA)) 



# this formula produced lowest bic for spatial and non spatial models as shown in "spatial mod selection.R" 
# note: changed "rural" to "rural_dest" to match new var names 
# kind of unsure about this strat versus nonstrat approach 
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural_dest) + ns(migterm, df = 4)*as.factor(rural_dest) + as.factor(rural_dest)


# get geography for use on spatial stuff 
cp = tidycensus::get_acs(geography = "county", year = 2019, output = "wide", variables = c(tpop = "B01003_001"), survey = "acs5", geometry = TRUE)





#load spatial weights matrix created previously 
load("queenw.Rdata")



# function for BIC for spatial models 
BICsplm = function(object, k=2){ 
  sp = summary(object)
  l = sp$logLik
  np = length(coef(sp))
  N = nrow(sp$model)
  bic = -2*l+log(N)*np
  names(bic) = "BIC"
  return(bic)
}


###############################################################################

bickij_int = data.frame()



#this previously was the loop - now goes into code 

nmk = nm %>% mutate(newk = i)
nmk$newk = as.numeric(nmk$newk) 
# wrangling 
tempdf = nmk %>%
  group_by(destid, year) %>% 
  mutate(migterm = (sum(out_o *(rate_o0 + newk), na.rm = TRUE) + (rate_d0) * (as.numeric(pop_d0)-as.numeric(totout_d)))/(sum(out_o, na.rm = TRUE) + (as.numeric(pop_d0)- totout_d))) %>% 
  distinct(destid, migterm, year, rate_d0, rate_d1, .keep_all = TRUE)


tempdf$newk = i

cpij = merge(cp, tempdf, by.x = "GEOID", by.y = "destid", all.y = TRUE)

# commented out because queenw was saved and can now be loaded directly from data_processed 
# spall = cpij %>% select(GEOID, geometry) %>% distinct()
# queen = spdep::poly2nb(spall, row.names = "GEOID", queen = T)
# queenw = spdep::nb2listw(queen, style = "W", zero.policy = TRUE)

cpij = plm::pdata.frame(cpij, index = c("GEOID", "year"))
cpij = cpij %>% filter(!duplicated(.)) 
cpij$ft = as.factor(cpij$year)





# some ks throw fail to converge errors..... trycatch is trying to continue past those errors... 

tryreltol = function(rel.tol) {

tryCatch({
  #this is the best spatial model as determined previously 
  # nospat_fff4 = splm::spml(formula = fff4, data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "none", na.rm = TRUE, rel.tol = rel.tol) 
  
  spat_fff4 = splm::spml(formula = fff4, 
                         data =cpij, model = "random", listw = queenw, lag = FALSE, spatial.error= "b", local = list(parallel = TRUE), rel.tol = rel.tol) 
  

return(spat_fff4)

}, error = function(err) {
    cat(paste("Error with rel.tol =", rel.tol, ": ", conditionMessage(err), "\n"))
    return(NULL)  # Return NULL if error occurs
  })
}


# Initial rel.tol value
initial_rel.tol <- 1e-10
current_rel.tol <- initial_rel.tol
converged <- FALSE

# Loop to decrease rel.tol by a factor of 10 until convergence
while (!converged) {
model = tryreltol(current_rel.tol) 
if(!is.null(model)) {

converged = TRUE
} else {
current_rel.tol = current_rel.tol / 10

if (current_rel.tol < 1e-25) {
cat("Rel.tol decreased below threshold. Model did not converge. \n")
break
}
} 
}

if (converged) {
bicspat = BICsplm(model) 
bick_nourb = data.frame(newk = unique(cpij$newk), 
	bicspat = bicspat, 
	rel.tol = current_rel.tol)
  
  save(bick_nourb, file = paste0("bick_nourb", bick_nourb$newk, ".txt"))
  

} else {

error = function(e){cat("ERROR :", conditionMessage(e))} #this will just go to the .out file 
}



#submit file needs to be set up to grab this output 

