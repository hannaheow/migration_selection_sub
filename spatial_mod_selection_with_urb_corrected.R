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





#confirmed that the two calls below yield the same results (plm versus splm package)
# they do return slightly different summaries...... so better to be consistent with the splm package! 
# m1plm = plm::plm(formula = rate_d1 ~ ft + migterm , data = cpp, model = "random")
# 
# m1mignospat = splm::spml(formula = rate_d1 ~ ft + migterm , listw = queenw, data = cpp, model = "random", lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )


# original models, no interaction for urbanicity 
fm = rate_d1 ~ ft + rate_d0 + migterm   
fnom = rate_d1 ~ ft + rate_d0 
fff2 = rate_d1 ~ ft + ns(rate_d0, df = 2) + ns(migterm, df = 2) 
fff4 = rate_d1 ~ ft + ns(rate_d0, df = 4) + ns(migterm, df = 4) 
fff3 = rate_d1 ~ ft + ns(rate_d0, df = 3) + ns(migterm, df = 3) 
fff5 = rate_d1 ~ ft + ns(rate_d0, df = 5) + ns(migterm, df = 5)  
fff2_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2) 
fff4_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4) 
fff3_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3) 
fff5_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5) 


#models with as.factor(rural) as an interaction with lagged and an interaction with migration
fmr = rate_d1 ~ ft + rate_d0*as.factor(rural) + migterm*as.factor(rural) + as.factor(rural)     
fnomr = rate_d1 ~ ft + rate_d0*as.factor(rural) + as.factor(rural)
fff2r = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + ns(migterm, df = 2)*as.factor(rural) + as.factor(rural)
fff4r = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + ns(migterm, df = 4)*as.factor(rural) + as.factor(rural)
fff3r = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + ns(migterm, df = 3)*as.factor(rural) + as.factor(rural)
fff5r = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + ns(migterm, df = 5)*as.factor(rural) + as.factor(rural)
fff2r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 2)*as.factor(rural) + as.factor(rural)
fff4r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 4)*as.factor(rural) + as.factor(rural)
fff3r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 3)*as.factor(rural) + as.factor(rural)
fff5r_nomig = rate_d1 ~ ft + ns(rate_d0, df = 5)*as.factor(rural) + as.factor(rural)



# some singularity errors..... trycatch is trying to continue past those errors... 
# halved the rel.tol value from default of 1e-10 to 1e-5. hopefully fewer singularities now 
tryCatch({

# with spatial stuff 
# original models, no urbanicity interaction 

spat_fm  = splm::spml(formula = fm, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fm , file = "spat_fm.txt")

spat_fnom  = splm::spml(formula = fnom, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fnom , file = "spat_fnom.txt")

spat_fff2  = splm::spml(formula = fff2, data =cpp, model = "random", listw=  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
#save(spat_fff2 , file = "spat_fff2.txt")

spat_fff3  = splm::spml(formula = fff3, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff3 , file = "spat_fff3.txt")


spat_fff4  = splm::spml(formula = fff4, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff4 , file = "spat_fff4.txt")


spat_fff5  = splm::spml(formula = fff5, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE))
#save(spat_fff5 , file = "spat_fff5.txt")



spat_fff2_nomig  = splm::spml(formula = fff2_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff2_nomig , file = "spat_fff2_nomig.txt")


spat_fff3_nomig  = splm::spml(formula = fff3_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff3_nomig , file = "spat_fff3_nomig.txt")


spat_fff4_nomig  = splm::spml(formula = fff4_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff4_nomig , file = "spat_fff4_nomig.txt")


spat_fff5_nomig  = splm::spml(formula = fff5_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff5_nomig , file = "spat_fff5_nomig.txt")





# with spatial stuff 
# models with as.factor(rural) as an interaction with lagged and an interaction with migration

spat_fmr  = splm::spml(formula = fmr, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fmr , file = "spat_fmr.txt")

spat_fnomr  = splm::spml(formula = fnomr, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fnomr , file = "spat_fnomr.txt")

spat_fff2r  = splm::spml(formula = fff2r, data =cpp, model = "random", listw=  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff2r , file = "spat_fff2r.txt")

spat_fff3r  = splm::spml(formula = fff3r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff3r , file = "spat_fff3r.txt")


spat_fff4r  = splm::spml(formula = fff4r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff4r , file = "spat_fff4r.txt")


spat_fff5r  = splm::spml(formula = fff5r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff5r , file = "spat_fff5r.txt")


spat_fff2r_nomig  = splm::spml(formula = fff2r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff2r_nomig , file = "spat_fff2r_nomig.txt")


spat_fff3r_nomig  = splm::spml(formula = fff3r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff3r_nomig , file = "spat_fff3r_nomig.txt")


spat_fff4r_nomig  = splm::spml(formula = fff4r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff4r_nomig , file = "spat_fff4r_nomig.txt")


spat_fff5r_nomig  = splm::spml(formula = fff5r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "b", local = list(parallel = TRUE) )
#save(spat_fff5r_nomig , file = "spat_fff5r_nomig.txt")





# without spatial stuff 
# original models, no urbanicity interaction 

nospat_fm  = splm::spml(formula = fm, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE), rel.tol = 1e-16 )
#save(nospat_fm , file = "nospat_fm.txt")

nospat_fnom  = splm::spml(formula = fnom, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fnom , file = "nospat_fnom.txt")

nospat_fff2  = splm::spml(formula = fff2, data =cpp, model = "random", listw=  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff2 , file = "nospat_fff2.txt")

nospat_fff3  = splm::spml(formula = fff3, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff3 , file = "nospat_fff3.txt")


nospat_fff4  = splm::spml(formula = fff4, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff4 , file = "nospat_fff4.txt")


nospat_fff5  = splm::spml(formula = fff5, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff5 , file = "nospat_fff5.txt")


nospat_fff2_nomig  = splm::spml(formula = fff2_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff2_nomig , file = "nospat_fff2_nomig.txt")


nospat_fff3_nomig  = splm::spml(formula = fff3_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff3_nomig , file = "nospat_fff3_nomig.txt")


nospat_fff4_nomig  = splm::spml(formula = fff4_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) , rel.tol = 1e-16)
#save(nospat_fff4_nomig , file = "nospat_fff4_nomig.txt")


nospat_fff5_nomig  = splm::spml(formula = fff5_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff5_nomig , file = "nospat_fff5_nomig.txt")





# without spatial stuff 
# models with as.factor(rural) as an interaction with lagged and an interaction with migration

nospat_fmr  = splm::spml(formula = fmr, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fmr , file = "nospat_fmr.txt")

nospat_fnomr  = splm::spml(formula = fnomr, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fnomr , file = "nospat_fnomr.txt")

nospat_fff2r  = splm::spml(formula = fff2r, data =cpp, model = "random", listw=  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE), rel.tol = 1e-16 )
#save(nospat_fff2r , file = "nospat_fff2r.txt")

nospat_fff3r  = splm::spml(formula = fff3r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff3r , file = "nospat_fff3r.txt")


nospat_fff4r  = splm::spml(formula = fff4r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff4r , file = "nospat_fff4r.txt")


nospat_fff5r  = splm::spml(formula = fff5r, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff5r , file = "nospat_fff5r.txt")


nospat_fff2r_nomig  = splm::spml(formula = fff2r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE), rel.tol = 1e-16 )
#save(nospat_fff2r_nomig , file = "nospat_fff2r_nomig.txt")


nospat_fff3r_nomig  = splm::spml(formula = fff3r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff3r_nomig , file = "nospat_fff3r_nomig.txt")


nospat_fff4r_nomig  = splm::spml(formula = fff4r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff4r_nomig , file = "nospat_fff4r_nomig.txt")


nospat_fff5r_nomig  = splm::spml(formula = fff5r_nomig, data =cpp, model = "random", listw =  queenw, lag = FALSE, spatial.error = "none", local = list(parallel = TRUE) )
#save(nospat_fff5r_nomig , file = "nospat_fff5r_nomig.txt")






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



# with spatial stuff 
# original models, no urbanicity interaction 
bicspat = c(fm = BICsplm(spat_fm),
            fnom = BICsplm(spat_fnom),
            fff2 = BICsplm(spat_fff2), 
            fff2_nomig = BICsplm(spat_fff2_nomig),
            fff3 = BICsplm(spat_fff3),
            fff3_nomig = BICsplm(spat_fff3_nomig),
            fff4 = BICsplm(spat_fff4),
            fff4_nomig = BICsplm(spat_fff4_nomig),
            fff5 = BICsplm(spat_fff5),
            fff5_nomig = BICsplm(spat_fff5_nomig))
names(bicspat) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
#spatchoice = names(bicspat[bicspat== min(bicspat)])

# models with as.factor(rural) as an interaction with lagged and an interaction with migration
bicspatr = c(fm = BICsplm(spat_fmr),
                  fnom = BICsplm(spat_fnomr),
                  fff2 = BICsplm(spat_fff2r), 
                  fff2_nomig = BICsplm(spat_fff2r_nomig),
                  fff3 = BICsplm(spat_fff3r),
                  fff3_nomig = BICsplm(spat_fff3r_nomig),
                  fff4 = BICsplm(spat_fff4r),
                  fff4_nomig = BICsplm(spat_fff4r_nomig),
                  fff5 = BICsplm(spat_fff5r),
                  fff5_nomig = BICsplm(spat_fff5r_nomig))
names(bicspatr) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
#spatchoicer = names(bicspatr[bicspatr== min(bicspatr)])

# without spatial stuff 
# original models, no urbanicity interaction 
bicnospat = c(fm = BICsplm(nospat_fm),
            fnom = BICsplm(nospat_fnom),
            fff2 = BICsplm(nospat_fff2), 
            fff2_nomig = BICsplm(nospat_fff2_nomig),
            fff3 = BICsplm(nospat_fff3),
            fff3_nomig = BICsplm(nospat_fff3_nomig),
            fff4 = BICsplm(nospat_fff4),
            fff4_nomig = BICsplm(nospat_fff4_nomig),
            fff5 = BICsplm(nospat_fff5),
            fff5_nomig = BICsplm(nospat_fff5_nomig))
names(bicnospat) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")
#spatchoice = names(bicspat[bicspat== min(bicspat)])

# models with as.factor(rural) as an interaction with lagged and an interaction with migration
bicnospatr = c(fm = BICsplm(nospat_fmr),
             fnom = BICsplm(nospat_fnomr),
             fff2 = BICsplm(nospat_fff2r), 
             fff2_nomig = BICsplm(nospat_fff2r_nomig),
             fff3 = BICsplm(nospat_fff3r),
             fff3_nomig = BICsplm(nospat_fff3r_nomig),
             fff4 = BICsplm(nospat_fff4r),
             fff4_nomig = BICsplm(nospat_fff4r_nomig),
             fff5 = BICsplm(nospat_fff5r),
             fff5_nomig = BICsplm(nospat_fff5r_nomig))
names(bicnospatr) = c("fm", "fnom", "fff2", "fff2_nomig", "fff3", "fff3_nomig","fff4", "fff4_nomig", "fff5", "fff5_nomig")






save(bicspat, file = "bicspat.txt")
save(bicspatr, file = "bicspatr.txt")
save(bicnospat, file = "bicnospat.txt")
save(bicnospatr, file = "bicnospatr.txt")
}, error = function(e){cat("ERROR :", conditionMessage(e))})


# i believe that this warning message: In sqrt(diag(object$vcov.errcomp)) : NaNs produced
# is the result of one year of migterms missing a rate_d0. based on this stackoverflow, it is not a current concern: https://stackoverflow.com/questions/67338560/zeroinfl-model-warning-message-in-sqrtdiagobjectvcov-nans-produced
