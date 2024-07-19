# process bic output from ksim 

files = list.files("chtc_outputs/output_bick")

bickdf = data.frame()

for (i in 1:length(files)){
  load(paste0("chtc_outputs/output_bick/",files[i]))
  tempdat = bickij_int
  bickdf = rbind(bickdf, bickij_int)
}

save(bickdf, file= "data_processed/bickdf.Rdata")




#####################################################
# process bic output from spatial_mod_selection_with_urb_corrected.R 

load("chtc_outputs/output_bic_reltol/bicnospat.txt")
load("chtc_outputs/output_bic_reltol/bicspat.txt")
load("chtc_outputs/output_bic_reltol/bicnospatr.txt")
load("chtc_outputs/output_bic_reltol/bicspatr.txt")

bicms = rbind(bicnospat, bicnospatr, bicspat, bicspatr)


#########################################################
# process bic output from ksim_nourb.R 

files = list.files("chtc_outputs/bick_nourb")

bick_nourb_tot = data.frame()

for (i in 1:length(files)){
  load(paste0("chtc_outputs/bick_nourb/",files[i]))
  tempdat = bick_nourb
  bick_nourb_tot = rbind(bick_nourb_tot, tempdat)
}

save(bick_nourb_tot, file= "data_processed/bick_nourb_tot.Rdata")



#########################################################
# process bic output from output_bick_restrict

files = list.files("chtc_outputs/output_bick_restrict")

bick_restrict = data.frame()

for (i in 1:length(files)){
  load(paste0("chtc_outputs/output_bick_restrict/",files[i]))
  tempdat = bickij_int
  bick_restrict = rbind(bick_restrict, tempdat)
}

save(bick_restrict, file= "data_processed/bick_restrict.Rdata")



#########################################################
# process bic output from output_bick_restrict_miss

files = list.files("chtc_outputs/output_bick_restrict_miss")

bick_restrict_miss = data.frame()

for (i in 1:length(files)){
  load(paste0("chtc_outputs/output_bick_restrict_miss/",files[i]))
  tempdat = bickij_int
  bick_restrict_miss = rbind(bick_restrict_miss, tempdat)
}

save(bick_restrict_miss, file= "data_processed/bick_restrict_miss.Rdata")


#########################################################################
# process the extra restricted ksim 

files = list.files("chtc_outputs/output_bick_restrict_kurkru")

bick_restrict_kurkru = data.frame()

for (i in 1:length(files)){
  load(paste0("chtc_outputs/output_bick_restrict_kurkru/",files[i]))
  tempdat = bickij_int
  bick_restrict_kurkru = rbind(bick_restrict_kurkru, tempdat)
}

save(bick_restrict_kurkru, file= "data_processed/bick_restrict_kurkru.Rdata")





###########################################################################
# combine bic restrict and bic restrict miss and bick restrict kurkru

bick_restrict_tot = rbind(bick_restrict, bick_restrict_miss, bick_restrict_kurkru)
save(bick_restrict_tot, file= "data_processed/bick_restrict_tot.Rdata")




