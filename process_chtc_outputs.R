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
