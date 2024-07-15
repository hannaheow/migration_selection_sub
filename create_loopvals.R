# ki = seq(-200, 200, by = 50)
# kj = ki
# kl = ki
# kk = ki

ki = seq(-40, 40, by = 10)
kj = ki
kl = ki
kk = ki 


testcon = file("data_processed/loopvals_restrict.txt", open = "a")
isOpen(testcon)


for(i in 1:length(ki)) {
  for(j in 1:length(kj)) {
    for (k in 1:length(kk)) {
      for (l in 1:length(kl)) {
        cat(paste(ki[i], kj[j], kk[k], kl[l], sep = ", "), file = testcon, append = TRUE, sep = "\n")
      }
    }
  }
}
close(testcon)




#####################################################################################
#####################################################################################
# for investigating the k vals that did not run in bick_restrict 

load("data_processed/bick_restrict.Rdata")


# Define the sequence
values <- seq(-40, 40, by=10)

# Generate all possible combinations
all_combinations <- expand.grid(kur = values, kru = values, kuu = values, krr = values)

bick_restrict = bick_restrict %>% mutate(kur = as.numeric(kur),
                                         kru = as.numeric(kru), 
                                         kuu = as.numeric(kuu),
                                         krr = as.numeric(krr))


# Check for missing combinations
missing_combinations <- anti_join(all_combinations, bick_restrict, by = c("kur", "kru", "kuu", "krr"))

write.table(missing_combinations, file = "data_processed/missingcombos.txt", 
          row.names = FALSE, col.names = FALSE, sep = ", ",
          quote = FALSE)

