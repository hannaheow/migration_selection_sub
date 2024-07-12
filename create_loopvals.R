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


