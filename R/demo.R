# Packages
library(parallel)
library(sf)
library(pbapply)

# Sample Data
mat = matrix(runif(1e6,max = 1e5), ncol = 2)
seqs = 1:(nrow(mat)-1)
ncores = 6 # Change as needed

start1 = Sys.time()
# Code not running in a function #########
cl = parallel::makeCluster(ncores)
parallel::clusterExport(cl=cl, varlist=c("mat"), envir = environment())
geoms = pbapply::pblapply(seqs, function(y){sf::st_linestring(mat[c(y,y+1),c(1,2)])}, cl = cl)
parallel::stopCluster(cl)

end1 = Sys.time()

# Same code in a function ###########
func = function(mat, seqs, ncores){
  #newEnv <- new.env()
  cl = parallel::makeCluster(ncores)
  parallel::clusterExport(cl=cl, varlist=c("mat"))
  geoms = pbapply::pblapply(seqs, function(y){sf::st_linestring(mat[c(y,y+1),c(1,2)])}, cl = cl)
  parallel::stopCluster(cl)
  return(geoms)
}

start2 = Sys.time()
res = func(mat, seqs, ncores)
end2 = Sys.time()

# # Nested Function
# func2 = function(mat, seqs){
#   geoms = func(mat, seqs, ncores)
# }
# 
# start3 = Sys.time()
# res2 = func(mat, seqs, ncores)
# end3 = Sys.time()

# Compare Results 
difftime(end1, start1, units = "s")
difftime(end2, start2, units = "s")
#difftime(end3, start3, units = "s")
