# Set-up: Parallel computing 

# number of cores to put in cluster
cpu_cores <- detectCores() - 1

# intialize cluster
cl <- makeCluster(cpu_cores)

# export functions and objects needed to run model
exp <- c("param_chunks", 
         "ini_list", 
         "newcore", 
         "iterate_model")
clusterExport(cl, exp)