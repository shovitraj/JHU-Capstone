library(doParallel)


startCluster<- function(){
        cores = detectCores() -1
        cluster<- makeCluster(cores)
        registerDoParallel(cluster)
        return (cluster)
}