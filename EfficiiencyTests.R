start <- Sys.time()
a <- c()
for(i in 1:10)
{
  for(j in 1:5000)
  {
    a <- c(a,  j^i)
  }
  
}
end <- Sys.time()
end-start



start <- Sys.time()
a <- c()
for(i in 1:10)
{
  a <- c(a, sapply(1:50000, function(x,y){x^y}, y = i))
}
end <- Sys.time()
end-start



no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
start <- Sys.time()
a <- c()
for(i in 1:10)
{
  clusterExport(cl, c())
  a <- c(a, parSapply(cl,1:50000, function(x,y){x^y}, y = i))
}
end <- Sys.time()
stopCluster(cl)
end-start


no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
start <- Sys.time()
a <- c()
a <- sapply(1:10, function(x, connection){clusterExport(cl, c())
                              parSapply(cl,1:50000, function(x,y){x^y}, y = i)}, connection = cl
            )

end <- Sys.time()
stopCluster(cl)
end-start



no_cores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(no_cores)
clusterExport(cl, c())
start <- Sys.time()
a <- parSapply(cl,1:10, function(i){sapply(1:50000, function(x,y){x^y}, y = i)})
end <- Sys.time()
stopCluster(cl)
end-start



start <- Sys.time()
aa <- c("a", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c")
paste("^", aa, " ", sep = "")
end <- Sys.time()
end-start


start <- Sys.time()
aa <- c("a", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c", "b", "c")
sprintf("%s%s%s","^", aa, " ")
end <- Sys.time()
end-start