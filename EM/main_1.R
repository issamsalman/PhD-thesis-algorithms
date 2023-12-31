setwd("/home/salman/gobnilp/BN2O")
library(filesstrings)
library(graph)
library(mclust)
library(mix)
library(bnlearn)
library(graph)
nod<-c("A", "B", "C", "D" ,"E", "F" ,"G", "H" ,"I", "J", "K", "L", "M", "N", "O" ,"P", "Q", "R", "S", "T")
# we assume binary data with variables taking values 0 and 1 only
library(filesstrings)
library(graph)
# we assume binary data with variables taking values 0 and 1 only
call.gobnilp <- function(data, version="BIC"){
  data[data == -1] <- 1
  vars <- colnames(data)
  nr.states <- rapply(data.frame(data),function(x)max(x))+1
  # nr.states <- rep(0,length(vars))
  # for (j in 1:length(vars)){
  #   nr.states[j] <- length(unique(data[,j]))
  # }  
  # prepare data input file for Gobnilp
  
  cat(file="data.dat", sep="", length(vars),"\n")
  cat(file="data.dat", sep=" ", append=TRUE, nr.states)
  cat(file="data.dat", append=TRUE, "\n")
  cat(file="data.dat", sep="",  append=TRUE, nrow(data))
  cat(file="data.dat", append=TRUE, "\n")
  # Gobnilp requires values 0 and 1 instead of 1 and 2
  
  write.table(file="data.dat", append=TRUE, sep=" ", x=data, row.names=FALSE, col.names=FALSE)  
  # run Cassio de Campos's script and the Gobnilp scriptee
  system("./scoring2a data.dat 1 4 noprune 0 bic > bic.scores", ignore.stderr=TRUE)
  #system("type gobnilp")
  system("/usr/pkgs/bin/gobnilp bic.scores", ignore.stdout=TRUE, ignore.stderr=TRUE)
  #system("rm data.dat", ignore.stdout=TRUE, ignore.stderr=TRUE)  
  
  # read Gobnilp learned DAG saved as adjacency matrix
  #bn <- as.matrix(read.table("bi.mat",header=FALSE))
  #colnames(bn) <- vars
  #rownames(bn) <- vars
  #graph.mat <- new("graphAM", adjMat=bn, edgemode='directed')  
  #return(list(am=bn,graph=as(graph.mat, "graphNEL")))  
}

system("chmod 777 scoring2a")

#files <- list.files( path='./data',pattern = ".dat")
files <- list.files( pattern = ".dat")

table<-matrix( nrow = max(seq_along(files)), ncol = 5)
colnames(table)<-c('A2','Time',' --- ', "A1", "Time")



for (i in seq_along(files)) {
  
  start.time <- Sys.time()
  
  s_omit=na.omit(assign(paste(files[i]), read.csv(files[i], header=TRUE, sep=" ")))
  assign(paste(files[i],'com',sep = "_"), s)
  write.table(s_omit,file=paste(substr(files[i],1,nchar(files[i])-4),'com.dat',sep = "_"),
              col.names =FALSE, row.names = FALSE, sep = " ")
  name_file=paste(substr(files[i],1,nchar(files[i])-4),'com.dat',sep = "_")
  file.move(name_file, "/home/salman/gobnilp/BN2O/A1",overwrite = TRUE)
  
  call.gobnilp(s_omit)
  file.rename("bi.mat", paste(substr(files[i],1,nchar(files[i])-4),'_com.mat'))
  file.rename("bic.scores", paste(substr(files[i],1,nchar(files[i])-4),'_com.scores'))
  ss=paste(substr(files[i],1,nchar(files[i])-4),'_com.mat')
  file.move(ss, "/home/salman/gobnilp/BN2O/A1/MAT",overwrite = TRUE)
  s1s=paste(substr(files[i],1,nchar(files[i])-4),'_com.scores')
  file.move(s1s, "/home/salman/gobnilp/BN2O/A1/MAT/score",overwrite = TRUE)
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  table[i,4]= paste(files[i])
  table[i,5]= as.double(time.taken)
  
  start.time <- Sys.time()
  s=imputeData(assign(paste(files[i]), read.csv(files[i],header=TRUE, sep=" ")))
  #s=imputeData(assign(paste(files[i]), read.csv(files[i])))
  s[,]=as.integer(s[,])
  assign(paste(files[i],'EM',sep = "_"), s)
  write.table(s,file=paste(substr(files[i],1,nchar(files[i])-4),'EM.dat',sep = "_"),
              col.names =FALSE, row.names = FALSE, sep = " ")
  
  name_file=paste(substr(files[i],1,nchar(files[i])-4),'EM.dat',sep = "_")
  file.move(name_file, "/home/salman/gobnilp/BN2O/A1",overwrite = TRUE)
  
  s[s < 0]<- s[s < 0]*-1
  
  call.gobnilp(s)
  
  
  file.rename("bi.mat", paste(substr(files[i],1,nchar(files[i])-4),'_EM.mat'))
  file.rename("bic.scores", paste(substr(files[i],1,nchar(files[i])-4),'_EM.scores'))
  ss=paste(substr(files[i],1,nchar(files[i])-4),'_EM.mat')
  file.move(ss, "/home/salman/gobnilp/BN2O/A1/MAT",overwrite = TRUE)
  s1s=paste(substr(files[i],1,nchar(files[i])-4),'_EM.scores')
  file.move(s1s, "/home/salman/gobnilp/BN2O/A1/MAT/score",overwrite = TRUE)
  
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  
  table[i,1]= paste(files[i])
  table[i,2]= as.double(time.taken)
  
  
  
  
}
write.csv(table,'A1_A2.csv')
file.move('A1_A2.csv', "/home/salman/gobnilp/BN2O/A1/Time", overwrite = TRUE)
