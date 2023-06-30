library(graph)
library(e1071)
library(mix)
library(bnlearn)
library(pcalg)
library(Rgraphviz)
source("plot-graph-functions.R")

setwd("/home/salman/gobnilp/BN2O/A1/MAT")

true_model <- read.net('./TrueModel.net')
nod<-c("A", "B", "C", "D" ,"E", "F" ,"G", "H" ,"I", "J", "K", "L", "M", "N", "O" ,"P", "Q", "R", "S", "T")

e1 = empty.graph(nod)
amat(e1) = amat(true_model)

files <- list.files(pattern = ".mat")
table<-matrix( nrow = max(seq_along(files)), ncol = 7)
colnames(table)<-c('Filename','Recall','Precision', "Hamming", "SHD", "FP","FN")
L1 = 10 # level 1 of BN2O
L2 = 10 #Level 2 of BN2O
for (i in seq_along(files)) {
  
  bi=(assign(paste(files[i]), read.csv(files[i],header=FALSE, sep=" ")))
  
  colnames(bi)<- nod
  row.names(bi)<- nod
  
  bi<-as.matrix(bi)
  #empty graph
  e = empty.graph(nod)
  amat(e) = bi
  #pdf(file=paste(substr(files[i],1,nchar(files[i])-4),sep="", ".pdf"),width = 8, height = 8)
  name2 <- paste(unlist(strsplit(files[i], split='.', fixed=TRUE))[1],".pdf",sep="")
  name3 <- paste(unlist(strsplit(files[i], split='.', fixed=TRUE))[1],"-compared.pdf",sep="")
  two.graphs.cmp(e1=e1,e2=e2,nr.L1=L1,nr.L2=L2,name2=name2,name3=name3)
  
  a<-compare (cpdag(e), cpdag(e1))
  rec<-a$tp/(a$tp+a$fn)
  pr<-(a$tp)/(a$tp+a$fp)
  table[i,1]= paste(files[i])
  table[i,2]=rec  
  table[i,3]=pr
  table[i,4]=hamming(cpdag(e),cpdag(e1))
  table[i,5]=sum(sapply(1:ncol(bi), function(i) hamming.distance(bi[,i], amat(e1)[,i])))
  table[i,6]=a$fp
  table[i,7]=a$fn
  
}
write.csv(table,'result.csv')
