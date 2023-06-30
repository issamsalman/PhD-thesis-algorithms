
#to make new dataset where nminoritie calss and subset from majority clas
sub_data <-function(x=c[,],x1=c[,], from, to){
  
  for (i in from:to){
    x<-rbind(x,x1[i,])
  }
  hh<-x
  return(hh)
}

# function to delete the row who has NA value 
n_column <- function(x=c[,]){
  newx <- x[c(0),]
  r<- nrow(x)
  for (k in 1:r){
    if(sum(is.na(x[k,]))==0) 
      newx <-rbind(newx, x[k,] )
  }
  hh<- newx
  return(hh)
}

# function to sort
sortt <- function(x=c[,]){
  newx <- x[c(0),]
  r<- nrow(x)
  for (k in r:1){
    
    newx <-rbind(newx, x[k,] )
  }
  hh<- newx
  return(hh)
}


#function to calculate the the probabilistic between two columns given the class
L_column <-function(x=c[,]){
  library(infotheo)
  f<-x[,3]
  f1<- x[,2]
  I <- mutinformation(f1,f) 
  I<-as.double(I)
  options(scipen=999)
  return (I)
}


##
#x<-read.csv("czech-data.csv",header=TRUE)
#x<-read.csv("czech-data-Copy.csv",header=TRUE)
#x<-read.csv("foo.csv",header=TRUE)
#x<-read.table("czech-data.dat", header = TRUE, sep=",") 
#x<-read.table("TEST-8-var1.dat", header = TRUE, sep=",")
#x<-read.csv("TEST-8-var.csv",header=TRUE)
x<-read.csv("TEST-8-var_20thousand.csv",header=TRUE)
#x<-read.table("TEST-12-var-new.dat", header = TRUE, sep=",")
#x<-read.csv("Syria-data.csv",header=TRUE)
#x<-read.csv("syria-data-Copy.csv",header=TRUE)
#x<-read.csv("DISCR2-copy.csv",header=TRUE)
#x<-read.csv("test-hugin-data.csv",header=TRUE)
#x<-read.csv("test-hugin-data2.csv",header=TRUE)
#x<-read.csv("test-hugin-data3.csv",header=TRUE)
#x<-read.csv("Binary-D-BIN.csv",header=TRUE)
h<- ncol(x) #length of col
c<-h-1
# matrix for graph
mat = matrix(nrow=h, ncol=h)
#mat<- as.array(nrow=h, ncol=h)
#name's of columns 
cc1<-colnames(x[1,])
colnames(mat)<- cc1
rownames(mat)<-cc1
for (k in 1:h){
  for (kk in 1:h){
    mat[k,kk]<-0
  }
  
}
mat1<-mat
mat2<-mat

# to do correlation matrix from to
#arcs<-matrix(, , 3)
arcs =  matrix ( ,ncol= 3)

#kkm<-c("from", "to", "weight")
#colnames(arcs)<-kkm
ee<-cc1[1]
#oo<-c(ee,ee,0)
#arcs[1,]<-oo


# to create the correlation matrix
arcs1<-arcs

for (i in 1:c){  # 1:c 
  j1=i+1
  Nfrom <- cc1[i]
  
  for(j in j1:h){
    Nto<- cc1[j]
    # the variables for CMI
    nn <- x[,c(1,i,j)] 
    sum_CMI<-0
    # to delete the NA
    nn1<-n_column(nn)
    mylist <- split(nn1, nn1$Mortality)
    # number of Minoritie class
    mano<- nrow(mylist[[2]])
    # nimber of majority class
    maj<- nrow(mylist[[1]])
    maj_data<-mylist[[1]]
    mono_data <-mylist[[2]]
    
    
    # majority class more than minoritie class L time
    L =  as.integer(maj/mano)
    to1<-L
    from1<-0 
    from1<-1 
    to1<-mano
    
    
    for (z1 in 1: L){
      mono_data1<-mono_data
      if (z1==L){  subdata<-sub_data(mono_data, maj_data, from1,maj)
      L_ij <- L_column(subdata) 
      sum_CMI<-sum_CMI+L_ij
      # cat("L[ ", i,"_",z1 , ", ",j,"_",z1,"]=",L_ij, "\n")
      }
      else {
        subdata<-sub_data(mono_data, maj_data, from1,to1)
        from1<-from1+mano
        to1<- to1+mano
        L_ij <- L_column(subdata)
        sum_CMI<-sum_CMI+L_ij
        # cat("L[ ", i,"_",z1 , ", ",j,"_",z1,"]=",L_ij, "\n")
      }
    }
    
    ava_CMI<-sum_CMI/L
    L_ij1<-signif(ava_CMI,digits=6)
    cat("L[ ", i , ", ",j,"]=",L_ij1, "\n")
    mat1[i,j]<- L_ij1
    
    if(i==1 & j==2){ 
      arcs[1,]<-c(i,j,L_ij1)
      arcs1[1,]<-c(Nfrom,Nto,L_ij1)
    }
    else{
      Q<-c(Nfrom,Nto, L_ij1)
      hhm<- signif(L_ij,digits=4)
      arcs <-rbind(arcs, c(i , j , hhm) )
      arcs1 <-rbind(arcs1, c(Nfrom,Nto,L_ij1) )
    }
    
  }
  
  
}


#to sort to correlation matrix from-to using name's node
lklk<-arcs[order(arcs[,3]),]
new_arcs<-sortt(lklk)


#to sort to correlation matrix from-to using number
lklk1<-arcs1[order(arcs1[,3]),]
new_arcs1<-sortt(lklk1)

edge_num= h -1
