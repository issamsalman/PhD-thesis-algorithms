
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
f<-x[,1]
f1<- x[,2]
f3<- x[,3]

I <- condinformation(f1,f3,f,method="emp") 
I<-as.double(I)
options(scipen=999)
return (I)
}

"*
#function to calculate the the probabilistic between two columns 
L_column <-function(x=c[,]){
  m <- min(x[,1],na.rm = TRUE)
  m1 <- min(x[,2],na.rm = TRUE)
  m2<- min(x[,3],na.rm = TRUE)
  b <- max(x[,1],na.rm = TRUE)
  b1 <- max(x[,2],na.rm = TRUE)
  b2 <- max(x[,3],na.rm = TRUE)
  r<- nrow(x)
  num1= as.numeric (0) # p(z)
  num2= as.numeric(0) # p(x,y,z)
  num3= as.numeric(0) # p(y,z)
  num4= as.numeric(0) #P(x,z)
  ni = as.numeric(0)
  nj = as.numeric(0)
  Lij=as.numeric(0)
  #cat (m," ", m1, "  ", b,"  ", b1)
  #p(x,y|z)
  
  for (i in m:b){
    for (j in m1:b1){ 
      for (k in m2:b2){
      for (f in 1:r) {
        
        fir <- x[f,1] # z = i
        sec <- x[f,2]  # y =j
        third <- x[f,3]  # x =k
         n_fir<- ifelse((fir==i),1,0)
        num1 <- num1 + n_fir
        n_sec <- ifelse((fir==i)& (sec==j)&(third==k),1,0)
        num2 <- num2 + n_sec
        n_thir <- ifelse((sec==j)&(fir==i),1,0)
        num3<-num3 + n_thir
        n_fourth <- ifelse((third==k)&(fir==i),1,0)
        num4 <- num4 + n_fourth
      }
      
        
    num1 <- num1 / r
  #  cat("num1 ",num1, "\n")
 
    num2 <- num2/r
  #  cat("num2-r ",num2, "\n")
    num3<- num3/r
  # cat("num3 ",num3, "\n")
    num4<- num4/r
  #  cat("num4 ",num2, "\n")
    if (num4 )
      if ((num4>0) & (num3>0) & (num2>0) & (num1>0) ){
        nj <- nj +1
    tt<- (num1*num2)/(num3*num4)
   
    kj<- (num2 * log (tt))
          Lij<- Lij + kj
      }
          num1<-0
          num2<-0
          num3<-0
          num4<-0
          kj<-0
    } 
    }
  
  }
  

  options(digits=9) 
 
 return(Lij)
}

"

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
#x<-read.csv("test-hugin2-miss5.csv",header=TRUE)
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
for (i in 2:c){  # 1:c 
  j1=i+1
  Nfrom <- cc1[i]
  
  for(j in j1:h){
    Nto<- cc1[j]
    nn <- x[,c(1,i,j)] 
    # mmm<- n_column(nn)
    nn1<-n_column(nn)
    if(nrow(nn1)>=1){
    L_ij <- L_column(nn1)
    L_ij1<-signif(L_ij,digits=6)}
    else {L_ij1<-0}
    
    #if (L_ij > 0.001){
    cat("L[ ", i , ", ",j,"]=",L_ij1, "\n")
    mat1[i,j]<- L_ij1
    

    #nn1 <- x[,c(1,j,i)] 
    #L_ij2 <- L_column1(nn)
    # L_ij12<-signif(L_ij,digits=4)
    #mat2[i,j]<- L_ij1
    #cat("L1[ ", j , ", ",i,"]=",L_ij1, "\n")
    #L_ij12<- L_ij12*(-1)
    mat2[j,i]<- L_ij1
    
    if(i==2 & j==3){ 
     arcs[1,]<-c(i,j,L_ij1)
      arcs1[1,]<-c(Nfrom,Nto,L_ij1)
     }
    else{
      Q<-c(Nfrom,Nto, L_ij1)
   hhm<- signif(L_ij,digits=4)
      arcs <-rbind(arcs, c(i , j , hhm) )
      arcs1 <-rbind(arcs1, c(Nfrom,Nto,L_ij1) )
    }
    
    
    #  }
  }

}


#to sort to correlation matrix from-to using name's node
lklk<-arcs[order(arcs[,3]),]
new_arcs<-sortt(lklk)


#to sort to correlation matrix from-to using number
lklk1<-arcs1[order(arcs1[,3]),]
new_arcs1<-sortt(lklk1)
