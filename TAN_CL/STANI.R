#                      >>>>>>>>>>to find the tree<<<<<<<<<<<<<<<<<<

#  >>>>>>>>>> function to check if the graph has acycle<<<<<<<<<<<

library(ggm)
library(graph)
is.acyclic <- function(a){
  
  comps <- conComp(a)
  nr.comps <- max(comps)
  acyclic <- TRUE
  for (i in 1:nr.comps){
    # check a node if it is as connectivity components
    #check_nodw <- sum(conComp(a)==i)
    if(sum(conComp(a)==i) > 1){
    if (length(fundCycles(a[conComp(a)==i,conComp(a)==i])) != 0){
      acyclic <- FALSE
      break
    }
  }
  } 
  #   >>>>>> return TRUE if there is no cycle<<<<<<
  return(acyclic)
}

# finle arcs
arcs2<-matrix ( ,ncol= 3)

# it will be the finl correlation matrix 
mat_con<-mat
# undirected graph
mat_con1<- mat_con
# matrix for components 
#mat_test<- mat
# number of node
nodes_num <- nrow(mat_con)


# number of all connectaion 
nm<-nrow(new_arcs)

# add the first two nodes to graph
i_nod<- new_arcs[1,1]
j_nod<-new_arcs[1,2]
Nfrom<-new_arcs1[1,1]
Nto<-new_arcs1[1,2]
w<-new_arcs1[1,3]
arcs2[1,1]<-Nfrom 
arcs2[1,2]<-Nto
arcs2[1,3]<-w
mat_con[i_nod,j_nod]<-1
mat_con1[i_nod,j_nod]<-1
mat_con1[j_nod, i_nod]<-1
i_nod1<- new_arcs[2,1]
j_nod1<-new_arcs[2,2]
mat_con[i_nod1,j_nod1]<-1
mat_con1[i_nod1,j_nod1]<-1
mat_con1[j_nod1, i_nod1]<-1
arcs2<-rbind(arcs2,c(new_arcs1[2,]))
jjm = 2
for(i in 3:nm){
  
  # it's a matrix which test after add new edg
  mat_test<- mat_con1
  from_1<- new_arcs[i,1]
  to_1<- new_arcs[i,2]
  
  mat_test[from_1,to_1]<-1
  mat_test[to_1, from_1]<-1
  a<-is.acyclic(mat_test)
  if (a==TRUE){
    mat_con[from_1,to_1]<-1
    mat_con1[from_1,to_1]<-1
    mat_con1[to_1,from_1]<-1
    arcs2<-rbind(arcs2,c(new_arcs1[i,]))
    jjm<- jjm+1
  }
  if (jjm==edge_num){break}
}

mat_con4<-mat_con

# to do a directed tree
for(i in nodes_num:2){
  #edg_in <- sum(mat_con[,i])
  if (sum(mat_con4[,i])>1){
    
    for (h in 2:nodes_num){
      if (mat_con4[h,i]==1){
        mat_con4[h,i]<-0
        mat_con4[i,h]<-1
      }
     # edg_in1<-sum(mat_con[,i])
      if(sum(mat_con4[,i])==1){break}
    }
  }
}

#to make edg between the class and all nodes for TAN 
for (i in 2:nodes_num) {mat_con4[1,i]<-1 }

# to do STAN
#function to calculate the MI  between each attribute and thethe class
MI_CA <-function(x=c[,]){
  library(infotheo)
  f<-x[,1]
  f1<- x[,2]
  I <- mutinformation(f1,f) 
  I<-as.double(I)
  options(scipen=999)
  return (I)
}

# to add the edge between the class and the attribute if MI >= 0.05
#to make edg between the class and all nodes for TAN 
for (i in 2:nodes_num) {
  nn <- x[,c(1,i)] 
  # mmm<- n_column(nn)
  #nn5<-n_column(nn)
  hh<- MI_CA(nn)
  cat("L[ ", 1 , ", ",i,"]=",hh, "\n")
  if(hh>0.03){
  mat_con4[1,i]<- 1}
  else {
    for (k in 1:nodes_num){
      mat_con4[i,k]<-0
      mat_con4[k,i]<-0
      
    }
    
  }
}

########### to do STAN with implanced #################

  
  for(i in 2:nodes_num){
   
    # the variables for CMI
    nn <- x[,c(1,i)] 
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
      L_ij <- MI_CA(subdata) 
      sum_CMI<-sum_CMI+L_ij
      # cat("L[ ", i,"_",z1 , ", ",j,"_",z1,"]=",L_ij, "\n")
      }
      else {
        subdata<-sub_data(mono_data, maj_data, from1,to1)
        from1<-from1+mano
        to1<- to1+mano
        L_ij <- MI_CA(subdata)
        sum_CMI<-sum_CMI+L_ij
        # cat("L[ ", i,"_",z1 , ", ",j,"_",z1,"]=",L_ij, "\n")
      }
    }
    
    ava_CMI<-sum_CMI/L
    L_ij1<-signif(ava_CMI,digits=6)
    cat("L[ ", i , ", ",1,"]=",L_ij1, "\n")

    if( L_ij1>0.03){
      mat_con4[1,i]<- 1}
    else {
      for (k in 1:nodes_num){
        mat_con4[i,k]<-0
        mat_con4[k,i]<-0
        
      }
      
    }
  
    
  }
  
  

#########################################


# drawing graph  tree:


am.graph<-new("graphAM", adjMat=mat_con4, edgemode="directed")
plot(am.graph, attrs = list(node = list(fillcolor = "lightblue"),
                            edge = list(arrowsize=1)))


### for testing 

library(bnlearn)
test_real<- read.csv("MyData_complete_data.csv",header=TRUE) #Dis.Data
test_real<- read.csv("BIN_DATA_com.csv",header=TRUE)

for (i in 1:23) {
  test_real[,i]<-as.factor(test_real[,i])
  }
e = empty.graph(cc1)
amat(e)= mat_con4
training.set = test_real[1:250,]
demo.set = test_real[250:h, ]
fit <- bn.fit(e, test_real)
bn.cv(test_real, bn = e)


# result using bayesclass

library(bnclassify)
e1 = empty.graph(cc1)
for (i in 1:h){
x[,i]<- as.factor(x[,i])
}
tan.x1 <- bnc('tan_cl', 'Mortality', x, smooth = 1, dag_args = list(score = 'aic'))
nb <- bnc('nb', 'Mortality', x, smooth = 1)
tan.x <- bnc('tan_cl', 'Mortality', x, smooth = 0)
ll<-plot(tan.x1)
kh<- as.bn(ll)
arc.set<-kh$arcs
arcs(e1) = arc.set

tn <- lp(tan.x1, x, smooth = 0.01)
p <- predict(tan.x1, x, prob = TRUE)

gg<-cv(tn, x, k = 50)
logLik(tn, x) # compute the log-likelihood


# to convert the graph to bn.fit
kh<- as.bn(ll)

fit <- bn.fit(kh, test_real)


#to save as hugin file
write.net(file = " type a name!!!!.net", fitted = fit)
