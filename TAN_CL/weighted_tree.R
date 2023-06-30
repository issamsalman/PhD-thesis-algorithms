#                      >>>>>>>>>>to find the tree<<<<<<<<<<<<<<<<<<

#  >>>>>>>>>> function to check if the graph has acycle<<<<<<<<<<<

library(ggm)
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


# it will be the finl correlation matrix 
mat_con<-mat
# undirected graph
mat_con1<- mat_con
# matrix for components 
#mat_test<- mat
# number of node
nodes_num <- nrow(mat_con)
#number of edges
edge_num= nodes_num -2

# number of all connectaion 
nm<-nrow(new_arcs)

# add the first two nodes to graph
i_nod<- new_arcs[1,1]
j_nod<-new_arcs[1,2]
mat_con[i_nod,j_nod]<-1
mat_con1[i_nod,j_nod]<-1
mat_con1[j_nod, i_nod]<-1
i_nod1<- new_arcs[2,1]
j_nod1<-new_arcs[2,2]
mat_con[i_nod1,j_nod1]<-1
mat_con1[i_nod1,j_nod1]<-1
mat_con1[j_nod1, i_nod1]<-1
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
    jjm<- jjm+1
  }
  if (jjm==edge_num){break}
}

# to do a directed tree
for(i in nodes_num:2){
  #edg_in <- sum(mat_con[,i])
  if (sum(mat_con[,i])>1){
    for (h in 1:nodes_num){
      if (mat_con[h,i]==1){
        mat_con[h,i]<-0
        mat_con[i,h]<-1
      }
     # edg_in1<-sum(mat_con[,i])
      if(sum(mat_con[,i])==1){break}
    }
  }
}

#to make edg between the gool and all nodes
for (i in 2:nodes_num) {mat_con[1,i]<- 1}
# drawing graph  tree:

library(graph)
am.graph<-new("graphAM", adjMat=mat_con, edgemode="directed")
plot(am.graph, attrs = list(node = list(fillcolor = "lightblue"),
                            edge = list(arrowsize=1)))

