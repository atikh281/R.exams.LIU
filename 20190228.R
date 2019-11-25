# 1)
#a.

# library(parallel). An example for a function in this package is parLapply
# which icludes some different arguments such as:
# cl:a cluster object
# x: a matrix of clusters
# fun: function
# chunk.size: number of invocations of functions in fun

#b.

# "The apply family of functions is directly parallizable. So when these 
# functions are used in non-parallel code, minimal changes are required."

# 2)

cluster <- setRefClass("cluster", 
                       fields = list(
                         clusters = "list",
                         observations="list",
                         n_clusters = "numeric",
                         n_points = "numeric",
                         pval = "numeric"
                       ),
                       
                       methods = list(
                         initialize = function(p)
                         {
                           stopifnot(class(p)=="numeric")
                           clusters <<- list(clu_center = as.numeric(), clu_n_points = as.numeric(), unique_ID = as.numeric())
                           observations <<- list(obs=as.numeric(), belong = as.numeric(), distance = as.numeric())
                           n_clusters <<- 0
                           n_points <<- 0
                           i <- 0
                           j <- 0
                           pval <<-p
                         }, 
                         
                         create_cluster = function(cluster_center)
                         {
                           stopifnot(class(cluster_center)=="numeric")
                           if(n_clusters > 0)
                           {
                             for (i in 1:n_clusters)
                               if(cluster_center == clusters$clu_center[i])
                               {
                                 stop("The centre shuld be Unique for each cluster!")
                               }
                             
                           }
                           clusters$clu_center <<- append(clusters$clu_center, cluster_center)
                           n_clusters <<- n_clusters + 1
                           clusters$clu_n_points <<- append(clusters$clu_n_points, 0)
                           clusters$unique_ID <<- append(clusters$unique_ID,n_clusters)
                           
                         },
                         add_observation = function(obs){
                           stopifnot(is.numeric(obs))
                           min <- abs(clusters$clu_center[1]-obs)^pval
                           ind_min <- 1
                           for(i in 2:n_clusters)
                           {
                             dis <- abs(clusters$clu_center[i]-obs)^pval
                             if( dis < min){
                               min <- dis
                               ind_min <- i
                             } # we don`t face with a situation in which min == dis since we don`t have similar centers
                             
                           }
                           n_points <<- n_points + 1
                           observations$obs<<-append(observations$obs,obs)
                           observations$belong<<-append(observations$belong,ind_min)
                           observations$distance<<-append(observations$distance,min)
                           clusters$clu_n_points[[ind_min]] <<- clusters$clu_n_points[[ind_min]]+1
                           
                           
                         },
                         print = function() {
                           for (i in 1:n_clusters) {
                             cat(paste("Observations in cluster", i, ":", "\n"))
                             for (j in 1:n_points) {
                               if(observations$belong[j] == i){
                                 cat(paste(j, " ", sep = ""))
                               }
                             }
                             cat("\n")
                           }
                         },
                         plot = function() {
                           myData<- data.frame(id = observations$belong, 
                                               value = observations$obs, 
                                               cluster = clusters$clu_n_points)
                           ggplot(myData) +
                             aes(x = id, y = value, color = cluster) +
                             geom_point() +
                             theme_bw()
                         }
                         
                         
                         
                       )
)

# a.

build_data_object <- function(p)
{
  b <- cluster$new(p)
  return(b)
}

data_obj <- build_data_object(p=2)

# b.

create_cluster=function(obj,n)
{
  tp=runif(n,10,50)
  
  for(i in 1:n)
  {
    obj$create_cluster(tp[i])
  }
}

create_cluster(data_obj,10)

# c.

add_obs=function(obj,n)
{
  tp=runif(n,10,50)
  
  for(i in 1:n)
  {
    obj$add_observation(tp[i])
  }
  
}

add_obs(data_obj,50)

# d.

data_obj$print()
data_obj$plot()


# Problem 3 ----
# a.
my_binom_coef <- function(n, k) {
  nValue <- 1
  for (i in 1:n) {
    nValue <- nValue * i
    if (i == (n - k)) {
      n_kValue <- nValue
    }
    if (i == k) {
      kValue <- nValue
    }
  }
  return(nValue / (n_kValue * kValue))
}

my_binom_coef(10, 8)

# b.
"O(1) or O(n)"

# c.
library("testthat")
y <- my_binom_coef(10, 8)
expect_equal(y, choose(10,8))
expect_equal(choose(10,8),45)
