source('source.R')

# paper example
x <- c(0,0.47,1.56,2.61,2.92,2.49,1.09,0.35,0.62,1.18,1.69,2.33,2.02,1.42,0.84)
y <- c(0.83,0.17,0,0.27,0.95,1.71,1.81,1.47,1.11,0.94,1.26,1.14,0.67,0.71,0.59)
t <- c("P","P","P","P","P","P","P","P","O","O","O","O","O","O","O")

## huang
# x <- c(0,1,1,1.5,4,4,1.5,0,1,2,3,2)
# y <- c(0,0,1.5,2,2,4,4,2.5,2.5,3.5,3.5,2.5 )
# t <- c("P","P","P","P","P","P","P","P","O","O","O","O")


df <- data.frame(x,y,t, stringsAsFactors = F)
plot.figure(df)


step_1_res <- step_1(df, plot = T)

step_2_res <- step_2(df, plot = T)

# step_3_res: polygons combinations,  split into groups according to number of polygons in the combination (1, 2, 3, ...)
#  [[1]] : list of dfs containing the combination's polygons coordinates
#  [[2]] : list of vectors containing the combination's polygons IDs
#  [[3]] : list of vectors containing the width of the polygon combination (weight of each element in [[2]])
step_3_res <- step_3(df, step_2_res = step_2_res)

# weight: width of each combination
weight <- (step_3_res[[3]] %>% unlist(recursive = F) %>% do.call(rbind,.))
sub.list <- step_3_res[[2]] %>% unlist(recursive = F) # same as step_3_res[[2]] but as a single list of vectors
# a: matrix: columns: subpolygons, rows: combinations, element[col][row]: 1 if subpolygon col is in combination row
a <- lapply(sub.list, function(x){
  a <- rep(0,length(step_3_res[[1]][[1]]))  # step_3_res[[1]][[1]] = list of combinations of single polygon => length(step_3_res[[1]][[1]]) = number of sub polygons
  a[x] <- 1
  a
}) %>% do.call(rbind,.)


## Step 4
## --- GLPK ---
library(Rglpk)
obj <- weight %>% as.vector()   # object function
mat <- a %>% as.matrix() %>% t()   # constraints left hand side
rhs <- rep(1,ncol(a))  # constraints right hand side
max <- FALSE  # minimize
dir <- rep('==',ncol(a))  # operators in the constraints (=, <=, <, > , >=)
types <- rep("B",ncol(a))   # all variables are binary

# result: df
#   solution: the chosen subpolygons combinations
result <- Rglpk_solve_LP(obj, mat, dir, rhs, max = max)

subpolygons.list = step_3_res[[1]] %>% unlist(recursive = F)
subpolygons_optimal <-subpolygons.list[result$solution==1]

print(result)
print("chosen combinations")
print(sub.list[result$solution==1]) # get the chosen combinations of subpolygons
print("chosen polygons")
print(subpolygons_optimal)

plot.figure.subpoly(subpolygons_optimal)

# objval## --- gurobi ---
# library(gurobi)

# model <- list()

# model$A          <- a %>% as.matrix() %>% t()  # constraints left hand side
# model$obj        <- weight %>% as.vector()  # object function
# model$modelsense <- 'min'
# model$rhs        <- rep(1,ncol(a))  # constraints right hand side
# model$sense      <- rep('=',ncol(a))  # operators in the constraints (=, <=, <, > , >=)
# model$vtype      <- 'B'  # all variables are binary

# params <- list(OutputFlag=0, PoolSearchMode=2)

# result <- gurobi(model, params)

# print('Solution:')
# print(result$objval)
# print(result$x)

