name <- "Christian Jonsson"
liuid <- "chjon338"

# step 1: Check if names df are the expected names
# step 2: Check if all the elements in the df are the expected type, i.e. vector, use an anonymus function and sapply so i can use it directly in stopifnot
 dijkstra <-function(graph, init_node.){
    expectedNames <- c("v1", "v2", "w")
    stopifnot(identical(names(graph), expectedNames))
    stopifnot(sapply(expectedNames,function(name) is.vector(graph[[name]])))

# Psedo-code: 
#   function Dijkstra(Graph, source):
#      for each vertex v in Graph.Vertices:
#          dist[v] ← INFINITY
#          prev[v] ← UNDEFINED
#          add v to Q
#      dist[source] ← 0
    
#      while Q is not empty:
#          u ← vertex in Q with minimum dist[u]
#          remove u from Q
        
#          for each neighbor v of u still in Q:
#              alt ← dist[u] + Graph.Edges(u, v)
#              if alt < dist[v]:
#                  dist[v] ← alt
#                  prev[v] ← u
    #  return dist[], prev[]
}
