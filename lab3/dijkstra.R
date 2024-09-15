name <- "Christian Jonsson"
liuid <- "chjon338"

# step 1: Check if names df are the expected names
# step 2: Check if all the elements in the df are the expected type, i.e. vector, use an anonymus function and sapply so i can use it directly in stopifnot
 dijkstra <-function(graph, init_node){
    expectedNames <- c("v1", "v2", "w")
    stopifnot(identical(names(graph), expectedNames))
    stopifnot(sapply(expectedNames,function(name) is.vector(graph[[name]])))
    stopifnot(is.numeric(init_node))

    priorityQueue <- getPriorityQueue(graph,init_node)

    while(length(priorityQueue) > 0){
        u <- getMinDistVertice(priorityQueue)
        priorityQueue[[u]] <- NULL

        break
    }
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


getPriorityQueue <- function(dataFrame,initNode){
    vertices <- unique(c(dataFrame$v1, dataFrame$v2))

    # Throw error if initNode not in provided data
    stopifnot(initNode %in% vertices)

    priorityQueue <- lapply(vertices, function(x) createProperties())

    # Have to apply the names explicitly in case of scrambled vertice numbers 
    names(priorityQueue) <- vertices
    
    priorityQueue[[initNode]]$dist <- 0
    return(priorityQueue)
}

createProperties <- function() {
  list(dist = Inf, prev = NA)
}

getMinDistVertice <-function(priorityQueue){
    minVertice <- list()
    verticeName <- c()

    for(name in names(priorityQueue)){
        evaluatedVertice <- priorityQueue[[name]]

        if(length(minVertice)==0){
            minVertice = evaluatedVertice
            verticeName <- name
        }

        if(minVertice$dist > evaluatedVertice$dist){
            minVertice <- evaluatedVertice
            verticeName <- name
        }
    }
    return(verticeName)
}
