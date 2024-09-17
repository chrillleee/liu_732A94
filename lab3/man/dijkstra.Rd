\name{dijkstra}
\alias{dijkstra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Capitalized Title (ideally limited to 65 characters)
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dijkstra(graph, init_node)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
%%     ~~Describe \code{graph} here~~
}
  \item{init_node}{
%%     ~~Describe \code{init_node} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().

## The function is currently defined as
function (graph, init_node) 
{
    expectedNames <- c("v1", "v2", "w")
    stopifnot(identical(names(graph), expectedNames))
    stopifnot(sapply(expectedNames, function(name) is.vector(graph[[name]])))
    stopifnot(is.numeric(init_node))
    priorityQueue <- getPriorityQueue(graph, init_node)
    savedData <- priorityQueue
    while (length(priorityQueue) > 0) {
        u <- getMinDistVertice(priorityQueue)
        savedData[[u]] <- priorityQueue[[u]]
        priorityQueue[[u]] <- NULL
        neighbors <- getNeighbours(graph, priorityQueue, u)
        for (neighbor in neighbors) {
            neighborData <- priorityQueue[[neighbor]]
            alt <- savedData[[u]]$dist + getWeightFromName(u, 
                neighbor, graph)
            if (alt < neighborData$dist) {
                priorityQueue[[neighbor]]$dist <- alt
                priorityQueue[[neighbor]]$prev <- u
            }
        }
    }
    outputVector <- unname(sapply(savedData, function(x) x$dist))
    return(outputVector)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
