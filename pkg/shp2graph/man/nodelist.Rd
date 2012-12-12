\name{nodelist}
\title{A structure for storing node information}
\description{
This is an intergradation of nodes from spatial data to graph data, and each node cooresponds to one row in the object, of which the row structure is
designed as [NodeID,coordinate(X,Y)].
}
\details{
This list is the immediate result of extracting nodes from a "SpatialLines" or "SpatialLinesDataFrame" object, see \link{readshpnw}. As a transition object, it is
a necessary input for many functions in this package, like\link{Degree.list}, \link{ME.simplification}, \link{Nodes.coordinates}. 
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{node}
