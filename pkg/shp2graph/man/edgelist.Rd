\name{edgelist}
\title{A structure for storing edge information}
\description{
This is an intergradation of edges from spatial data to graph data, and each node cooresponds to one row in the object, of which the row structure is
designed as [EdgeID,NodeID(from),NodeID(to)].
}
\details{
This list is the immediate result of establishing edges between nodes from a "SpatialLines" or "SpatialLinesDataFrame" object, see \link{readshpnw}. As a transition object, it is
a necessary input for many functions in this package, like\link{Degree.list}, \link{ME.simplification}, \link{nel2graphNEL}. 
}
\note{
If the argument "Detailed" specified in \link{readshpnw} is TRUE, all the endnodes constructing polyines will be extracted as nodes, then the generated graph will have
the same spatial details with the transformed "SpatialLines" or "SpatialLinesDataFrame" object. On the consideration of distributing attributes to the new edges based on
the original information in the transformed "SpatialLinesDataFrame" object, the original edge ID is also kept and the row structure will be [EdgeID,eid,NodeID(from),NodeID(to)],
in which EdgeID means the new edge id while eid represents the original edge id.  
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{edge}
