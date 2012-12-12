\name{Directed}
\title{Orientate each edge in a given edgelist}
\description{
This function is to set the orientation of each edge according to the passing property in the given vector.
}
\usage{
Directed(edgelist, direction.v=rep(0,length(edgelist[,1])), eadf=NULL)
}
\arguments{
  \item{edgelist}{An edgelist object, see \link{edgelist}}
  \item{direction.v}{A vector (of length equal to number of edges in edgelist) consisted of 1 or 0, see "details";}
  \item{eadf}{Attribute data frame for all the edges;}  
}
\value{
A list consisted of:
  \item{newEdgelist}{A new edge list consisted of directed edges}
  \item{newEadf}{A new attribute data frame for the new edgelist}
}
\details{
In a vector road-network map, some road segments might be one-way while the rest are double way. This suitation makes it complex to organise all these edges in a graph.
This property could be specified in "direction.v": 1 means one-way and 0 represents double-way. All the double-way edges in edgelist are clarified as two directed edges
with opposite directions (i.e. edge(nid1, nid2)-->edge(nid1, nid2) and edge(nid2, nid1)) and duplicate the corresponding attributes. The one-way edges could be  arbitrarily
regarded directed.
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{directed, graph}
