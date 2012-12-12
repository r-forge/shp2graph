\name{nel2igraph}
\title{Produce a "igraph" object}
\description{
This function is to produce an "igraph" object based on package "igraph" using generated nodelist and edgelist from a given network.
}
\usage{
nel2igraph(nodelist, edgelist, weight=NULL, eadf=NULL, mode="all")
}
\arguments{
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{weight}{A numberic vector (of the length equal to the number of edges)for weighting all the edges in the edgelist;}
  \item{eadf}{Attribute data frame for all the edges;}
  \item{mode}{Character scalar, it specifies whether the graph to create is undirected (‘all’ or ‘total’) or directed; and in the latter case, whether it contains the outgoing (‘out’) or the incoming (‘in’) neighbors of the vertices.
}   
}
\value{
  \item{gr}{An object of "igraph"} 
}
\details{
1. The vector "weight" will be the default weight for all the computations carried out on the produced "igraph" object.
2. The coordinates of vertices are attached to them as vertex attributes "X" and "Y", which could be retrived with function "get.vertex.attribute"
from package "igraph".
}
\examples{
data(ORN)
rtNEL<-readshpnw(rn, ELComputed=TRUE)
#Add the edge length as the weight for graph
igr<-nel2igraph(rtNEL[[2]],rtNEL[[3]],weight=rtNEL[[4]])
coord<-cbind(get.vertex.attribute(igr, "X"), get.vertex.attribute(igr, "Y"))
#plot.igraph(igr,axes=FALSE,layout=coord, mark.shape="circle", mark.col="green", mark.expand=2,vertex.label=NA)
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\seealso{\code{\link{igraph}}}
\keyword{spatial,igraph}
