\name{nel2igraph}
\title{Produce a "igraph" object}
\description{
This function is to produce a "igraph" object using generated nodelist and edgelist from a given network.
}
\usage{
nel2igraph(nodelist, edgelist, weight=NULL, eadf=NULL, Directed=F)
}
\arguments{
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{weight}{A numberic vector (of the length equal to the number of edges)for weighting all the edges in the edgelist;}
  \item{eadf}{Attribute data frame for all the edges;}
  \item{Directed}{A logical key to specify whether edges in edgelist directed or not.}  
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
#plot.igraph(igr,layout=coord, vertex.shape="circle", vertex.color="green", vertex.size=2,vertex.label=NA)
plot.igraph(igr)
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\seealso{\code{\link{graphNEL}}}
\keyword{graph, igraph}