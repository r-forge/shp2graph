\name{nel2graphAM}
\title{Produce a "graphAM" object}
\description{
This function is to produce a "graphAM" object using generated nodelist and edgelist from a given network.
}
\usage{
nel2graphAM(nodelist, edgelist, weight=NULL, eadf=NULL, Directed=F)
}
\arguments{
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{weight}{A numberic vector (of the length equal to the number of edges)for weighting all the edges in the edgelist;}
  \item{eadf}{Attribute data frame for all the edges;}
  \item{Directed}{A logical key to specify whether edges in edgelist directed or not.}   
}
\value{
  \item{gr}{An object of "graphAM"} 
}
\details{
1. The vector "weight" will be the default weight for all the computations carried out on the produced "graphAM" object.
2. This kind of graph-representation does not allow multiple edges, which will be overlayed one after another; while a "graphNEL" avoids this suitation.
3. When the number of edges is huge, this function might fail due to the limitation of memory.
}
\examples{
data(ORN)
rtNEL<-readshpnw(rn, ELComputed=TRUE)
#Add the edge length as the weight for graph
gr1<-nel2graphNEL(rtNEL[[2]],rtNEL[[3]], weight=rtNEL[[4]])
gr2<-nel2graphAM(rtNEL[[2]],rtNEL[[3]], weight=rtNEL[[4]])
gr1
gr2
#Optimization on self-loops, mutiple-edges and pseudo-nodes
res.pn<-PN.amalgamation(rtNEL[[2]],rtNEL[[3]])
res.me<-ME.simplification(res.pn[[1]],res.pn[[2]])
res.sl<-SL.extraction(res.me[[1]],res.me[[2]])
gr1<-nel2graphNEL(res.sl[[1]],res.sl[[2]], weight=rtNEL[[4]])
gr2<-nel2graphAM(res.sl[[1]],res.sl[[2]], weight=rtNEL[[4]])
#Difference between gr1 and gr2
gr1
gr2
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\seealso{\code{\link{graphAM}}}
\keyword{graphAM, graph}
