\name{readshpnw}
\title{Read a network from a "SpatialLines" or "SpatialLinesDataFrame" object}
\description{
This function is to extract nodes and edges from a "SpatialLines" or "SpatialLinesDataFrame" object, and prepare them in a \link{nodelist} and \link{edgelist}.
}
\usage{
readshpnw(data=list(), ELComputed=FALSE, longlat=FALSE, Detailed=FALSE, ea.prop=NULL)
}
\arguments{
  \item{data}{A "SpatialLines" or "SpatialLinesDataFrame" object;}
  \item{ELComputed}{A key to specify whether the edge length is computed or not;}
  \item{longlat}{If TRUE, use distances on an ellipse with WGS84 parameters;}
  \item{Detailed}{Default is FALSE, only two endpoints of each polyline are extracted as nodes and collected in the nodelist; If TRUE, All the detailed points will be extracted as nodes.}
  \item{ea.prop}{If data is a "SpatialLinesDataFrame" object and Detailed is TRUE, the ea.prop will be a vector (of lenght equal to the number of columns in data.frame(data)) for generating a new data frame for each edge,
  of which each element to be 0 or 1: 0 means for the cooresponding column equalization is used to evalute the item for each new edge from the original edges, while 1 presumes the cooresponding column lies on the edge length
  and a weighted-mean based on the edge length is adopted.}  
}
\value{
A list consisted of:
  \item{Detailed}{TRUE if the output is under a "Detailed" mode, and "edgelist" will have a different structure;}
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{Edgelength}{A vector (of length equal to the number of edges) of edge lengths if ELComputed is TRUE;}
  \item{Eadf}{A data frame of edge attributes, [EdgeID,...(items extracted from the "SpatialLinesDataFrame" object )...]}
  \item{nodexlist}{A vector contains X-coordinates of all the nodes}
  \item{nodeylist}{A vector contains Y-coordinates of all the nodes}  
}
\details{
Make preparation for generating a kind of graph of "graph-class" by reading network data from a "SpatialLinesDataFrame", and producing a data list including a nodelist, an edgelist and a data frame for edge attributes from the origianl data file. 
}
\examples{
data(ORN)
rtNEL<-readshpnw(rn)
nl<-rtNEL[[2]]
el<-rtNEL[[3]]
#Compute edge length
rtNEL<-readshpnw(rn, ELComputed=TRUE)
edgelength<-rtNEL[[4]]
eadf<-rtNEL[[5]]
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{spatial, graph}
