\name{nt.connect}
\title{Check if the network is connected}
\description{
This function is to check the connectivity of a given network. Its principle is to traverse all the nodes and do the classifation: for any two different
node they fall into a same category if one can be visited from the other one. Different categories (connected parts) are represented in different colors in the output plot
and the majority connected part is returned as a "SpatialLinesDataFrame" object.
}
\arguments{
  \item{nt}{A "SpatialLines" or "SpatialLinesDataFrame" object.}  
}
\usage{
res<-nt.connect(nt)
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{spatial, graph}
