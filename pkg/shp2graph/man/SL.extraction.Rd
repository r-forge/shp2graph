\name{SL.extraction}
\title{Extract self-loops form a given network}
\description{
This function is to extract self-loops existed in a given network away.
}
\usage{
SL.extraction(nodelist, edgelist, eadf=NULL,Directed=F, DegreeL=NULL, InDegreeL=NULL, OutDegreeL=NULL,Nexception=NULL,Eexception=NULL)
}
\arguments{
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{eadf}{Attribute data frame for all the edges;}
  \item{Directed}{A logical key to specify whether edges are directed or not;}  
  \item{DegreeL}{A vector (of length equal to the number of nodes) of degrees for the nodes, and it should be null if "Directed" is TRUE;}
  \item{InDegreeL}{A vector (of length equal to the number of nodes) of in-degrees for the nodes;}
  \item{OutDegreeL}{A vector of out-degrees for the nodes;}
  \item{Nexception}{A vector of node IDs considered as exceptions, and all the nodes included won't be processed;}
  \item{Eexception}{A vector of edge IDs considered as exceptions, and all the edges included won't be processed;}   
}
\value{
Two types of list for undirected and directed edges respectively:
For undirected type:
  \item{newNodelist}{The new nodelist with pseudo-node withdrawn;}
  \item{newEdgelist}{The new edgelist with edges connected by a pseudo-node amalgamated;}
  \item{newEadf}{The new attribute data frame for the amended edgelist;}
  \item{DegreeL}{The new degree vector cooresponding to the amended nodelist;}
For directed type:
  \item{newNodelist}{The new nodelist with pseudo-node withdrawn;}
  \item{newEdgelist}{The new edgelist with edges connected by a pseudo-node amalgamated;}
  \item{newEadf}{The new attribute data frame for the amended edgelist;}
  \item{InDegreeL}{The new in-degree vector cooresponding to the amended nodelist;}
  \item{OutDegreeL}{The new out-degree vector cooresponding to the amended nodelist;}  
}
\details{
If edges in edgelist are directed, the InDegreeL and OutDegreeL will be calculated (if not given) and updated synchronized with self-loop extration; otherwise only
DegreeL is returned.  
}
\seealso{\code{\link{PN.amalgamation}}, \code{\link{ME.simplification}}}
\author{Binbin Lu \email{lubinbin220@gmail.com}}

\keyword{self-loop}
