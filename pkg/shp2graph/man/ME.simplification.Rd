\name{ME.simplification}
\title{Simplify multiple edges in a network}
\description{
This function simplfies multiple-edge into one representative edge.
}
\usage{
ME.simplification(nodelist, edgelist, eadf=NULL, ea.prop=NULL, Directed=F, DegreeL=NULL, InDegreeL=NULL, OutDegreeL=NULL,Nexception=NULL,Eexception=NULL)
}
\arguments{
  \item{nodelist}{An nodelist object, see \link{nodelist};}
  \item{edgelist}{An edgelist object, see \link{edgelist};}
  \item{eadf}{Attribute data frame for all the edges;}
  \item{ea.prop}{a vector (of lenght equal to the number of columns in "eadf") consisted of 1, 2, 3 or 4, and one value cooresponds to a kind of redefination 
  function, see \link{Redef.functions};}
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
\note{
If input edges are directed, the judgement of a set of multiple-edge will be based on its in-degree and out-degree equalling to 1, then InDegreeL and OutDegreeL should be specified;
Otherwise, DegreeL is the significative but optional input. The output changes accordingly with different input.
}
\seealso{\code{\link{SL.extraction}}, \code{\link{PN.amalgamation}}}
\author{Binbin Lu \email{lubinbin220@gmail.com}}

\keyword{spatial, graph}
