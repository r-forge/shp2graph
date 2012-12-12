\name{Degree.list}
\title{Degree (In-degree and Out-degree)of nodes}
\description{
The function returns degrees of nodes from provided nodelist and edgelist (in-degrees and out-degrees if edges are directed).
}
\usage{
Degree.list(nodelist, edgelist, Directed=F)
}
\arguments{
 \item{nodelist}{An nodelist object, see \link{nodelist}}
 \item{edgelist}{An edgelist object, see \link{edgelist}}
 \item{Directed}{Specify edges in edgelist are directed or not} 
}
\value{
Two types of lists for undirected and directed edges respectively:
For undirected type:
  \item{DegreeL}{The degree vector with each element orderly cooresponding to  each node in nodelist}
For directed type:
  \item{InDegreeL}{The in-degree vector with each element orderly cooresponding to each node in nodelist}
  \item{OutDegreeL}{The out-degree vector with each element orderly cooresponding to each node in nodelist}  
}
\note{
The output of this function has been divided into two tyes: undirected and directed, actually DegreeL can also be computed by DegreeL=InDegreeL+OutDegreeL.
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{degree, node}
