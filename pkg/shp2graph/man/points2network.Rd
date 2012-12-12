\name{points2network}
\title{Integrate a point set with a network}
\description{
The function establish a mapping relationship between points and nodes in the network, each point will correspond to a specific node.
}
\usage{
points2network(ntdata,pointsxy,mapping.method=1,output="",longlat=F,Detailed=F, ea.prop=NULL)
}
\arguments{
  \item{ntdata}{a spatial data frame, here only "SpatialLinesDataFrame" is accepted.}
  \item{pointsxy}{X-Y coordinates of the points.}
  \item{mapping.method}{specify which kind of mapping method will be used, see \link{details}}
  \item{ELComputed}{specify if the edge lengths is needed to be computed according to spatial coordinates in the data file}
  \item{longlat}{if TRUE, use distances on an ellipse with WGS84 parameters}
  \item{Detailed}{if TRUE, All the detailed points will be regarded as nodes; if FALSE, only endpoints of polylines are included in the nodelist}
  \item{ea.prop}{A vector consist of 1 or 0, of which each element cooresponds to each piece of attributes, see in details.}  
}
\value{
A list consisted of:
  \item{nodelist}{a list for extracted nodes, of which the element has a structure: [NodeID,coordinate(X,Y)]}
  \item{edgelist}{a list for edges, of which the element has a structure: [EdgeID,NodeID(from),NodeID(to)], if under a "Detailed" mode, will be a different structure [eid,EdgeID,NodeID(from),NodeID(to)], where EdgeID refers to the original attribute data frame}
  \item{CoorespondIDs}{A vector contains cooresponding node IDs of each point}
  \item{nodexlist}{A vector contains X-coordinates of all the nodes}
  \item{nodeylist}{A vector contains Y-coordinates of all the nodes} 
  \item{Eadf}{data frame of edge attributes, dataframe(EdgeID,...(items extracted from original file)...)}
  \item{VElist}{A list of virtual edges if existed}
  \item{Edgelength}{If ELComputed is TRUE, Edgelength will be a vector consisted of edge lengths cooresponding to each edge, else it will be NULL}
   
}
\details{
The value of mapping.method can be 1,2,3,4 and the meanings are as follows:
1: Mapping each point to the nearest node in the network/graph
2: Mapping each point to the nearest point (add them as nodes if they are not) on the network
3: Add a new edge(Virtual edge) between each point and the nearest node
4: Add a new edge(Virtual edge) between each point and the nearest point
}
\seealso{\code{\link{ptsinnt.view}}}
\examples{
data(ORN)
pts<-spsample(rn, 100, type="random")
ptsxy<-coordinates(pts)[,1:2]
ptsxy<-cbind(ptsxy[,1]+0.008,ptsxy[,2]+0.008)
#Mapping each point to the nearest node in the network/graph
res<-points2network(ntdata=rn,pointsxy=ptsxy, mapping.method=1)
ptsinnt.view(ntdata=rn, nodelist=res[[1]], pointsxy=ptsxy, CoorespondIDs=res[[3]])
#Mapping each point to the nearest point (add them as nodes if they are not) on the network
res<-points2network(ntdata=rn,pointsxy=ptsxy, mapping.method=2,ea.prop=rep(0,37))
ptsinnt.view(ntdata=rn, nodelist=res[[1]], pointsxy=ptsxy, CoorespondIDs=res[[3]]) 
#Add a new edge(Virtual edge) between each point and the nearest node
res<-points2network(ntdata=rn,pointsxy=ptsxy, mapping.method=3,ea.prop=rep(0,37))
VElist<-res[[7]]
ptsinnt.view(ntdata=rn, nodelist=res[[1]], pointsxy=ptsxy, CoorespondIDs=res[[3]], VElist=VElist)
#Add a new edge(Virtual edge) between each point and the nearest point
res<-points2network(ntdata=rn,pointsxy=ptsxy, mapping.method=4,ea.prop=rep(0,37))
VElist<-res[[7]]
ptsinnt.view(ntdata=rn, nodelist=res[[1]], pointsxy=ptsxy, CoorespondIDs=res[[3]], VElist=VElist) 
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{spatial, graph}
