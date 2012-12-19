\name{g.plot}
\title{Visualize a spatial-enabled \link{graph} object}
\description{
This function is to visualize a spatial-enabled \link{graph} object, of which the spatial coordinate for each vertex is defined clearly.
}
\usage{
g.plot(g, labels=NULL,coord=NULL, label.adj=c(1.5,0.5),label.offsets=c(0,0), arrows=0, circles=0.05, pad=NULL, vertex.pch=20, label.cex=1, 
vertex.cex=1, label.col=1, display.edge.labels=F, edge.label.pos=1/6, edge.label.cex=1, edge.label.offset=0.03,edge.col=1, vertex.col=1, 
arrowhead.angle=10, arrowhead.length=0.2,edge.lty=1, edge.lwd=0, circles.fg=1, circles.bg=NA, circles.lwd=1, axes=FALSE, ylab="", xlab="",main=NULL)
}
\arguments{
  \item{g}{A spatial-enabled "graph-class" object;}
  \item{labels}{A vector containing label text for each vertex;}
  \item{coord}{Optional coordinates for each vertex;}
  \item{label.adj}{A 2-row matrix containing x and y alignment adjustments for each label between [0,1];}
  \item{label.offsets}{	A 2-row matrix containing x and y offsets for each label;}
  \item{arrows}{A vector (of length equal to number of edges in g) with values non-zero designating connections that will be drawn as arrows, while other connections will be draw as non-directional line segments;}  
  \item{circles}{A vector of circle radius for each vertex, and if NULL no circles will be drawn;}
  \item{pad}{The amount to be used in padding the edges of the plot to prevent clipping; if NULL, then equal to the maximum radius of any specified circles.}
  \item{vertex.pch}{A vector of symbols to draw at each vertex, and a value of -1 for any vertex will suppress drawing a symbol;}
  \item{label.cex}{A vector of magnifications for each vertex label;}
  \item{vertex.cex}{A vector of magnifications for each vertex symbol;}
  \item{label.col}{A vector of colors for each label;}
  \item{display.edge.labels}{A key specifying whether to show labels for edges or not;}
  \item{edge.label.pos}{Fraction of way along edge that the labels with be drawn;}
  \item{edge.label.cex}{A vector of magnifications for edge label text}
  \item{edge.label.offset}{An offset matrix for edge labels from the edge itself;}
  \item{edge.col}{A vector (of length equal to number of edges in g) of colors for each edge;}
  \item{vertex.col}{A vector of colors for each vertex symbol;}
  \item{arrowhead.angle}{The angle of the edge arrow heads, defaults to 10;}
  \item{arrowhead.length}{The length of the edge arrow heads, defaults to 0.2;}
  \item{edge.lty}{A vector (of length equal to number of edges in g) of line types for each edge;}
  \item{edge.lwd}{A vector (of length equal to number of edges in g) of line widths for each edge;}
  \item{circles.fg}{A vector of foreground colors for each vertex circle;}
  \item{circles.bg}{A vector of background (fill) colors for each vertex circle;}
  \item{circles.lwd}{A vector of line widths for each vertex circle;}
  \item{axes}{A key to specify whether to draw axes or not;}
  \item{ylab}{The y axis label text;}
  \item{xlab}{The x axis label text;}
  \item{main}{The main title text;}   
}
\note{
This function can only be used for visualizing a "graph-class" object, which can be an immediate result of \link{nel2graphNEL} or \link{nel2graphAM}.
}
\examples{
data(ORN)
rtNEL<-readshpnw(rn, ELComputed=TRUE)
g1<-nel2graphAM(rtNEL[[2]],rtNEL[[3]], weight=rtNEL[[4]])
g.plot(g1,coord=Nodes.coordinates(rtNEL[[2]]), arrows=0, circles=NULL, labels=NULL,display.edge.labels=FALSE, 
vertex.col=rainbow(dim(slot(g1,"adjMat"))[2]),edge.col=rainbow(dim(slot(g1,"adjMat"))[2]))
#Optimization on self-loops, mutiple-edges and pseudo-vertexs
res.pn<-PN.amalgamation(rtNEL[[2]],rtNEL[[3]])
res.me<-ME.simplification(res.pn[[1]],res.pn[[2]])
res.sl<-SL.extraction(res.me[[1]],res.me[[2]])
g2<-nel2graphAM(res.sl[[1]],res.sl[[2]], weight=rtNEL[[4]])
g.plot(g2,arrows=0, circles=NULL, labels=NULL,display.edge.labels=FALSE, vertex.col=rainbow(dim(slot(g2,"adjMat"))[2]),
edge.col=rainbow(dim(slot(g2,"adjMat"))[2]))
}
\references
{
  Neil Klepeis, function "gplot2" in R package "heR.Misc" (version 0.41), \url{http://www.exposurescience.org/heR.doc/library/heR.Misc/html/gplot2.html}
}
\author{Binbin Lu \email{lubinbin220@gmail.com}}
\keyword{plot, graph}
