##Update from the code for function "gplot2" in package {heR.Misc}
# g.plot - A function for visualizing directed/undirected graph transformed from a shp file 
# Original `gplot2' by Neil Klepeis, included in the R package {heR.Misc}version 0.41
g.plot<-function(g, labels=NULL,
         coord=NULL, label.adj=c(1.5,0.5),
	 label.offsets=c(0,0), arrows=0, circles=0.05,
	 pad=NULL, vertex.pch=20,
	 label.cex=1, vertex.cex=1, label.col=1,
	 display.edge.labels=F, edge.label.pos=1/6, edge.label.cex=1,
	 edge.label.offset=0.03,
	 edge.col=1, vertex.col=1,
	 arrowhead.angle=10, arrowhead.length=0.2,
	 edge.lty=1, edge.lwd=0, 
	 circles.fg=1, circles.bg=NA, circles.lwd=1, axes=FALSE,
	 ylab="", xlab="",
	 main=NULL)
{
  #Extract the nodes and edges in the graph 
  nodes<- nodes(g)  
  Nn <-length(nodes) 
  EMat<-edgeMatrix(g)
  Ne<-dim(EMat)[2]
  em<-edgemode(g)
  ###Check if the coordinates are provided
  if (is.null(coord)||dim(coord)[[1]]!=Nn)
     coord<-nodecoord(g)
   x <- coord[, 1]
   y <- coord[, 2]
  #Check/set arrows vertex,edge,circles,offsets,pad specifications,
  #       recycling if necessary
  
  # Note: Arrows are drawn by default
  arrows <- rep(arrows, length=Ne)
  arrowhead.length <- rep(arrowhead.length, length=Ne)
  edge.col <- rep(edge.col, length=Ne)
  edge.lwd <- rep(edge.lwd, length=Ne)
  edge.lty <- rep(edge.lty,, length=Ne)
    
  vertex.col <- rep(vertex.col, length=Nn)
  vertex.pch <- rep(vertex.pch, length=Nn)
  vertex.cex <- rep(vertex.cex, length=Nn)##	a vector of magnifications for each vertex symbol
 
  
  if (!is.null(circles)) {
    circles.fg <- rep(circles.fg, length=Nn)
    circles.bg <- rep(circles.bg, length=Nn)
    circles.lwd <- rep(circles.lwd, length=Nn)
    circles <- rep(circles, length=Nn)
    if (!is.null(edge.offsets))
      edge.offsets <- rep(edge.offsets, length=Nn)
    else 
      edge.offsets <- circles 
    if (is.null(pad)) pad <- max(circles)
  }
  
  label.offsets <- matrix(label.offsets, nrow=2, ncol=Nn)
  label.adj <- matrix(label.adj, nrow=2, ncol=Nn)
  label.cex <- rep(label.cex, length=Nn)
  label.col <- rep(label.col, length=Nn)
  
  
  
  #Which nodes should we use?  Forget this and the whole sna package...
  
  #use <- displayisolates | (!is.isolate(d,ego=1:dim(d)[1]))   
  
  
  #Create plot and add the vertices and, optionally, the circles
  
  if (is.null(pad)) pad <- 0
  par(pty="s", xpd=NA) # square plot with device clipping
  plot.new()
  plot.window(xlim=c(min(x)-pad,max(x)+pad),ylim=c(min(y)-pad,max(y)+pad))
  title(xlab=xlab,ylab=ylab,main=main)
  if (axes) {axis(1);axis(2);box()}
  
  for (i in 1:Nn) 
  {
     if (!is.null(circles))
      symbols(x[i], y[i], circles=circles[i], add=TRUE, inches=FALSE, fg=circles.fg[i], bg=circles.bg[i], lwd=circles.lwd[i])
  
     if (vertex.pch[i] < 1)
       type <- "n"
     else
       type <- "p"
     points(x[i],y[i],type=type,pch=vertex.pch[i],col=vertex.col[i],cex=vertex.cex[i])
  }
  
  
  # Draw the edges (and loops - currently on TODO list)
  eprinted<-matrix(c(0,0), nrow=2)
  for (i in 1:Ne)
  {
    etmp=EMat[,i]
    if (!edge.printed(etmp, eprinted))
    {
      eprinted<-cbind(eprinted, etmp)
      xi.tmp <- x[etmp[1]]
  	  xj.tmp <- x[etmp[2]]
  	  yi.tmp <- y[etmp[1]]
  	  yj.tmp <- y[etmp[2]]
      # Draw arrows or segments between vertices
      if (arrows[i] > 0)
              arrows(xi.tmp,yi.tmp,xj.tmp,yj.tmp,length=arrowhead.length[i],angle=arrowhead.angle,col=edge.col[i],lty=edge.lty[i],lwd=edge.lwd[i])
    	else
              segments(xi.tmp,yi.tmp,xj.tmp,yj.tmp,col=edge.col[i],lty=edge.lty[i],lwd=edge.lwd[i])        
    }
    else
    {
      if (arrows[i] > 0)
              arrows(xi.tmp,yi.tmp,xj.tmp,yj.tmp,length=arrowhead.length[i],angle=arrowhead.angle,col=edge.col[i],lty=edge.lty[i],lwd=edge.lwd[i])
    }
    # Draw edge labels
    if (display.edge.labels) 
    {
  	  l <- sqrt((xj.tmp-xi.tmp)^2 + (yj.tmp-yi.tmp)^2)
  	  m <- (yj.tmp-yi.tmp)/(xj.tmp-xi.tmp)
  	  m2 <- sqrt(m^2+1)
            if (d[i,j] > 0) 
            {
              if (xj.tmp >= xi.tmp) 
              {
  	             xl <- xj.tmp - edge.label.pos*l/m2
  	             if (yj.tmp >= yi.tmp) xoff <- -edge.label.offset
  	             else xoff <- edge.label.offset
  	          } 
              else 
              {
  	             xl <- xj.tmp + edge.label.pos*l/m2
                 if (yj.tmp >= yi.tmp) xoff <- edge.label.offset
  	             else xoff <- -edge.label.offset
  	          }
              yl <- m*(xl - xi.tmp) + yi.tmp
        	    yoff <- edge.label.offset
        	    points(xl,yl,pch=16,cex=edge.label.cex)
        	    text(x=xl+xoff, y=yl+yoff, labels=d[i,j], cex=label.cex[i])
  	       }
          if (d[j,i] > 0) 
          {
              if (xj.tmp >= xi.tmp) 
              {
        	      xl <- xi.tmp + edge.label.pos*l/m2
        	      if (yj.tmp >= yi.tmp) xoff <- -edge.label.offset
      	        else xoff <- edge.label.offset
  	          } 
              else 
              {
        	      xl <- xi.tmp - edge.label.pos*l/m2
        	      if (yj.tmp >= yi.tmp) xoff <- edge.label.offset
        	      else xoff <- -edge.label.offset
  	          }
              yl <- m*(xl - xi.tmp) + yi.tmp
        	    yoff <- edge.label.offset
        	    points(xl,yl,pch=16, cex=edge.label.cex)
        	    text(x=xl+xoff, y=yl+yoff, labels=d[j,i], cex=label.cex[j])
  	     }
  	}
  }  
  # Add text labels at each vertex; auto placement of labels is really
  #     only good for circular placements of nodes
  
  for (i in 1:Nn) 
     if (!is.null(labels[i])) 
     {
       if (label.offsets[1,i] == "auto" | label.offsets[2,i] == "auto") 
       {
         label.offsets[,i] <- c(edge.offsets[i]*sign(x[i]),edge.offsets[i]*sign(y[i]))
         if (round(x[i],digits=4) == 0) label.offsets[2,i] <- 1.20*as.numeric(label.offsets[2,i])
         if (round(y[i],digits=4) == 0) label.offsets[1,i] <- 1.20*as.numeric(label.offsets[1,i])
         label.adj[,i] <- c(-sign(x[i])+1, -sign(y[i])+1)/2
       }
       text(x[i]+as.numeric(label.offsets[1,i]),y[i]+as.numeric(label.offsets[2,i]),labels[i],adj=label.adj[,i],cex=label.cex[i],col=label.col[i])
     }    
  par(pty="m", xpd=FALSE)
}
edge.printed<-function(edge, eprinted)
{
  from<-edge[1]
  to<-edge[2]
  n<-dim(eprinted)[2]
  res<-FALSE
  for (i in 1:n)
  {
    if ((from==eprinted[1,i]&&to==eprinted[2,i])||(from==eprinted[2,i]&&to==eprinted[1,i]))
      res<-TRUE
  }
  res
}

###Get coordinates of nodes in a spatial-referenced graph
nodecoord<-function(g)
{
  nd<-nodeData(g)
  n<-length(nd)
  ncoords<-c()
  for(i in 1:n)
  {
    ncoords<-rbind(ncoords, c(nd[[i]][["X"]],nd[[i]][["Y"]]))
  }
  ncoords 
}