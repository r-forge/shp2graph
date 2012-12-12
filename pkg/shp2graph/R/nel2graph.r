nel2graphNEL<-function (nodelist, edgelist, weight = NULL, eadf = NULL, Directed = F)
{
    nodes <- as.character(nodelist[, 1])
    edgelist[, 2] <- as.character(edgelist[, 2])
    edgelist[, 3] <- as.character(edgelist[, 3])
    Ne <- length(edgelist[, 1])
    Nn <- length(nodes)
    edL = vector("list", length = Nn)
    names(edL) <- nodes
    if (is.null(weight)) {
        if (length(weight) != Ne && is.numeric(weight))
            stop("Please give right edge weight, which must be numeric and the same length as edges elment")
    }
    if (!is.null(eadf)) {
        if (length(eadf[, 1]) != Ne)
            stop("The eadf must be numeric and the same length as edges elment")
    }
    for (i in 1:Nn) {
        edL[[i]] <- list(edges = c(), weights = c())
    }
   total=Ne
   pb <- txtProgressBar(min = 0, max = total, style = 3)
    if (Directed) {
        for (i in 1:Ne) {
            oidx <- which(nodes == edgelist[i, 2])
            eidx <- which(nodes == edgelist[i, 3])
            if (is.null(weight))
                edL[[oidx]] <- list(edges = c(edL[[oidx]][["edges"]],
                  eidx), weights = c(edL[[oidx]][["weights"]]))
            else edL[[oidx]] <- list(edges = c(edL[[oidx]][["edges"]],
                eidx), weights = c(edL[[oidx]][["weights"]],
                weight[i]))
        setTxtProgressBar(pb, i)
        }
        gr = new("graphNEL", nodes = nodes, edgeL = edL, edgemode = "directed")
    }
    else {
        for (i in 1:Ne) {
            oidx <- which(nodes == edgelist[i, 2])
            eidx <- which(nodes == edgelist[i, 3])
            if (is.null(weight)) {
                edL[[oidx]] <- list(edges = c(edL[[oidx]][["edges"]],
                  eidx), weights = c(edL[[oidx]][["weights"]]))
                edL[[eidx]] <- list(edges = c(edL[[eidx]][["edges"]],
                  oidx), weights = c(edL[[eidx]][["weights"]]))
            }
            else {
                edL[[oidx]] <- list(edges = c(edL[[oidx]][["edges"]],
                  eidx), weights = c(edL[[oidx]][["weights"]],
                  weight[i]))
                edL[[eidx]] <- list(edges = c(edL[[eidx]][["edges"]],
                  oidx), weights = c(edL[[eidx]][["weights"]],
                  weight[i]))
            }
            setTxtProgressBar(pb, i)
        }
        gr = new("graphNEL", nodes = nodes, edgeL = edL, edgemode = "undirected")
    }
    nodeDataDefaults(gr, attr = "X") <- 0
    nodeDataDefaults(gr, attr = "Y") <- 0
    total=Nn
    pb <- txtProgressBar(min = 0, max = total, style = 3)
    print("Add coordinates of nodes of the graph")
    for (i in 1:Nn) {
        nodeData(gr, n = nodes[i], attr = "X") <- nodelist[i,
            2][[1]][1]
        nodeData(gr, n = nodes[i], attr = "Y") <- nodelist[i,
            2][[1]][2]
        setTxtProgressBar(pb, i)
    }
    if (!is.null(eadf)) {
        attrnames <- names(eadf)
        na <- length(attrnames)
        for (i in 1:na) {
            edgeDataDefaults(gr, attr = attrnames[i]) <- NA
            if (Directed) {
                for (j in 1:Ne) {
                  edgeData(gr, from = edgelist[j, 2], to = edgelist[j,
                    3], attr = attrnames[i]) <- eadf[j, i]
                }
            }
            else {
                for (j in 1:Ne) {
                  edgeData(gr, from = edgelist[j, 2], to = edgelist[j,
                    3], attr = attrnames[i]) <- eadf[j, i]
                  edgeData(gr, from = edgelist[j, 3], to = edgelist[j,
                    2], attr = attrnames[i]) <- eadf[j, i]
                }
            }
        }
    }
    gr
}
nel2graphAM<-function(nodelist, edgelist, weight=NULL, eadf=NULL, Directed=F)
{
   nodes<-as.character(nodelist[,1])
   edgelist[,2]<-as.character(edgelist[,2])
   edgelist[,3]<-as.character(edgelist[,3])
   Ne<-length(edgelist[,1])
   Nn<-length(nodes)
   amat<-matrix(rep(0, Nn*Nn), nrow=Nn, ncol=Nn)
   rownames(amat)<-colnames(amat)<-nodes
   if (is.null(weight))
   {
     if (length(weight)!=Ne&&is.numeric(weight))
     stop("Please give right edge weight, which must be numeric and the same length as edges elment")
   }
   if (!is.null(eadf))
   {
     if (length(eadf[,1])!=Ne)
     stop("The eadf must be numeric and the same length as edges elment")
   }
   if (Directed)
   {
      for (i in 1:Ne)
      {
        oidx<-which(nodes==edgelist[i,2])
        eidx<-which(nodes==edgelist[i,3])
        if(is.null(weight))
        amat[oidx, eidx]<-1
        else
        amat[oidx, eidx]<-weight[i]
      }
      gr=new("graphAM", adjMat=amat,edgemode="directed")
   }
   else
   {
     for (i in 1:Ne)
      {
        oidx<-which(nodes==edgelist[i,2])
        eidx<-which(nodes==edgelist[i,3])
        if(is.null(weight))
        {
          amat[oidx, eidx]<-1
          amat[eidx, oidx]<-1
        }
        else
        {
          amat[oidx, eidx]<-weight[i]
          amat[eidx, oidx]<-weight[i]
        }
      }
      gr=new("graphAM", adjMat=amat,edgemode="undirected")
   }
   #########################Add coordinate 
   nodeDataDefaults(gr, attr="X")<-0
   nodeDataDefaults(gr, attr="Y")<-0
   for (i in 1:Nn)
   {
     nodeData(gr, n=nodes[i],attr="X")<-nodelist[i,2][[1]][1]
     nodeData(gr, n=nodes[i],attr="Y")<-nodelist[i,2][[1]][2]
   }
   ########################Add edge attributs
   if (!is.null(eadf))
   {
     attrnames<-names(eadf)
     na<-length(attrnames)
     for (i in 1:na)
     {
       edgeDataDefaults(gr, attr=attrnames[i])<-NA
       if (Directed)
       {
         for (j in 1:Ne)
         {
           edgeData(gr, from=edgelist[j,2],to=edgelist[j,3],attr=attrnames[i])<-eadf[j,i]
         }
       }
       else
       {
         for (j in 1:Ne)
         {
           edgeData(gr, from=edgelist[j,2],to=edgelist[j,3],attr=attrnames[i])<-eadf[j,i]
           edgeData(gr, from=edgelist[j,3],to=edgelist[j,2],attr=attrnames[i])<-eadf[j,i]
         }
       }
       
     }
   }
   gr
}


