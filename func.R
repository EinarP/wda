
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# TODO: Perhaps call all functions through wrapper

# Construction of analysis sequence object
asq <- function(aname, adata) {
  
  # data frame for sequence
  s <- data.frame(id=1, parent=NA, transformation=paste0("asq('", aname, "')"),
    checkpoint=NA, community=NA, stringsAsFactors=FALSE)
  
  # list of graphs
  g <- list(graph.empty())

  # return an object composed of meta data, sequence data frame, and graph list
  structure(list(name=aname, seed=1, output='plot', data=adata,
    seq=s, graph=g), class='asq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Print method
print.asq <- function(x, seed=NA) {

  # Set random number generator
  set.seed(ifelse(is.na(seed), x$seed, seed))

  # Excerpt of trace table
  cat(x$name, '\n\n')
  print(tail(x$seq[ ,c('transformation','checkpoint')], 3))

  # Plot on requested device (perhaps qgraph instead of igraph)
  ang <- tail(x$graph, 1)[[1]]
  if (vcount(ang) > 0) {
    if (x$output=='plot') {
      
      plopt <- list(xlab=tail(x$seq, 1)$transformation, edge.arrow.size=0.2,
        vertex.frame.color=NA, vertex.label.family='sans', edge.label.cex=0.8)
      
      cmethod <- tail(x$seq, 1)$community
      if (is.na(cmethod)) {
        
        do.call('plot', c(list(ang), plopt))
      } else {
        
        if (cmethod=='obs_community') {
          obs <- get(x$data)
          comm <- obs[obs$property=='community', ]
          mship <- as.integer(comm[match(V(ang)$name, comm$object),'value'])
          anc <- make_clusters(ang, mship)
        } else {
          anc <- do.call(cmethod, list(ang))
        }
        
        do.call('plot', c(list(anc, ang), plopt))
      }
    } else {
      tkplot(ang)
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence tracing function
addAnalysisStep <- function(aseq, cmd, chkp=NA, ang=NA, anc=NA) {

  # Append row to sequence data frame
  newtr <- tail(aseq$seq, 1)
  
  newtr$parent <- newtr$id
  newtr$id <- newtr$id + 1
  
  newtr$transformation <- gsub(' = ', '=', deparse(cmd))
  newtr$checkpoint <- chkp
  newtr$community <- ifelse(is.na(anc), newtr$community, anc)
  
  aseq$seq <- rbind(aseq$seq, newtr)

  # Append new graph or copy last if not present
  if (is_igraph(ang)) {
    aseq$graph[[length(aseq$graph)+1]] <- ang
  } else {
    aseq$graph[[length(aseq$graph)+1]] <- aseq$graph[[length(aseq$graph)]]
  }

  aseq
}

################################################################################
# CENTER transformations
################################################################################

# Add centers
addCenter <- function(aseq, center, depth=1, chkp=NA) {
  
  ang <<- aseq$graph[[length(aseq$graph)]]

  sapply(center, function(x) addNeighbor(aseq, x, depth))

  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

# Add neighbors when adding centers
addNeighbor <- function(aseq, center, depth) {

  # add head center if not present
  if (!is.element(center, vertex.attributes(ang)$name)) {
    ang <<- ang + vertices(center, label=center, shape='square', size=15)
  }

  if (depth > 0) {  
  
    obs <- get(aseq$data)
    obs <- obs[obs$property=='linkdef', c('object','value')]
    
    # top-down linking of tails
    tailc <- obs[grep(paste0(center, '>'), obs$object), ]
    linkspec <- paste(tailc$object, tailc$value, sep='>')
    
    # bottom-up linking of tails
    tailc <- obs[obs$value==center, ]
    linkspec <- c(linkspec, paste(tailc$object, tailc$value, sep='>'))

    # add tail center and link head to tail
    sapply(linkspec, function(curlink) {
      
      newhead <- unlist(strsplit(curlink, '>', fixed=TRUE))[1]
      newtail <- unlist(strsplit(curlink, '>', fixed=TRUE))[3]
      addNeighbor(aseq, ifelse(center==newhead, newtail, newhead), depth-1)
      
      ange <- get.edgelist(ang, names=T)
      anglink <- paste(ange[,1], E(ang)$label, ange[,2], sep='>')
      if (!is.element(curlink, anglink)) {
        newlink <- unlist(strsplit(curlink, '>', fixed=TRUE))[2]
        ang <<- ang + edge(newhead, newtail, label=newlink, arrow.mode=2)
      }
    })
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve all center references
getCenter <- function(aseq) {

  ang <- aseq$graph[[length(aseq$graph)]]
  
  vertex.attributes(ang)$name  
}

################################################################################
# BOUNDARY transformations
################################################################################

# Add boundary
addBoundary <- function(aseq, method=NA, chkp=NA) {
  
  if (is.na(method)) {
    anc <- 'obs_community'
  } else {
    anc <- method
  }
  
  addAnalysisStep(aseq, match.call(), chkp, anc=anc)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Describe current boundary
getBoundary <- function(x) {
  
  tail(x$seq, 1)$community
}

################################################################################
# SCALE transformations
################################################################################

# Add attributes and/or values to centers
drillDown <- function(aseq, center, values=FALSE, chkp=NA) {
  
  ang <<- aseq$graph[[length(aseq$graph)]]
  
  obs <- get(aseq$data)

  # Add attributes
  attrspec <- obs[is.element(obs$object, center) & obs$property=='attribute', ]
  apply(attrspec, 1, function(x) {
    aname <- paste(x[1], x[3], sep='>')
    if (!is.element(aname, vertex.attributes(ang)$name)) {
      ang <<- ang + vertices(aname, label=x[3], shape='circle', size=5)
      ang <<- ang + edge(x[1], aname, label=NA, arrow.mode=0)
    }
  })
  
  # Add values
  if (values) {
    valuespec <- obs[obs$property=='value', ]
    apply(valuespec, 1, function(x) {
      valuespec <- valuespec[grep(paste0(center, '>'), valuespec$object), ]
      newlabel <- paste0(unlist(strsplit(x[1], '>', fixed=TRUE))[2], '=', x[3])
      ang <<- set_vertex_attr(ang, 'label', index=V(ang)$name==x[1], value=newlabel)
    })
  }
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Level-of-scale implementation


################################################################################
# VOID transformations
################################################################################

# Remove centers
voidCenter <- function(aseq, center, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  ang <- delete.vertices(ang, center)
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

