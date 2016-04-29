
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Contruct the transformations sequence object
trsq <- function(sqname, trfdata, ...) {

  # Empty graph with default settings  
  ang <- trfattr(graph.empty(), match.call(), name=sqname,
    data=trfdata, seed=1, output='plot', partitioning=NA, ...)
  
  # Return a list comprised of an empty graph
  structure(list(ang), class='trsq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, trans, cl, ...) {
  
  # Update the last structure and community definitions
  ang <- trfattr(tail(sq, 1)[[1]], cl, ...)

  # Call the transformation function
  ang <- do.call(match.fun(trans), args=list(ang, ...))
  
  # Update communities if partitioning present
  if (!is.na(ang$partitioning) & trans != 'boundary') {
    curang <- boundary(ang=curang)
  }
  
  # Expand the structure list
  sq[[length(sq)+1]] <- ang
   
  # Return the sequence  
  sq
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Internal function for setting tranformation attributes
trfattr <- function(ang, cl, ...) {
  
  # Mandatory attributes
  ang$dtstamp <- Sys.time()
  if (class(cl) == 'call') ang$transformation <- gsub(' = ', '=', deparse(cl))
  
  # Arbitrary attributes
  aargs <- list(...)
  knownargs <- c('name', 'data', 'seed', 'partitioning', 'output')
  for (idx in seq_along(aargs)) {
    curname <- names(aargs[idx])
    if (is.element(curname, knownargs)) {
      ang <- set_graph_attr(ang, curname, aargs[[idx]])
    }
  }
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Return a summary table
summary.trsq <- function(sq) {

  sumrows <- lapply(sq, function(x) {
    data.frame(dtstamp=x$dtstamp,
        transformation=x$transformation, stringsAsFactors=FALSE)
  })
  Reduce(function(...) merge(..., all=TRUE), sumrows)
}  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence overview printing
print.trsq <- function(sq, seed=NA) {

  ang <- tail(sq, 1)[[1]]
  
  # Set random number generator
  set.seed(ifelse(is.na(seed), ang$seed, seed))
  
  # Textual output
  curxlab <- ang$transformation
  if (grepl('=tryanp', curxlab)) {
    curxlab <- ang$partitioning
    print(curxlab)
  } else {
    cat(ang$name, '\n\n')
    # print(cbind(id=format(sqtail$id, "%R"), sqtail[ ,2:3]), row.names=FALSE)
    print(tail(summary(sq), 3)[1:2])
    par(mfrow = c(1,1), cex.lab=0.8)
  }
  
  # Plot on requested device
  if (vcount(ang) > 0) {
    
    # Graphics parameters
    plopt <- list(xlab=curxlab, edge.arrow.size=0.2,
      vertex.frame.color=NA, vertex.label.family='sans', edge.label.cex=0.8)
    
    if (ang$output=='plot') {
      plot.trsq(sq, plopt)      
    } else {
      tkplot(ang, unlist(plopt))
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence last graph plotting
plot.trsq <- function(sq, plopt) {
  
  # TODO: Perhaps by using qgraph instead of igraph)
  ang <- tail(sq, 1)[[1]]
  anp <- ang$partitioning
  
  # Output a graph or a graph with community
  if (is.na(anp)) {
    do.call('plot', c(list(ang), plopt))
  } else {
    mship <- V(ang)$membership
    anp <- make_clusters(ang, as.numeric(factor(mship)))
    do.call('plot', c(list(anp, ang), plopt))
  }
}

################################################################################
# CENTER transformations
################################################################################

# Explore center candidates
browseCenters <- function(sq) {
  
  ang <- tail(sq, 1)[[1]]
  obs <- get(ang$data)
  
  attrobs <- obs[obs$property=='attribute','object']
  
  linkobs <- strsplit(obs[grepl('link_', obs$property), 'object'], '>')
  linkobs <- sapply(linkobs, function(x) x[1])
  
  sort(unique(c(attrobs, linkobs)))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
addCenters <- function(sq, centers, depth=1) {
  trf(sq, 'center', centers=centers, depth=depth, cl=match.call())
}

# Canonical function call
center <- function(ang, ...) {
  
  # Temporary global graph
  tmpang <<- ang
  
  # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers
  depth <- aargs$depth
  
  # Process the vector of center names
  lapply(centers, function(x) addNeighbor(x, depth))
  
  # Return the updated graph
  tmpang
}

# Add centers and their neighbors
addNeighbor <- function(center, depth) {
  
  # add head center if not present
  if (!is.element(center, vertex.attributes(tmpang)$name)) {
    tmpang <<- tmpang + vertices(center, label=center, shape='square', size=15)
  }
  
  # add neighbors
  if (depth > 0) {  
    
    obs <- get(tmpang$data)
    obs <- obs[obs$property=='link_def', c('object','value')]
    
    # top-down linking of tails
    tailc <- obs[grep(paste0(center, '>'), obs$object), ]
    linkspec <- paste(tailc$object, tailc$value, sep='>')
    
    # bottom-up linking of tails
    tailc <- obs[obs$value==center, ]
    linkspec <- c(linkspec, paste(tailc$object, tailc$value, sep='>'))
    
    # add tail center and link head to tail
    lapply(linkspec, function(curlink) {
      
      newhead <- unlist(strsplit(curlink, '>', fixed=TRUE))[1]
      newtail <- unlist(strsplit(curlink, '>', fixed=TRUE))[3]
      addNeighbor(ifelse(center==newhead, newtail, newhead), depth-1)
      
      ange <- get.edgelist(tmpang, names=T)
      anglink <- paste(ange[,1], E(tmpang)$label, ange[,2], sep='>')
      if (!is.element(curlink, anglink)) {
        newlink <- unlist(strsplit(curlink, '>', fixed=TRUE))[2]
        tmpang <<- tmpang + edge(newhead, newtail,
          label=newlink, edge.arrow.size=0.2, arrow.mode=2)
      }
    })
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getCenters <- function(sq) {
  ang <- tail(sq, 1)[[1]]
  V(ang)$name[!grepl('>', V(ang)$name)]
}

################################################################################
# BOUNDARY transformations
################################################################################

# Explore clustering algorithms
browseBoundaries <- function(sq) {
  
  # Available algorithms
  methods <- c('cluster_edge_betweenness','cluster_infomap','cluster_label_prop',
    'cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Apply the algorithms
  par(mfrow=c(2,3), cex.lab=1, mar=c(5, 0, 3, 0))
  lapply(methods, function(tryanp) applyBoundary(sq, tryanp))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
applyBoundary <- function(sq, partitioning) {
  trf(sq, 'boundary', partitioning=partitioning, cl=match.call())
}

# Add boundary
boundary <- function(ang, ...) {

  tmpang <<- ang
  anp <- ang$partitioning
  
  if (existsFunction(anp)) {
    
    # Apply community detection algorithm
    comm <- do.call(anp, list(tmpang))
    V(tmpang)$membership <- comm$membership
  } else {

    # Use pregiven communities    
    obs <- get(ang$data)
    comm <- obs[obs$property==anp, c('object','value')]
    if (nrow(comm)) {
      apply(comm, 1, function(x) {
        
        # Center
        tmpang <<- set_vertex_attr(tmpang, 'membership',
          index=V(ang)$name==x[1], value=x[2])
        
        # Center attributes
        tmpang <<- set_vertex_attr(tmpang, 'membership',
          index=grepl(paste0(x[1], '>'), V(tmpang)$name), value=x[2])
      })
    } else {
      stop('No memberships pregiven.')
    }
  }
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Describe current boundary
getBoundary <- function(x) {
  tail(x, 1)[[1]]$partitioning
}

################################################################################
# SCALE transformations
################################################################################

# Explore attribute candidates
tryAttributes <- function(sq, centers=NA) {

  obs <- get(sq$data)
  if (!is.na(centers[1])) obs <- obs[obs$object==centers, ]
  
  attrobs <- with(obs[obs$property=='attribute', ], paste(object, value, sep='>'))
  linkobs <- obs[grepl('link_', obs$property), 'object']
  
  sort(unique(c(attrobs, linkobs)))
}

# Explore value candidates
tryValues <- function(sq, centers=NA) {
  
  obs <- get(sq$data)
  if (!is.na(centers[1])) obs <- obs[obs$object==centers, ]
  
  obs[obs$property=='value', c('object','value')]
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
drillDown <- function(sq, centers=NA, values=FALSE) {
    trf(sq, 'scale', centers=centers, values=values, cl=match.call())
}

# Add values to centers and/or attributes 
scale <- function(ang, anp, dataref, ...) {

  # Temporary global graph
  tmpang <<- ang

  # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers
  values <- aargs$values
  
  obs <- get(dataref)
  
  # Add attributes
  attrspec <- obs[is.element(obs$object, centers) & obs$property=='attribute', ]
  apply(attrspec, 1, function(x) {
    aname <- paste(x[1], x[3], sep='>')
    if (!is.element(aname, vertex.attributes(tmpang)$name)) {
      tmpang <<- tmpang + vertices(aname, label=x[3], shape='circle', size=5)
      tmpang <<- tmpang + edge(x[1], aname, label=NA, arrow.mode=0)
    }
  })
  
  # Add values
  if (values) {
    valuespec <- obs[obs$property=='value', ]
    apply(valuespec, 1, function(x) {
      curcenter <- unlist(strsplit(x[1], '>', fixed=TRUE))[1]
      if (is.element(curcenter, centers)) {
        newlabel <- paste0(V(tmpang)$label[V(tmpang)$name==x[1]],  '=', x[3])
        tmpang <<- set_vertex_attr(tmpang, 'label',
          index=V(tmpang)$name==x[1], value=newlabel)
      }
    })
  }
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current attributes
getAttributes <-function(sq) {
  ang <- tail(sq$struct, 1)[[1]]
  V(ang)$name[grepl('>', V(ang)$name)]
}

# Retrieve current values
getValues <-function(sq) {
  
  ang <- tail(sq$struct, 1)[[1]]
  
  values <- grepl('=', V(ang)$label)
  as.data.frame(list(center=V(ang)$name[values], attrvalue=V(ang)$label[values]))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Level-of-scale implementation

################################################################################
# ALTERNATION transformations
################################################################################

# Implement bipartite graph with alternating centers and attributes. 

# Might be useful when exploring data at the attribute level
# Select links and convert to attributes with two links

# User-friendly function call
addAlternation <- function(sq) {
  trf(sq, 'alternation', cl=match.call())
}

alternation <- function(ang, anp, dataref, ...) {
  
  tmpang <- ang
  
  tmpang
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current alternation status
getAlternation <- function(sq) {
  ang <- tail(sq$struct, 1)[[1]]
  is.bipartite(ang)
}

################################################################################
# SYMMETRY transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

addSymmetries <- function(sq) {
  trf(sq, 'symmetry', cl=match.call())
}

symmetry <- function(ang, anp, dataref, ...) {
 
  tmpang <- ang
  
  tmpang
}

################################################################################
# SPACE transformations
################################################################################

addSpace <- function(sq, method=NA) {
  trf(sq, 'space', method=method, cl=match.call())
}

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

#TODO: replace add with apply prefix if not adding new elements
space <- function(ang, dataref, method, ...) {

  if (!is.na(method)) {
  # TODO: If algorithmic convert to center-only graph (function?) 
    tmpang <- alternation(ang, remove=TRUE)
    tmpang <- void(tmpang, getAttributes(ang))
  }
  
#  V(ang)$size <- sample(10:20, length(V(ang)), replace=TRUE)
  btw <- betweenness(ang) + 3
  V(ang)$size <- btw
  
  E(ang)$width <- sample(1:5, length(E(ang)), replace=TRUE)
  E(ang)$arrow.size <- E(ang)$width^2
  
  ang
}

################################################################################
# ROUGHNESS transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

applySeed <- function(sq, ...) {

}

roughness <- function(aseq, chkp=NA) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# GRADIENT transformations
################################################################################

# Fading or transparency based on some measures
# Remote Centers without attributes as well?

applyGradient <- function(sq, ...) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# CONTRAST transformations
################################################################################

# Highlight paths (kateto.net good example?) and perhaps also centers/attributes
contrast <- function(aseq, chkp=NA) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# INTERLOCK transformations
################################################################################

# Group centers and attributes to logical units
group <- function(aseq, member, group, chkp=NA) {
  
  tmpang <- aseq$struct[[length(aseq$struct)]]

# Select attributes presented by member argument  

# If members found create group vertex (sphere symbol or {name})
  
# Recreate member node edges connected to group vertex
  
# Delete member edges

  tmpang
}

# TODO: Possible to convert communities to groups?

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#degroup function (or simplify?)

################################################################################
# ECHO transformations
################################################################################

# TODO: Define layouts (colors, lines, shapes) and apply to sequence

applyDesign <- function(sq, ...) {
  
}


################################################################################
# SHAPE transformations
################################################################################

applyLayout <- function(sq, ...) {
  
}

################################################################################
# SIMPLICITY transformations
################################################################################

applySimplicity <- function(sq, ...) {
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


################################################################################
# VOID transformations
################################################################################

# Remove centers
voidCenter <- function(sq, centers) {
  trf(sq, 'void', centers=centers, cl=match.call())
}

void <- function(ang, ...) {
  
    # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers

  ang <- delete.vertices(tmpang, centers)
  
  #TODO: Void related attributes if any
  
  ang
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Either belongs to alternation transformation or calls alternation from here
voidAlternation <- function(sq) {
  tail(trdemo$struct, 1)[[1]] 
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: fix if (!is.na(anp)) newtr$community <- ifelse(anp=='remove', NA, anp)
voidBoundary <- function(aseq, chkp=NA) {
  
}


################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# TODO: Author, version, include data

# Conclude the sequnce
signoff <- function(sq, ...) {
  
}
