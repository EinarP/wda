
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Construction of the sequence object
trsq <- function(sqname, sqdata) {
  
  # Return an object composed of metadata, sequence table, list of structures
  structure(list(name=sqname, seed=1, output='plot', data=sqdata,
    seq=trfdesc(match.call()), struct=list(graph.empty())), class='trsq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, trans, chkp=NA, data=NA, cl=NA, anc=NA, ...) {

  # Last structure and community definitions
  curang <- tail(sq$struct, 1)[[1]]
  curanc <- ifelse(is.na(anc), tail(sq$seq, 1)$community, anc)

  # Call the transformation function
  curang <- do.call(match.fun(trans),
    args=list(dataref=sq$data, chkp=chkp, ang=curang, anc=curanc, ...))
  
  # Update partitioning if present
  if (!is.na(curanc) & trans != 'boundary') {
    curang <- boundary(ang=curang, anc=curanc, dataref=sq$data)
  }
  
  # Append a row to sequence table (capturing the call if called directly)
  if (class(cl) != 'call')  cl <- match.call()
  sq$seq <- rbind(sq$seq, trfdesc(cl, chkp, cmty=curanc))
  
  # Expand the structure list
  sq$struct[[length(sq$struct)+1]] <- curang
  
  # Return the sequence  
  sq
}  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Transformation descriptor line generation
trfdesc <- function(cl, chkp=NA, ann=NA, cmty=NA) {
  
  data.frame(id=Sys.time(), transformation=gsub(' = ', '=', deparse(cl)),
    checkpoint=chkp, annotation=ann, community=cmty, stringsAsFactors=FALSE)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence overview printing
print.trsq <- function(sq, seed=NA) {
  
  # Set random number generator
  set.seed(ifelse(is.na(seed), sq$seed, seed))

  # Textual output
  curxlab <- tail(sq$seq, 1)$transformation
  if (grepl('=tryanc', curxlab)) {
    curxlab <- tail(sq$seq, 1)$community
    print(curxlab)
  } else {
    cat(sq$name, '\n\n')
    sqtail <- tail(sq$seq, 3)
#    print(cbind(id=format(sqtail$id, "%R"), sqtail[ ,2:3]), row.names=FALSE)
    print(tail(sq$seq, 3)[2:3])
    par(mfrow = c(1,1), cex.lab=0.8)
  }
  
  # Plot on requested device
  ang <- tail(sq$struct, 1)[[1]]
  if (vcount(ang) > 0) {

    # Graphics parameters
    plopt <- list(xlab=curxlab, edge.arrow.size=0.2,
      vertex.frame.color=NA, vertex.label.family='sans', edge.label.cex=0.8)

    if (sq$output=='plot') {
      plot.trsq(sq, plopt)      
    } else {
      tkplot(ang, unlist(plopt))
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TODO: Perhaps by using qgraph instead of igraph)

# Sequence last graph plotting
plot.trsq <- function(sq, plopt) {
  
  ang <- tail(sq$struct, 1)[[1]]
  anc <- tail(sq$seq, 1)$community
  
  # Output a graph or a graph with community
  if (is.na(anc)) {
    do.call('plot', c(list(ang), plopt))
  } else {
    mship <- V(ang)$membership
    anc <- make_clusters(ang, as.numeric(factor(mship)))
    do.call('plot', c(list(anc, ang), plopt))
  }
}

################################################################################
# CENTER transformations
################################################################################

# Explore center candidates
tryCenters <- function(sq) {
  
  obs <- get(sq$data)
  
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

# Add centers
center <- function(ang, anc, dataref, ...) {

  # Temporary global graph
  tmpang <<- ang

  # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers
  depth <- aargs$depth

  # Process the vector of center names
  lapply(centers, function(x) addNeighbor(x, dataref, depth))

  # Return the updated graph
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add centers and their neighbors
addNeighbor <- function(center, dataref, depth) {

  # add head center if not present
  if (!is.element(center, vertex.attributes(tmpang)$name)) {
    tmpang <<- tmpang + vertices(center, label=center, shape='square', size=15)
  }
  
  # add neighbors
  if (depth > 0) {  
    
    obs <- get(dataref)
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
      addNeighbor(ifelse(center==newhead, newtail, newhead), dataref, depth-1)
      
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
  ang <- tail(sq$struct, 1)[[1]]
  V(ang)$name[!grepl('>', V(ang)$name)]
}

################################################################################
# BOUNDARY transformations
################################################################################

# Explore clustering algorithms
tryBoundaries <- function(sq) {
  
  # Available algorithms
  methods <- c('cluster_edge_betweenness','cluster_infomap','cluster_label_prop',
    'cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Apply the algorithms
  par(mfrow=c(2,3), cex.lab=1, mar=c(5, 0, 3, 0))
  lapply(methods, function(tryanc) addBoundary(sq, tryanc))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
addBoundary <- function(sq, community) {
  trf(sq, 'boundary', anc=community, cl=match.call())
}

# Add boundary
boundary <- function(ang, anc, dataref, ...) {

  tmpang <<- ang
  
  if (existsFunction(anc)) {
    
    # Apply community detection algorithm
    comm <- do.call(anc, list(tmpang))
    V(tmpang)$membership <- comm$membership
  } else {

    # Use pregiven communities    
    obs <- get(dataref)
    comm <- obs[obs$property==anc, c('object','value')]
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
  tail(x$seq, 1)$community
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
scale <- function(ang, anc, dataref, ...) {

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

alternation <- function(ang, anc, dataref, ...) {
  
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

symmetry <- function(ang, anc, dataref, ...) {
 
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

polish <- function(aseq, chkp=NA) {

}

polishSeed <- function(aseq, chkp=NA) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

polishLayout <- function(aseq, chkp=NA) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# GRADIENT transformations
################################################################################

# Fading or transparency based on some measures
# Remote Centers without attributes as well?

gradient <- function(aseq, chkp=NA) {
  
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

style <- function(aseq, center, chkp=NA) {
  
}

################################################################################
# SHAPE transformations
################################################################################

shape <- function(aseq, center, chkp=NA) {
  
}

################################################################################
# SIMPLICITY transformations
################################################################################

simplicity <- function(aseq, center, chkp=NA) {
  
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

# TODO: fix if (!is.na(anc)) newtr$community <- ifelse(anc=='remove', NA, anc)
voidBoundary <- function(aseq, chkp=NA) {
  
}


################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# TODO: Author, version, include data

# Conclude the sequnce
signoff <- function(aseq, center, chkp=NA) {
  
}
