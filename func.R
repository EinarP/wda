
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Construct an object composed of metadata, sequence table, and graph list
trsq <- function(sqn, sqd) {
  
  structure(list(name=sqn, seed=1, output='plot', data=sqd,
    seq=trfdesc(match.call()), graph=list(graph.empty())), class='trsq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, tr, chkp=NA, data=NA, cl=NA, ...) {

  # Last graph and community definition
  curang <- tail(sq$graph, 1)[[1]]
  curanc <- tail(sq$seq, 1)$community

  # Call the transformation function
  retval <- do.call(match.fun(tr),
    args=list(dataref=sq$data, chkp=chkp, ang=curang, anc=curanc, ...))
  
  # Assign transformation results accordingly
  if (is_igraph(retval)) {
    curang <- retval
  } else { 
    curanc <- retval
  }

  # Capture the call if function called directly
  if (class(cl) != 'call')  cl <- match.call()

  # Append a row to sequence data frame and expand the graph list
  sq$seq <- rbind(sq$seq, trfdesc(cl, chkp, cmty=curanc))
  sq$graph[[length(sq$graph)+1]] <- curang
  
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
  curlab <- tail(sq$seq, 1)$transformation
#  if grep('=tryang', curlab) curlab <- tail(sq$seq, 1)$community
  if (grepl('=tryanc', curlab)) {
    curlab <- tail(sq$seq, 1)$community
    print(curlab)
  } else {
    cat(sq$name, '\n\n')
    sqtail <- tail(sq$seq, 3)
    print(cbind(id=format(sqtail$id, "%R"), sqtail[ ,2:3]), row.names=FALSE)
    par(mfrow = c(1,1), cex.lab=0.8)
  }
  
  # Plot on requested device
  ang <- tail(sq$graph, 1)[[1]]
  if (vcount(ang) > 0) {

    # Graphics parameters
    plopt <- list(xlab=curlab, edge.arrow.size=0.2,
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
  
  ang <- tail(sq$graph, 1)[[1]]
  anc <- tail(sq$seq, 1)$community
  
  # Plot a graph or a graph with community
  if (is.na(anc)) {
    do.call('plot', c(list(ang), plopt))
  } else {
    if (existsFunction(anc)) {
      anc <- do.call(anc, list(ang))
    } else {
      
      #TODO: Include attibutes. Separate function perhaps
      obs <- get(sq$data)
      comm <- obs[obs$property=='community' & obs$property==anc, ]
      if (nrow(comm)) {
        mship <- as.integer(comm[match(V(ang)$name, comm$object),'value'])
        anc <- make_clusters(ang, mship)
      }
    }
    if (class(anc)=='communities') do.call('plot', c(list(anc, ang), plopt))
  }
}

################################################################################
# CENTER transformations
################################################################################

# List center candidates
tryCenters <- function(sq) {
  obs <- get(sq$data)
  unique(obs[obs$property=='attribute','object'])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
addCenters <- function(sq, centers, depth=1) {
  trf(sq, 'center', centers=centers, depth=depth, cl=match.call())
}

# Add centers
center <- function(ang, dataref, ...) {

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
        tmpang <<- tmpang + edge(newhead, newtail, label=newlink, arrow.mode=2)
      }
    })
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getCenters <- function(sq) {
  ang <- tail(sq$graph, 1)[[1]]
  vertex.attributes(ang)$name  
}

################################################################################
# BOUNDARY transformations
################################################################################

tryBoundaries <- function(sq) {
  
  # Available algorithms
  methods <- c('cluster_edge_betweenness','cluster_infomap','cluster_label_prop',
    'cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Apply the algorithms
  par(mfrow=c(3,3), cex.lab=1)
  lapply(methods, function(tryanc) addBoundary(sq, tryanc))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
addBoundary <- function(sq, community) {
  trf(sq, 'boundary', community=community, cl=match.call())
}

# Add boundary
boundary <- function(community, ...) {

#  if (is.na(method)) {
#    anc <- 'remove'
#  } else {
#    anc <- method
#  }
  
  community
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Describe current boundary
getBoundary <- function(x) {
  
  tail(x$seq, 1)$community
}

################################################################################
# SCALE transformations
################################################################################

# Add attributes and/or values to centers  (TODO: rename to "scale"?)
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
    apply(valuespec, 1, function(x, centers=center) {
      curcenter <- unlist(strsplit(x[1], '>', fixed=TRUE))[1]
      if (is.element(curcenter, centers)) {
        newlabel <- paste0(V(ang)$label[V(ang)$name==x[1]],  '=', x[3])
        ang <<- set_vertex_attr(ang, 'label', index=V(ang)$name==x[1], value=newlabel)
      }
    })
  }
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Level-of-scale implementation

################################################################################
# ALTERNATION transformations
################################################################################

# Implement bipartite graph with alternating centers and attributes. 
# Might be useful when exploring data at the attribute level
# Select links and convert to attributes with two links

alternation <- function(aseq, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

voidAlternation <- function(aseq, chkp=NA) {
  
}

################################################################################
# SYMMETRY transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

symmetry <- function(aseq, chkp=NA) {
 
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang) 
}

################################################################################
# SPACE transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

#TODO: replace add with apply prefix if not adding new elements
space <- function(aseq, chkp=NA) {
  
}

################################################################################
# ROUGHNESS transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

polish <- function(aseq, chkp=NA) {

}

polishSeed <- function(aseq, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

polishLayout <- function(aseq, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# GRADIENT transformations
################################################################################

# Fading or transparency based on some measures
# Remote Centers without attributes as well?

gradient <- function(aseq, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# CONTRAST transformations
################################################################################

# Highlight paths (kateto.net good example?) and perhaps also centers/attributes
contrast <- function(aseq, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

################################################################################
# INTERLOCK transformations
################################################################################

# Group centers and attributes to logical units
group <- function(aseq, member, group, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]

# Select attributes presented by member argument  

# If members found create group vertex (sphere symbol or {name})
  
# Recreate member node edges connected to group vertex
  
# Delete member edges
  
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

################################################################################
# VOID transformations
################################################################################

void <- function(aseq, center, chkp=NA) {
  
}

# Remove centers
voidCenter <- function(aseq, center, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  ang <- delete.vertices(ang, center)
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: fix if (!is.na(anc)) newtr$community <- ifelse(anc=='remove', NA, anc)      


################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# TODO: Author, version, include data

# Conclude the sequnce
signoff <- function(aseq, center, chkp=NA) {
  
}

