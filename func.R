
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Contruct the transformations sequence object
trsq <- function(sqname, trfdata, ...) {

  # Empty graph with default settings  
  ang <- set_trsq_attr(graph.empty(), match.call(), name=sqname, seed=1, 
    data=trfdata, output='plot', partitioning=NA, alternation=FALSE, ...)
  
  # Return a list comprised of an empty graph
  structure(list(ang), class='trsq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, trans, cl, ...) {
  
  # Update the last structure and community definitions
  curang <- set_trsq_attr(tail(sq, 1)[[1]], cl, ...)

  # Call the transformation function
  trfang <- do.call(match.fun(trans), args=list(curang, ...))
  
  # Update communities if partitioning present
  if (!is.na(trfang$partitioning) & trans != 'boundary') {
    trfang <- boundary(ang=trfang)
  }
  
  # Expand the structure list
  sq[[length(sq)+1]] <- trfang
   
  # Return the sequence  
  sq
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Internal function for setting tranformation attributes
set_trsq_attr <- function(ang, cl, ...) {
  
  # Mandatory attributes
  ang$dtstamp <- Sys.time()
  if (class(cl) == 'call') ang$transformation <- gsub(' = ', '=', deparse(cl))
  
  # Arbitrary attributes
  aargs <- list(...)
  knownargs <- c('name','data',
    'checkpoint','seed','partitioning','alternation','output')
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
summary.trsq <- function(sq, full=FALSE) {

  # Convert sequence list to a data frame
  sumrows <- lapply(sq, function(ang) data.frame(get.graph.attribute(ang)))
  sumall <- Reduce(function(...) merge(..., all=TRUE), sumrows)
  
  # Return all or selection of columns
  if (full) {
    sumall
  } else {
    if (is.element('checkpoint', names(sumall))) {
      sumall[ ,c('transformation','checkpoint')]
    } else {
      sumall$time <- format(sumall$dtstamp, '%R')
      sumall[ ,c('time','transformation')]
    } 
  }
}  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence overview printing
print.trsq <- function(sq, seed=NA) {

  ang <- tail(sq, 1)[[1]]
  
  # Set random number generator
  set.seed(ifelse(is.na(seed), ang$seed, seed))
  
  # Textual output
  curxlab <- ang$transformation
  if (!grepl('=try', curxlab)) {
    cat(ang$name, '\n\n')
    # print(cbind(id=format(sqtail$id, "%R"), sqtail[ ,2:3]), row.names=FALSE)
    print(tail(summary(sq), 3))
    par(mfrow = c(1,1), cex.lab=0.8)
  } else {
    if (grepl('=tryanp', curxlab)) curxlab <- paste('method:', ang$partitioning)
    if (grepl('=tryseed', curxlab)) curxlab <- paste('seed:', ang$seed)
    print(curxlab)
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
browseSource <- function(type) {

  function(dsrc) {
      
  # Connect the data source
  obs <- get(ifelse(class(dsrc)=='trsq', tail(dsrc, 1)[[1]]$data, dsrc$data))

  # Subset based on object type
  obsl <- switch(type, 
    entity=obs[!grepl('>', obs$object), ],
    attribute=obs[grepl('>', obs$object) & !grepl('\\|', obs$object), ],
    relationship=obs[grepl('\\|', obs$object), ],
    all=obs)
  
  # Tidy up checkpoints for reshaping
  if (!is.element('checkpoint', names(obsl))) {
    obsl$checkpoint <- 'all'
  } else {
    obsl[is.na(obsl$checkpoint), 'checkpoint'] <- 'all' 
  }
    
  # Convert from long to wide format
  obsw <- reshape(obsl, direction='wide',
    idvar=c('object','checkpoint'), timevar='property')
    
  # Append helper columns
  objsplit <- strsplit(sub('\\|', '>', obsw$object), '>', fixed=TRUE)
  obsw$objsrc <- sapply(objsplit, function(x) x[1])
  obsw$objattr <- sapply(objsplit, function(x) x[2])
  obsw$objdest <- sapply(objsplit, function(x) x[3])

  # Return observations in wide format
  names(obsw) <- gsub('value.', '', names(obsw))
  obsw[with(obsw, order(object, checkpoint)), ]
  }
}  

browseData <- browseSource('all')
browseEntities <- browseSource('entity')
browseAttributes <- browseSource('attribute')
browseRelations <- browseSource('relationship')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Introduce 'type' attr for differentiating entities and attributes

# User-friendly function call
addCenters <- function(sq, centers, depth=1, addelem=FALSE, addval=FALSE) {
  trf(sq, 'center', cl=match.call(),
    centers=centers, depth=depth, addelem=addelem, addval=addval)
}

# Transformation function
center <- function(ang, ...) {
  
  # Temporary global graph
  tmpang <<- ang

  # Arbitrary attributes
  aargs <- list(...)
  
  # Process the vector of center names
  lapply(aargs$centers, function(x)
    add_entities(x, aargs$depth, addelem=aargs$addelem, addval=aargs$addval))

  # Return the updated graph
  tmpang
}

# Add centers and their neighbors
add_entities <- function(center, depth, addelem, addval) {
  
  # add head center if not present
  if (!is.element(center, V(tmpang)$name)) {
    
    # TODO: size related to space, if not present use center type defults
    tmpang <<- tmpang + vertices(center, label=center, shape='square', size=15)
  }
  
  # Add attributes and values
  if (addelem) add_attributes(tmpang, center, addval)
  if (addval) add_values(tmpang, center)
  
  # add neighboring entities
  if (depth > 0) {  

    obs <- browseRelations(tmpang)
    linkspec <- obs[obs$objsrc==center | obs$objdest==center, ]
    apply(linkspec, 1, function(clnk) {
      
      nextcenter <- ifelse(clnk['objsrc']==center, clnk['objdest'], clnk['objsrc'])
      add_entities(nextcenter, depth-1, addelem, addval)
      tmpang <<- add_link(tmpang, clnk['objsrc'], clnk['objdest'], clnk['objattr'])
#      if (!is.element(clnk['object'], attr(E(tmpang), 'vnames'))) {
#        tmpang <<- tmpang + edge(clnk['objsrc'], clnk['objdest'], 
#          label=clnk['objattr'], edge.arrow.size=0.2, arrow.mode=2, width=1)
#      }
    })
  }
}

# Add elements
add_attributes <- function(ang, centers, addval=FALSE) {
  
  # Temporary global graph
  tmpang <<- ang
  
  # Either attribute or related entity can be passed
  if (grepl('>', centers)) {
    objsrc <- sub('>.*', '', centers)
    objattr <- sub('.*>', '', centers)
    attrspec <- data.frame(object=centers, objsrc=objsrc, objattr=objattr)
  } else {
    obs <- browseAttributes(tmpang)
    attrspec <- obs[is.element(obs$objsrc, centers), ]
  }
  
  if (nrow(attrspec)) {
    apply(attrspec, 1, function(cattr) {
      if (!is.element(cattr['object'], V(tmpang)$name)) {
        tmpang <<- tmpang + vertices(cattr['object'], label=cattr['objattr'], 
          shape='circle', size=5)
        
#        tmpang <<- add_link(tmpang, cattr['objsrc'], cattr['object'])
        tmpang <<- tmpang + edge(cattr['objsrc'], cattr['object'], 
          id=cattr['object'], label=NA, arrow.mode=0, width=1)
      }
      
      if (addval) add_values(tmpang, cattr['object'])
    })
  }
  
  tmpang
}

# Add values
add_values <- function(ang, centers) {

  obs <- browseData(ang)
  valuespec <- obs[!is.na(obs$value), ]
  if (nrow(valuespec)) {
    apply(valuespec, 1, function(cval) {
      if (is.element(cval['object'], centers)) {
        
        curlabel <- V(tmpang)$label[V(tmpang)$name==cval['object']]
        curlabel <- unlist(strsplit(curlabel, '=', fixed=TRUE))[1]
        newlabel <- paste0(curlabel, '=', cval['value'])
        
        tmpang <<- set_vertex_attr(tmpang, 'label',
          index=V(tmpang)$name==cval['object'], value=newlabel)
      }
    })
  }

  #TODO: Edge values
  
  tmpang
}

# Connect vertices
add_link <- function(ang, vsrc, vdest, lnklabel) {

  if (ang$alternation) {
    linkname <- paste(paste(vsrc, lnklabel, sep='>'), vdest, sep='|')  
    if (!is.element(linkname, attr(E(tmpang), 'vnames'))) {
      newattr <- paste(vsrc, lnklabel, sep='>')
      ang <- add_attributes(ang, newattr)
      ang <- ang + edge(newattr, vdest, edge.arrow.size=0.2, arrow.mode=2, width=1)
    }
  } else {
    linkname <- paste(vsrc, vdest, sep='|')  
    if (!is.element(linkname, attr(E(ang), 'vnames'))) {
      ang <- ang + edge(vsrc, vdest,
        label=lnklabel, edge.arrow.size=0.2, arrow.mode=2, width=1)
    }
  }
  
  ang
}
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getCenters <- function(sq) {
  ang <- tail(sq, 1)[[1]]
  data.frame(get.vertex.attribute(tmpang))
}

# Retrieve current relations
getRelations <-function(sq) {
  ang <- tail(sq, 1)[[1]]
  data.frame(name=attr(E(ang), 'vnames'), get.edge.attribute(ang))
}

################################################################################
# BOUNDARY transformations
################################################################################

# Explore clustering algorithms
browseBoundaries <- function(sq) {
  
  # Available algorithms
  methods <- c('cluster_edge_betweenness','cluster_label_prop',
    'cluster_infomap','cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Apply the algorithms
  par(mfrow=c(2,3), cex.lab=1, mar=c(5, 0, 3, 0))
  lapply(methods, function(tryanp) applyBoundary(sq, tryanp))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call for partitioning
applyBoundary <- function(sq, partitioning) {
  trf(sq, 'boundary', partitioning=partitioning, cl=match.call())
}

# User-friendly function call for removing partitioning
removeBoundary <- function(sq) {
  trf(sq, 'boundary', partitioning=NA, cl=match.call())
}

# Add boundary
boundary <- function(ang, ...) {

  anp <- ang$partitioning
  
  if (is.na(anp)) {
    
    # Remove partitioning
    V(ang)$membership <- NA
  } else {
  
    if (existsFunction(anp)) {
    
      # Apply community detection algorithm
      comm <- do.call(anp, list(ang))
      V(ang)$membership <- comm$membership
    } else {

      # Use pregiven communities    
      obs <- browseData(ang)
      comm <- obs[!is.na(obs[ ,anp]), c('object',anp)]
      if (nrow(comm)) {
        ents <- sub('>.*', '', V(ang)$name)
        V(ang)$membership <- comm[match(ents, comm$object), anp]
      } else {
        stop('No memberships pregiven.')
      }
    }
  }
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Describe current boundary
getBoundary <- function(sq) {
  tail(sq, 1)[[1]]$partitioning
}

################################################################################
# SCALE transformations
################################################################################

# User-friendly function call
applyScaling <- function(sq, centers=NA, values=FALSE) {
    trf(sq, 'scale', centers=centers, values=values, cl=match.call())
}

scale <- function(ang, ...) {
  
  ang
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Current scaling
getScaling <- function(sq) {
  curscaling <- tail(sq, 1)[[1]]$scaling
  ifelse (is.null(curscaling), FALSE, curscaling)
}

################################################################################
# ALTERNATION transformations
################################################################################

# User-friendly transformation function calls
applyAlternation <- function(sq) {
  trf(sq, 'alternation', alternation=TRUE, cl=match.call())
}

removeAlternation <- function(sq) {
  trf(sq, 'alternation', alternation=FALSE, cl=match.call())
}

# Transformation function
alternation <- function(ang, ...) {

  for(idx in seq_along(E(ang))) {  
    
    objsrc <- sub('\\|.*', '', attr(E(ang)[idx], 'vnames'))
    objdest <- sub('.*\\|', '', attr(E(ang)[idx], 'vnames'))
    
    # Add new links and mark obsolete edges/vertices for deletion
    if (ang$alternation) {
      if (!grepl('>', attr(E(ang)[idx], 'vnames'))) {
        
        objattr <- E(ang)[idx]$label
        ang <- add_link(ang, objsrc, objdest, objattr)
        
        E(ang)[idx]$label <- 'DELETE'
      }
    } else {
      if (grepl('>', objsrc)) {
        
        objattr <- sub('.*>', '', objsrc)
        newobjsrc <- sub('>.*', '', objsrc)
        ang <- add_link(ang, newobjsrc, objdest, objattr)
        
        V(ang)[objsrc]$label <- 'DELETE'
      }
    }
  }
  
  # Delete obsolete vertices and edges
  ang <- delete.vertices(ang, which(V(ang)$label=='DELETE'))
  ang <- delete.edges(ang, which(E(ang)$label=='DELETE'))
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current alternation status
getAlternation <- function(sq) {
  tail(sq, 1)[[1]]$alternation
}

################################################################################
# SYMMETRY transformations
################################################################################

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

addSymmetries <- function(sq) {
  trf(sq, 'symmetry', cl=match.call())
}

symmetry <- function(ang, ...) {
 
  tmpang <- ang
  
  tmpang
}

################################################################################
# SPACE transformations
################################################################################

applySizing <- function(sq, method=NA) {
  trf(sq, 'space', method=method, cl=match.call())
}

space <- function(ang, ...) {

  aargs <- list(...)
  method <- aargs$method
  
  if (is.na(method)) {
    ang <- space_pregiven(ang)
  } else {

    # Calculate sizes
    #  V(ang)$size <- sample(10:20, length(V(ang)), replace=TRUE)
    
    # TODO: If algorithmic convert to center-only graph (function?) 
    
    #tmpang <- alternation(ang, remove=TRUE)
    #tmpang <- void(tmpang, getAttributes(ang))
    
    btw <- betweenness(ang) + 3
    V(ang)$size <- btw
    
    E(ang)$width <- sample(1:5, length(E(ang)), replace=TRUE)
    E(ang)$arrow.size <- E(ang)$width^2
  }
  
  ang
}

# Apply pregiven sizes
space_pregiven <- function(ang) {
  
  obs <- browseData(ang)
  sizing <- obs[!is.na(obs$size), ]

  for (idx in seq_along(sizing$object)) {
    cname <- sizing[idx,'object']
    csize <- as.numeric(sizing[idx,'size'])

    # Resize vertices
    if (is.na(sizing[idx,'objdest'])) {
      ang <- set_vertex_attr(ang, 'size', index=V(ang)$name==cname, value=csize)  
    } else {
#      ang <- set_edge_attr(ang, 'width', index=E(ang)$id==cname, value=csize)
      ang <- set_edge_attr(ang, 'width',
          index=attr(E(ang), 'vnames')==cname, value=csize)
    }
  }  
  
  # Resize edges (one for loop probably sufficient)
#  esizing <- sizing[grepl('>', sizing$object), ]
#  for (idx in seq_along(esizing$object)) {
#    ename <- gsub('>', '|', esizing[idx,'object'])
#    esize <- as.numeric(esizing[idx,'value'])
#    ang <- set_edge_attr(ang, 'width', index=E(ang)[ename], value=esize)
#  }  
  
  ang  
}


################################################################################
# ROUGHNESS transformations
################################################################################

# Explore clustering algorithms
browseSeeds <- function(sq) {

  npanels <- 6
  seeds <- sample(1:1000, npanels)
  
  par(mfrow=c(2,3), cex.lab=1, mar=c(5, 0, 3, 0))
  lapply(seeds, function(tryseed) applySeed(sq, tryseed))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly transformation function call
applySeed <- function(sq, seed=123) {
  trf(sq, 'roughness', seed=seed, cl=match.call())
}

# Transformation function
roughness <- function(ang, ...) {
  ang
}

################################################################################
# GRADIENT transformations
################################################################################

# Fading or transparency based on some measures
# Remote Centers without attributes as well?

applyGradient <- function(sq, ...) {
  
  ang <- aseq$struct[[length(aseq$struct)]]

  ang
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

  ang <- delete.vertices(ang, centers)
  
  # Delete attributes
  obs <- browseData(ang)
  ang <- delete.vertices(ang, 
    is.element(obs[match(V(ang)$name, obs$object), 'objsrc'], centers))
  
  ang
}

################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# TODO: Author, version, include data

# Conclude the sequnce
signoff <- function(sq, ...) {
  
}
