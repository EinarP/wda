
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Contruct the transformations sequence object
trsq <- function(name, data, ...) {

  # Empty graph with default settings  
  ang <- set_trsq_attr(graph.empty(), match.call(), name=name, data=data,  
    output='plot', checkpoint=NA, partitioning=NA, alternation=FALSE, 
    seed=1, ...)
  
  # Return a list comprised of an empty graph
  structure(list(ang), class='trsq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, trans, cl, ...) {
  
  # Retrieve latest structure with updated attributes
  curang <- set_trsq_attr(tail(sq, 1)[[1]], cl, ...)

  # Remove what does not belong to new checkpoint
  if (!is.null(list(...)$checkpoint)) {
    curang <- apply_checkpoint(curang)
  }
  
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

# Adjust the structure to match the current checkpoint
apply_checkpoint <- function(ang) {
  
  ckptobs <- browseData(ang)
  
  # Remove vertices
  cntr <- getCenters(ang)$name
  ang <- void(ang, centers=cntr[!(cntr %in% ckptobs$object)])

  # TODO: Adjust vertex values
  
  # TODO: Remove edges and adjust values
  
  # TODO: Adjust memberships
  
  # TODO: Adjust weights
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Return a summary table
summary.trsq <- function(sq, all=FALSE) {

  # Convert sequence list to a data frame
  sumrows <- lapply(sq, function(ang) data.frame(get.graph.attribute(ang)))
  sumall <- Reduce(function(...) merge(..., all=TRUE), sumrows)
  
  # Return all or selection of columns
  if (all) {
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

  # TODO: Limit sequence output to 80 columns (long trans truncated with ...)
  
  # Set random number generator
  set.seed(ifelse(is.na(seed), ang$seed, seed))

  # TODO: Limit axis labels to one line
  
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

  function(dsrc, all=FALSE) {
  
    if(class(dsrc)=='trsq') dsrc <- tail(dsrc, 1)[[1]]
  
    # Connect the data source
    obs <- get(dsrc$data)

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
    
    # Filter observations based on current checkpoint
    if (!(is.null(dsrc$checkpoint) | all)) {
      obsl <- obsl[grep(paste0('all|', dsrc$checkpoint), obsl$checkpoint), ]
    }
    
    # Convert from long to wide format
    obsw <- reshape(obsl, direction='wide',
      idvar=c('object','checkpoint'), timevar='property')

    # Concert weights to numeric
    wtcol <- grep('wt_', names(obsw))
    obsw[ ,wtcol] <- apply(obsw[ ,wtcol], 2, as.numeric)
    
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
addCenters <- function(sq, cntr, depth=1, attrs=FALSE, vals=FALSE, ckpt=NA) {
  
  trf(sq, 'center', cl=match.call(), centers=cntr,
    depth=depth, attrs=attrs, vals=vals, checkpoint=ckpt)
}

# Transformation function
center <- function(ang, ...) {
  
  # Temporary global graph
  tmpang <<- ang

  # Arbitrary attributes
  aargs <- list(...)
  
  # Process the vector of center names
  lapply(aargs$centers, function(x)
    add_entities(x, aargs$depth, attrs=aargs$attrs, vals=aargs$vals))

  # Return the updated graph
  tmpang
}

# Add centers and their neighbors
add_entities <- function(center, depth, attrs, vals) {
  
  # add head center if not present
  if (!is.element(center, V(tmpang)$name)) {
    
    # TODO: size related to space, if not present use center type defults
    tmpang <<- tmpang + vertices(center, label=center, shape='square', size=15)
  }
  
  # Add attributes and values
  if (attrs) add_attributes(tmpang, center, vals)
  if (vals) add_values(tmpang, center)
  
  # add neighboring entities
  if (depth > 0) {  
    obs <- browseRelations(tmpang)
    linkspec <- obs[obs$objsrc==center | obs$objdest==center, ]
    if (nrow(linkspec) > 0) {
      apply(linkspec, 1, function(clnk) {
        nextcenter <- ifelse(clnk['objsrc']==center, clnk['objdest'], clnk['objsrc'])
        add_entities(nextcenter, depth-1, attrs, vals)
        tmpang <<- add_link(tmpang, clnk['objsrc'], clnk['objdest'], clnk['objattr'])
      })
    }
  }
}

# Add elements
add_attributes <- function(ang, centers, vals=FALSE) {
  
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
      
      if (vals) add_values(tmpang, cattr['object'])
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
  
  # TODO: Implement get_ang
  if (class(sq) == 'trsq') {
    ang <- tail(sq, 1)[[1]]
  } else {
    ang <- sq
  }

  data.frame(get.vertex.attribute(ang), stringsAsFactors=FALSE)
}

# Retrieve current relationships
getRelations <- function(sq) {
  
  if(class(sq) == 'trsq') {
    ang <- tail(sq, 1)[[1]]
  } else {
    ang <- sq
  }
  
  data.frame(name=attr(E(ang), 'vnames'), 
    get.edge.attribute(ang), stringsAsFactors=FALSE)
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

applySizing <- function(sq, method=NULL) {
  trf(sq, 'space', method=method, cl=match.call())
}

space <- function(ang, ...) {

  aargs <- list(...)
  method <- ifelse(is.null(aargs$method), 'wt', aargs$method)
  
  if (existsFunction(method)) {
    # Calculate sizes
    #  V(ang)$size <- sample(10:20, length(V(ang)), replace=TRUE)
    
    # TODO: If algorithmic convert to center-only graph (function?) 
    
    #tmpang <- alternation(ang, remove=TRUE)
    #tmpang <- void(tmpang, getAttributes(ang))
    
    btw <- betweenness(ang) + 3
    V(ang)$size <- btw
    
    E(ang)$width <- sample(1:5, length(E(ang)), replace=TRUE)
    E(ang)$arrow.size <- E(ang)$width^2
  } else {
    ang <- space_pregiven(ang, method)
  }
  
  ang
}

# Apply pregiven sizes
space_pregiven <- function(ang, method) {
  
  obs <- browseData(ang)
  sizing <- obs[!is.na(obs[ ,method]), ]

  for (idx in seq_along(sizing$object)) {
    cname <- sizing[idx,'object']
    csize <- as.numeric(sizing[idx,method])

    # Resize vertices
    if (is.na(sizing[idx,'objdest'])) {
      ang <- set_vertex_attr(ang, 'size', index=V(ang)$name==cname, value=csize)  
    } else {
      ang <- set_edge_attr(ang, 'width',
          index=attr(E(ang), 'vnames')==cname, value=csize)
    }
  }

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

# Can be useful for handling repeating blocks like audit fields

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

# Non-essential sans names? etc in the background

applySimplicity <- function(sq, ...) {
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

################################################################################
# VOID transformations
################################################################################

# Remove centers
voidCenters <- function(sq, centers=NULL, ckpt=NA) {
  trf(sq, 'void', centers=centers, checkpoint=ckpt, cl=match.call())
}

void <- function(ang, ...) {
  
  # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers

  ang <- delete.vertices(ang, centers)
  
  # Delete attributes
  obs <- browseData(ang, all=TRUE)
  ang <- delete.vertices(ang, 
    is.element(obs[match(V(ang)$name, obs$object), 'objsrc'], centers))
  
  ang
}

################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# TODO: Author, version, etc. personalization
# TODO: Archive data, code, results time-proof manner

# Conclude the sequnce
signoff <- function(sq, ...) {
  
}
