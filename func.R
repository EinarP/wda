
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# Contruct the transformations sequence object
trsq <- function(sqname, trfdata, ...) {

  # Empty graph with default settings  
  ang <- set_trsq_attr(graph.empty(), match.call(), name=sqname,
    data=trfdata, seed=1, output='plot', partitioning=NA, ...)
  
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

browseCenters <- browseSource('all')
browseEntities <- browseSource('entity')
browseAttributes <- browseSource('attribute')
browseRelationships <- browseSource('relationship')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call
addCenters <- function(sq, centers, depth=1, addelem=FALSE, addval=FALSE) {
  
  trf(sq, 'center', cl=match.call(),
    centers=centers, depth=depth, addelem=addelem, addval=addval)
}

# Canonical function call
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

    obs <- browseRelationships(tmpang)
    linkspec <- obs[obs$objsrc==center | obs$objdest==center, ]
    apply(linkspec, 1, function(clnk) {
      
      nextcenter <- ifelse(clnk['objsrc']==center, clnk['objdest'], clnk['objsrc'])
      add_entities(nextcenter, depth-1, addelem, addval)

      if (!is.element(clnk['object'], E(tmpang)$id)) {
        tmpang <<- tmpang + edge(clnk['objsrc'], clnk['objdest'], 
          id=clnk['object'], label=clnk['objattr'], edge.arrow.size=0.2, arrow.mode=2, width=1)
      }
    })
  }
}

# Add elements
add_attributes <- function(ang, centers, addval) {
  
  # Temporary global graph
  tmpang <<- ang
  
  obs <- browseAttributes(tmpang)
  attrspec <- obs[is.element(obs$objsrc, centers), ]
  
  if (nrow(attrspec)) {
    apply(attrspec, 1, function(cattr) {
      if (!is.element(cattr['object'], V(tmpang)$name)) {
        tmpang <<- tmpang + vertices(cattr['object'], label=cattr['objattr'], 
          shape='circle', size=5)
        
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

  obs <- browseCenters(ang)
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
  
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getCenters <- function(sq) {
  
  ang <- tail(sq, 1)[[1]]
  
  ent <- V(ang)$name[!grepl('>', V(ang)$name)]
  elem <- V(ang)$name[grepl('>', V(ang)$name)]
  
  vals <- grepl('=', V(ang)$label)
  val <- as.data.frame(list(center=V(ang)$name[vals], value=V(ang)$label[vals]))
  
  
  # Return list of entities, elements, values
  list(entities=ent, elements=elem, values=val)
}

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
      obs <- browseCenters(ang)
      comm <- obs[!is.na(obs[ ,anp]), c('object',anp)]
      if (nrow(comm)) {
        ents <- obs[match(V(ang)$name, obs$object), 'objsrc']
        V(ang)$membership <- comm[match(ents, comm$object), 'member']
      } else {
        stop('No memberships pregiven.')
      }
    }
  }
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Describe current boundary
getBoundary <- function(x) {
  tail(x, 1)[[1]]$partitioning
}

################################################################################
# SCALE transformations
################################################################################


# User-friendly function call
applyScale <- function(sq, centers=NA, values=FALSE) {
    trf(sq, 'scale', centers=centers, values=values, cl=match.call())
}

scale <- function(ang, ...) {
  
  ang
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

removeAlternation <- function(sq) {
  tail(trdemo$struct, 1)[[1]] 
}

alternation <- function(ang, ...) {
  
  tmpang <- ang
  
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current alternation status
getAlternation <- function(sq) {
  ang <- tail(sq, 1)[[1]]
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
  
  obs <- browseCenters(ang)
  sizing <- obs[!is.na(obs$size), ]

  for (idx in seq_along(sizing$object)) {
    cname <- sizing[idx,'object']
    csize <- as.numeric(sizing[idx,'size'])

    # Resize vertices
    if (is.na(sizing[idx,'objdest'])) {
      ang <- set_vertex_attr(ang, 'size', index=V(ang)$name==cname, value=csize)  
    } else {
      ang <- set_edge_attr(ang, 'width', index=E(ang)$id==cname, value=csize)
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

# Somehow create local symmetries, e.g. replicate attribute counts,
# neighboring centers, etc.

applySeed <- function(sq, ...) {

}

roughness <- function(aseq, chkp=NA) {
  
  ang <- aseq$struct[[length(aseq$struct)]]
  
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
  obs <- browseCenters(ang)
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
