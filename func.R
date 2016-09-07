
library(igraph, quietly=TRUE)

################################################################################
# Analysis object and related methods
################################################################################

# Contruct the transformations sequence object
analysis <- function(name, dataref, ...) {

  obsl <- get(dataref)
  
  # Tidy up checkpoints for reshaping
  if (!is.element('checkpoint', names(obsl))) {
    obsl$checkpoint <- 'all'
  } else {
    obsl[is.na(obsl$checkpoint),'checkpoint'] <- 'all'
  }
  
  # Convert from long to wide format
  obsl <- unique(obsl[ ,1:4])
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
  
  # Tidy up
  names(obsw) <- gsub('value.', '', names(obsw))
  obsw <- obsw[with(obsw, order(object, checkpoint)), ]
  
  # Caching of long to wide transformation
  assign(paste0(dataref,'w'), obsw, envir=.GlobalEnv)

  # Empty graph with default settings  
  ang <- set_trsq_attr(graph.empty(), match.call(),

    # Generic (sequence or multi-step) metadata
    name=name, data=dataref, checkpoint=NA, output='plot',

    # Global properties with no default value
    partitioning=NA, scaling=NA, symmetry=NA, sizing=NA, layout=NA, 

    # Global properties with default value
    seed=1, theme='expressive', 

    # Global boolean properties
    gradient=FALSE, alternation=FALSE, simplicity=FALSE, ...)

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
# TODO: Redo checkpoint concept    
  #  curang <- apply_checkpoint(curang)
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
  ang$transformation <- gsub(' = ', '=', deparse(cl))[1]
  
  # Arbitrary attributes
  aargs <- list(...)
  for (idx in seq_along(aargs)) {
    curname <- names(aargs[idx])
    isKnownName <- is.element(curname, list.graph.attributes(ang))
    if (substr(ang$transformation, 1, 8) == 'analysis' | isKnownName) {
      if (!is.null(aargs[[idx]])) {
        ang <- set_graph_attr(ang, curname, aargs[[idx]]) 
      }
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
  ang <- thevoid(ang, centers=cntr[!(cntr %in% ckptobs$object)])

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

# TODO: Limit sequence output to 80 columns (long trans truncated with ...)
# TODO: Limit axis labels to one line

# Sequence overview printing
print.trsq <- function(sq, ...) {

  ang <- tail(sq, 1)[[1]]

  # Textual output
  curxlab <- ang$transformation[1]
  cat(ang$name, '\n\n')
  # print(cbind(id=format(sqtail$id, "%R"), sqtail[ ,2:3]), row.names=FALSE)
  print(tail(summary(sq), 3))
  par(mfrow = c(1,1), cex.lab=0.8)

  # Graphical output
  if (vcount(ang) > 0) {
    plot.trsq(sq, title=curxlab)
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Perhaps by using qgraph instead of igraph)

# Output last graph of the sequence
plot.trsq <- function(sq, title='plot1') {
  
  ang <- tail(sq, 1)[[1]]

  # Set random number generator
  set.seed(ang$seed)
  
  # Apply layout
  if (is.na(ang$layout)) {
    layout <- layout_nicely(remove.graph.attribute(ang, 'layout'))
  } else {
    layout <- do.call(ang$layout, list(ang))
  }

  # Determine plotting options  
  plopt <- list(xlab=title)
  thmopt <- get_plopt(ang$theme)
  
  V(ang)$color <- thmopt$vertex_color
  V(ang)[V(ang)$contrast]$color <- thmopt$vertex_contrast_color

  # Simplified/full presentation
  # Non-essential sans names? etc in the background
  if (ang$simplicity) {
    plopt <- c(plopt, edge.arrow.mode=0, vertex.label.cex=0.8)
    
    if (length(V(ang)) > 7) {
      # Set big enough criteria to sevent largest value
      bigenough <- V(ang)[order(V(ang)$size, decreasing=TRUE)[7]]$size
      for(idx in seq_along(V(ang))) {
        if (V(ang)[idx]$size < bigenough) {
          V(ang)[idx]$label <- NA
        }
      }
      E(ang)$label <- NA
    }
  }
  
  # Apply theme
  if (ang$theme == 'expressive') {
    if (is.na(ang$partitioning)) {
      
    } else if (ang$simplicity) {
      mbrpn <- as.numeric(as.factor(membership(V(ang))))
      #palette <- colorRampPalette(c('yellow','brown'), alpha=0.8)
      #colrs <- palette(max(mbrpn))[mbrpn]
      colrs <- rainbow(max(mbrpn), alpha=0.8)[mbrpn]
      plopt <- c(plopt, vertex.color=list(colrs))
    }
  }
  if (ang$theme == 'minimalist') {
    if (is.na(ang$partitioning)) {

    } else if (ang$simplicity) {
      mbrpn <- as.numeric(as.factor(membership(V(ang))))
      colrs <- gray.colors(max(mbrpn))[mbrpn]
      plopt <- c(plopt, vertex.color=list(colrs))
    }
  }
  
  plopt <- c(plopt, layout=list(layout), 
    vertex.frame.color=NA, vertex.label.family='sans', edge.arrow.size=0.2,
    edge.label.cex=0.8, edge.curved=list(0.3*which_mutual(ang)))
  
  # TODO: cluster coloring
  # If Simplicity, color notes to represent communities
  # do.call('plot', c(list(anp, ang), plopt, list(col=c("green","blue") )))
  
  # Apply partitioning
  anp <- ang$partitioning
  
  # Output a graph or a graph with community
  if (is.na(anp) | ang$simplicity) {
      do.call(ang$output, c(list(ang), plopt))
  } else {
    mbrp <- V(ang)$membership
    anp <- make_clusters(ang, as.numeric(factor(mbrp)))
    
    # TODO: communities in tkplot using color
    do.call('plot', c(list(anp, ang), plopt))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Output multiple plots
multiplot <- function(x, ...) UseMethod('multiplot')

multiplot.trsq <- function(sq, fun, items) {
  
  nrows <- round(length(items)/3)
  ncols <- ifelse(length(items) < 3, length(items), 3)
  par(mfrow=c(nrows,ncols), cex.lab=1, mar=c(4,1,1,1))
  
  for (item in items) {
    plot(do.call(fun, list(sq, item)), item)
  }
}

################################################################################
# CENTER transformations
################################################################################

# Explore center candidates
browseData <- function(sq, all=FALSE) {
  
  if(class(sq)=='trsq') sq <- tail(sq, 1)[[1]]
  
  obsw <- get(paste0(sq$data, 'w'))

  # Filter based on checkpoint
  if (!all) {
    if (is.na(sq$checkpoint)) sq$checkpoint <- 'all'
    obsw <- subset(obsw, checkpoint==sq$checkpoint)
  }
  
  obsw
}

browseEntities <- function(sq) {
  subset(browseData(sq), is.na(objattr))
}

browseAttributes <- function(sq) {
  subset(browseData(sq), !is.na(objattr) & is.na(objdest))
}

# TODO: Implement function
browseValues <- function(sq) {
  browseData(sq)
}

browseRelations <- function(sq) {
  subset(browseData(sq), !is.na(objdest))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Introduce 'type' attr for differentiating entities and attributes

# User friendly transformation function call
grow <- function(sq, cntr, depth=1, attrs=FALSE, vals=FALSE, ckpt=NULL, ...) {
  trf(sq, 'center', cl=match.call(), centers=cntr,
    depth=depth, attrs=attrs, vals=vals, checkpoint=ckpt, ...)
}

# Adding centers: entities, attributes, relations, values
center <- function(ang, ...) {
  
  # Temporary global graph and related observations
  tmpang <<- ang
  tmpobs <<- browseRelations(ang)
  
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
  
  obslnk <- tmpobs[tmpobs$objsrc==center | tmpobs$objdest==center, ]
  
  # Add entity if not present
  if (!is.element(center, V(tmpang)$name)) {

    # TODO: Separate meaning from presentation (shape and size)    
    eattr <- list(name=center, label=center, type='entity', contrast=FALSE)
    eattr <- c(eattr, shape='square', size=15)
    tmpang <<- add_vertices(tmpang, 1, attr=eattr)
  }
  
  # Add attributes and values
  if (attrs) add_attributes(tmpang, center, vals)
  if (vals) add_values(tmpang, center)
  
  # Add neighboring entities and update links
  if (nrow(obslnk) > 0) {
    apply(obslnk, 1, function(clnk) {
      
        nextcenter <- ifelse(clnk['objsrc']==center, clnk['objdest'], clnk['objsrc'])
      
        if (depth > 0) {
          add_entities(nextcenter, depth-1, attrs, vals)
        }
        
        # TODO: Convert an attribute to a link if present
        entities <- getCenters(tmpang, ctype='entity')$name
        if (nextcenter %in% entities) {
          tmpang <<- add_link(tmpang, clnk['objsrc'],
            clnk['objdest'], elabel=clnk['objattr'], etype=clnk['type'])
        }
    })
  }
}

# Add attribute elements
add_attributes <- function(ang, centers, vals=FALSE, atype='attribute') {
  
  # TODO: Shape selection should be part of theme
  ashape <- ifelse(atype=='attribute', 'circle', 'raster')
  
  # Temporary global graph
  tmpang <<- ang
  
  # Either an attribute or related entity can be passed
  if (grepl('>', centers)) {
    objsrc <- sub('>.*', '', centers)
    objattr <- sub('.*>', '', centers)
    attrspec <- data.frame(object=centers, objsrc=objsrc, objattr=objattr)
  } else {
    obs <- browseAttributes(tmpang)
    attrspec <- obs[is.element(obs$objsrc, centers), ]
  }
  
  # Add all attributes if not already present
  if (nrow(attrspec)) {
    links <- getRelations(ang)
    apply(attrspec, 1, function(cattr) {
      clinks <- links[grepl(paste0(cattr['objsrc'], '\\|'), links$name), ]
      newattr <- unname(cattr['object'])
      attrlink <- paste(cattr['objsrc'], newattr, sep='|') 
      if (!attrlink %in% clinks$name & !cattr['objattr'] %in% clinks$label) {
        newlabel <- unname(cattr['objattr'])
        if (!newattr %in% V(ang)$name) {
          aattr <- list(name=newattr, label=newlabel)
          aattr <- c(aattr, shape=ashape, type=atype, size=5, contrast=FALSE)
          ang <<- add_vertices(tmpang, 1, attr=aattr)
          tmpang <<- add_link(ang, cattr['objsrc'], newattr, etype='association')
        }
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
add_link <- function(ang, vsrc, vdest, elabel=NA, etype='defined') {
  
  elabel <- unname(elabel)
  etype <- unname(etype)
  
  # TODO: Convert attribute to link if already exists
  
  # TODO: Link width
  lattr <- list(width=1, contrast=FALSE)
  
  # Line type according to link type
  lattr <- c(lattr, type=etype, lty=ifelse(etype=='scanned', 'dashed', 'solid'))

  # Create different types of links
  if (etype=='association') {
    lattr <- c(lattr, arrow.mode=0)
    ang <- add.edges(ang, c(vsrc,vdest), attr=lattr)
  } else {
    if (ang$alternation) {
      linkname <- paste(paste(vsrc, elabel, sep='>'), vdest, sep='|')  
      if (!is.element(linkname, attr(E(ang), 'vnames'))) {
        newattr <- paste(vsrc, elabel, sep='>')
        ang <- add_attributes(ang, newattr)
        lattr <- c(lattr, arrow.mode=2)
        ang <- add.edges(ang, c(newattr,vdest), attr=lattr)
      }
    } else {
      linkname <- paste(vsrc, vdest, sep='|')  
      if (!is.element(linkname, attr(E(ang), 'vnames'))) {
        lattr <- c(lattr, label=elabel, arrow.mode=2)
        ang <- add.edges(ang, c(vsrc,vdest), attr=lattr)
      }
    } 
  }
  
  ang
}
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getCenters <- function(sq, ctype=NA) {
  
  if (class(sq) == 'trsq') {
    ang <- tail(sq, 1)[[1]]
  } else {
    ang <- sq
  }

  centers <- data.frame(get.vertex.attribute(ang), stringsAsFactors=FALSE)
  if (is.na(ctype)) {
    centers
  } else {
    centers[centers$type==ctype, ]
  }
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
browsePartitionings <- function(sq, plot=TRUE, ...) {
  
  # Available algorithms
  methods <- c('cluster_edge_betweenness','cluster_label_prop',
     'cluster_infomap','cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Plot or list algorithms
  if (plot) {
    multiplot(sq, 'applyPartitioning', methods)
  } else {
    methods
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User friendly transformation function call for partitioning
applyPartitioning <- function(sq, partitioning) {
  trf(sq, 'boundary', partitioning=partitioning, cl=match.call())
}

# User-friendly transformation function call for removing partitioning
removePartitioning <- function(sq) {
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
        ang <- set_vertex_attr(ang, 
          'membership', index=is.na(V(ang)$membership), value='ERR')
      } else {
        stop('No memberships pregiven.')
      }
    }
  }
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current partitioning algorithm or source data
getPartitioning <- function(sq) {
  tail(sq, 1)[[1]]$partitioning
}

################################################################################
# SCALE transformations
################################################################################

# User friendly transformation function call
doScaling <- function(sq, scaling) {
    trf(sq, 'scale', scaling=scaling, cl=match.call())
}

# TODO: Implement the function
scale <- function(ang, ...) {

  vl <- c("OBSERVATION>checkpoint","VERTEX>label","VERTEX>shape","VERTEX>type")
  vl <- c(vl,"GRAPH>alternation","GRAPH>checkpoint","GRAPH>gradient","GRAPH>layout","GRAPH>scaling")
  vl <- c(vl,"GRAPH>seed","GRAPH>simplicity","GRAPH>symmetry","GRAPH>theme","EDGE>type")

  thevoid(ang, centers=vl)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Current scaling settings => obsolete?
getScaling <- function(sq) {
  tail(sq, 1)[[1]]$scaling
}

################################################################################
# ALTERNATION transformations
################################################################################

# User-friendly function call for alternating
applyAlternation <- function(sq) {
  trf(sq, 'alternation', alternation=TRUE, cl=match.call())
}

# User-friendly function call removing alternation
removeAlternation <- function(sq) {
  trf(sq, 'alternation', alternation=FALSE, cl=match.call())
}

# Transformation function
alternation <- function(ang, ...) {

# TODO: type property handling
  
  for(idx in seq_along(E(ang))) {  
    
    objsrc <- sub('\\|.*', '', attr(E(ang)[idx], 'vnames'))
    objdest <- sub('.*\\|', '', attr(E(ang)[idx], 'vnames'))
    
    # Add new links and mark obsolete edges/vertices for deletion
    if (ang$alternation) {
      if (!grepl('>', attr(E(ang)[idx], 'vnames'))) {
        
        objlbl <- E(ang)[idx]$label
        objtype <- E(ang)[idx]$type
        ang <- add_link(ang, objsrc, objdest, elabel=objlbl, etype=objtype)
        
        E(ang)[idx]$label <- 'DELETE'
      }
    } else {
      if (grepl('>', objsrc)) {
      
        objattr <- sub('.*>', '', objsrc)
        newobjsrc <- sub('>.*', '', objsrc)
        ang <- add_link(ang, newobjsrc, objdest, elabel=objattr)
        
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

# User friendly transformation function call
applySymmetry <- function(sq) {
  trf(sq, 'symmetry', cl=match.call())
}

symmetry <- function(ang, ...) {
 
  tmpang <- ang
  
  tmpang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Get current symmerty settings => obsolete?
getSymmetry <- function(sq) {
  tail(sq, 1)[[1]]$symmetry
}

################################################################################
# SPACE transformations
################################################################################

# TODO: Implement function
browseSizings <- function(sq) {
  
}

# User friendly transformation function call
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

# Limit the range and maximum size
  maxsize <- max(V(ang)$size)
  if (maxsize > 35) {
    for (idx in seq_along(V(ang))) {
      if (V(ang)[idx]$size > 0) {
        V(ang)[idx]$size <- round(3 + 30*V(ang)[idx]$size/maxsize)
      }
    }
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

# TODO: Implement function
removeSizing <- function(sq) {
  
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current sizing algorithm or source data
getSizing <- function(sq) {
  tail(sq, 1)[[1]]$sizing
  
# TODO: Perhaps output some summary statitcs about sizing as well  
}
################################################################################
# ROUGHNESS transformations
################################################################################

# Explore randomizations
browseSeeds <- function(sq) {
  seeds <- sample(1:1000, size=9)
  multiplot(sq, 'applySeed', seeds)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly transformation function call
applySeed <- function(sq, seed=123) {
  trf(sq, 'roughness', seed=seed, cl=match.call())
}

# Transformation function
roughness <- function(ang, ...) {
  seed <- list(...)$seed
  if (is.numeric(seed)) {
    if (seed == as.integer(seed)) {
      ang
    } else {
      stop('Seed must be integer.')
    }
  } else {
    stop('Seed must be numeric.')
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current seed
getSeed <- function(sq) {
  tail(sq, 1)[[1]]$seed
}

################################################################################
# GRADIENT transformations
################################################################################

# Fading or transparency based on some measures
# Remote Centers without attributes as well?

# User friendly transformation function call
doGradients <- function(sq, center) {
  trf(sq, 'gradient', center=center, cl=match.call())
}

# Transformation function
gradient <- function(ang, ...) {

# TODO: Calculate gradient of every entity and adjust number of attributes accordingly
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Get current gradient settings => obsolete?
getGradient <- function(sq) {
  tail(sq, 1)[[1]]$gradient
}

################################################################################
# CONTRAST transformations
################################################################################

# Highlight parts of the structure
# TODO: Highlight links, values, perhaps even paths as well

# User friendly transformation function call
applyHighlight <-  function(sq, centers, ...) {
  trf(sq, 'contrast', centers=centers, highlight=TRUE, cl=match.call())
}

removeHighlight <-  function(sq, centers, ...) {
  trf(sq, 'contrast', centers=centers, highlight=FALSE, cl=match.call())
}

contrast <- function(ang, ...) {
  
  centers <- list(...)$centers
  hlc <- V(ang)$name %in% centers
  
  hl <- list(...)$highlight
  ang <- set_vertex_attr(ang, 'contrast', index=hlc, value=hl)
  
  ang
}

################################################################################
# INTERLOCK transformations
################################################################################

# Can be useful for handling repeating blocks like audit fields
# TODO: Group by community parameter
# TODO: Group by community parameter 

# User friendly transformation function call
group <- function(sq, group, members, ...) {
  trf(sq, 'interlock', group=group, members=members, cl=match.call())
}

# TODO: Implement, but not so crucial: just avoid unnecessary grouping
degroup <-  function(sq, group, ...) {
  trf(sq, 'interlock', group=group, cl=match.call())
}

# TODO: Implement function to identify group contents
# UseMethod getGroup()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

interlock <- function(ang, ...) {
  
  group <- list(...)$group
  members <- list(...)$members
  
  # Select attributes presented by member argument  
  
  # If members found create group vertex (sphere symbol or {name})
  
  # Recreate member node edges connected to group vertex
  ang <- add_attributes(ang, group, atype='agroup')
  
  # Delete obselete vertices and edges
  ang <- delete.vertices(ang, match(members, V(ang)$name))

  ang
}

################################################################################
# ECHO transformations
################################################################################

# Explore available themes
browseThemes <- function(sq, plot=TRUE) {
  
  themes <- get_plopt()
  
  # Plot or list themes
  if (plot) {
    multiplot(sq, 'applyTheme', themes)
  } else {
    themes
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User friendly transformation function call
applyTheme <- function(sq, theme, ...) {
  trf(sq, 'echo', theme=theme, cl=match.call(), ...) 
}

echo <- function(ang, ...) {
  aargs <- list(...)
  if(is.element(aargs$theme, browseThemes(sq, plot=FALSE))) {
    ang 
  } else {
    stop(paste('Unknown theme:', aargs$theme))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current design template
getTheme <- function(sq, ...) {
  tail(sq, 1)[[1]]$theme
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

get_plopt <- function(theme=NA) {

  if (is.na(theme)) {
    c('expressive', 'minimalist')
  } else {
    
    # TODO: Attribute color?
    vc <- switch(theme, minimalist='lightgrey', 'orange1')
    vcc <- switch(theme, minimalist='grey', 'red')
    plopt <- list(vertex_color=vc, vertex_contrast_color=vcc)

#    plopt <- c(plopt, vertex_contrast_color=vcc)
    plopt
  } 
}

################################################################################
# SHAPE transformations
################################################################################

# TODO: layout parameters: 
# -layout.reingold.tilford(ang, circular=T), 
# -layout <- layout.fruchterman.reingold(ang, niter=10000)

# Explore available layouts
browseLayouts <- function(sq, plot=TRUE, ...) {

  # TODO: Replace with a list limited to useful in this context layouts
  layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1]
  
  # TODO: bipartite if alternation=TRUE
  layouts <- layouts[!grepl("bipartite|nicely|sugiyama|tree", layouts)]
  
  # Plot or list layouts
  if (plot) {
    multiplot(sq, 'applyLayout', layouts)
  } else {
    layouts 
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User-friendly function call for setting the layout
applyLayout <- function(sq, layout, ...) {
  trf(sq, 'shape', layout=layout, cl=match.call(), ...)  
}

# Transformation function
shape <- function(ang, ...) {
  aargs <- list(...)
  if(is.element(aargs$layout, browseLayouts(sq, plot=FALSE))) {
    ang 
  } else {
    stop(paste('Unknown layout:', aargs$layout))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current layout algorithm
getLayout <- function(sq, ...) {
  tail(sq, 1)[[1]]$layout
}

################################################################################
# SIMPLICITY transformations
################################################################################

# User-friendly function call for setting simplified presentation mode
applySimplicity <- function(sq, ...) {
  trf(sq, 'simplicity', simplicity=TRUE, cl=match.call(), ...)  
}

# User-friendly function call for setting full presentation mode
removeSimplicity <- function(sq, ...) {
  trf(sq, 'simplicity', simplicity=FALSE, cl=match.call(), ...)
}

# Transformation function
simplicity <- function(ang, ...) {

  # TODO: Restore labels etc in removing simplicity  
  ang
}

# Get current simplicity settings
getSimplicity <- function(sq, ...) {
  tail(sq, 1)[[1]]$simplicity
}

################################################################################
# VOID transformations
################################################################################

# User friendly transformation function call
void <- function(sq, centers, ckpt=NULL, ...) {
  trf(sq, 'thevoid', centers=centers, checkpoint=ckpt, cl=match.call(), ...)
}

# Remove centers
thevoid <- function(ang, ...) {
  
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

# User friendly transformation function call
signoff <- function(sq, ...) {
  trf(sq, 'notseparateness', cl=match.call())
}

# Conclude the sequence
notseparateness <- function(ang, ...) {
  
  # TODO: Implement required functionality
  cat('Some summary info about the sequence...', '\n\n')
  
  cat('Packaging the data and the code...', '\n\n')
  
  ang
}