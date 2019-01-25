
# TODO: Split into smaller files
# TODO: Use highlighting in (partitioning, sizing, etc.) error handling
# TODO: Non-igraph layouts: trees, timelines?

suppressPackageStartupMessages(library(igraph))
suppressPackageStartupMessages(library(tidyr))

################################################################################
# Analysis object and related methods
################################################################################

# Construct the transformation sequence object
analysis <- function(name, obs = NULL, ckpt = NULL) {
  
  # Default sequence properties  
  ang <- set_asq_attr(graph.empty(), match.call(),
                      
    # Generic (sequence or multi-step) metadata
    name = name, data = NA, checkpoint = NA, instance = NA, output = 'plot',
                      
    # Global properties with no default value
    layout = NA, partitioning = NA, partitioning2 = NA,
    sizing = NA, sizing2 = NA, simplicity = NA, 
                      
    # Global properties with default value
    seed = 1, theme = 'expressive', 
                      
    # Global boolean properties
    alternation = FALSE
  )
  
  # Initialise the sequence
  sq <- structure(list(ang), class = 'asq')
  trf(sq, 'init', cl = match.call(), obs = obs, checkpoint = ckpt)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Wrapper for transformations
trf <- function(sq, trans, cl, ...) {

  # Add observations
  if (!is.null(sq_data <- list(...)$obs)) {
    sq_data <- trf_obs(sq, sq_data, sq_name = as.character(cl[[2]]))
  }

  # TODO: New checkpoint processing
  new_ckpt <- list(...)$checkpoint
  
  # Retrieve latest structure with updated attributes
  curang <- set_asq_attr(tail(sq, 1)[[1]], cl, data = sq_data, ...)

  # Perform the transformation
  if (trans != 'init') {
    
    trfang <- do.call(match.fun(trans), args = list(curang, ...))
    
    # Updates after structural growth
    if (trans %in% c('center', 'alternation')) {
      if (!(is.na(trfang$partitioning) & is.na(trfang$partitioning2))) {
        trfang <- partitioning(trfang)
      }
      if (!(is.na(trfang$sizing) & is.na(trfang$sizing2))) {
        trfang <- sizing(trfang)
      }
    }
    
    # Expand the structure list
    sq[[length(sq)+1]] <- trfang
  } else {
    
    sq[[1]] <- curang
  }
  
  # Return the sequence  
  sq
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add observations

trf_obs <- function(sq, new_obs, sq_name) {
  
  ang <- tail(sq, 1)[[1]]

  # Retrieve observations indirectly through referenced variable name
  if (class(new_obs) == 'character') {
    
    data_long_ref <- new_obs
    obsl <- as_obs(get(as.character(substitute(data_long_ref))))
    
  # Retrieve and/or append observations directly
  } else {
    
    if (is.na(ang$data)) {
      ang$data <- paste0(gsub(' ', '_', tolower(sq_name)), '_obs')
    }
    data_long_ref <- ang$data
    obsl <- get0(as.character(substitute(data_long_ref)))
    obsl <- rbind(obsl, as_obs(new_obs))
  }
  
  # TODO: Don't convert everything to wide format all the time (if not fix browseDataTable as well)?
  data_wide_ref <- paste0(data_long_ref, '_wide')
  
  # Convert from long to wide format
  obsw <- spread(unique(obsl), property, value)
  
  # Convert weights to numeric
  wtcol <- grep('wt_', names(obsw))
  obsw[ ,wtcol] <- apply(obsw[ ,wtcol], 2, as.numeric)
  
  # Append helper columns (with dummy attribute if needed)
  hc <- sapply(obsw$object, function(x) {
    if (grepl('|', x) & !grepl('>', x))
      sub('\\|', sub('\\|', '_', paste0('>.', x, '>')), x)
    else
      sub('\\|', '>', x)
  })
  hc <- strsplit(hc, '>', fixed=TRUE)
  obsw$objsrc <- sapply(hc, function(x) x[1])
  obsw$objattr <- sapply(hc, function(x) x[2])
  obsw$objdest <- sapply(hc, function(x) x[3])
  
  # Tidy up
  names(obsw) <- gsub('value.', '', names(obsw))
  obsw <- obsw[with(obsw, order(object, checkpoint)), ]
  
  # Caching of long to wide transformation???
  assign(data_long_ref, obsl, envir=.GlobalEnv)
  assign(data_wide_ref, obsw, envir=.GlobalEnv)
  
  # Reference to a data source
  data_long_ref
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Add observations
as_obs <- function(x) {

  new_obs <- as.data.frame(x, stringsAsFactors = FALSE)   
  
  if (ncol(new_obs) < 3) {
    stop('Mandatory observation fields missing.')
  } else{
    colnames(new_obs)[1:3] <- c('object', 'property', 'value')
  }

  if (!('checkpoint' %in% colnames(new_obs))) {
    new_obs$checkpoint <- 'all'
  } else {
    new_obs[is.na(new_obs$checkpoint),'checkpoint'] <- 'all'
  }
    
  if (!('instance' %in% colnames(new_obs))) {
    new_obs$instance <- NA
  }
  
  # TODO: To be introduced
  if (!('source' %in% colnames(new_obs))) {
    new_obs$source <- NA
  }

  # TODO: obsl <- unique(obsl[ ,1:4])?
  cols <- c('object', 'property', 'value', 'checkpoint', 'instance')
  new_obs[ ,cols]
}

# Convert a data frame to ENTITY observations
as_obs_entity <- function(df, entity = 'ENTITY', id_cols = NULL) {
  
  # Same values differentiated by checkpoint
  if (!('checkpoint' %in% colnames(df))) {
    df$checkpoint <- 'all'
  }
  
  # Unique Id
  if (missing(id_cols)) {
    df$instance <- NA
  } else {
    df$instance <- do.call(paste, c(df[c(id_cols)], sep = '_'))
  }

  # Reshape to long format
  df_long <- data.frame(apply(df, 2, as.character), stringsAsFactors = FALSE)
  df_long <- gather(df_long, object, value, -checkpoint, -instance)

  # Value observations
  df_long$object <- paste0(entity, '>', df_long$object)
  df_long$property <- 'value'
  df_long$checkpoint <- as.character(df_long$checkpoint)

  # Currently used columns
  cols <- c('object', 'property', 'value', 'checkpoint', 'instance')
  df_long <- data.frame(df_long[ ,cols])
  
  # Data type observations as entity>attribute>type=x
  lapply(colnames(df), function (x) {
    if (!(x %in% c('checkpoint', 'instance'))) {
      type_obs <- data.frame(object = paste0(entity, '>', x), property = 'type',
        value = class(df[ ,x]), checkpoint = 'all', instance = NA)
      df_long <<- rbind(df_long, type_obs)
    }
  })

  df_long
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Internal function for setting tranformation attributes
set_asq_attr <- function(ang, cl, ...) {
  
  # Mandatory attributes
  ang$dtstamp <- Sys.time()
  ang$transformation <- gsub('"', "'", deparse(cl))[1]

  # Arbitrary attributes
  aargs <- list(...)
  for (idx in seq_along(aargs)) {
    curname <- names(aargs[idx])
    isKnownName <- is.element(curname, list.graph.attributes(ang))
    if (substr(ang$transformation[1], 1, 8) == 'analysis' | isKnownName) {
      if (!is.null(aargs[[idx]]) & curname != 'obs') {
        ang <- set_graph_attr(ang, curname, aargs[[idx]]) 
      }
    }
  }
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Adjust the structure to match the current checkpoint
trf_checkpoint <- function(ang) {
  
  ckptobs <- browseData(ang)
  
  # Remove vertices
  cntr <- getElements(ang)$name
  ang <- thevoid(ang, centers=cntr[!(cntr %in% ckptobs$object)])

  # TODO: Adjust vertex values
  
  # TODO: Remove edges and adjust values
  
  # TODO: Adjust memberships
  
  # TODO: Adjust weights
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Return a summary table
summary.asq <- function(sq, all = FALSE, plot = FALSE) {

  # Convert sequence list to a data frame
  sumrows <- lapply(sq, function(ang) {
    data.frame(get.graph.attribute(ang), stringsAsFactors = FALSE)
  })
  sumall <- Reduce(function(...) merge(..., all = TRUE), sumrows)
  
  # Return all or selection of columns
  if (all) {
    sumall
  } else {
    cols <- 'transformation'
    if (sum(!is.na(sumall$checkpoint))) cols <- c(cols, 'checkpoint')
    if (sum(!is.na(sumall$instance))) cols <- c(cols, 'instance')
    sumall[ ,cols, drop = FALSE]
  }
  
# TODO: ? Obsolete: sumall$time <- format(sumall$dtstamp, '%R')
}  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Sequence overview printing
print.asq <- function(sq, ...) {

  ang <<- tail(sq, 1)[[1]]

  # Textual output
  cat(ang$name, '\n\n')
  
  sqs <- summary(sq)

  print(tail(sqs, 3))
  
  # TODO: Output relevant global options?
  
  # Graphical output
  if (vcount(ang) > 0) {
    par(mfrow = c(1,1), cex.lab=0.8)
    plot_ang(ang, paste0(length(sq), ': ', ang$transformation[1]))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Plot the analysis sequence
plot.asq <- function(sq, xlab=NULL, main=NULL, steps=NULL, ncol=2) {
  if (is.null(steps)) {
    plot_ang(tail(sq, 1)[[1]], xlab, main)
  } else {
    
    # TODO: Reuse multiplot for plot panel determination
    nrow <- round(length(steps)/ncol)
    par(mfrow=c(nrow,ncol), cex.lab=1, mar=c(4,1,4,1))
    
    trfs <- summary(sq)$transformation
    
    steps <- sort(steps)
    nstep <- min(max(steps), length(sq))
    
    curseq <- NA
    steps_idx <- startstep <- 1
    for (curstep in 1:nstep) {
      
      # Collect the sequence steps
      curtrf <- sub('\\(.*$', '', trfs[curstep])
      if (is.na(curseq[1])) {
        curseq <- curtrf 
      } else {
        curseq <- c(curseq, curtrf)
      }
      
      # Plot requested steps
      if (curstep==steps[steps_idx]) {

        if (startstep==curstep) {
          seqnums <- paste0('[', startstep, ']')
        } else {
          seqnums <- paste0('[', startstep, '-', curstep, ']')
        }
        sqsteps <- paste(seqnums, paste(curseq, collapse='>'))
        
        plot_ang(sq[[curstep]], sqsteps, main=main[steps_idx])
        
        startstep <- curstep + 1
        steps_idx <- steps_idx + 1
        curseq <- NA
      }
    }
  }
}

# Plot the analysis graph
plot_ang <- function(ang, xlab=NULL, main=NULL) {
  
  # Retrieve pre-defined plotting options  
  thmopt <- get_plopt(ang$theme)

  # TODO: Limit sequence label to 80 chars (lengthy labels truncated with ...)
  plopt <- list(xlab=xlab, main=main)
  
  # Set random number generator
  set.seed(ang$seed)
  
  # Apply layout
  if (is.na(ang$layout)) {
    layout <- layout_nicely(remove.graph.attribute(ang, 'layout'))
  } else {
    layout <- do.call(ang$layout, list(ang))
  }

  # Shapes corresponding to vertex type
  V(ang)[V(ang)$type=='entity']$shape <- 'square'
  V(ang)[V(ang)$type=='egroup']$shape <- 'fsquare'
  V(ang)[V(ang)$type=='attribute']$shape <- 'circle'
  V(ang)[V(ang)$type=='agroup']$shape <- 'fcircle' 
  
  V(ang)$frame.color <- thmopt$vertex_frame_color
  V(ang)[!V(ang)$type %in% c('egroup','agroup')]$frame.color <- NA
  V(ang)$frame.width <- thmopt$vertex_frame_width
    
  # Simplified/full presentation
  # TODO: Non-essential (sans names?, etc.) in the background?
  if (!is.na(ang$simplicity)) {
    plopt <- c(plopt, edge.arrow.mode=0, vertex.label.cex=0.8)
    
    # TODO: Dynamic or parameterized determination of big enough criteria
    namedsz <- ang$simplicity
    if (length(V(ang)) > namedsz) {
      bigenough <- V(ang)[order(V(ang)$size, decreasing=TRUE)[namedsz]]$size
      for(idx in seq_along(V(ang))) {
        if (V(ang)[idx]$size < bigenough) {
          V(ang)[idx]$label <- NA
        }
      }
      E(ang)$label <- NA
    }
  } else {
    E(ang)$label <- ifelse(substring(E(ang)$label, 1, 1) == '.', NA, E(ang)$label)
  }
  
  # Apply theme
  if (ang$theme == 'expressive') {
    if (is.na(ang$partitioning)) {
      
    } else if (!is.na(ang$simplicity)) {
      mbrpn <- as.numeric(as.factor(membership(V(ang))))
      #palette <- colorRampPalette(c('yellow','brown'), alpha=0.8)
      #colrs <- palette(max(mbrpn))[mbrpn]
      colrs <- rainbow(max(mbrpn), alpha=0.8)[mbrpn]
      plopt <- c(plopt, vertex.color=list(colrs))
    }
  }
  if (ang$theme == 'minimalist') {
    if (is.na(ang$partitioning)) {

    } else if (!is.na(ang$simplicity)) {
      mbrpn <- as.numeric(as.factor(membership(V(ang))))
      colrs <- gray.colors(max(mbrpn))[mbrpn]
      plopt <- c(plopt, vertex.color=list(colrs))
    }
  }
  
  label_color = adjustcolor('blue4', 0.8)
  plopt <- c(plopt, layout=list(layout), 
    vertex.label.family='sans', edge.label.family='sans', edge.arrow.size=0.4,
    edge.label.cex=0.8, edge.curved=list(0.3*which_mutual(ang)), 
    vertex.label.color = label_color, edge.label.color = label_color
  )
  
  # TODO: cluster coloring
  # If Simplicity, color notes to represent communities
  # do.call('plot', c(list(anp, ang), plopt, list(col=c("green","blue") )))
  
  # Adjust element sizes
  if (is.na(ang$sizing)) {
    V(ang)[V(ang)$type=='entity']$size <- thmopt$entity_size
    V(ang)[V(ang)$type=='egroup']$size <- thmopt$egroup_size
    V(ang)[V(ang)$type=='attribute']$size <- thmopt$attribute_size
    V(ang)[V(ang)$type=='agroup']$size <- thmopt$agroup_size
  } else {
    # TODO: Adjust different types differently
#    maxsize <- max(V(ang)$size)
#    if (maxsize > 100) {
 #     V(ang)$size <- log1p(V(ang)$size)
  #    maxsize <- max(V(ang)$size)
 #     V(ang)$size <- log(V(ang)$size, 1.1)
#    }
    # TODO: vertex_scaling should be adjusted by center count
#    V(ang)$size <- round(V(ang)$size/maxsize * thmopt$vertex_scaling)
 #   maxsize <- max(V(ang)$size)
#    V(ang)$size <- V(ang)$size/maxsize * thmopt$vertex_scaling
    # TODO: Max should be proportional to number of items
    V(ang)$size <- 5 + V(ang)$size/max(V(ang)$size)*35
  }
  
  # Differentiated communities by entity coloring
  if (is.na(ang$partitioning2)) {
    V(ang)$color <- thmopt$vertex_color
  } else {
    nc <- length(unique(V(ang)$membership2))
    V(ang)$color <- rainbow(nc)[as.factor(V(ang)$membership2)]
  }
  
  # Highlight
  hl_idx <- as.logical(V(ang)$contrast)
  V(ang)[hl_idx]$color <- thmopt$vertex_contrast_color
  V(ang)[hl_idx]$label.cex <- thmopt$vertex_contrast_label_cex
  
  # Label sizes in different elements
  V(ang)[V(ang)$type=='attribute']$label.cex <- 0.8
  
  # Output a graph or a graph with community
  if (is.na(ang$partitioning) | !is.na(ang$simplicity)) {
      do.call(ang$output, c(list(ang), plopt))
  } else {
    mbrp <- V(ang)$membership

    # TODO: Directed graph clustering correctness is undetermined
    anp <- make_clusters(as.undirected(ang), as.numeric(factor(unlist(mbrp))))
 
    if (ang$theme == 'minimalist') {
      plopt <- c(plopt, mark.border=NA, mark.col=list(c("gray95")))
      plopt <- c(plopt, edge.color='black')
    }
    
    # TODO: communities in tkplot using color
    plopt <- c(plopt, col=list(V(ang)$color))
    do.call('plot', c(list(anp, ang), plopt))
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Output multiple plots
multiplot <- function(x, ...) UseMethod('multiplot')

multiplot.asq <- function(sq, fun, items) {
  
  n <- length(items)
  if (n < 5) {
    nrows <- ifelse(n < 3, 1, 2)
    ncols <- ifelse(n < 2, 1, 2)
  } else {
    nrows <- round(n/3)
    ncols <- ifelse(n < 3, length(items), 3)
  }
  par(mfrow=c(nrows,ncols), cex.lab=1, mar=c(4,1,1,1))
  
  for (item in items) {
    plot(do.call(fun, list(sq, item)), item)
  }
}

################################################################################
# CENTER transformations
################################################################################

# Explore center candidates
browseData <- function(sq, ckpt=NULL) {
  
  if(class(sq)=='asq') sq <- tail(sq, 1)[[1]]
  
  obsw <- get(paste0(sq$data, '_wide'))

  # Filter based on checkpoint
  if (is.null(ckpt)) {
    if (!is.na(sq$checkpoint)) {
      obsw <- subset(obsw, checkpoint==sq$checkpoint)
    } 
  } else {
    if (ckpt != 'all') {
      obsw <- subset(obsw, checkpoint==ckpt) 
    }
  }
  
  obsw
}

browseEntities <- function(sq, ckpt=NULL) {
  subset(browseData(sq, ckpt), is.na(objattr))
}

browseAttributes <- function(sq, ckpt=NULL) {
  subset(browseData(sq, ckpt), !is.na(objattr) & is.na(objdest))
}

# TODO: Implement function
browseValues <- function(sq, ckpt=NULL) {
  browseData(sq, ckpt)
}

browseRelations <- function(sq, ckpt=NULL) {
  subset(browseData(sq, ckpt), !is.na(objdest))
}

# Display observations in a table format
browseDataTable <- function(sq, object, checkpoint = NULL, instance = NULL) {

  # TODO: If object is a link, dest must be present
  sq_data <- browseData(sq)
  sq_data <- sq_data[sq_data$objsrc == object & is.na(sq_data$objdest), ]
  
  # Present in wide format
  sqdw <- sq_data[!is.na(sq_data$value),c('checkpoint', 'instance', 'objattr', 'value')]
  sqdw <- spread(sqdw, objattr, value)
  
  # Apply data types
  if ('type' %in% colnames(sq_data)) {
    lapply(colnames(sqdw), function (x) {
        col_type <- sq_data[!is.na(sq_data$type) & sq_data$objattr == x, 'type']
        if (length(col_type)) {
          sqdw[ ,x] <<- do.call(paste0('as.', col_type), list(sqdw[ ,x]))
        }
    })
  }

  # Output the table
  drops <- c(ifelse(is.null(checkpoint), 'checkpoint', ''), ifelse(is.null(instance), 'instance', ''))
  sqdw[ ,!(names(sqdw) %in% drops)]
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Introduce 'type' attr for differentiating entities and attributes

# User friendly transformation function call
grow <- function(sq, elems, width = 1, depth = 0, ckpt = NULL, ...) {
  
  # TODO: Gradually phase out historical arguments attrs and vals below
  attrs <- ifelse(depth > 0, TRUE, FALSE)
  vals <- ifelse(depth > 1, TRUE, FALSE)
  
  dots <- list(...)
  if (is.logical(dots$attrs)) attrs <- dots$attrs
  if (is.logical(dots$vals)) vals <- dots$vals

  trf(sq, 'center', cl = match.call(), elems = elems, width = width,
    depth = depth, attrs = attrs, vals = vals, checkpoint = ckpt, ...)
}

# Adding elements like entities, attributes, relations, values
center <- function(ang, ...) {
  
  # TODO: Temporary global graph and related observations ugly
  tmpang <<- ang
  tmpobs <<- browseRelations(ang)
  
  # Arbitrary attributes
  aargs <- list(...)
  
# Process the vector of center names
#  lapply(aargs$elems, function(x)
#    add_entities(x, aargs$width, attrs=aargs$attrs, vals=aargs$vals))

  elems <- aargs$elems
  for (idx in seq_along(elems)) {
    vals <- ifelse(grepl('=', elems[idx]), TRUE, aargs$vals)
    if (grepl('>', elems[idx])) {
      add_attributes(tmpang, elems[idx], vals = vals, atype = 'attribute')    
    } else {
      add_entities(elems[idx], aargs$width, attrs = aargs$attrs, vals = vals)
    }
  }
  
  # Return the updated graph
  tmpang
}

# Add centers and their neighbors
add_entities <- function(center, width, attrs, vals) {
  
  obslnk <- tmpobs[tmpobs$objsrc==center | tmpobs$objdest==center, ]
  
  # Add entity if not present
  if (!is.element(center, V(tmpang)$name)) {
    eattr <- list(name=center, label=center, type='entity', contrast=FALSE)
    tmpang <<- add_vertices(tmpang, 1, attr=eattr)
  }
  
  # Add attributes and values
  if (attrs) add_attributes(tmpang, center, vals)
  if (vals) add_values(tmpang, center)
  
  # Add neighboring entities and update links
  if (nrow(obslnk) > 0) {
    apply(obslnk, 1, function(clnk) {
      
        nextcenter <- ifelse(clnk['objsrc']==center, clnk['objdest'], clnk['objsrc'])
      
        if (width > 0) {
          add_entities(nextcenter, width-1, attrs, vals)
        }
        
        # TODO: Convert an attribute to a link if present
        entities <- getElements(tmpang, ctype='entity')$name
        if (nextcenter %in% entities) {
          tmpang <<- add_link(tmpang, clnk['objsrc'],
            clnk['objdest'], elabel=clnk['objattr'], etype=clnk['type'])
        }
    })
  }
}

# Add attribute elements
add_attributes <- function(ang, centers, vals=FALSE, atype='attribute') {
  
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
      src <- cattr['objsrc']
      anames <- links[grepl(paste0(src, '\\|'), links$name), 'name']
      alabels <- links[grepl(paste0(src, '\\|'), links$name), 'label']
      newattr <- unname(cattr['object'])
      attrlink <- paste(src, newattr, sep='|')
      if (!attrlink %in% anames) {
        if (!(!is.na(tmpang$simplicity) & cattr['objattr'] %in% alabels)) {
          newlabel <- unname(cattr['objattr'])
          if (!newattr %in% V(ang)$name) {
            
            # Don't add if link exists
            el <- get.edgelist(ang)
            noLink <- sum(E(ang)$label==newlabel & el[ ,1]==src, na.rm=T) < 1
            if (noLink | ang$alternation) {
              aattr <- list(name=newattr, label=newlabel)
              aattr <- c(aattr, type=atype, contrast=FALSE)
              ang <<- add_vertices(tmpang, 1, attr=aattr)
              tmpang <<- add_link(ang, src, newattr, etype='association')
            }
          }
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
# TODO: Scanned and defined or smth like fixed and flexible?
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
        
        # Delete source attribute if present
        srca <- paste(vsrc, elabel, sep='>')
        if (srca %in% V(ang)$name) {
          ang <- delete.vertices(ang, srca)
        }
      }
    } 
  }
  
  ang
}
 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve current centers
getElements <- function(sq, ctype=NA) {
  
  if (class(sq) == 'asq') {
    ang <- tail(sq, 1)[[1]]
  } else {
    ang <- sq
  }

  centers <- data.frame(get.vertex.attribute(ang), stringsAsFactors = FALSE)
  if (is.na(ctype)) {
    centers
  } else {
    centers[centers$type==ctype, ]
  }
}

# Retrieve current relationships
getRelations <- function(sq) {
  
  if(class(sq) == 'asq') {
    ang <- tail(sq, 1)[[1]]
  } else {
    ang <- sq
  }
  
  data.frame(name=attr(E(ang), 'vnames'), 
    get.edge.attribute(ang), stringsAsFactors=FALSE)
}

################################################################################
# MEMBERSHIP-PARTITIONING transformations
################################################################################

# TODO: Possibility to add legends (especially for partitioning2)
# TODO: Grouped layout (see strfa page) for partitioning 2

# Explore clustering algorithms
browsePartitionings <- function(sq, plot = TRUE, ...) {
  
  # Available algorithms
#  methods <- c('cluster_edge_betweenness','cluster_label_prop',
  methods <- c('cluster_edge_betweenness',
      'cluster_infomap','cluster_optimal','cluster_spinglass','cluster_walktrap')  
  
  # Plot or list algorithms
  if (plot) {
    multiplot(sq, 'applyPartitioning', methods)
  } else {
    methods
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User friendly transformation function calls for partitioning
applyPartitioning <- function(sq, partitioning, ...) {
  trf(sq, 'partitioning', partitioning=partitioning, cl=match.call(), ...)
}

applyPartitioning2 <- function(sq, partitioning, ...) {
  trf(sq, 'partitioning', partitioning2=partitioning, cl=match.call(), ...)
}

# User-friendly transformation function calls for removing partitioning
removePartitioning <- function(sq) {
  trf(sq, 'partitioning', partitioning=NA, cl=match.call())
}

removePartitioning2 <- function(sq) {
  trf(sq, 'partitioning', partitioning2=NA, cl=match.call())
}

# Add boundary
partitioning <- function(ang, ...) {

  # TODO: Support grouping before setting the membership
  comm_idx <- V(ang)[V(ang)$type %in% c('entity', 'attribute')]

  comm <- get_communities(ang, ang$partitioning)
  V(ang)$membership[comm_idx] <- comm[comm_idx]

  # TODO: Not most efficient to always reapply both
  comm <- get_communities(ang, ang$partitioning2)
  V(ang)$membership2[comm_idx] <- comm[comm_idx]

  ang
}

get_communities <- function(ang, method) {
  
  if (is.na(method)) {
    comm <- NA
  } else {
    if (existsFunction(method)) {
    
      # Apply community detection algorithm
      res <- do.call(method, list(ang))
      comm <- res$membership
    } else {

      # Use pregiven communities    
      obsw <- browseData(ang, 'all')
      ocomm <- obsw[!is.na(obsw[ ,method]),c('object',method)]
      if (nrow(ocomm)) {
        
        # Presume that only entities have membership defined
        ents <- sub('>.*', '', V(ang)$name)
        comm <- ocomm[match(ents, ocomm$object),method]

        # Some attributes may have membership defined as well     
        ocomm <- ocomm[match(V(ang)$name, ocomm$object),method]

        # Use entity membership if attribute membership not defined
        comm <- apply(cbind(comm, ocomm), 1, function(x) {
          ifelse(sum(is.na(x)), x[1], x[2])
        })
      } else {
        stop('No memberships pregiven.')
      }
    }
    
    # Avoid crashing if vertex without membership present
    comm[is.na(comm)] <- 'ERR'  
  }
  comm
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Get current partitioning algorithm or source data
getPartitionings <- function(sq) {
  
  ang <- tail(sq, 1)[[1]]
  
  p <- c(ang$partitioning, ang$partitioning2)
  names(p) <- c('partitioning', 'partitioning2')
  
  p
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

#  vl <- c("OBSERVATION>checkpoint","VERTEX>shape","VERTEX>type","VERTEX>contrast")
#  vl <- c(vl,"EDGE","VERTEX>membership2","VERTEX>size2")
#  vl <- c(vl,"GRAPH>alternation","GRAPH>checkpoint","GRAPH>partitioning2","GRAPH>sizing2")
#  vl <- c(vl,"GRAPH>seed","GRAPH>layout","GRAPH>simplicity","GRAPH>output","GRAPH>theme","EDGE>type")
  vl <- getElements(meta)[V(ang)$size < 4,'name']
  thevoid(ang, centers=vl)
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

  if (ang$alternation) {
    
    for(idx in seq_along(E(ang))) {  
    
      objsrc <- sub('\\|.*', '', attr(E(ang)[idx], 'vnames'))
      objdest <- sub('.*\\|', '', attr(E(ang)[idx], 'vnames'))
    
      # Add new links and mark obsolete edges/vertices for deletion
      if (!grepl('>', attr(E(ang)[idx], 'vnames'))) {
        
        objlbl <- E(ang)[idx]$label
        objtype <- E(ang)[idx]$type
        ang <- add_link(ang, objsrc, objdest, elabel=objlbl, etype=objtype)
        
        E(ang)[idx]$label <- 'DELETE'
      }
    }
  
    # Delete obsolete vertices and edges
    ang <- delete.vertices(ang, which(V(ang)$label=='DELETE'))
    ang <- delete.edges(ang, which(E(ang)$label=='DELETE'))
  } else {
    
    el <- get.edgelist(ang)
    el <- el[grep('>', el[ ,1]), ]
    for(idx in 1:nrow(el)) {
      src <- sub('>.*', '', el[idx,1])
      label <- sub('.*>', '', el[idx,1])
      ang <- add_link(ang, src, el[idx,2], elabel=label)
    } 
  }
  
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
doSymmetry <- function(sq) {
  trf(sq, 'symmetry', cl=match.call())
}

symmetry <- function(ang, ...) {
 
#  vl <- c("VERTEX>shape","VERTEX>type","VERTEX>contrast")
  vl <- c("GRAPH>alternation",'GRAPH>partitioning2',"GRAPH>sizing2")
  vl <- c(vl,"GRAPH>output","GRAPH>theme")
  
  thevoid(ang, centers=vl)
}


################################################################################
# SPACE transformations
################################################################################

# TODO: Implement function
browseSizings <- function(sq) {}

# User friendly transformation function calls
applySizing <- function(sq, method, ...) {
  trf(sq, 'sizing', sizing=method, cl=match.call())
}  

# TODO: Implement function
applySizing2 <- function(sq, method, ...) {}

sizing <- function(ang, ...) {
  
  method <- ang$sizing
  
  if (existsFunction(method)) {
    ang <- sizing_fun(ang, method, ...)
  } else {
    ang <- sizing_pregiven(ang, method, ...)
  }
  
  # Avoid crashing because of NA size
  V(ang)$size[is.na(V(ang)$size)] <- 0
  
  ang
}

sizing_fun <- function(ang, method, ...) {

  sz <- do.call(method, args=list(ang))
  ang <- set_vertex_attr(ang, 'size', value=sz)
    # Calculate sizes
    #  V(ang)$size <- sample(10:20, length(V(ang)), replace=TRUE)
    
    # TODO: If algorithmic convert to center-only graph (function?) 
    
    #tmpang <- alternation(ang, remove=TRUE)
    #tmpang <- void(tmpang, getAttributes(ang))
    
 #   btw <- betweenness(ang) + 3
#    V(ang)$size <- btw
    
#    E(ang)$width <- sample(1:5, length(E(ang)), replace=TRUE)
#    E(ang)$arrow.size <- E(ang)$width^2

# Limit the range and maximum size
#  maxsize <- max(V(ang)$size)
#  if (maxsize > 35) {
#    for (idx in seq_along(V(ang))) {
#      if (V(ang)[idx]$size > 0) {
#        V(ang)[idx]$size <- round(3 + 30*V(ang)[idx]$size/maxsize)
#      }
#    }
#  }
  
  ang
}

# Apply pregiven sizes
sizing_pregiven <- function(ang, method, ...) {
  
  obs <- browseData(ang, 'all')
  sizing <- obs[!is.na(obs[ ,method]), ]

  for (idx in seq_along(sizing$object)) {
    cname <- sizing[idx,'object']
    csize <- as.numeric(sizing[idx,method])

    # Resize vertices
    if (is.na(sizing[idx,'objdest'])) {
      if (length(V(ang)$name==cname) == 0) print(cname)
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
applySeed <- function(sq, seed = 123) {
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

  c <- getElements(meta)$name

  # SEQUENCE  
  #v <- grep('OBSERVATION>', c, value=T)
  #v <- c(v, 'VERTEX>contrast', 'VERTEX>size2', 'VERTEX>type', 'VERTEX>shape', 'VERTEX>membership2', 'VERTEX>label')
  #v <- c(v, 'EDGE>contrast', 'EDGE>type', 'EDGE>label')
  #v <- c(v,"GRAPH>alternation","GRAPH>checkpoint","GRAPH>partitioning2","GRAPH>sizing2")
  #v <- c(v,"GRAPH>seed","GRAPH>layout","GRAPH>simplicity","GRAPH>output","GRAPH>theme")
 
  # GRAPH
  v <- grep('OBSERVATION>', c, value=T)
  v <- c(v, 'VERTEX>contrast', 'VERTEX>size2', 'VERTEX>type', 'VERTEX>shape', 'VERTEX>membership2')
  v <- c(v, 'EDGE>contrast', 'EDGE>type')
  
  thevoid(ang, centers=v)
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
applyHighlight <-  function(sq, elems, ...) {
  trf(sq, 'contrast', elems=elems, highlight=TRUE, cl=match.call())
}

removeHighlight <-  function(sq, elems, ...) {
  trf(sq, 'contrast', elems=elems, highlight=FALSE, cl=match.call())
}

contrast <- function(ang, ...) {
  
  elems <- list(...)$elems
  hlc <- V(ang)$name %in% elems
  
  hl <- list(...)$highlight
  ang <- set_vertex_attr(ang, 'contrast', index=hlc, value=hl)
  
  ang
}

################################################################################
# INTERLOCK transformations: Combine the elements
################################################################################

# TODO: Implement function to browse group contents

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User friendly grouping transformation function call
group <- function(sq, name = 'group1', members = NULL, by = NULL, ...) {
  trf(sq, 'group_trf', gname=name, members=members, by=by, cl=match.call(), ...)
}

group_trf <- function(ang, ...) {
  
  grp_name <- list(...)$gname
  grp_members <- list(...)$members  
  grp_attr <- list(...)$by

  # Store original entities
  V(ang)$group <- V(ang)$name
  
  # Group either by given members or attribute
  if (is.null(grp_attr)) {
    gidx <- ifelse(V(ang)$name %in% grp_members, 'isgroupmember', V(ang)$name)
    
    gidxb <- gidx=='isgroupmember'
    ang <- set_vertex_attr(ang, 'name', index=gidxb, value=grp_name)
    if (grepl('>', grp_members[1])) {
      ang <- set_vertex_attr(ang, 'type', index=gidxb, value='agroup')
      grp_label <- sub('.*>', '', grp_name)
    } else {
      ang <- set_vertex_attr(ang, 'type', index=gidxb, value='egroup')
      ang <- set_vertex_attr(ang, 'label', index=gidxb, value=grp_name)
      grp_label <- grp_name
    }
    ang <- set_vertex_attr(ang, 'label', index=gidxb, value=grp_label)
  } else {
    gidx <- get.vertex.attribute(ang, grp_attr)
    
    V(ang)$type <- 'egroup'
    V(ang)$name <- get.vertex.attribute(ang, grp_attr)
    V(ang)$label <- get.vertex.attribute(ang, grp_attr)
  }
  
  # Contract the graph by grouping index
  ang <- contract(ang, as.factor(gidx), vertex.attr.comb=toString)
  
  # Tidy up the atrributes
  for (cur_attr in list.vertex.attributes(ang)) {
    
    # Numeric attributes
    if (cur_attr %in% c('size')) {
      
      cur_attr_val <- sapply(V(ang)$size, function(x) {
        sum(as.numeric(unlist(strsplit(gsub(' ', '', x), ','))))
      })
      ang <- delete_vertex_attr(ang, 'size')

    # Boolean attributes
    } else if (cur_attr %in% c('contrast')) {
      
      cur_attr_val <- gsub(',.*$', '', get.vertex.attribute(ang, cur_attr))
      cur_attr_val <- ifelse(cur_attr_val=='TRUE', TRUE, FALSE)
      
    # Character attributes
    } else {
      cur_attr_val <- gsub(',.*$', '', get.vertex.attribute(ang, cur_attr))
    }
    
    ang <- set_vertex_attr(ang, cur_attr, value=cur_attr_val)
  }

  if (is.null(grp_attr)) {
    
    # Remove self-references
    el <- get.edgelist(ang)
    ang <- delete.edges(ang, E(ang)[el[ ,1]==grp_name & el[ ,1]==el[ ,2]])
    
    # Grouped attribute labels ambigous
    el <- get.edgelist(ang)
    E(ang)[el[ ,1]==grp_name]$label <- E(ang)[el[ ,2]==grp_name]$label <- NA
  } else {
    
    # TODO: maintain edge attributes like labels
    ang <- simplify(ang, remove.loops=T)
  }
  
  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# User friendly degrouping transformation function calls
degroup <- function(sq, group) {
  trf(sq, 'degroup_trf', group=group, members=NA, cl=match.call())
}

# TODO: Implement, but not so crucial: just avoid unnecessary grouping
degroup_trf <- function(ang, ...) {
  
  group <- list(...)$group

  ang
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Group symbols
# http://lists.gnu.org/archive/html/igraph-help/2013-03/msg00030.html

# TODO: Use function generator

fcircle <- function(coords, v=NULL, params) {
    vertex.color <- params("vertex", "color")
    if (length(vertex.color) != 1 && !is.null(v)) {
      vertex.color <- vertex.color[v]
    }
    vertex.size <- 1/200 * params("vertex", "size")
    if (length(vertex.size) != 1 && !is.null(v)) {
      vertex.size <- vertex.size[v]
    }
    vertex.frame.color <- params("vertex", "frame.color")
    if (length(vertex.frame.color) != 1 && !is.null(v)) {
      vertex.frame.color <- vertex.frame.color[v]
    }
    vertex.frame.width <- params("vertex", "frame.width")
    if (length(vertex.frame.width) != 1 && !is.null(v)) {
      vertex.frame.width <- vertex.frame.width[v]
    }
    
    mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
           vertex.size, vertex.frame.width,
           FUN=function(x, y, bg, fg, size, lwd) {
             symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                     circles=size, add=TRUE, inches=FALSE)
           })
}

add.vertex.shape('fcircle', clip=igraph.shape.noclip,
  plot=fcircle, parameters=list(vertex.frame.color=1, vertex.frame.width=1))

fsquare <- function(coords, v=NULL, params) {
  vertex.color <- params("vertex", "color")
  if (length(vertex.color) != 1 && !is.null(v)) {
    vertex.color <- vertex.color[v]
  }
 # Size should be somehow related to normal squre size (initially 1/200)
  vertex.size <- 1/100 * params("vertex", "size")
  if (length(vertex.size) != 1 && !is.null(v)) {
    vertex.size <- vertex.size[v]
  }
  vertex.frame.color <- params("vertex", "frame.color")
  if (length(vertex.frame.color) != 1 && !is.null(v)) {
    vertex.frame.color <- vertex.frame.color[v]
  }
  vertex.frame.width <- params("vertex", "frame.width")
  if (length(vertex.frame.width) != 1 && !is.null(v)) {
    vertex.frame.width <- vertex.frame.width[v]
  }
  
  mapply(coords[,1], coords[,2], vertex.color, vertex.frame.color,
         vertex.size, vertex.frame.width,
         FUN=function(x, y, bg, fg, size, lwd) {
           symbols(x=x, y=y, bg=bg, fg=fg, lwd=lwd,
                   squares=size, add=TRUE, inches=FALSE)
         })
}

add.vertex.shape('fsquare', clip=igraph.shape.noclip,
  plot=fsquare, parameters=list(vertex.frame.color=1, vertex.frame.width=1))

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
# TODO: Paritioning colors handling

get_plopt <- function(theme=NA) {

  if (is.na(theme)) {
    c('expressive', 'minimalist')
  } else {
    
    # Vertex color
    vc <- switch(theme, minimalist='grey', 'orange1')
    plopt <- list(vertex_color=vc)
    
    # Highlight
    vchl <- switch(theme, minimalist='red2', 'red')
    
    plopt <- c(plopt, vertex_contrast_color=vchl, vertex_contrast_label_cex=1.2)

    # TODO: Color for minimalist group frame
    
    # Vertex frame
    vfc <- switch(theme, minimalist='grey60', 'lightgrey')
    vfw <- 5
    plopt <- c(plopt, vertex_frame_color=vfc, vertex_frame_width=vfw)

    # TODO: Attribute color?
    
    # Default element sizes
    plopt <- c(plopt, vertex_scaling=25, entity_size=15, egroup_size=20)
    plopt <- c(plopt, attribute_size=5, agroup_size=10, edge_size=1)
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

# TODO: Instead TRUE/FALSE number of displayed labels?

# User-friendly function call for setting simplified presentation mode
applySimplicity <- function(sq, n=10, ...) {
  trf(sq, 'simplicity', simplicity=n, cl=match.call(), ...)  
}

# User-friendly function call for setting full presentation mode
removeSimplicity <- function(sq, ...) {
  trf(sq, 'simplicity', simplicity=NA, cl=match.call(), ...)
}

# Transformation function
simplicity <- function(ang, ...) {

  # TODO: Restore labels, paritioning, etc in removing simplicity  
  ang
}

# Get current simplicity settings
getSimplicity <- function(sq, ...) {
  tail(sq, 1)[[1]]$simplicity
}

################################################################################
# VOID transformations
################################################################################

# TODO: Void relations/edges

# User friendly transformation function call
void <- function(sq, centers, ckpt = NULL, ...) {
  trf(sq, 'thevoid', centers=centers, checkpoint=ckpt, cl=match.call(), ...)
}

# Remove centers
thevoid <- function(ang, ...) {
  
  # Specific arguments
  aargs <- list(...)
  centers <- aargs$centers

  ang <- delete.vertices(ang, centers)
  
  # Delete attributes
  obs <- browseData(ang, ckpt='all')
  ang <- delete.vertices(ang, 
    is.element(obs[match(V(ang)$name, obs$object), 'objsrc'], centers))
  
  ang
}

################################################################################
# NOT-SEPARATENESS transformations
################################################################################

# Conclude the sequence
signoff <- function(sq, thumb_seq=NULL, thumb_narr=NULL, ...) {

  if (!is.null(thumb_seq)) {

    # TODO: Use checkpoints if thumb_narr not present
    n <- length(thumb_seq)
    nc <- ifelse(n%%3==0 & n > 4, 3, 2)
    plot(sq, main=thumb_narr, steps=thumb_seq, ncol=nc)
  }

  # TODO: Implement packaging of the data and the code
}