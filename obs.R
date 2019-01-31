################################################################################
# OBSERVATIONS handling
################################################################################

obs <- function(x, ...) UseMethod('obs')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs.character <- function(obs) {
  
  mtx <- do.call(rbind, lapply(obs, function(x) {
    
    # Separate value and split to entities
    vs <- strsplit(x, '=')[[1]]
    v <- vs[2]
    
    es <- strsplit(vs[1], '\\|')[[1]]
    e1s <- strsplit(es[1], '>')[[1]]
    o <- e1s[1]
    
    # Process things if one entity
    if (is.na(es[2])) {
      
      p <- ifelse(e1s[2] == '', NA, e1s[2])
      m <- ifelse(!is.na(v) & is.na(p) & is.na(e1s[3]), 'id', e1s[3])
      
      if (!is.na(p) & !is.na(m)) { o <- paste0(o, '>', p) ; p <- m }
      if (is.na(p) & !is.na(m)) o <- paste0(o, '>', m)
      
    # Process links
    } else {
      
      e2s <- strsplit(es[2], '>')[[1]]
      p <- ifelse(is.na(e1s[2]), 'id', e1s[2])
      
      o <- paste0(o, '>', p, '|', e2s[1])
      p <- ifelse(is.na(e2s[2]), 'type', e2s[2])
      
      if (is.na(v)) v <- 'weak'
    }
    
    # Output vector
    c(object = o, property = p, value = v)
  }))
  
  addObsHelper(data.frame(mtx, stringsAsFactors = FALSE))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs.data.frame <- function(opvci) {
  
  addObsHelper(opvci)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

addObsHelper <- function(opvc) {
  
  # Add missing or tidy up existing checkpoints
  if (!('checkpoint' %in% colnames(opvc))) {
    opvc$checkpoint <- 'all'
  } else {
    opvc[is.na(opvc$checkpoint),'checkpoint'] <- 'all'
  }
  
  # Helper entity columns
  es <- strsplit(opvc$object, '\\|')
  opvc$h_ent2 <- lapply(es, function(x) x[2])
  
  opvc$h_ent <- unlist(lapply(es, function(x) x[1]))
  e1s <- strsplit(opvc$h_ent, '>')
  opvc$h_ent <- lapply(e1s, function(x) x[1])
  
  # Helper attibute and meta columns
  attr <- lapply(e1s, function(x) x[2])
  opvc$h_attr <- ifelse(!is.na(opvc$h_ent2), attr,
    ifelse(is.na(attr), opvc$property, ifelse(is.na(opvc$property), NA, attr)))
  opvc$h_meta <- ifelse(!is.na(opvc$h_ent2), opvc$property,
    ifelse(is.na(attr), NA, ifelse(is.na(opvc$property), attr, opvc$property)))
  
  opvc
}