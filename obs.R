################################################################################
# Observations handling
################################################################################

obs <- function(x, ...) UseMethod('obs')

obs.character <- function(obs) {
# obs = {E1>a|E2, E1|E2, E1|E2=v, E>a, E>a=v}; TODO: E=id>a=v
  
  obs_mtx <- do.call(rbind, lapply(obs, function(x) {
    
    # Extract value
    s <- strsplit(x, '=')[[1]]
    op <- s[1] ; v = s[2]
    
    # Extract object and property
    s <- strsplit(op, '>')[[1]]
    o <- s[1] ; p = s[2]
    
    # Default missing object, property, or value
    if (is.na(p)) {
      if (grepl('\\|', o)) {
        s <- strsplit(op, '\\|')[[1]]
        o <- paste0(s[1], '>id|', s[2]) ; p <- 'type'
        if (is.na(v)) v <- 'weak'
      } else {
 #       p <- 'id'
      }
    }
   
    # Output vector
    c(object = o, property = p, value = v)
  }))
  
  addObsHelperFields(data.frame(obs_mtx, stringsAsFactors = FALSE))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Perhaps ouput E, E2, A, MA, V, ID, CKPT in obs already?
addObsHelperFields <- function(opvc) {

  # Add missing or tidy up existing checkpoints
  if (!('checkpoint' %in% colnames(opvc))) {
    opvc$checkpoint <- 'all'
  } else {
    opvc[is.na(opvc$checkpoint),'checkpoint'] <- 'all'
  }
  
  # Link between entities if h_ent2 present
  s <- strsplit(opvc$object, '\\|')
  opvc$h_ent <- unlist(lapply(s, function(x) x[1]))
  opvc$h_ent2 <- lapply(s, function(x) x[2])

  s <- strsplit(opvc$h_ent, '>')
  opvc$h_ent <- lapply(s, function(x) x[1])
  opvc$h_attr <- ifelse(is.na(opvc$h_ent2), opvc$property, lapply(s, function(x) x[2]))
  opvc$h_meta <- ifelse(is.na(opvc$h_ent2), NA, opvc$property) 

  opvc$h_value <- opvc$value
  
  opvc
}