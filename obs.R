################################################################################
# Observations handling
################################################################################

obs <- function(x, ...) UseMethod('obs')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  
<<<<<<< HEAD
  completeObsFields(data.frame(obs_mtx, stringsAsFactors = FALSE))
=======
  addObsHelperFields(data.frame(obs_mtx, stringsAsFactors = FALSE))
>>>>>>> 841a4b28d84447672fbba74239d20953e9c35c23
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

obs.data.frame <- function(opvci) {
  
  completeObsFields(opvci)
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Perhaps ouput E, E2, A, MA, V, ID, CKPT in obs already?
<<<<<<< HEAD
completeObsFields <- function(opvci) {

  # Add missing or tidy up existing checkpoints
  if (!is.element('checkpoint', colnames(opvci))) {
    opvci$checkpoint <- 'all'
  } else {
    opvci[is.na(opvci$checkpoint),'checkpoint'] <- 'all'
  }

  # Add id for instance modelling
  if (!is.element('id', colnames(opvci))) {
    opvci$id <- NA
  }
    
  # Link between entities if h_ent2 present
  s <- strsplit(opvci$object, '\\|')
  opvci$h_ent <- unlist(lapply(s, function(x) x[1]))
  opvci$h_ent2 <- lapply(s, function(x) x[2])

  s <- strsplit(opvci$h_ent, '>')
  opvci$h_ent <- lapply(s, function(x) x[1])
  opvci$h_attr <- ifelse(is.na(opvci$h_ent2), opvci$property, lapply(s, function(x) x[2]))
  opvci$h_meta <- ifelse(is.na(opvci$h_ent2), NA, opvci$property) 

  opvci
=======
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
>>>>>>> 841a4b28d84447672fbba74239d20953e9c35c23
}