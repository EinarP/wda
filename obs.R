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
      if (grepl('|', o)) {
        s <- strsplit(op, '\\|')[[1]]
        o <- paste0(s[1], '>id|', s[2]) ; p <- 'type'
        if (is.na(v)) v <- 'weak'
      } else {
        p <- 'id'
      }
    }
   
    # Output vector
    c(object = o, property = p, value = v)
  }))
  
  splitObs(data.frame(obs_mtx, stringsAsFactors = FALSE))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# TODO: Perhaps ouput E, E2, A, MA, V, ID, CKPT in obs already?
splitObs <- function(obs) {

  s <- strsplit(obs$object, '\\|')
  obs$h_ent_src <- unlist(lapply(s, function(x) x[1]))
  obs$h_ent_dest <- lapply(s, function(x) x[2])

  s <- strsplit(obs$h_ent_src, '>')
  obs$h_ent_src <- lapply(s, function(x) x[1])
  obs$h_attr <- ifelse(is.na(obs$h_ent_dest), obs$property, lapply(s, function(x) x[2]))
  obs$h_meta_attr <- ifelse(is.na(obs$h_ent_dest), NA, obs$property) 

  obs$h_value <- obs$value
  
  obs
}