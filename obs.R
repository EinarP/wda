################################################################################
# Observations handling
################################################################################

obs <- function(x, ...) UseMethod('obs')

obs.character <- function(obs_def) {

  # TODO: Missing = error handling
  s <- strsplit(obs_def, '=')[[1]]
  if (length(s) < 2) {}

  property <- ifelse(grepl('|', s[1]), 'type', 'value')
  
  data.frame(list(object = s[1], property = property, value = s[2]))
}
