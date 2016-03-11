library(igraph)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Analysis trace

ant <- list()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CENTER transformations

addCenter <- function(ang, center) {
  
  if (!is.element(center, vertex.attributes(ang)$name)) {
    ang <- ang + vertices(center)
  }
  
  linkList <- obs[grep(paste0(center, '>'), obs$object), 'value']
  for (i in 1:length(linkList)) {
    if (!is.element(linkList[i], vertex.attributes(ang)$name)) {
      ang <- ang + vertices(linkList[i])
    }
    ang <- ang + edge(center, linkList[i])
  }
  
  ang
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VOID transformations

voidCenter <- function(ang, center) {
  delete.vertices(ang, center)
}
