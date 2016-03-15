library(igraph)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Analysis trace

#construction
asq <- function(x) {
    
    m <- list(name=x)
    
    t <- data.frame(id=1, parent=NA, cmd='init', timeline=NA,
        stringsAsFactors=FALSE)
    
    s <- list(graph.empty())
    
    structure(list(meta=m, trace=t, struct=s), class='asq')
}

#print method
print.asq <- function(x) {
    
    cat(x$meta$name, '\n\n')
    print(tail(x$trace, 3))
    
    if (vcount(x$struct[[1]]) > 0) tkplot(x$struct[[1]])
}

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
