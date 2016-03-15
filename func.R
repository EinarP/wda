library(igraph)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Analysis trace

#construction
asq <- function(x) {
  
  m <- list(name=x, outp='plot')
  
  t <- data.frame(id=0, parent=NA, cmd='init', timeline=NA,
    stringsAsFactors=FALSE)
  
  s <- list(graph.empty())
  
  structure(list(meta=m, trace=t, struct=s), class='asq')
}

#print method
print.asq <- function(x) {
  
  cat(x$meta$name, '\n\n')
  print(tail(x$trace, 3))
  
  if(vcount(x$struct[[1]]) > 0) {
    if(x$meta$outp=='plot')
      plot(x$struct[[1]], edge.arrow.size=0.3, vertex.frame.color=NA,
           vertex.label.family='sans')   
    else
      tkplot(x$struct[[1]])
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#CENTER transformations

addCenter <- function(analseq, center) {
  
  ang <- analseq$struct[[1]]
  
  if (!is.element(center, vertex.attributes(ang)$name)) {
    ang <- ang + vertices(center, shape='rectangle')
  }
  
  linkList <- obs[grep(paste0(center, '>'), obs$object), 'value']
  for (i in 1:length(linkList)) {
    if (!is.element(linkList[i], vertex.attributes(ang)$name)) {
      ang <- ang + vertices(linkList[i], shape='rectangle')
    }
    ang <- ang + edge(center, linkList[i], edge.label='loll')
  }
  
  analseq$trace <- rbind(analseq$trace, c(1, 0, 'addCenter', NA))
  analseq$struct[[1]] <- ang
  
  analseq
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#VOID transformations

voidCenter <- function(analseq, center) {
  
  ang <- analseq$struct[[1]]
  
  ang <- delete.vertices(ang, center)
  
  analseq$trace <- rbind(analseq$trace, c(1, 0, 'voidCenter', NA))
  analseq$struct[[1]] <- ang
  
  analseq
}
