
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# congraphion
asq <- function(x) {
  
  s <- data.frame(id=1, parent=NA, transformation=paste0("asq('", x, "')"),
    checkpoint=NA, stringsAsFactors=FALSE)
  
  g <- list(graph.empty())
  c <- NA

  structure(list(name=x, seed=1, output='plot', seq=s, graph=g, community=c),
    class='asq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# print method
print.asq <- function(x, seed=NA) {

#Set random number generator
  set.seed(ifelse(is.na(seed), x$seed, seed))

#Excerpt of trace table
  cat(x$name, '\n\n')
  print(tail(x$seq[ ,3:ncol(x$seq)], 3))

#Plot on requested device (perhaps qgraph instead of igraph)
  curg <- tail(x$graph, 1)[[1]]
  if(vcount(curg) > 0) {
    if(x$output=='plot') {
      plot(curg, xlab=tail(x$seq, 1)$transformation,
        edge.arrow.size=0.3, vertex.frame.color=NA, vertex.label.family='sans',
        edge.label.cex=0.8)   
    } else {
      tkplot(curg)
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# trace sequence
addAnalysisStep <- function(aseq, cmd, chkp=NA, ang=NA, anc=NA) {
  
  newtr <- tail(aseq$seq, 1)
  
  newtr$parent <- newtr$id
  newtr$id <- newtr$id + 1
  newtr$transformation <- deparse(cmd)
  newtr$checkpoint <- chkp
  
  aseq$seq <- rbind(aseq$seq, newtr)

  aseq$graph[[length(aseq$graph)+1]] <- ang
  
  aseq
}

################################################################################
# CENTER transformations
################################################################################

# Add centers
addCenter <- function(aseq, center, depth=1, chkp=NA) {
  
  ang <<- aseq$graph[[length(aseq$graph)]]
  
  sapply(center, function(x) addNeighbor(x, depth))

  addAnalysisStep(aseq, match.call(), ang, chkp)
}

# Add neighbors when adding centers
addNeighbor <- function(center, depth) {

  # add head center if not present
  if (!is.element(center, vertex.attributes(ang)$name)) {
    ang <<- ang + vertices(center, shape='rectangle')
  }

  if (depth > 0) {  
  
    # top-down linking of tails
    tailc <- obs[grep(paste0(center, '>'), obs$object), ]
    linkspec <- paste(tailc$object, tailc$value, sep='>')
    
    # bottom-up linking of tails
    tailc <- obs[obs$value==center,c('object','value')]
    linkspec <- c(linkspec, paste(tailc$object, tailc$value, sep='>'))

    # add tail center and link head to tail
    sapply(linkspec, function(curlink) {
      
      newhead <- unlist(strsplit(curlink, '>', fixed=TRUE))[1]
      newtail <- unlist(strsplit(curlink, '>', fixed=TRUE))[3]
      addNeighbor(ifelse(center==newhead, newtail, newhead), depth-1)
      
      ange <- get.edgelist(ang, names=T)
      anglink <- paste(ange[,1], E(ang)$label, ange[,2], sep='>')
      if (!is.element(curlink, anglink)) {
        newlink <- unlist(strsplit(curlink, '>', fixed=TRUE))[2]
        ang <<- ang + edge(newhead, newtail, label=newlink)
      }
    })
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve all center references
getCenter <- function(aseq) {

  ang <- aseq$graph[[length(aseq$graph)]]
  
  vertex.attributes(ang)$name  
}

################################################################################
# BOUNDARY transformations
################################################################################

# Add boundary
addBoundary <- function(aseq, method=NA, chkp=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  if (is.na(method)) {
    anc <- make_clusters(ang, clustering)
  } else {
    
  }
  
  tc <- 
  plot(tc, trp)
  
  addAnalysisStep(aseq, match.call(), chkp, ang=ang, anc=anc)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Describe current boundary
getBoundary <- function(x) {
  
}
################################################################################
#VOID transformations
################################################################################

# Remove centers
voidCenter <- function(aseq, center, tline=NA) {
  
  ang <- aseq$graph[[length(aseq$graph)]]
  
  ang <- delete.vertices(ang, center)
  
  addAnalysisStep(aseq, match.call(), ang, tline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

