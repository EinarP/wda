
library(igraph)

################################################################################
# Analysis object and related methods
################################################################################

# construction
asq <- function(x) {
  
  s <- data.frame(id=1, parent=NA, transformation=paste0("asq('", x, "')"),
    checkpoint=NA, stringsAsFactors=FALSE)

#TODO: Seems better to store vertices, edges, communities as data frame
  g <- list(graph.empty())

#structure(list(meta=m, trace=t, struct=s), class='asq')
  structure(list(name=x, seed=1, output='plot', seq=s, struct=g), class='asq')
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# print method
print.asq <- function(x, seed=NA) {

#Set random number generator
  if (is.na(seed)) {
    seed <- x$seed
  }
  set.seed(seed)

#Excerpt of trace table
  cat(x$name, '\n\n')
  print(tail(x$seq[ ,2:ncol(x$seq)], 3))

#Plot on requested device
  if(vcount(x$struct[[1]]) > 0) {
    if(x$output=='plot') {
      plot(x$struct[[1]], xlab=tail(x$seq, 1)$transformation,
        edge.arrow.size=0.3, vertex.frame.color=NA, vertex.label.family='sans')   
    } else {
      tkplot(x$struct[[1]])
    }
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# trace sequence
addAnalysisStep <- function(asq, cmd, ang, tline=NA) {
  
  newtr <- tail(asq$trace, 1)
  
  newtr$parent <- newtr$id
  newtr$id <- newtr$id + 1
  newtr$cmd <- deparse(cmd)
  newtr$tline <- tline
  
  asq$trace <- rbind(asq$trace, newtr)

#TODO: Likely need to save every graph and also clusters separately 
  asq$struct[[1]] <- ang
  
  asq
}

################################################################################
# CENTER transformations
################################################################################

# Add centers
addCenter <- function(aseq, center, level=0, tline=NA) {
  
  ang <- aseq$struct[[1]]
  
  for (i in 1:length(center)) {
    
    if (!is.element(center[i], vertex.attributes(ang)$name)) {
      ang <- ang + vertices(center[i], shape = 'rectangle')
    }
    
    if (level > 0) {
      lcenter <- obs[grep(paste0(center[i], '>'), obs$object), 'value']
      
      for (j in 1:length(lcenter)) {
        
        if (!is.element(lcenter[j], vertex.attributes(ang)$name)) {
          aseq$struct[[1]] <- ang
          ang <- addCenter(aseq, lcenter[j], level-1)$struct[[1]]
        }
        
        ang <- ang + edge(center[i], lcenter[j])
      }
    }
  }
  
  addAnalysisStep(aseq, match.call(), ang, tline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Retrieve all center references
getCenter <- function(aseq) {

  ang <- aseq$struct[[1]]
  
  vertex.attributes(ang)$name  
}

################################################################################
# BOUNDARY transformations
################################################################################

# Add boundary
addBoundary <- function(aseq, clustering, tline=NA) {
  
  trp <- aseq$struct[[1]]
  tc <- make_clusters(trp, clustering)
  plot(tc, trp)
  
  addAnalysisStep(aseq, match.call(), ang, tline)
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
  
  ang <- aseq$struct[[1]]
  
  ang <- delete.vertices(ang, center)
  
  addAnalysisStep(aseq, match.call(), ang, tline)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

