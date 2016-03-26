
#Common functions
source('func.R')
#demo data
obs <- data.frame(dtstamp='20160321', object='C1', property='field', value='id',
  stringsAsFactors=FALSE)

obs <- rbind(obs, c('20160321', 'C2', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C3', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C4', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C5', 'field', 'id'))

obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C2'))
obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C3'))
obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C4'))
obs <- rbind(obs, c('20160321', 'C5>id', 'linkdef', 'C1'))

par(mfrow = c(1, 3), cex.lab=1.3)
trdemo <- asq('Transformations demonstration')

#Center tranformation
trdemo <- addCenter(trdemo, c('C1','C5'), level=1)
trdemo

getCenter(trdemo)

set.seed(1)
#Boundary transformation [addBoundary(trdemo, c(2, 2, 2, 2, 1))]
clustering <- c(2, 2, 2, 2, 1)
bclust <- make_clusters(trdemo$struct[[1]], clustering)
plot(bclust, trdemo$struct[[1]], xlab='addBoundary(aseq = trdemo, clustering = c(2, 2, 2, 2, 1))',
     edge.arrow.size=0.3, vertex.frame.color=NA, vertex.label.family='sans') 

set.seed(1)
#voidCenter()
trdemo <- voidCenter(trdemo, 'C2')
clustering <- c(2, 2, 2, 1)
bclust <- make_clusters(trdemo$struct[[1]], clustering)
plot(bclust, trdemo$struct[[1]], xlab="voidCenter(aseq = trdemo, center = 'C2')",
     edge.arrow.size=0.3, vertex.frame.color=NA, vertex.label.family='sans') 


actors <- data.frame(name=c("Alice", "Bob", "Cecil", "David", "Esmeralda"),
  age=c(48,33,45,34,21), gender=c("F","M","F","M","F"))

relations <- data.frame(from=c("Bob", "Cecil", "Cecil", "David",                           "David", "Esmeralda"),
  to=c("Alice", "Bob", "Alice", "Alice", "Bob", "Alice"),
  same.dept=c(FALSE,FALSE,TRUE,FALSE,FALSE,TRUE),
  friendship=c(4,5,5,2,1,1), advice=c(4,5,5,4,2,3))

g <- graph_from_data_frame(relations, directed=TRUE, vertices=actors)

print(g, e=TRUE, v=TRUE)
