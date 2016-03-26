
par(mfrow = c(1,1), cex.lab=0.8)

#Common functions
source('func.R')

#demo data
obs <- data.frame(dtstamp='20160321', object='C1',
  property='field', value='id', stringsAsFactors=FALSE)

obs <- rbind(obs, c('20160321', 'C2', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C3', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C4', 'field', 'id'))
obs <- rbind(obs, c('20160321', 'C5', 'field', 'id'))

obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C2'))
obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C3'))
obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C4'))
obs <- rbind(obs, c('20160321', 'C5>id', 'linkdef', 'C1'))

obs

#Start the analysis sequence
trdemo <- asq('Transformations demonstration')

#Add centers
trdemo <- addCenter(trdemo, c('C1','C5'), level=1)
trdemo

#List current centers
getCenter(trdemo)