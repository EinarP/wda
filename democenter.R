
#don't show 

par(mfrow = c(1,1), cex.lab=0.8)

# demo data generation
# obs <- data.frame(dtstamp='20160321', object='C1',
#  property='field', value='id', stringsAsFactors=FALSE)
# obs <- rbind(obs, c('20160321', 'C2', 'field', 'id'))
# obs <- rbind(obs, c('20160321', 'C3', 'field', 'id'))
# obs <- rbind(obs, c('20160321', 'C4', 'field', 'id'))
# obs <- rbind(obs, c('20160321', 'C5', 'field', 'id'))
# obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C2'))
# obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C3'))
# obs <- rbind(obs, c('20160321', 'C1>id', 'linkdef', 'C4'))
# obs <- rbind(obs, c('20160321', 'C5>id', 'linkdef', 'C1'))

obs <- data.frame(object=character(), property=character(),
    value=character(), dtstamp=character(), stringsAsFactors=FALSE)

obs[1, ] <- c('C1>id','linkdef','C2','')
obs[2, ] <- c('C1>id','linkdef','C3','')
obs[3, ] <- c('C4>id','linkdef','C1','')
obs[4, ] <- c('C2>id','linkdef','C5','')
obs[5, ] <- c('C6>id','linkdef','C2','')
obs[6, ] <- c('C3>id','linkdef','C7','')
obs[7, ] <- c('C8>id','linkdef','C3','')
obs[8, ] <- c('C9>id','linkdef','C3','')
obs[9, ] <- c('C10>id','linkdef','C7','')
obs[10, ] <- c('C7>id','linkdef','C2','')

# Start display

# Load common functions
source('func.R')

# Initial demo data
obs

# Start the analysis sequence
trdemo <- asq('Transformations demonstration')

# Create centers
trdemo <- addCenter(trdemo, 'C1', depth=1)
trdemo

#List current centers
getCenter(trdemo)
