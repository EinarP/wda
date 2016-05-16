
# Common functions
source('func.R')

# Input data
library(openxlsx)
anobs <- read.xlsx('../data/anobj.xlsx')

# Initialize the analysis sequence
anobj <- trsq('Analysis object', 'anobs')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CONTEXT
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create centers
anobj <- addCenters(anobj, 'SEQUENCE', depth=2)

# Apply appropriate seed and partition
anobj <- applySeed(sq=anobj, seed=163)
anobj <- applyBoundary(anobj, 'member')
anobj

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OBJECT DETAILS
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

anobj <- voidCenter(anobj, c('REPORT','STORAGE'))
anobj <- addCenters(anobj, c('VERTEX','EDGE'), depth=1, addelem=TRUE)

anobj <- applySizing(anobj)
anobj

anobj <- addAlternation(anobj)
anobj
