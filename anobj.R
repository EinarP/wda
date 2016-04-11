
# Common functions
source('func.R')

# Initialize the analysis sequence
anobj <- asq('Analysis object', 'anobs')

# Input data
library(xlsx)
anobs <- read.xlsx('../data/anobj.xlsx', 1, stringsAsFactors=FALSE)

# Create centers
anobj <- addCenter(anobj, 'ANALYSIS', depth=2)

# Partition
anobj <- addBoundary(anobj, 'community')
anobj
