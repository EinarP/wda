
# Common functions
source('func.R')

# Sample data loading
# trdemobs <- read.csv('../data/trdemo.csv', stringsAsFactors=FALSE)
library(openxlsx)
trdemobs <- read.xlsx('../data/trdemo.xlsx')

# Structure of sample data
str(trdemobs)

# Summary of properties
table(trdemobs$property)

# Initialize the analysis sequence
trdemo <- trsq('Demonstration sequence', 'trdemobs')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Center candidates
browseEntities(trdemo)

# Create centers
trdemo <- addCenters(trdemo, 'C01', depth=2)
trdemo

# List current centers
getCenters(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Visualize pregiven communities 
tail(trdemobs[trdemobs$property=='member', ])
trdemo <- applyBoundary(trdemo, partitioning='member')
trdemo

# Apply community detection algorithm
browseBoundaries(trdemo)
trdemo <- applyBoundary(trdemo, partitioning='cluster_walktrap')
trdemo

# Current clustering
getBoundary(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Possible elements
browseAttributes(trdemo)
browseRelationships(trdemo)

# Add certain elements
trdemo <- addCenters(trdemo, c('C01','C02'), depth=0, addelem=TRUE)
trdemo

# Possible values
browseCenters(trdemo)$values

# Add certain values
trdemo <- addCenters(trdemo, 'C01', depth=0, addelem=TRUE, addval=TRUE)
trdemo

# Current centers
getCenters(trdemo)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Alternation
getAlternation(trdemo)
trdemo <- addAlternation(trdemo)
getAlternation(trdemo)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Symmetries
trdemo <- addSymmetries(trdemo)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Space
applySizing(trdemo)
# Power centrality calculation (TODO: integrate with sp)
# power_centrality(trdemo$struct[[length(trdemo$struct)]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Roughness

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gradient

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Contrast

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Interlock

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Echo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Roughness

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Shape: 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simplicity: Simplify the analysis

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Void: Remove clutter

trdemo <- voidCenter(trdemo, c('C02','C07'))
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Not-separateness: sign, package and freeze the analysis?
