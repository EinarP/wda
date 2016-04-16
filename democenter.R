
# Common functions
source('func.R')

# Initialize the analysis sequence
trdemo <- trsq('Demonstration sequence', 'trdemobs')
trdemo

# Sample data
trdemobs <- read.csv('../data/trdemo.csv', stringsAsFactors=FALSE)
str(trdemobs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Subset of data for creating centers
head(trdemobs[trdemobs$property=='link_def', ])

# Create centers
trdemo <- addCenter(trdemo, 'C1', depth=2)
trdemo

# List current centers
getCenter(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Subset of data for pregiven communities
tail(trdemobs[trdemobs$property=='community', ])

# Visualize pregiven communities 
trdemo <- trf(trdemo, boundary, community='community')
trdemo

# Apply community detection algorithm
trdemo <- trf(trdemo, boundary, community='cluster_walktrap')
trdemo <- addBoundary(trdemo, community='cluster_walktrap')
trdemo

# Current clustering
getBoundary(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Subset of data with attribute information
head(trdemobs[trdemobs$property=='attribute', ])

# Add all attributes
attrCenters <- unique(trdemobs[trdemobs$property=='attribute', 'object'])
trdemo <- drillDown(trdemo, attrCenters)
trdemo

# Subset of data with value information
trdemobs[trdemobs$property=='value', ]

# Add all values
trdemo <- drillDown(trdemo, c('C1','C2'), values=TRUE)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Alternation
trdemo <- addAlternation(trdemo)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Symmetries
trdemo <- addSymmetries(trdemo)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Space

# Power centrality calculation (TODO: integrate with sp)
power_centrality(trdemo$graph[[length(trdemo$graph)]])

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

trdemo <- voidCenter(trdemo, 'C4')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Not-separateness: sign, package and freeze the analysis?
