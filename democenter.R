
# Common functions
source('func.R')

# Sample data loading
# trdemobs <- read.csv('../data/trdemo.csv', stringsAsFactors=FALSE)
library(readxl)
trdemobs <- read_excel('../data/trdemo.xlsx')

# Structure of sample data
str(trdemobs)

# Summary of properties
table(trdemobs$property)

# Initialize the analysis sequence
trdemo <- newsq('Demonstration sequence', 'trdemobs')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source data
head(browseData(trdemo), 3)
tail(browseData(trdemo), 3)

# Create the initial structure
trdemo <- grow(trdemo, 'C01', depth=2)
trdemo

# Add attributes and values to certain entities
trdemo <- grow(trdemo, c('C01','C02'), depth=0, attrs=TRUE, vals=TRUE)
trdemo

# Existing centers and their connections
getCenters(trdemo)
getRelations(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply community detection algorithm
browsePartitionings(trdemo)
trdemo <- applyPartitioning(trdemo, partitioning='cluster_walktrap')
trdemo

# Visualize pregiven communities 
browseEntities(trdemo)[ ,c('object', 'member')]
trdemo <- applyPartitioning(trdemo, partitioning='member')
trdemo

# Current clustering
getPartitioning(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Global scaling settings
getScaling(trdemo)

# Sample of sizing information
browseData(trdemo)[sample(1:nrow(browseData(trdemo)), 6), c('object','size')]

# Scaling by pregiven sizes
# trdemo <- applyScale(trdemo, values=3)
# trdemo

# Scaling by pregiven sizes

# Global scaling settings after transformations
# getScaling(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Alternation
getAlternation(trdemo)
trdemo <- applyAlternation(trdemo)
getAlternation(trdemo)
trdemo

trdemo <- removeAlternation(trdemo)
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Symmetries
# trdemo <- addSymmetries(trdemo)
# trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Space
# trdemo <- applySizing(trdemo)
# Power centrality calculation (TODO: integrate with sp)
# power_centrality(trdemo$struct[[length(trdemo$struct)]])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Roughness
browseSeeds(trdemo)
trdemo <- applySeed(trdemo, seed=543)
trdemo

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

trdemo <- applySimplicity(trdemo)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Void: Remove clutter

trdemo <- void(trdemo, c('C02','C07'))
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Not-separateness: sign, package and freeze the analysis
signoff(trdemo)
