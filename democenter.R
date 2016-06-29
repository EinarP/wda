
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

# Growing the structure with no scaling
getScaling(trdemo)
grow(trdemo, "C10", attrs=TRUE)

# Growing the network with scaling by pregiven sizes
attrs <- grepl('C10>', browseData(trdemo)$object)
browseData(trdemo)[attrs, c('object','size','linktype','objattr')]

trdemo <- applyScaling(trdemo, scaling=3)
grow(trdemo, "C10", attrs=TRUE)

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
applySymmetry(trdemo)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Space
# trdemo <- applySizing(trdemo)
trdemo

# TODO: integrate with transformation
# power_centrality(trdemo$struct[[length(trdemo$struct)]])
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Roughness
browseSeeds(trdemo)

trdemo <- applySeed(trdemo, seed=543)
trdemo

getSeed(trdemo)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Gradient
trdemo <- applyGradient(trdemo, 'C01')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Highlight C02
trdemo <- highlight(trdemo, 'C02')
trdemo

trdemo <- dehighlight(trdemo, 'C02')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Group items
trdemo <- group(trdemo, 'C03&07', c('C03','C07'))
trdemo

# Disband the group
trdemo <- degroup(trdemo, 'C03&07')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Echo
trdemo <- removePartitioning(trdemo)

browseThemes(trdemo)
trdemo <- applyTheme(trdemo, 'minimalist')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Shape:
browseLayouts(trdemo)
trdemo <- applyLayout(trdemo, 'layout_with_kk')
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Simplicity: Simplify the display of the structure
trdemo <- applySimplicity(trdemo)
trdemo
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Void: Remove clutter

trdemo <- void(trdemo, c('C02','C07'))
trdemo

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Not-separateness: sign, package and freeze the analysis
signoff(trdemo)
