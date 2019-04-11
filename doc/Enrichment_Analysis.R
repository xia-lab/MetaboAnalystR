## ------------------------------------------------------------------------
library(MetaboAnalystR)

## When input is a list

# Create vector consisting of compounds for enrichment analysis 
tmp.vec <- c("Acetoacetic acid", "Beta-Alanine", "Creatine", "Dimethylglycine", "Fumaric acid", "Glycine", "Homocysteine", "L-Cysteine", "L-Isolucine", "L-Phenylalanine", "L-Serine", "L-Threonine", "L-Tyrosine", "L-Valine", "Phenylpyruvic acid", "Propionic acid", "Pyruvic acid", "Sarcosine")

# Create mSetObj
mSet<-InitDataObjects("conc", "msetora", FALSE)

#Set up mSetObj with the list of compounds
mSet<-Setup.MapData(mSet, tmp.vec);

# Cross reference list of compounds against libraries (hmdb, pubchem, chebi, kegg, metlin)
mSet<-CrossReferencing(mSet, "name");

## ---- eval=FALSE---------------------------------------------------------
#  # Example compound name map
#  mSet$name.map
#  
#  $query.vec
#   [1] "Acetoacetic acid"   "Beta-Alanine"       "Creatine"           "Dimethylglycine"    "Fumaric acid"
#   [6] "Glycine"            "Homocysteine"       "L-Cysteine"         "L-Isolucine"        "L-Phenylalanine"
#  [11] "L-Serine"           "L-Threonine"        "L-Tyrosine"         "L-Valine"           "Phenylpyruvic acid"
#  [16] "Propionic acid"     "Pyruvic acid"       "Sarcosine"
#  
#  $hit.inx
#   [1]  42  40  46  62  88  78 588 446  NA 104 120 109 103 702 131 159 164 185
#  
#  $hit.values
#   [1] "Acetoacetic acid"   "Beta-Alanine"       "Creatine"           "Dimethylglycine"    "Fumaric acid"
#   [6] "Glycine"            "Homocysteine"       "L-Cysteine"         NA                   "L-Phenylalanine"
#  [11] "L-Serine"           "L-Threonine"        "L-Tyrosine"         "L-Valine"           "Phenylpyruvic acid"
#  [16] "Propionic acid"     "Pyruvic acid"       "Sarcosine"
#  
#  $match.state
#   [1] 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1

## ------------------------------------------------------------------------
# Create the mapping results table
mSet<-CreateMappingResultTable(mSet)

# Input the name of the compound without any matches 
mSet<-PerformDetailMatch(mSet, "L-Isolucine");

# Create list of candidates to replace the compound
mSet <- GetCandidateList(mSet);

# Identify the name of the compound to replace
mSet<-SetCandidate(mSet, "L-Isolucine", "L-Isoleucine");

# Set the metabolite filter
mSet<-SetMetabolomeFilter(mSet, F);

# Select metabolite set library
mSet<-SetCurrentMsetLib(mSet, "smpdb_pathway", 2);

# Calculate hypergeometric score, results table generated in your working directory
mSet<-CalculateHyperScore(mSet)

# Plot the ORA, bar-graph
mSet<-PlotORA(mSet, "ora_0_", "bar", "png", 72, width=NA)

## ------------------------------------------------------------------------
# Create mSetObj
mSet<-InitDataObjects("conc", "msetqea", FALSE)

# Read in data table
mSet<-Read.TextData(mSet, "http://www.metaboanalyst.ca/MetaboAnalyst/resources/data/human_cachexia.csv", "rowu", "disc");

# Perform cross-referencing of compound names
mSet<-CrossReferencing(mSet, "name");

# Create mapping results table
mSet<-CreateMappingResultTable(mSet)

# Mandatory check of data 
mSet<-SanityCheckData(mSet);

# Replace missing values with minimum concentration levels
mSet<-ReplaceMin(mSet);

# Perform no normalization
mSet<-PreparePrenormData(mSet)
mSet<-Normalization(mSet, "NULL", "NULL", "NULL", "PIF_178", ratio=FALSE, ratioNum=20)

# Plot normalization
mSet<-PlotNormSummary(mSet, "norm_0_", "png", 72, width=NA)

# Plot sample-wise normalization
mSet<-PlotSampleNormSummary(mSet, "snorm_0_", "png", 72, width=NA)

# Set the metabolome filter
mSet<-SetMetabolomeFilter(mSet, F);

# Set the metabolite set library to pathway
mSet<-SetCurrentMsetLib(mSet, "smpdb_pathway", 2);

# Calculate the global test score
mSet<-CalculateGlobalTestScore(mSet)

# Plot the QEA
mSet<-PlotQEA.Overview(mSet, "qea_0_", "bar", "png", 72, width=NA)


