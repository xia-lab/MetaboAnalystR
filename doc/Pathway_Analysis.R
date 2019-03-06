## ------------------------------------------------------------------------
# Create vector consisting of compounds for enrichment analysis 
tmp.vec <- c("Acetoacetic acid", "Beta-Alanine", "Creatine", "Dimethylglycine", "Fumaric acid", "Glycine", "Homocysteine", "L-Cysteine", "L-Isolucine", "L-Phenylalanine", "L-Serine", "L-Threonine", "L-Tyrosine", "L-Valine", "Phenylpyruvic acid", "Propionic acid", "Pyruvic acid", "Sarcosine")

# Create mSetObj for storing objects created during your analysis
mSet<-InitDataObjects("conc", "pathora", FALSE)

# Set up mSetObj with the list of compounds
mSet<-Setup.MapData(mSet, tmp.vec);

# Cross reference list of compounds against libraries (hmdb, pubchem, chebi, kegg, metlin)
mSet<-CrossReferencing(mSet, "name");

mSet<-CreateMappingResultTable(mSet);

# Perform matching against compound w. out matches
mSet<-PerformDetailMatch(mSet, "L-Isolucine");

# Get list of candidates for matching
mSet<-GetCandidateList(mSet);

# Replace selected compound
mSet<-SetCandidate(mSet, "L-Isolucine", "L-Isoleucine");

# Select the pathway library, ranging from mammals to prokaryotes
mSet<-SetKEGG.PathLib(mSet, "hsa")

# Set the metabolite filter
mSet<-SetMetabolomeFilter(mSet, F);

# Calculate the over representation analysis score, here we selected to use the hypergeometric test (alternative is Fisher's exact test)
# A results table "pathway_results.csv" will be created and found within your working directory
mSet<-CalculateOraScore(mSet, "rbc", "hyperg")

# Plot of the Pathway Analysis Overview 
mSet<-PlotPathSummary(mSet, "path_view_0_", "png", 72, width=NA)

# Plot a specific metabolic pathway, in this case "Glycine, serine and threonine metabolism"
mSet<-PlotKEGGPath(mSet, "Glycine, serine and threonine metabolism",528, 480, "png", NULL)

