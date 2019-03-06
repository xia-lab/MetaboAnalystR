## ---- eval=FALSE---------------------------------------------------------
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/integ_genes.txt", "integ_genes.txt", "curl")
#  
#  download.file("https://www.metaboanalyst.ca/MetaboAnalyst/resources/data/integ_cmpds.txt", "integ_cmpds.txt", "curl")
#  

## ---- eval=FALSE---------------------------------------------------------
#  ##
#  ## METABOLITE-DISEASE INTERACTION NETWORK
#  ##
#  
#  # Create objects for storing processed data from the network explorer module
#  mSet<-InitDataObjects("conc", "network", FALSE)
#  
#  # Set organism to human, at the moment only human data can be accomodated
#  mSet<-SetOrganism(mSet, "hsa")
#  
#  # Set geneListFile as a file containing your gene list
#  geneListFile<-"integ_genes.txt"
#  
#  # Read in the geneListFile
#  # This will import a plain text file as single character string
#  geneList<-readChar(geneListFile, file.info(geneListFile)$size)
#  
#  # Perform gene ID mapping
#  mSet<-PerformIntegGeneMapping(mSet, geneList, "hsa", "symbol");
#  
#  # Set cmpdListFile as a file containing your metablolite list
#  cmpdListFile<-"integ_cmpds.txt"
#  
#  # Read in the cmpdListFile
#  # This will import a plain text file as single character string
#  cmpdList<-readChar(cmpdListFile, file.info(cmpdListFile)$size)
#  
#  # Perform compound ID mapping
#  mSet<-PerformIntegCmpdMapping(mSet, cmpdList, "hsa", "kegg");
#  
#  # Create the mapping results table for compounds
#  mSet<-CreateMappingResultTable(mSet)
#  
#  # Create the mapping results table for genes
#  mSet<-GetNetworkGeneMappingResultTable(mSet)
#  
#  # Prepare the data for network analysis, saves a .json file that can be uploaded
#  # to external sites/packages to view the network
#  mSet<-PrepareNetworkData(mSet);
#  
#  # Map user's data to internal interaction network
#  mSet<-SearchNetDB(mSet, "pheno", "global", FALSE, 0.5)
#  
#  # Create graph and subnetworks
#  mSet<-CreateGraph(mSet)
#  

