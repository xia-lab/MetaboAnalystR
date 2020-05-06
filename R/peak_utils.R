#' Create Mummichog Libraries from KEGG
#' @description Function to create mummichog libraries from
#' MetaboAnalyst pathway libraries (metpa).
#' Outputs the RDS files in the current working directory. RDS files
#' are saved using the KEGG organism code.
#' @param folder Input the path of the folder containing the metpa rda files.
#' @param kegg_compounds Input the name of the KEGG dictionary containing the 
#' KEGG compound IDs, KEGG compopund names, and molecular weight.
#' @usage CreateMummichogLibs("~/Desktop/MetaboAnalyst/mummichog/2020_mummichog_libs/test", kegg_compounds_2020)
#' @export
CreateMummichogLibs <- function(folder, kegg_compounds){

  # Step 1: Get list of pathways to make mummichog libraries from 
  folder <- folder
  files <- list.files(folder, pattern = ".rda$")
  
  if(length(files) == 0){
    AddErrMsg("No .rda files found in folder!")
    return(0)
  }
  
  # Step2: Create the models list 
  models <- Map(rda2list, file.path(folder, files))
  names(models) <- tools::file_path_sans_ext(files)
  org <- names(models)
  
  kegg_compounds <<- kegg_compounds
  
  # Step 3: Create the pathways
  pathway <- lapply(models, function(f) {fillpathways(f)} )
  
  # Step 4: Create cpd.lib
  cpd.lib <- lapply(pathway, function(l) {make_cpdlib(l)})
  
  # Step 5: Create mummichog libraries
  # Will output the .RDS files in the current working directory
  output <- mapply(CreateLibFromKEGG, cpd.lib, pathway, org)
  
}

#' Utility function
#' Make list of KEGG rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

#' Fill in the pathways 
fillpathways <- function(f){ 
  
  pathways <- list()
  p <- list()
  
  cpds <- unname(f$metpa$mset.list)
  
  for(i in 1:length(cpds)){
    all_cpds <- as.vector(unlist(cpds[[i]]))
    matched_cpds <- all_cpds[which(all_cpds %in% kegg_compounds)] 
    p[[i]] <- matched_cpds
  }
  
  pathways$cpds <- p
  pathways$name <- names(f$metpa$path.ids)
  
  return(pathways)
}

#' Gets names and exact mass of all cpds (cpd.lib)
make_cpdlib <- function(org){
  
  all_cpds <- unique(unlist(org$cpds))
  
  index <- match(all_cpds, kegg_compounds[,1])
  
  ids <- list()
  names <- list()
  mass <- list()
  
  for(i in 1:length(index)){
    inx <- index[i]
    ids[i] <- kegg_compounds[inx,1]
    names[i] <- kegg_compounds[inx,2]
    mass[i] <- kegg_compounds[inx,3]
  }
  
  ids <- unlist(ids)
  names <- unlist(names)
  mass <- as.numeric(unlist(mass))
  
  cpd.lib <- list(
    id = ids,
    name = names,
    mw = mass
  )
  return(cpd.lib)
}

#' Creates cpd.tree
CreateLibFromKEGG <- function(cpd.lib, pathways, org){
  
  cpd.lib <- cpd.lib;
  ms_modes <- c('dpj_positive', 'positive', 'negative');
  adducts <- list();
  for (ms_mode in ms_modes){
    adducts[[ms_mode]] <- Compound_function_mzlist(ms_mode, cpd.lib$mw);
  }
  cpd.lib$adducts <- adducts;
  
  # create a dictionary for look up in the range of 50-2000
  # now need to create ladder (tree) for each new mz
  # key is the mass 50 to 2000, values are the compounds (if any of their modified mw gives the value)
  # now create cpd tree for each mass pos
  # note, this can be slow, but this can be created before hand
  # for each species and for each mode
  # note l2 only stores the index of the cpd.lib
  
  cpd.tree <- list();
  for (ms_mode in ms_modes){
    l2 <- list();
    l2[[49]] <- "";
    l2[[2001]] <- "";
    mz.mat <- cpd.lib$adducts[[ms_mode]];
    floor.mzs <- floor(mz.mat);
    for(i in 1:nrow(floor.mzs)){
      neighbourhood <- floor.mzs[i,];
      for(n in neighbourhood){
        if((n>50) & (n<2000)){
          l2[[n]] <- append(l2[[n]], i);
        }
      }
    }
    cpd.tree[[ms_mode]] <- lapply(l2, unique);
  }
  
  # set up the variables
  mummichog.lib <- list(
    pathways = pathways,
    cpd.tree = cpd.tree,
    cpd.lib = cpd.lib
  )
  
  print(paste0(org, " mummichog library created!"))
  file_name <- paste0(org, "_kegg.rds")
  
  saveRDS(mummichog.lib, file=file_name);
}

#' Makes adducts
Compound_function_mzlist <- function(ms_mode, mw){
  
  load_stringr()
  
  PROTON <- 1.00727646677;
  mw_modified <- NULL;
  
  if (ms_mode == "dpj_positive"){
    mw_modified <- cbind(mw, mw + PROTON, mw/2 + PROTON, mw +1.0034 + PROTON, mw/2 + 0.5017 + PROTON, mw +1.9958 + PROTON, mw +1.9972 + PROTON, mw + 21.9820 + PROTON, mw/2 + 10.991 + PROTON, mw + 37.9555 + PROTON, mw + 67.9874 + PROTON, mw + 83.9613 + PROTON);
    colnames(mw_modified) <- c('M[1+]', 'M+H[1+]', 'M(C13)+H[1+]', 'M(C13)+H[1+]', 'M(C13)+2H[2+]', 'M(S34)+H[1+]', 'M(Cl37)+H[1+]', 'M+Na[1+]', 'M+H+Na[2+]', 'M+K[1+]', 'M+HCOONa[1+]', 'M+HCOOK[1+]');
    
  }else if (ms_mode == "positive" | ms_mode == 'generic'){
    mw_modified <- cbind(mw, mw + PROTON, mw/2 + PROTON, mw/3 + PROTON, mw +1.0034 + PROTON, mw/2 + 0.5017 + PROTON, mw/3 + 0.3344 + PROTON, mw +1.9958 + PROTON, mw +1.9972 + PROTON, mw + 21.9820 + PROTON, mw/2 + 10.991 + PROTON, mw + 37.9555 + PROTON, mw + 18.0106 + PROTON, mw - 18.0106 + PROTON, mw - 36.0212 + PROTON, mw - 17.0265 + PROTON, mw - 27.9950 + PROTON, mw - 43.9898 + PROTON, mw - 46.0054 + PROTON, mw + 67.9874 + PROTON, mw - 67.9874 + PROTON, mw + 57.9586 + PROTON, mw - 72.0211 + PROTON, mw + 83.9613 + PROTON, mw - 83.9613 + PROTON);
    colnames(mw_modified) <- c('M[1+]', 'M+H[1+]', 'M+2H[2+]', 'M+3H[3+]', 'M(C13)+H[1+]', 'M(C13)+2H[2+]', 'M(C13)+3H[3+]', 'M(S34)+H[1+]', 'M(Cl37)+H[1+]', 'M+Na[1+]', 'M+H+Na[2+]', 'M+K[1+]', 'M+H2O+H[1+]', 'M-H2O+H[1+]', 'M-H4O2+H[1+]', 'M-NH3+H[1+]', 'M-CO+H[1+]', 'M-CO2+H[1+]', 'M-HCOOH+H[1+]', 'M+HCOONa[1+]', 'M-HCOONa+H[1+]', 'M+NaCl[1+]', 'M-C3H4O2+H[1+]', 'M+HCOOK[1+]', 'M-HCOOK+H[1+]');
    
  }else if (ms_mode == "negative"){
    mw_modified <- cbind(mw - PROTON, mw/2 - PROTON, mw + 1.0034 - PROTON, mw + 1.9958 - PROTON, mw + 1.9972 - PROTON, mw + 21.9820 - 2*PROTON, mw + 37.9555 - 2*PROTON, mw - 18.0106 - PROTON, mw + 34.9689, mw + 36.9659, mw + 78.9183, mw + 80.9163, mw + 2*12 + 3*1.007825 + 14.00307 - PROTON, mw + 1.007825 + 12 + 2*15.99491, mw + 3*1.007825 + 2*12 + 2*15.99491, mw - PROTON + 15.99491);
    colnames(mw_modified) <- c('M-H[-]', 'M-2H[2-]', 'M(C13)-H[-]', 'M(S34)-H[-]', 'M(Cl37)-H[-]', 'M+Na-2H[-]', 'M+K-2H[-]', 'M-H2O-H[-]', 'M+Cl[-]', 'M+Cl37[-]', 'M+Br[-]', 'M+Br81[-]', 'M+ACN-H[-]', 'M+HCOO[-]', 'M+CH3COO[-]', 'M-H+O[-]');
    
  }else{
    print("Unrecognized mode of instrumentation.")
  }
  
  return(mw_modified);
}