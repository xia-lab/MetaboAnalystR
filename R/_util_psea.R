###############################
######## For R Package ########
###############################

#' Function to get compound details from a specified pathway
#' @description Function to get compound details from a specified pathway.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries. Significant compounds will be indicated with an asterisk.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param msetNm Input the name of the pathway
#' @export
GetMummichogPathSetDetails <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$api$lib)){
    
    # get file from api.metaboanalyst.ca
    toSend = list(pathName = msetNm)
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/pathsetdetails/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    
    if(query_results$status_code == 200){
      filename <- httr::content(query_results, "text")
    }
    
    endpointfile <- paste0("/getFile", "/", mSetObj$api$guestName, "/", filename)
    callfile <- paste(base, endpointfile, sep="")
    download.file(callfile, destfile = basename(filename))
    print(paste0(filename, " saved to current working directory!"))
    return(.set.mSet(mSetObj));
  }
  
  version <- mum.version <- mSetObj$paramSet$version;
  inx <- which(mSetObj$pathways$name == msetNm)
  
  if(is.na(inx)){
    AddErrMsg("Invalid pathway name!")
    return(0)
  }
  
  if(version=="v2" & mSetObj$paramSet$mumRT){
    mset <- mSetObj$pathways$emp_cpds[[inx]];
    mset_cpds <- mSetObj$pathways$cpds[[inx]];
    
    hits.all <- unique(mSetObj$total_matched_ecpds)
    hits.sig <- mSetObj$input_ecpdlist;
    
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    ref.ecpds <- mset[which(refs & !sigs)]
    sig.ecpds <- mset[sigs]
    
    ref.mzs <- lapply(ref.ecpds, function(x) paste(as.numeric(unique(unlist(mSetObj$ec2mz_dict[x]))), collapse = "; ")) 
    sig.mzs <- lapply(sig.ecpds, function(x) paste(as.numeric(unique(unlist(mSetObj$ec2mz_dict[x]))), collapse = "; "))  
    
    ref.cpds <- lapply(ref.ecpds, function(x) paste(unique(unlist(mSetObj$ecpd_cpd_dict[x])), collapse = "; "))
    sig.cpds <- lapply(sig.ecpds, function(x) paste(unique(unlist(mSetObj$ecpd_cpd_dict[x])), collapse = "; "))
    
    path.results <- matrix(c(unlist(sig.mzs), unlist(ref.mzs), unlist(sig.cpds), unlist(ref.cpds)), ncol=2) 
    colnames(path.results) <- c("mzs", "cpds")
    rownames(path.results) <- c(paste0(sig.ecpds, "*"), ref.ecpds)
    
    name <- paste0(gsub(" ", "_", msetNm), "_ecpd_mz_info.csv")
    fast.write.csv(path.results, name)
  }else{
    mset <- mSetObj$pathways$cpds[[inx]];
    
    hits.all <- unique(mSetObj$total_matched_cpds)
    hits.sig <- mSetObj$input_cpdlist;
    
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    ref.cpds <- mset[which(refs & !sigs)]
    sig.cpds <- mset[sigs]
    
    ref.mzs <- lapply(ref.cpds, function(x) paste(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[x]))), collapse = "; ")) 
    sig.mzs <- lapply(sig.cpds, function(x) paste(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[x]))), collapse = "; "))  
    
    path.results <- matrix(c(unlist(sig.mzs), unlist(ref.mzs)), ncol=1) 
    colnames(path.results) <- "mzs"
    rownames(path.results) <- c(paste0(sig.cpds, "*"), ref.cpds)
    
    name <- paste0(gsub(" ", "_", msetNm), "_cpd_mz_info.csv")
    fast.write.csv(path.results, name)
  }
  
  return(.set.mSet(mSetObj));
}

#' Function to get adduct details from a specified compound
#' @description Function to get adduct details from a specified compound.
#' The results will be both printed in the console as well as saved
#' as a csv file. Note that performing this function multiple times will
#' overwrite previous queries.
#' @param mSetObj Input the name of the created mSetObj object.
#' @param cmpd.id Input the name of the selected compound.
#'@import qs
#' @export
GetCompoundDetails <- function(mSetObj=NA, cmpd.id){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$api$lib)){
    
    # get file from api.metaboanalyst.ca
    toSend = list(cmpdId = cmpd.id)
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/compounddetails/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    
    if(query_results$status_code == 200){
      filename <- httr::content(query_results, "text")
    }
    
    endpointfile <- paste0("/getFile", "/", mSetObj$api$guestName, "/", filename)
    callfile <- paste(base, endpointfile, sep="")
    download.file(callfile, destfile = basename(filename))
    print(paste0(filename, " saved to current working directory!"))
    return(.set.mSet(mSetObj));
  }
  
  forms <- mSetObj$cpd_form_dict[[cmpd.id]];
  
  if(is.null(forms)){
    print("This compound is not valid!")
    return(0)
  }
  
  matched_res <- qs::qread("mum_res.qs");
  mz <- matched_res[which(matched_res$Matched.Compound == cmpd.id), 1] 
  mass.diff <- matched_res[which(matched_res$Matched.Compound == cmpd.id), 4]
  tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
  
  res <- cbind(rep(cmpd.id, length(mz)), mz, forms, mass.diff, tscores) 
  colnames(res) <- c("Matched.Compound", "m.z", "Matched.Form", "Mass.Diff", "T.Scores")
  fast.write.csv(res, "mummichog_compound_details.csv")
  return(.set.mSet(mSetObj));
}

# Function to return the unique m/zs from the selected pathways 
# based on the compounds

GetMummichogMZHits <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(!is.null(mSetObj$api$lib)){
    
    # get file from api.metaboanalyst.ca
    toSend = list(pathName = msetNm)
    
    load_httr()
    base <- api.base
    endpoint <- paste0("/mzhits/", mSetObj$api$guestName)
    call <- paste(base, endpoint, sep="")
    query_results <- httr::POST(call, body = toSend, encode= "json")
    
    if(query_results$status_code == 200){
      result <- httr::content(query_results, "text")
    }
    mSetObj$mz.hits <- result
    print(paste0("Unique m/z features in ", msetNm, ": ", result))
    return(.set.mSet(mSetObj));
  }
  
  inx <- which(mSetObj$pathways$name == msetNm)
  mset <- mSetObj$pathways$cpds[[inx]];
  mzs <- as.numeric(unique(unlist(mSetObj$cpd2mz_dict[mset]))) 
  result <- intersect(mzs, mSetObj$dataSet$input_mzlist)
  mSetObj$mz.hits <- result
  
  return(.set.mSet(mSetObj));
}

#################################################################
######## Functions for updating or building new libraries ########
##################################################################

#' Create Mummichog Libraries from KEGG
#' @description Function to create mummichog libraries from
#' MetaboAnalyst pathway libraries (metpa).
#' Outputs the RDS files in the current working directory. RDS files
#' are saved using the KEGG organism code.
#' @param folder Input the path of the folder containing the metpa rda files.
#' @param kegg_compounds Input the name of the KEGG dictionary containing the 
#' KEGG compound IDs, KEGG compopund names, and molecular weight.
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

## Utility function
## Make list of KEGG rda files
rda2list <- function(file) {
  e <- new.env()
  load(file, envir = e)
  as.list(e)
}

## Fill in the pathways 
fillpathways <- function(f){ 
  
  pathways <- list()
  p <- list()
  
  cpds <- unname(f$current.kegglib$mset.list)
  
  for(i in 1:length(cpds)){
    all_cpds <- as.vector(unlist(cpds[[i]]))
    matched_cpds <- all_cpds[which(all_cpds %in% kegg_compounds)] 
    p[[i]] <- matched_cpds
  }
  
  pathways$cpds <- p
  pathways$name <- names(f$current.kegglib$path.ids)
  
  return(pathways)
}

## Gets names and exact mass of all cpds (cpd.lib)
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

## Creates cpd.tree
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
  file_name <- paste0(org, "_kegg.qs")
  
  qs::qsave(mummichog.lib, file=file_name);
}

# Makes adducts
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