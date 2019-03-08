### An R package for pathway enrichment analysis for untargeted metabolomics
### based on high-resolution LC-MS platform
### This is based on the mummichog algorithm implemented in python (http://mummichog.org/)
### The goals of developing MummichogR are
### 1) to make this available to the R user community
### 2) high-performance (similar or faster compared to python)
### 3) broader pathways support - by adding support for 21 common organisms based on KEGG pathways
### 4) companion web interface on MetaboAnalyst - the "MS Peaks to Pathways" module
### @authors J. Chong \email{jasmine.chong@mail.mcgill.ca}, J. Xia \email{jeff.xia@mcgill.ca}
### McGill University, Canada
### License: GNU GPL (>= 2)

#'Constructor to read uploaded user files into the mummichog object
#'@description This function handles reading in CSV or TXT files and filling in the mSet object
#' for mummichog analysis. It makes sure that all necessary columns are present.
#'@usage Read.PeakListData(mSetObj=NA, filename = NA)
#'@param mSetObj Input the name of the created mSetObj.
#'@param filename Input the path name for the CSV/TXT files to read.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
Read.PeakListData <- function(mSetObj=NA, filename = NA) {
  
    mSetObj <- .get.mSet(mSetObj);
  
    input <- read.table(filename, header=TRUE, as.is=TRUE);
    # must contain the three labels
    hits <- c("p.value", "m.z", "t.score") %in% colnames(input);
    if(!all(hits)){
      AddErrMsg("Missing information, data must contain three columns: 'p.value', 'm.z', 't.score'");
      return (0);
    }

    mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score);
    mSetObj$msgSet$read.msg <- paste("A total of", length(input$p.value), "m/z features were found in your uploaded data.");
    return(.set.mSet(mSetObj));

}

#'Convert mSetObj to proper format for MS Peaks to Pathways module
#'@description Following t-test analysis, this functions converts the results from the mSetObj 
#'to the proper format for mummichog analysis
#'@param mSetObj Input the name of the created mSetObj.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

Convert2Mummichog <- function(mSetObj=NA, rt=FALSE){
  
  tt.pval <- mSetObj$analSet$tt$p.value
  fdr <- p.adjust(tt.pval, "fdr")
  mz.pval <- names(tt.pval)
  pvals <- cbind(as.numeric(mz.pval), as.numeric(fdr))
  colnames(pvals) <- c("m.z", "p.value")
  
  tt.tsc <- mSetObj$analSet$tt$t.score
  mz.tsc <- names(tt.tsc)
  tscores <- cbind(as.numeric(mz.tsc), as.numeric(tt.tsc))
  colnames(tscores) <- c("m.z", "t.score")
  
  if(rt == TRUE & !.on.public.web){
    camera_output <- readRDS("annotated_peaklist.rds")
    mz.cam <- round(camera_output$mz, 5) 
    rt.cam <- round(camera_output$rt, 5) 
    camera <- cbind(mz.cam, rt.cam)
    colnames(camera) <- c("m.z", "r.t")
    mummi_new <- Reduce(function(x,y) merge(x,y,by="m.z", all = TRUE), list(camera, pvals, tscores))
  }else{
    mummi_new <- merge(pvals, tscores)
  }
  
  filename <- paste0("mummichog_input_", Sys.Date(), ".txt")
  
  write.table(mummi_new, filename, row.names = FALSE)
  
  return(.set.mSet(mSetObj))
}

#'Update the mSetObj with user-selected parameters for MS Peaks to Pathways.
#'@description This functions handles updating the mSet object for mummichog analysis. It is necessary to utilize this function
#'to specify to the organism's pathways to use (libOpt), the mass-spec mode (msModeOpt), mass-spec instrument (instrumentOpt), 
#'the p-value cutoff (pvalCutoff), and the number of permutations (permNum).
#'@usage UpdateMummichogParameters(mSetObj=NA, instrumentOpt, msModeOpt, pvalCutoff, custom=FALSE)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param instrumentOpt Define the mass-spec instrument used to perform untargeted metabolomics.
#'@param msModeOpt Define the mass-spec mode of the instrument used to perform untargeted metabolomics.
#'@param pvalCutoff Numeric, specify the p-value cutoff to define significant m/z features from reference m/z features.
#'@param custom Logical, select adducts for mummichog to consider.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
UpdateMummichogParameters <- function(mSetObj=NA, instrumentOpt, msModeOpt, pvalCutoff, custom = FALSE){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$instrument <- instrumentOpt;
  mSetObj$dataSet$mode <- msModeOpt;
  mSetObj$dataSet$cutoff <- pvalCutoff;
  mSetObj$custom <- custom;
  
  return(.set.mSet(mSetObj));
}

#'Sanity Check Data
#'@description SanityCheckData is used for data processing, and performs a basic sanity
#'check of the uploaded data, ensuring that the data is suitable for further analysis.
#'The function ensure that all parameters are properly set based on updated parameters.
#'@usage SanityCheckMummichogData(mSetObj=NA)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
SanityCheckMummichogData <- function(mSetObj=NA){
  
  mSetObj <- .get.mSet(mSetObj);
  msg.vec <- NULL;

  cutoff <- mSetObj$dataSet$cutoff;
  ndat <- mSetObj$dataSet$mummi.orig;

  # sort mzs by p-value
  ord.inx <- order(ndat[,1]);
  ndat <- ndat[ord.inx,]; # order by p-vals
  
  # filter based on mz
  mznew <- ndat[,2];
  # trim to fit within 50 - 2000
  my.inx <- mznew > 50 & mznew < 2001;
  trim.num <- sum(!my.inx);
  
  msg.vec <- c(msg.vec, "Only a small percentage (below 10%) peaks in your input peaks should be significant.");
  msg.vec <- c(msg.vec, "The algorithm works best for <u>200~500 significant peaks in 3000~7000 total peaks</u>.");

  if(trim.num > 0){
    ndat <- ndat[my.inx,]
    msg.vec <- c(msg.vec, paste("A total of", trim.num, "were excluded to fit within mz range of 50-2000"));
  }
  
  # remove duplicated mzs (make unique)
  dup.inx <- duplicated(ndat);
  dup.num <- sum(dup.inx);
  
  if(dup.num > 0){
    ndat <- ndat[!dup.inx,];
    msg.vec <- c(msg.vec, paste("A total of", dup.num, " duplciated mz features were removed."));
  }
  
  ref_mzlist <- ndat[,2];

  # set up expression (up/dn)
  tscores <- as.numeric(ndat[,3]);
  names(tscores) <- ref_mzlist;

  ref.size <- length(ref_mzlist);
  
  msg.vec <- c(msg.vec, paste("A total of ", ref.size, "input mz features were retained for further analysis"));

  if(ref.size > 20000){
    msg.vec <- c(msg.vec, "There are too many input features, the performance may be too slow");
  }

  my.inx <- ndat[,1] < cutoff;
  input_mzlist <- ref_mzlist[my.inx];

  # note, most of peaks are assumed to be not changed significantly, more than 25% should be warned
  sig.size <- length(input_mzlist);
  sig.part <- round(100*sig.size/length(ref_mzlist),2);
  if(sig.part > 75){
     msg.vec <- c(msg.vec, paste("<font color=\"red\">Warning: over", sig.part, "percent were significant based on your cutoff </font>."));
     msg.vec <- c(msg.vec, "You need to adjust p-value cutoff to control the percentage");
  }else if(sig.part > 25){
     msg.vec <- c(msg.vec, paste("<font color=\"orange\">Warning: over", sig.part, "percent were significant based on your cutoff</font>."));
     msg.vec <- c(msg.vec, "You should adjust p-value cutoff to control the percentage");
  }else{
     msg.vec <- c(msg.vec, paste("A total of", sig.size, "or", sig.part, "percent signficant mz features were found based on the selected p-value cutoff:", cutoff));
  }

  if(sig.size > 2000){
    msg.vec <- c(msg.vec, "There are too many significant features based on the current cutoff, possibly too slow.");
  }else if(sig.size < 30){
    msg.vec <- c(msg.vec, "The number of significant features is small based on the current cutoff, possibly not accurate.");
  }
  
  mSetObj$msgSet$check.msg <- c(mSetObj$msgSet$check.msg, msg.vec);
  mSetObj$dataSet$mummi.proc <- ndat;
  mSetObj$dataSet$input_mzlist <- input_mzlist;
  mSetObj$dataSet$expr_dic <- tscores;
  mSetObj$dataSet$N <- sig.size;
  mSetObj$dataSet$ref_mzlist <- ref_mzlist;
  
  return(.set.mSet(mSetObj));
  
}

#'Main function to perform mummichog
#'@description This is the main function that performs the mummichog analysis. 
#'@usage PerformMummichog(mSetObj=NA, lib, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object 
#'@param lib Input the name of the organism library, default is hsa 
#'@param permNum Numeric, the number of permutations to perform
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformMummichog <- function(mSetObj=NA, lib, permNum = 100){
  
    mSetObj <- .get.mSet(mSetObj);
  
    filenm <- paste(lib, ".rds", sep="")
    biocyc <- grepl("biocyc", lib)
    
    if(!is.null(mSetObj$curr.cust)){
      
      if(biocyc){
        user.curr <- mSetObj$curr.map$BioCyc
      }else{
        user.curr <- mSetObj$curr.map$KEGG
      }
      
      currency <<- user.curr
      
      if(length(currency)>0){
        mSetObj$mummi$anal.msg <- c("Currency metabolites were successfully uploaded!")
      }else{
        mSetObj$mummi$anal.msg <- c("Errors in currency metabolites uploading!")
      }
    }
  
    if(.on.public.web==TRUE){
        mummichog.lib <- readRDS(paste("../../libs/mummichog/", filenm, sep=""));
    }else{
        if(!file.exists(filenm)){
            mum.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", filenm, sep="")
            download.file(mum.url, destfile = filenm, method="libcurl", mode = "wb")
            mummichog.lib <- readRDS(filenm);
        }else{
            mummichog.lib <- readRDS(filenm);
        }
    }
  
    if(!is.null(mSetObj$adduct.custom)){
      mw <- mummichog.lib$cpd.lib$mw
      new_adducts <- new_adduct_mzlist(mSetObj, mw)
      
      cpd.lib <- list(
        mz.mat = new_adducts,
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name
      );
      
    }else{
      cpd.lib <- list(
        mz.mat = mummichog.lib$cpd.lib$adducts[[mSetObj$dataSet$mode]],
        mw = mummichog.lib$cpd.lib$mw,
        id = mummichog.lib$cpd.lib$id,
        name = mummichog.lib$cpd.lib$name
      );
    }

    cpd.tree <- mummichog.lib$cpd.tree[[mSetObj$dataSet$mode]];

    mSetObj$pathways <- mummichog.lib$pathways;
    mSetObj$lib.organism <- lib; #keep track of lib organism for sweave report
  
    mSetObj <- .search.compoundLib(mSetObj, cpd.lib, cpd.tree);
    mSetObj <- .perform.mummichogPermutations(mSetObj, permNum);
    mSetObj <- .compute.mummichogSigPvals(mSetObj);
    mSetObj$mummi.anal <- "orig"
    mummichog.lib <- NULL;
    return(.set.mSet(mSetObj));
}

#'Map currency metabolites to KEGG & BioCyc
#'@description This function maps the user selected list
#'of compounds to its corresponding KEGG IDs and BioCyc IDs
#'@param mSetObj Input the name of the created mSetObj object 
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformCurrencyMapping <- function(mSetObj = NA){
  
  mSetObj <- .get.mSet(mSetObj);
  
  qvec <- mSetObj$dataSet$cmpd;
  curr_db <- .read.metaboanalyst.lib("curr_db.rds");
  hit.inx <- match(tolower(qvec), tolower(curr_db$Common.Name));
  match.values <- curr_db[hit.inx,];
  curr.met <- nrow(match.values)

  mSetObj$curr.map <- match.values
  
  if(curr.met > 0){
    mSetObj$mummi$curr.msg <- paste("A total of ", curr.met ," currency metabolites were successfully uploaded!", sep = "")
  }else{
    mSetObj$mummi$curr.msg <- c("No currency metabolites were selected!")
  }
  
  mSetObj$curr.cust <- TRUE;
  return(.set.mSet(mSetObj));
}

#'Read Adduct List
#'@description This function reads in the user's adduct list and 
#'saves it as a matrix. 
#'@usage Read.AdductData(mSetObj=NA, adductList)
#'@param mSetObj Input the name of the created mSetObj object 
#'@param adductList Input the name of the adduct list
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformAdductMapping <- function(mSetObj=NA, add.mode){
  
  mSetObj <- .get.mSet(mSetObj);
  
  adducts <- mSetObj$dataSet$adduct.list
  
  if(add.mode == "positive"){
    add_db <- .read.metaboanalyst.lib("pos_adduct.rds");
  }else if(add.mode == "negative"){
    add_db <- .read.metaboanalyst.lib("neg_adduct.rds");
  }else{
    msg <- c("Adduct mode is not valid")
  }

  hit.inx <- match(tolower(adducts), tolower(add_db$Ion_Name));
  match.values <- add_db[hit.inx,];
  sel.add <- nrow(match.values)
  
  if(sel.add > 0){
    mSetObj$mummi$add.msg <- paste("A total of ", sel.add ," adducts were successfully selected!", sep = "")
  }else{
    mSetObj$mummi$add.msg <- c("No adducts were selected!")
  }
  
  mSetObj$add.map <- match.values
  
  return(.set.mSet(mSetObj));
}

# internal function to create new mz matrix from user-curated list of adducts
new_adduct_mzlist <- function(mSetObj=NA, mw){
  
  mSetObj <- .get.mSet(mSetObj);
  
  ion.name <- mSetObj$add.map$Ion_Name
  ion.mass <- mSetObj$add.map$Ion_Mass
  
  mw_modified <- NULL;
  
  mass.list <- as.list(ion.mass)
  mass.user <- lapply(mass.list, function(x) eval(parse(text = paste(x)))) 
  
  mw_modified <- cbind(mw, do.call(cbind, mass.user));
  colnames(mw_modified) <- c('M[1+]', ion.name);

  return(mw_modified);
}

# internal function for searching compound library
.search.compoundLib <- function(mSetObj, cpd.lib, cpd.tree){
  
  ref_mzlist <- mSetObj$data$ref_mzlist;
  t.scores <- mSetObj$data$ref_mzlist;

  modified.states <- colnames(cpd.lib$mz.mat);
  my.tols <- mz_tolerance(ref_mzlist, mSetObj$dataSet$instrument);
  
  # get mz ladder (pos index)
  self.mzs <- floor(ref_mzlist);
  all.mzs <- cbind(self.mzs-1, self.mzs, self.mzs+1);
  
  # matched_res will contain detailed result (cmpd.id. query.mass, mass.diff) for all mz;
  # use a high-performance variant of list
  matched_res <- myFastList();
  
  for(i in 1:length(ref_mzlist)){
    mz <- ref_mzlist[i];
    my.tol <- my.tols[i];
    all.mz <- all.mzs[i,];
    pos.all <- as.numeric(unique(unlist(cpd.tree[all.mz])));
    
    for(pos in pos.all){
      id <- cpd.lib$id[pos];
      mw.all <- cpd.lib$mz.mat[pos,]; #get modified mzs
      diffs <- abs(mw.all - mz); #modified mzs - mz original
      hit.inx <- which(diffs < my.tol);
      if(length(hit.inx)>0){
        for(spot in 1:length(hit.inx)){
          hit.pos <- hit.inx[spot];# need to match all
          index <- paste(mz, id, hit.pos, sep = "_");
          matched_res$add(index, c(i, id, mz, mw.all[hit.pos], modified.states[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
        }
      }
    }
  }
  
  # convert to regular list
  matched_res <- matched_res$as.list();
  matched_res <- data.frame(matrix(unlist(matched_res), nrow=length(matched_res), byrow=T), stringsAsFactors = FALSE);
  
  # re-order columns for output
  matched_res <- matched_res[, c(3,2,5,6)];
  colnames(matched_res) <- c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff");
  write.csv(matched_res, file="mummichog_matched_compound_all.csv", row.names=FALSE);

  # now update expr. profile
  matched_mz<- matched_res[,1];
  matched_ts <- mSetObj$dataSet$expr_dic[matched_mz];

  # get the expression profile for each 
  exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
  cpd_exp_dict <- Covert2Dictionary(exp.mat);

   # create average exp
  exp.vec <- unlist(lapply(cpd_exp_dict, mean));

  form.mat <- cbind(matched_res[,2], matched_res[,3]);
  cpd_form_dict <- Covert2Dictionary(form.mat);

  # now need to get the mapping from mz to compound id (one mz can have 0, 1, or more id hits)
  mz2cpd_dict <- Covert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
  cpd2mz_dict <- Covert2Dictionary(matched_res[,c(2,1)]); # indexed/named by id
  
  # now do matching to identify significant input_cpdlist
  refmz <- names(mz2cpd_dict)
  hits.index <- which(refmz %in% as.character(mSetObj$data$input_mzlist));
  cpd1 <- unique(unlist(mz2cpd_dict[hits.index]));
  cpd1 <- cpd1[!(cpd1 %in% currency)];
  
  mSetObj$matches.res <- matched_res;
  mSetObj$mz2cpd_dict <- mz2cpd_dict;
  mSetObj$cpd_exp_dict <- cpd_exp_dict;
  mSetObj$cpd_exp <- exp.vec;
  mSetObj$cpd_form_dict <- cpd_form_dict;
  mSetObj$cpd2mz_dict <- cpd2mz_dict;
  mSetObj$total_matched_cpds <- unique(as.vector(mSetObj$matches.res$Matched.Compound));
  mSetObj$input_cpdlist <- cpd1;
  
  return(mSetObj);
}

# Internal function for permutation
.perform.mummichogPermutations <- function(mSetObj, permNum){
  
  num_perm <- permNum;
  print(paste('Resampling, ', num_perm, 'permutations to estimate background ...'));
  permutation_hits <- permutation_record <- vector("list", num_perm);
  for(i in 1:num_perm){ # for each permutation, create list of input compounds and calculate pvalues for each pathway
    input_mzlist <- sample(mSetObj$data$ref_mzlist, mSetObj$data$N)
    t <- make_cpdlist(mSetObj, input_mzlist);
    perm <- ComputeMummichogPermPvals(t, mSetObj$total_matched_cpds, mSetObj$pathways, mSetObj$matches.res, input_mzlist, mSetObj$cpd2mz_dict);
    permutation_record[[i]] <- perm[1]
    permutation_hits[[i]] <- perm[2]
  }

  mSetObj$perm_record <- permutation_record;
  mSetObj$perm_hits <- permutation_hits

  return(mSetObj);
}

# Internal function for significant p value 
.compute.mummichogSigPvals <- function(mSetObj){
  
  qset <- unique(unlist(mSetObj$input_cpdlist)); #Lsig ora.vec
  query_set_size <- length(qset); #q.size
  
  total_cpds <- unique(mSetObj$total_matched_cpds) #all matched compounds
  total_feature_num <- length(total_cpds)
  
  current.mset <- mSetObj$pathways$cpds; #all compounds per pathway
  path.num <- unlist(lapply(current.mset, length));

  cpds <- lapply(current.mset, function(x) intersect(x, total_cpds)); #pathways & all ref cpds
  set.num <- unlist(lapply(cpds, length)); #cpdnum
  
  feats <- lapply(current.mset, function(x) intersect(x, qset)); #pathways & lsig
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset)); #empty lists
  
  for(i in 1:length(current.mset)){ # for each pathway
        sizes[[i]] <- min(feat_len[i], count_cpd2mz(mSetObj$cpd2mz_dict, unlist(feats[i]), mSetObj$data$input_mzlist)) #min overlap or mz hits
        negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size; # 
  }

  #error fixing for negatives, problem occurs when total_feat_num and query_set_size too close (lsig too close to lall)
  negneg <- rapply(negneg, function(x) ifelse(x<0,0,x), how = "replace") 

  unsize <- as.integer(unlist(sizes));
  
  # prepare for the result table
  res.mat <- matrix(0, nrow=length(current.mset), ncol=5);
  
  fishermatrix <- cbind(unsize, (set.num - unsize), (query_set_size - unsize), unlist(negneg)); 
  first <- unlist(lapply(sizes, function(x) max(0, x-1)));
  easematrix <- cbind(first, (set.num - unsize + 1), (query_set_size - unsize), unlist(negneg)); 

  res.mat[,1] <- path.num;  
  res.mat[,2] <- set.num;
  res.mat[,3] <- unsize;
  res.mat[,4] <- apply(easematrix, 1, function(x) fisher.test(matrix(x, nrow=2), alternative = "greater")$p.value);
  res.mat[,5] <- apply(fishermatrix, 1, function(x) fisher.test(matrix(x, nrow = 2), alternative = "greater")$p.value);
  colnames(res.mat) <- c("Pathway total", "Hits.total", "Hits.sig", "EASE", "FET");
  rownames(res.mat) <- mSetObj$pathways$name
  
  mSetObj$pvals <- res.mat;
  permutations_hits <- matrix(unlist(mSetObj$perm_hits), nrow=length(mSetObj$perm_hits), byrow=TRUE);
  sig_hits <- res.mat[,3]; # sighits
  sigpvalue <- res.mat[,4]; # EASE scores
  
  perm_record <- unlist(mSetObj$perm_record);
  perm_minus <- abs(0.9999999999 - perm_record);
  
  tryCatch({
    fit.gamma <- fitdistrplus::fitdist(perm_minus, distr = "gamma", method = "mle", lower = c(0, 0), start = list(scale = 1, shape = 1));
    rawpval <- as.numeric(sigpvalue);
    adjustedp <- 1 - (pgamma(1-rawpval, shape = fit.gamma$estimate["shape"], rate = fit.gamma$estimate["scale"]));
  }, error = function(e){
      print(e)   
  }, finally = {
       if(!exists("adjustedp")){
         adjustedp <- rep(NA, length = length(res.mat[,1]))
       }
       res.mat <- cbind(res.mat, Gamma=adjustedp);
  })

  # remove those no hits
  hit.inx <- as.numeric(as.character(res.mat[,3])) > 0;
  res.mat <- res.mat[hit.inx, ];

  # prepare json element for network
  hits.all <- cpds[hit.inx];
  hits.sig <- feats[hit.inx];  
  path.nms <- mSetObj$pathways$name[hit.inx];
  
  # order by p-values
  ord.inx <- order(res.mat[,6]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 5);
  mSetObj$mummi.resmat <- res.mat;

  json.res <- list(
        cmpd.exp = mSetObj$cpd_exp,
        path.nms = path.nms[ord.inx],
        hits.all = convert2JsonList(hits.all[ord.inx]),
        hits.sig = convert2JsonList(hits.sig[ord.inx]),
        fisher.p = as.numeric(res.mat[,5])
    );

  write.csv(res.mat, file="mummichog_pathway_enrichment.csv", row.names=TRUE);
  json.mat <- RJSONIO::toJSON(json.res, .na='null');
  sink("mummichog_query.json");
  cat(json.mat);
  sink();
  return(mSetObj);
}

####
#### NEW functions for inclusion of fgsea
####

#'New main function to perform fast pre-ranked mummichog
#'@description This is the main function that performs the mummichog analysis.
#'@usage PerformGSEA(mSetObj=NA, lib, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object
#'@param lib Input the name of the organism library, default is hsa
#'@param permNum Numeric, the number of permutations to perform
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
#'@import fgsea

PerformGSEA <- function(mSetObj=NA, lib, permNum = 100){
  
  mSetObj <- .get.mSet(mSetObj);
  
  filenm <- paste(lib, ".rds", sep="")
  biocyc <- grepl("biocyc", lib)
  
  if(.on.public.web){
    load_fGSEA()
    load_data.table()
  }
  
  if(!is.null(mSetObj$curr.cust)){
    
    if(biocyc){
      user.curr <- mSetObj$curr.map$BioCyc
    }else{
      user.curr <- mSetObj$curr.map$KEGG
    }
    
    currency <<- user.curr
    
    if(length(currency)>0){
      mSetObj$mummi$anal.msg <- c("Currency metabolites were successfully uploaded!")
    }else{
      mSetObj$mummi$anal.msg <- c("Errors in currency metabolites uploading!")
    }
  }
  
  if(.on.public.web==TRUE){
    mummichog.lib <- readRDS(paste("../../libs/mummichog/", filenm, sep=""));
  }else{
    if(!file.exists(filenm)){
      mum.url <- paste("https://www.metaboanalyst.ca/resources/libs/mummichog/", filenm, sep="")
      download.file(mum.url, destfile = filenm, method="libcurl", mode = "wb")
      mummichog.lib <- readRDS(filenm);
    }else{
      mummichog.lib <- readRDS(filenm);
    }
  }
  
  if(!is.null(mSetObj$adduct.custom)){
    mw <- mummichog.lib$cpd.lib$mw
    new_adducts <- new_adduct_mzlist(mSetObj, mw)
    
    cpd.lib <- list(
      mz.mat = new_adducts,
      mw = mummichog.lib$cpd.lib$mw,
      id = mummichog.lib$cpd.lib$id,
      name = mummichog.lib$cpd.lib$name
    );
    
  }else{
    cpd.lib <- list(
      mz.mat = mummichog.lib$cpd.lib$adducts[[mSetObj$dataSet$mode]],
      mw = mummichog.lib$cpd.lib$mw,
      id = mummichog.lib$cpd.lib$id,
      name = mummichog.lib$cpd.lib$name
    );
  }
  
  cpd.tree <- mummichog.lib$cpd.tree[[mSetObj$dataSet$mode]];
  
  mSetObj$pathways <- mummichog.lib$pathways;
  mSetObj$lib.gsea.organism <- lib; #keep track of lib organism for sweave report
  
  mSetObj <- .search.compoundLib(mSetObj, cpd.lib, cpd.tree);

  mSetObj <- .compute.mummichog.fgsea(mSetObj, permNum);
  mummichog.lib <- NULL;
  
  mSetObj$mummi.anal <- "fgsea"
  
  return(.set.mSet(mSetObj));
}

.compute.mummichog.fgsea <- function(mSetObj, permNum){

  num_perm <- permNum;
  total_cpds <- mSetObj$cpd_exp #scores from all matched compounds
  
  current.mset <- mSetObj$pathways$cpds; #all compounds per pathway
  names(current.mset) <- mSetObj$pathways$name
  path.size <- unlist(lapply(mSetObj$pathways$cpds, length)) #total size of pathways
  
  df.scores <- data.frame(id=names(total_cpds), scores=total_cpds)
  ag.scores <- aggregate(id ~ scores, data = df.scores, paste, collapse = "; ")
  
  ag.sorted <- ag.scores[order(-ag.scores$scores),]
  row.names(ag.sorted) <- NULL
  
  dt.scores <- data.table::data.table(ag.sorted)
  dt.scores.out <- dt.scores[, list(scores=scores, id = unlist(strsplit(id, "; ", fixed = TRUE))), by=1:nrow(dt.scores)]
  
  rank.vec <- as.numeric(dt.scores.out$nrow)
  names(rank.vec) <- as.character(dt.scores.out$id)
  
  scores.vec <- as.numeric(ag.sorted$scores)
  names(scores.vec) <- as.character(ag.sorted$id)
  
  # run fgsea
  fgseaRes <- fgsea2(mSetObj, current.mset, scores.vec, rank.vec, num_perm)
  
  res.mat <- matrix(0, nrow=length(fgseaRes$pathway), ncol=6)
  
  path.size <- unlist(lapply(current.mset, length))
  matched.size <- path.size[match(fgseaRes$pathway, names(path.size))]
  
  # create result table
  res.mat[,1] <- matched.size
  res.mat[,2] <- fgseaRes$size
  res.mat[,3] <- fgseaRes$pval
  res.mat[,4] <- fgseaRes$padj
  res.mat[,5] <- fgseaRes$NES
  
  rownames(res.mat) <- fgseaRes$pathway
  colnames(res.mat) <- c("Pathway_Total", "Hits", "P_val", "P_adj", "NES", "Leading_Edge")
  
  # order by p-values
  ord.inx <- order(res.mat[,4]);
  res.mat <- signif(as.matrix(res.mat[ord.inx, ]), 4);
  
  res.mat[,6] <- as.character(fgseaRes$leadingEdge)[ord.inx]
  
  mSetObj$mummi.gsea.resmat <- res.mat;
  
  write.csv(res.mat, file="mummichog_fgsea_pathway_enrichment.csv", row.names=TRUE);
  return(mSetObj);
}

##### For local only ####

#' PlotIntegPaths
#' @description Plot both the original mummichog and the GSEA results 
#' @param format Character, input the format of the image to create.
#' @param dpi Numeric, input the dpi of the image to create.
#' @param width Numeric, input the width of the image to create.
#' @param Labels Character, indicate if the plot should be labeled. By default
#' it is set to "default", and the 5 top-ranked pathways per each algorithm will be plotted.
#' Users can adjust the number of pathways to be annotated per pathway using the "labels.x" 
#' and "labels.y" parameters.
#' Users can set this to "none" for no annotations, or "all" to annotate all pathways. 
#' @param labels.x Numeric, indicate the number of top-ranked pathways using the fGSEA algorithm 
#'  to annotate on the plot. 
#' @param labels.y Numeric, indicate the number of top-ranked pathways using the original 
#' mummichog algorithm to annotate on the plot. 
#' @author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#' McGill University, Canada
#' License: GNU GPL (>= 2)
#' @export
#' @import ggplot2
#' @import plotly

PlotIntegPaths <- function(mSetObj=NA, format = "png", dpi = 72, width = 9, labels = "default", labels.x = 5,
                           labels.y = 5){
  
  mSetObj <- .get.mSet(mSetObj);
  
  if(.on.public.web==TRUE){
    return(0)
  }
  
  ora.all <- mSetObj$mummi.resmat
  ora.lib <- mSetObj$lib.organism
  
  gsea.all <- mSetObj$mummi.gsea.resmat
  gsea.lib <- mSetObj$lib.gsea.organism
  
  # check if mummichog + gsea was performed
  if(is.null(ora.all) | is.null(gsea.all)){
    print("Both mummichog and fGSEA must be performed!")
    return(0)
  }
  
  # check if same libraries were used
  if(ora.lib != gsea.lib){
    print("The same pathway library must be used for mummichog and fGSEA!")
    return(0)
  }
  
  ora.pval <- ora.all[,5]
  ora.paths <- names(ora.pval)
  ora.mat <- cbind(ora.paths, ora.pval)
  
  gsea.pval <- gsea.all[,3]
  gsea.paths <- names(gsea.pval)
  gsea.mat <- cbind(gsea.paths, gsea.pval)

  if(length(ora.paths)>length(gsea.paths)){
    # ora paths is bigger, need to subset
    matched_paths <- na.omit(match(gsea.paths, ora.paths))
    matched_ora <- ora.mat[matched_paths,]
    mum.matrix <- cbind(matched_ora, gsea.mat)
  }else{
    # gsea paths is bigger, need to subset 
    matched_paths <- na.omit(match(ora.paths, gsea.paths))
    matched_gsea <- gsea.mat[matched_paths,]
    mum.matrix <- cbind(ora.mat, matched_gsea)
    mum.df <- data.frame(pathways = mum.matrix[,1], mummichog = as.numeric(mum.matrix[,2]), 
                         gsea = as.numeric(mum.matrix[,4]))
  }
  
  # Sort values based on mummichog pvalues
  y <- -log(mum.df$mummichog);
  y <- scales::rescale(y, c(0,4))
  
  x <- -log(mum.df$gsea);
  x <- scales::rescale(x, c(0,4))
  
  inx <- order(y, decreasing= T);
  
  x <- x[inx]; 
  y <- y[inx];
  path.nms <- mum.df$pathways[inx];
  
  min.x<- min(x, na.rm = TRUE);
  max.x <- max(x, na.rm = TRUE);
  
  if(min.x == max.x){ # only 1 value
    max.x = 1.5*max.x;
    min.x = 0.5*min.x;
  }
  
  maxR <- (max.x - min.x)/40;
  minR <- (max.x - min.x)/160;
  radi.vec <- minR+(maxR-minR)*(x-min.x)/(max.x-min.x);
  
  # set background color according to y
  bg.vec <- heat.colors(length(y));
  
  if(format == "png"){
    bg = "transparent";
  }else{
    bg="white";
  }
  
  if(is.na(width)){
    w <- 7;
  }else if(width == 0){
    w <- 7;
  }else{
    w <- width;
  }
  h <- w;
  
  df <- data.frame(path.nms, x, y)
  
  if(labels == "default"){
    mummi.inx <- GetTopInx(df$y, labels.y, T)
    gsea.inx <- GetTopInx(df$x, labels.x, T)
    all.inx <- mummi.inx | gsea.inx;
  }

  imgName = paste("integ_path_plot", "dpi", dpi, ".", format, sep="");
  
  Cairo::Cairo(file = imgName, unit="in", dpi=dpi, width=w, height=h, type=format, bg=bg);
  op <- par(mar=c(6,5,2,3));
  plot(x, y, type="n", axes=F, xlab="GSEA -log(p)", ylab="Mummichog -log(p)");
  axis(1);
  axis(2);
  symbols(x, y, add = TRUE, inches = F, circles = radi.vec, bg = bg.vec, xpd=T);
  
  if(labels=="default"){
    text(x[all.inx], y[all.inx], labels = path.nms[all.inx], pos=3, xpd=T, cex=0.8)
  }else if(labels == "all"){
    text(x, y, labels = path.nms, pos=3, xpd=T, cex=0.8)
  }
  
  par(op);
  dev.off();
  
  return(mSetObj);
}

# Function to return the unique m/zs from the selected pathways 
# based on the compounds

GetMummichogMZHits <- function(mSetObj=NA, msetNm){
  
  mSetObj <- .get.mSet(mSetObj);
  inx <- which(mSetObj$pathways$name == msetNm)
  mset <- mSetObj$pathways$cpds[[inx]];
  
  mzs <- as.numeric(unique(unlist(mSetObj$cpd2mz_dict[mset])))
  
  result <- intersect(mzs, mSet$dataSet$input_mzlist)

  return(result)
}

##### For Web ################

GetMatchingDetails <- function(mSetObj=NA, cmpd.id){
  
   mSetObj <- .get.mSet(mSetObj);
   forms <- mSetObj$cpd_form_dict[[cmpd.id]];
   tscores <- mSetObj$cpd_exp_dict[[cmpd.id]];
   # create html table
   res <- paste("<li>", "<b>", forms, "</b>: ", tscores, "</li>",sep="", collapse="");
   return(res);
}

GetMummichogHTMLPathSet <- function(mSetObj=NA, msetNm){
  
    mSetObj <- .get.mSet(mSetObj);
    inx <- which(mSetObj$pathways$name == msetNm)
    mset <- mSetObj$pathways$cpds[[inx]];
    hits.all <- unique(mSetObj$total_matched_cpds) #matched compounds
    hits.sig <- mSetObj$input_cpdlist;

    # highlighting with different colors
    refs <- mset %in% hits.all;
    sigs <- mset %in% hits.sig;
    
    red.inx <- which(sigs);
    blue.inx <- which(refs & !sigs);

    # use actual cmpd names
    #nms <- names(mset);
    nms <- mset;
    nms[red.inx] <- paste("<font color=\"red\">", "<b>", nms[red.inx], "</b>", "</font>",sep="");
    nms[blue.inx] <- paste("<font color=\"blue\">", "<b>", nms[blue.inx], "</b>", "</font>",sep="");
    return(cbind(msetNm, paste(nms, collapse="; ")));
}

GetMummiResMatrix <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi.resmat);
}

GetMummiResRowNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(rownames(mSetObj$mummi.resmat));
}

GetMummiResColNames <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(colnames(mSetObj$mummi.resmat));
}

GetCurrencyMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi$curr.msg)
}

GetAdductMsg <- function(mSetObj=NA){
  mSetObj <- .get.mSet(mSetObj);
  return(mSetObj$mummi$add.msg)
}

################## Utility Functions #########
# Global variables define currency compounds
currency <- c('C00001', 'C00080', 'C00007', 'C00006', 'C00005', 'C00003',
              'C00004', 'C00002', 'C00013', 'C00008', 'C00009', 'C00011',
              'G11113', '', 'H2O', 'H+', 'Oxygen', 'NADP+', 'NADPH', 'NAD+', 'NADH', 'ATP',
              'Pyrophosphate', 'ADP', 'Orthophosphate', 'CO2');

# mz tolerance based on instrument type
# input: a vector of mz,
# output: a vector of distance tolerance
# Review on mass accuracy by Fiehn: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1464138/

mz_tolerance <- function(mz, ms.type){
  if(tolower(ms.type) == "five"){
    return(0.000005*mz)
  }else if(tolower(ms.type) == "three"){
    return(0.000003*mz)
  }else if(tolower(ms.type) == "one"){
    return(0.000001*mz)
  }else if(tolower(ms.type) == "pointone"){
    return(0.0000001*mz)
  }else{
    return(0.000010*mz)
  }
}

#'Utility function to create compound lists for permutation analysis
#'@description From a vector of m/z features, this function outputs a vector of compounds.
#'@usage make_cpdlist(mSetObj=NA, input_mzs)
#'@param mSetObj Input the name of the created mSetObj
#'@param input_mzs The vector of randomly drawn m/z features.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
make_cpdlist <- function(mSetObj=NA, input_mzs){
  cpd <- unique(unlist(mSetObj$mz2cpd_dict[input_mzs]));
  cpd <- cpd[!is.null(cpd)];
  cpd <- cpd[!(cpd %in% currency)];
  
  return(cpd);
}

# Utility function to adjust for the fact that a single m/z feature can match to several compound identifiers
# input: a vector of compound ids
# output: a length of unique mzs corresponding to those compounds

count_cpd2mz <- function(cpd2mz_dict, cpd.ids,  inputmzlist){ # inputmz is either t or input cpd_list and cpd.ids are overlap features
  
  if(length(cpd.ids)==0){
    return(0);
  }
  mzs <- as.numeric(unique(unlist(cpd2mz_dict[cpd.ids])));
  if(length(mzs)==0){
    return(0);
  }else{
    result <- intersect(mzs, inputmzlist); #intersect to only mzs from the input mz list
    return(length(result));
  }
}

########### Utility Functions #########

# convert single element vector in list to matrix
# b/c single element vector will convert to scalar in javascript, force to matrix
convert2JsonList <- function(my.list){
  lapply(my.list, function(x){
    if(length(x) == 1){
      matrix(x);
    }else{
      x;
    }
  });
}

# input: a two-col (id, val) data with potential duplicates (same id may be associated with 1 or more values
# output: a list named by unique id, with multiple values will be merged to vector
Covert2Dictionary <- function(data, quiet=T){
  
  all.ids <- data[,1];
  dup.inx <- duplicated(all.ids);
  if(sum(dup.inx) > 0){
    uniq.ids <- all.ids[!dup.inx];
    uniq.vals <- data[!dup.inx,2];
    
    # convert two-col data it to list (vals as list values, ids as list names)
    uniq.list <- split(uniq.vals, uniq.ids)
    
    # the list element orde will be sorted by the names alphabetically, need to get updated ones
    uniq.id.list <- names(uniq.list)
    
    dup.ids <- all.ids[dup.inx];
    uniq.dupids <- unique(dup.ids);
    uniq.duplen <- length(uniq.dupids);
    
    for(id in uniq.dupids){ # only update those with more than one hits
      hit.inx.all <- which(all.ids == id);
      hit.inx.uniq <- which(uniq.id.list == id);
      uniq.list[[hit.inx.uniq]]<- data[hit.inx.all,2];
    }
    
    AddMsg(paste("A total of ", sum(dup.inx), " of duplicates were merged.", sep=""));
    return(uniq.list);
  }else{
    AddMsg("All IDs are unique.");
    uniq.list <- split(data[,2], data[,1]);
    return(uniq.list);
  }
}

# Calculate p-values for each Lperm
# Used in higher mummichogR functions
ComputeMummichogPermPvals <- function(input_cpdlist, total_matched_cpds, pathways, matches.res, input_mzlist, cpd2mz_dict){

  ora.vec <- input_cpdlist; #Lperm
  query_set_size <- length(ora.vec)
  current.mset <- pathways$cpds; #all
  total_cpds <- unique(total_matched_cpds) #matched compounds
  total_feature_num <- length(total_cpds)
  
  size <- negneg <- vector(mode="list", length=length(current.mset));
  
  cpds <- lapply(current.mset, function(x) intersect(x, total_cpds)); # pathways & all ref cpds
  feats <- lapply(current.mset, function(x) intersect(x, ora.vec)); #pathways & lsig
  feat_len <- unlist(lapply(feats, length)); # length of overlap features
  set.num <- unlist(lapply(cpds, length)); #cpdnum
  
  negneg <- sizes <- vector(mode="list", length=length(current.mset));
  
  for(i in 1:length(current.mset)){ # for each pathway
    sizes[[i]] <- min(feat_len[i], count_cpd2mz(cpd2mz_dict, unlist(feats[i]), input_mzlist))
    negneg[[i]] <- total_feature_num + sizes[[i]] - set.num[i] - query_set_size;
  }

  unsize <- as.integer(unlist(sizes))
  res.mat <- matrix(0, nrow=length(current.mset), ncol=1)
  fishermatrix <- cbind(unsize, (set.num - unsize), (query_set_size - unsize), unlist(negneg))
  res.mat[,1] <- apply(fishermatrix, 1, function(x) fisher.test(matrix(x, nrow = 2))$p.value);
  perm_records <- list(res.mat, as.matrix(unsize));
  return(perm_records);
}

# utility function for fast list expanding (dynamic length)
# We need to repeatedly add an element to a list. With normal list concatenation
# or element setting this would lead to a large number of memory copies and a
# quadratic runtime. To prevent that, this function implements a bare bones
# expanding array, in which list appends are (amortized) constant time.
# https://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time-o1

myFastList <- function(capacity = 100) {
  buffer <- vector('list', capacity)
  names <- character(capacity)
  length <- 0
  methods <- list()
  
  methods$double.size <- function() {
    buffer <<- c(buffer, vector('list', capacity))
    names <<- c(names, character(capacity))
    capacity <<- capacity * 2
  }
  
  methods$add <- function(name, val) {
    if(length == capacity) {
      methods$double.size()
    }
    
    length <<- length + 1
    buffer[[length]] <<- val
    names[length] <<- name
  }
  
  methods$as.list <- function() {
    b <- buffer[0:length]
    names(b) <- names[0:length]
    return(b)
  }
  
  methods
}

####
####
#### from the fgsea R package, minor edits to adapt to untargeted metabolomics

#'Pre-ranked gsea adapted for untargeted metabolomics
#'@export
#'@import fgsea

fgsea2 <- function(mSetObj, pathways, stats, ranks,
                   nperm,
                   minSize=1, maxSize=Inf,
                   nproc=0,
                   gseaParam=1,
                   BPPARAM=NULL) {
  
  # Warning message for ties in stats
  ties <- sum(duplicated(stats[stats != 0]))
  if (ties != 0) {
    warning("There are ties in the preranked stats (",
            paste(round(ties * 100 / length(stats), digits = 2)),
            "% of the list).\n",
            "The order of those tied m/z features will be arbitrary, which may produce unexpected results.")
  }
  
  # Warning message for duplicate gene names
  if (any(duplicated(names(stats)))) {
    warning("There are duplicate m/z feature names, fgsea may produce unexpected results")
  }
  
  granularity <- 1000
  permPerProc <- rep(granularity, floor(nperm / granularity))
  if (nperm - sum(permPerProc) > 0) {
    permPerProc <- c(permPerProc, nperm - sum(permPerProc))
  }
  seeds <- sample.int(10^9, length(permPerProc))
  
  if (is.null(BPPARAM)) {
    if (nproc != 0) {
      if (.Platform$OS.type == "windows") {
        # windows doesn't support multicore, using snow instead
        BPPARAM <- BiocParallel::SnowParam(workers = nproc)
      } else {
        BPPARAM <- BiocParallel::MulticoreParam(workers = nproc)
      }
    } else {
      BPPARAM <- BiocParallel::bpparam()
    }
  }
  
  minSize <- max(minSize, 1)
  stats <- abs(stats) ^ gseaParam
  
  # returns list of indexs of matches between pathways and rank names
  pathwaysPos <- lapply(pathways, function(p) { as.vector(na.omit(fastmatch::fmatch(p, names(ranks)))) })
  pathwaysFiltered <- lapply(pathwaysPos, function(s) { ranks[s] })
  
  # adjust for the fact that a single m/z feature can match to several compound identifiers
  pathway2mzSizes <- sapply(pathways, function(z) { length(intersect(as.numeric(unique(unlist(mSetObj$cpd2mz_dict[z]))), mSetObj$dataSet$input_mzlist))} )
  oldpathwaysSizes <- sapply(pathwaysFiltered, length)
  
  pathwaysSizes <- pmin(pathway2mzSizes, oldpathwaysSizes)
  
  toKeep <- which(minSize <= pathwaysSizes & pathwaysSizes <= maxSize)
  m <- length(toKeep)
  
  if (m == 0) {
    return(data.table(pathway=character(),
                      pval=numeric(),
                      padj=numeric(),
                      ES=numeric(),
                      NES=numeric(),
                      nMoreExtreme=numeric(),
                      size=integer(),
                      leadingEdge=list()))
  }
  
  pathwaysFiltered <- pathwaysFiltered[toKeep]
  pathwaysSizes <- pathwaysSizes[toKeep]
  
  K <- max(pathwaysSizes)
  
  #perform gsea
  gseaStatRes <- do.call(rbind,
                         lapply(pathwaysFiltered, fgsea::calcGseaStat,
                                stats=stats,
                                returnLeadingEdge=TRUE))
  
  leadingEdges <- mapply("[", list(names(stats)), gseaStatRes[, "leadingEdge"], SIMPLIFY = FALSE)
  pathwayScores <- unlist(gseaStatRes[, "res"])
  
  #perform permutations
  universe <- seq_along(stats)
  
  counts <- BiocParallel::bplapply(seq_along(permPerProc), function(i) {
    nperm1 <- permPerProc[i]
    leEs <- rep(0, m)
    geEs <- rep(0, m)
    leZero <- rep(0, m)
    geZero <- rep(0, m)
    leZeroSum <- rep(0, m)
    geZeroSum <- rep(0, m)
    if (m == 1) {
      for (i in seq_len(nperm1)) {
        randSample <- sample.int(length(universe), K)
        randEsP <- fgsea::calcGseaStat(
          stats = stats,
          selectedStats = randSample,
          gseaParam = 1)
        leEs <- leEs + (randEsP <= pathwayScores)
        geEs <- geEs + (randEsP >= pathwayScores)
        leZero <- leZero + (randEsP <= 0)
        geZero <- geZero + (randEsP >= 0)
        leZeroSum <- leZeroSum + pmin(randEsP, 0)
        geZeroSum <- geZeroSum + pmax(randEsP, 0)
      }
    } else {
      aux <- fgsea:::calcGseaStatCumulativeBatch(
        stats = stats,
        gseaParam = 1,
        pathwayScores = pathwayScores,
        pathwaysSizes = pathwaysSizes,
        iterations = nperm1,
        seed = seeds[i])
      leEs = get("leEs", aux)
      geEs = get("geEs", aux)
      leZero = get("leZero", aux)
      geZero = get("geZero", aux)
      leZeroSum = get("leZeroSum", aux)
      geZeroSum = get("geZeroSum", aux)
    }
    data.table::data.table(pathway=seq_len(m),
                           leEs=leEs, geEs=geEs,
                           leZero=leZero, geZero=geZero,
                           leZeroSum=leZeroSum, geZeroSum=geZeroSum
    )
  }, BPPARAM=BPPARAM)
  
  counts <- data.table::rbindlist(counts)
  
  # Getting rid of check NOTEs
  leEs=leZero=geEs=geZero=leZeroSum=geZeroSum=NULL
  pathway=padj=pval=ES=NES=geZeroMean=leZeroMean=NULL
  nMoreExtreme=nGeEs=nLeEs=size=NULL
  leadingEdge=NULL
  .="damn notes"
  
  pval <- unlist(lapply(counts$pathway, function(c) min((1+sum(counts[c,]$leEs)) / (1 + sum(counts[c,]$leZero)),
                                                        (1+sum(counts[c,]$geEs)) / (1 + sum(counts[c,]$geZero)))))
  
  leZeroMean <- unlist(lapply(counts$pathway, function(d) sum(counts[d,]$leZeroSum) / sum(counts[d,]$leZero)))
  geZeroMean <- unlist(lapply(counts$pathway, function(e) sum(counts[e,]$geZeroSum) / sum(counts[e,]$geZero)))
  nLeEs <- unlist(lapply(counts$pathway, function(f) sum(counts[f,]$leEs)))
  nGeEs <- unlist(lapply(counts$pathway, function(g) sum(counts[g,]$geEs)))
  
  pvals <- data.frame(pval=pval, leZeroMean=leZeroMean, geZeroMean=geZeroMean, nLeEs=nLeEs, nGeEs=nGeEs)
  
  padj <- p.adjust(pvals$pval, method="fdr")
  ES <- pathwayScores
  NES <- ES / ifelse(ES > 0, pvals$geZeroMean, abs(pvals$leZeroMean))
  pvals$leZeroMean <- NULL
  pvals$geZeroMean <- NULL
  
  nMoreExtreme <- ifelse(ES > 0, pvals$nGeEs, pvals$nLeEs)
  pvals$nLeEs <- NULL
  pvals$nGeEs <- NULL
  
  size <- pathwaysSizes
  pathway <- names(pathwaysFiltered)
  
  leadingEdge <- sapply(leadingEdges, paste0, collapse = "; ")
  leadingEdge2 <- sapply(leadingEdge, function(x) strsplit(x, "; "))
  pathway.cpds <- sapply(pathwaysFiltered, attributes)
  
  matches <- mapply(intersect, leadingEdge2, pathway.cpds)
  
  leadingEdgeMatched <- sapply(matches, paste0, collapse = "; ")
  
  pvals.done <- cbind(pathway, pvals, padj, ES, NES, nMoreExtreme, size, leadingEdgeMatched)
  
  return(pvals.done)
}

calcGseaStat2 <- function(stats, selectedStats, gseaParam=1,
                          returnAllExtremes=FALSE,
                          returnLeadingEdge=FALSE) {
  
  S <- selectedStats
  r <- stats
  p <- gseaParam
  
  S <- sort(S)
  
  # account for 1 mz can be multiple cpds
  S.scores <- r[S]
  u.S <- S[!duplicated(S.scores)]
  scores <- unique(S.scores)
  
  m <- length(scores)
  N <- length(r)
  
  if (m == N) {
    stop("GSEA statistic is not defined when all genes are selected")
  }
  
  NR <- (sum(abs(scores)^p))
  rAdj <- abs(scores)^p
  if (NR == 0) {
    # this is equivalent to rAdj being rep(eps, m)
    rCumSum <- seq_along(rAdj) / length(rAdj)
  } else {
    rCumSum <- cumsum(rAdj) / NR
  }
  
  tops <- rCumSum - (u.S - seq_along(u.S)) / (N - m)
  if (NR == 0) {
    # this is equivalent to rAdj being rep(eps, m)
    bottoms <- tops - 1 / m
  } else {
    bottoms <- tops - rAdj / NR
  }
  
  maxP <- max(tops)
  minP <- min(bottoms)
  
  if(maxP > -minP) {
    geneSetStatistic <- maxP
  } else if (maxP < -minP) {
    geneSetStatistic <- minP
  } else {
    geneSetStatistic <- 0
  }
  
  if (!returnAllExtremes && !returnLeadingEdge) {
    return(geneSetStatistic)
  }
  
  res <- list(res=geneSetStatistic)
  if (returnAllExtremes) {
    res <- c(res, list(tops=tops, bottoms=bottoms))
  }
  if (returnLeadingEdge) {
    leadingEdge <- if (maxP > -minP) {
      u.S[seq_along(u.S) <= which.max(bottoms)]
    } else if (maxP < -minP) {
      rev(u.S[seq_along(u.S) >= which.min(bottoms)])
    } else {
      NULL
    }
    
    res <- c(res, list(leadingEdge=leadingEdge))
  }
  res
}