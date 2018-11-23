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

    mSetObj$dataSet$orig <- cbind(input$p.value, input$m.z, input$t.score);
    mSetObj$msgSet$read.msg <- paste("A total of", length(input$p.value), "m/z features were found in your uploaded data.");
    return(.set.mSet(mSetObj));

}

#'Upates the mSetObj with user-selected parameters
#'@description This functions handles updating the mSet object for mummichog analysis. It is necessary to utilize this function
#'to specify to the organism's pathways to use (libOpt), the mass-spec mode (msModeOpt), mass-spec instrument (instrumentOpt), 
#'the p-value cutoff (pvalCutoff), and the number of permutations (permNum).
#'@usage UpdateMummichogParameters(mSetObj=NA, instrumentOpt, msModeOpt, pvalCutoff)
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects).
#'@param instrumentOpt Define the mass-spec instrument used to perform untargeted metabolomics.
#'@param msModeOpt Define the mass-spec mode of the instrument used to perform untargeted metabolomics.
#'@param pvalCutoff Numeric, specify the p-value cutoff to define significant m/z features from reference m/z features.
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export
UpdateMummichogParameters <- function(mSetObj=NA, instrumentOpt, msModeOpt, pvalCutoff){
  
  mSetObj <- .get.mSet(mSetObj);
  
  mSetObj$dataSet$instrument <- instrumentOpt;
  mSetObj$dataSet$mode <- msModeOpt;
  mSetObj$dataSet$cutoff <- pvalCutoff;
  mSetObj$custom <- FALSE
  
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
  ndat <- mSetObj$dataSet$orig;

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
  
  mSetObj$msgSet$check.msg <- msg.vec;
  mSetObj$dataSet$proc <- ndat;
  mSetObj$dataSet$input_mzlist <- input_mzlist;
  mSetObj$dataSet$expr_dic <- tscores;
  mSetObj$dataSet$N <- sig.size;
  mSetObj$dataSet$ref_mzlist <- ref_mzlist;
  
  return(.set.mSet(mSetObj));
  
}

#'Main function to perform mummichog
#'@description This is the main function that performs the mummichog analysis. 
#'@usage PerformMummichog(mSetObj=NA, lib, enrichOpt, pvalOpt, permNum = 100)
#'@param mSetObj Input the name of the created mSetObj object 
#'@param lib Input the name of the organism library, default is hsa 
#'@param enrichOpt Input the method to perform enrichment analysis
#'@param pvalOpt Input the method to calculate p-values
#'@param permNum Numeric, the number of permutations to perform
#'@author Jasmine Chong, Jeff Xia \email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: GNU GPL (>= 2)
#'@export

PerformMummichog <- function(mSetObj=NA, lib, enrichOpt, pvalOpt, permNum = 100){
  
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
    mSetObj <- .compute.mummichogSigPvals(mSetObj, enrichOpt, pvalOpt);
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
#'@usage PerformAdductMapping(mSetObj=NA, add.mode)
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
.compute.mummichogSigPvals <- function(mSetObj, enrichOpt, pvalOpt){
  
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
