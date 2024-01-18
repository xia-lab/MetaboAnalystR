.search_MS2compoundLib <- function(mSetObj, 
                                   cpd.lib, 
                                   cpd.treep, 
                                   cpd.treen){
  
  ref_mzlist <- as.numeric(mSetObj$dataSet$ref_mzlist);
  ref_cmpdlist <- as.matrix(mSetObj$dataSet$ref_cmpdlist);
  
  print(paste0("Got ", length(ref_mzlist), " mass features."))
  print(paste0("Got ", length(ref_cmpdlist[ref_cmpdlist != ""]), " Compound features."))
  
  pos_inx <- mSetObj$dataSet$pos_inx;
  
  # split all mzs (users' input) based on Modes
  ref_mzlistp <- ref_mzlist[pos_inx];
  ref_mzlistn <- ref_mzlist[!pos_inx];
  
  # split all compounds identified (users' input) from MS2
  ref_cmpdlist <- mSetObj$dataSet$ref_cmpdlist;
  ref_cmpdlistp <- ref_cmpdlist[pos_inx,];
  ref_cmpdlistn <- ref_cmpdlist[!pos_inx,];
  
  version <- mSetObj$paramSet$version;
  
  # for empirical compounds
  if(mSetObj$paramSet$mumRT & version=="v2"){
    ord_rt <- rank(mSetObj$dataSet$ret_time, ties.method = "random")
    ret_time_pos <- mSetObj$dataSet$ret_time[pos_inx];
    ret_time_rank_pos <- ord_rt[pos_inx];
    ret_time_neg <- mSetObj$dataSet$ret_time[!pos_inx];
    ret_time_rank_neg <- ord_rt[!pos_inx];
    rt_tol <- mSetObj$dataSet$rt_tol;
    rt_tol_rank <- length(ref_mzlist)*mSetObj$dataSet$rt_frac;
  } else {
    # add fake RT
    ret_time_pos <- rep(1, length(ref_mzlistp))
    ret_time_rank_pos <- rep(1, length(ref_mzlistp))
    ret_time_neg <- rep(1, length(ref_mzlistn))
    ret_time_rank_neg <- rep(1, length(ref_mzlistn))
  }
  
  modified.statesp <- colnames(cpd.lib$mz.matp);
  modified.statesn <- colnames(cpd.lib$mz.matn);
  my.tolsp <- mz_tolerance(ref_mzlistp, mSetObj$dataSet$instrument);
  my.tolsn <- mz_tolerance(ref_mzlistn, mSetObj$dataSet$instrument);
  
  # get mz ladder (pos index)
  self.mzsp <- floor(ref_mzlistp);
  all.mzsp <- cbind(self.mzsp-1, self.mzsp, self.mzsp+1);
  # get mz ladder (neg index)
  self.mzsn <- floor(ref_mzlistn);
  all.mzsn <- cbind(self.mzsn-1, self.mzsn, self.mzsn+1);

  # matched_res will contain detailed result (cmpd.id. query.mass, mass.diff) for all mz;
  # use a high-performance variant of list
  matched_resp <- myFastList();
  matched_resn <- myFastList();
  
  if(mSetObj$dataSet$mode != "negative"){
    # processing data from ESI+ mode
    for(i in 1:length(ref_mzlistp)){
      mz <- ref_mzlistp[i];
      rt <- ret_time_pos[i];
      rt_rank <- ret_time_rank_pos[i];
      my.tol <- my.tolsp[i];
      all.mz <- all.mzsp[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treep[all.mz])));
      
      for(pos in pos.all){
        id <- cpd.lib$id[pos];
        mw.all <- cpd.lib$mz.matp[pos,]; #get modified mzs
        diffs <- abs(mw.all - mz); #modified mzs - mz original
        hit.inx <- which(diffs < my.tol);
        if(length(hit.inx)>0){
          for(spot in 1:length(hit.inx)){
            hit.pos <- hit.inx[spot];# need to match all
            index <- paste(mz, id, rt, hit.pos, sep = "___");
            matched_resp$add(index, c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesp[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
          }
        }
      }
    }
  }
  
  all.mzsn <<- all.mzsn
  
  if (mSetObj$dataSet$mode != "positive") {
    # processing data from ESI- mode
    for(i in 1:length(ref_mzlistn)){
      mz <- ref_mzlistn[i];
      rt <- ret_time_neg[i];
      rt_rank <- ret_time_rank_neg[i];
      my.tol <- my.tolsn[i];
      all.mz <- all.mzsn[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treen[all.mz])));
      
      for(pos in pos.all){
        id <- cpd.lib$id[pos]; # position of compound in cpd.tree
        mw.all <- cpd.lib$mz.matn[pos,]; #get modified mzs
        diffs <- abs(mw.all - mz); #modified mzs - mz original
        hit.inx <- which(diffs < my.tol);
        if(length(hit.inx)>0){
          for(spot in 1:length(hit.inx)){
            hit.pos <- hit.inx[spot];# need to match all
            index <- paste(mz, id, rt, hit.pos, sep = "___"); #name in fast_list
            matched_resn$add(index, c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesn[hit.pos], diffs[hit.pos])); #replaces previous when hit.inx>1
          }
        }
      }
    }
  }
  
  # convert to regular list
  if (mSetObj$dataSet$mode == "mixed") {
    
    matched_resn <- matched_resn$as.list();
    matched_resp <- matched_resp$as.list();
    
    neg_matches <- length(matched_resn) > 0
    pos_matches <- length(matched_resp) > 0
    
    if(!neg_matches & !pos_matches){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    if(neg_matches){
      matched_resn <- data.frame(matrix(unlist(matched_resn), 
                                        nrow=length(matched_resn), byrow=T), 
                                 stringsAsFactors = FALSE);
      print("Empirical Compounds Filtration based on MS/MS results..")
      # an extra filtration based on MS2 results
      matched_IDs <- matched_resn[,2];
      matched_ID_typed <- vapply(matched_IDs, function(x){
        idx <- cpd.lib$id == x;
        cpd.lib$ms2IDs[idx][1]
      }, FUN.VALUE = character(1L));
      
      keepRow.idx <- vapply(1:nrow(matched_resn), function(i){
        id <- matched_resn$X2[i];
        idx <- which(cpd.lib[["id"]]==id)
        dbTypedID <- cpd.lib$ms2IDs[idx];
        if(dbTypedID == "") {
          return(TRUE)
        } else if(all(ref_cmpdlistn[idx,] == "")) {
          return(TRUE)
        } else {
          if(any(dbTypedID %in% ref_cmpdlistn[idx,])) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }, logical(1L));
      print(paste0("Total of ",length(which(!keepRow.idx)),"/", length(keepRow.idx), " Empirical Compounds has been filtered from ESI- Mode!"))
      matched_resn <- matched_resn[keepRow.idx,];
      neg_matches <- length(matched_resn) > 0
    }
    
    if(pos_matches){
      matched_resp <- data.frame(matrix(unlist(matched_resp), 
                                        nrow=length(matched_resp), byrow=T), 
                                 stringsAsFactors = FALSE);
      
      print("Empirical Compounds Filtration based on MS/MS results..")
      # an extra filtration based on MS2 results
      matched_IDs <- matched_resp[,2];
      matched_ID_typed <- vapply(matched_IDs, function(x){
        idx <- cpd.lib$id == x;
        cpd.lib$ms2IDs[idx][1]
      }, FUN.VALUE = character(1L));
      
      keepRow.idx <- vapply(1:nrow(matched_resp), function(i){
        id <- matched_resp$X2[i];
        idx <- which(cpd.lib[["id"]]==id)
        dbTypedID <- cpd.lib$ms2IDs[idx];
        if(dbTypedID == "") {
          return(TRUE)
        } else if(all(ref_cmpdlistp[idx,] == "")) {
          return(TRUE)
        } else {
          if(any(dbTypedID %in% ref_cmpdlistp[idx,])) {
            return(TRUE)
          } else {
            return(FALSE)
          }
        }
      }, logical(1L));
      print(paste0("Total of ",length(which(!keepRow.idx)),"/", length(keepRow.idx), " Empirical Compounds has been filtered from ESI+ Mode!"))
      matched_resp <- matched_resp[keepRow.idx,];
      pos_matches <- length(matched_resp) > 0;
    }
    
    if(neg_matches & pos_matches){ # both w. matches
      matched_res <- rbind(matched_resp, matched_resn)
    } else if (neg_matches & !pos_matches) { # only neg w. matches
      matched_res <- matched_resn
    } else { # only pos w. matches
      matched_res <- matched_resp
    }
    
  } else if(mSetObj$dataSet$mode == "positive") {
    # ESI + mode
    matched_resp <- matched_resp$as.list();
    
    if(is.null(unlist(matched_resp))){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    matched_resp <- data.frame(matrix(unlist(matched_resp), nrow=length(matched_resp), byrow=T), stringsAsFactors = FALSE);
    matched_res <- matched_resp;
    print("Empirical Compounds Filtration based on MS/MS results..")
    # an extra filtration based on MS2 results
    matched_IDs <- matched_res[,2];
    matched_ID_typed <- vapply(matched_IDs, function(x){
      idx <- cpd.lib$id == x;
      cpd.lib$ms2IDs[idx][1]
    }, FUN.VALUE = character(1L));
    
    keepRow.idx <- vapply(1:nrow(matched_res), function(i){
      id <- matched_res$X2[i];
      idx <- which(cpd.lib[["id"]]==id)
      dbTypedID <- cpd.lib$ms2IDs[idx];
      if(dbTypedID == "") {
        return(TRUE)
      } else if(all(ref_cmpdlistp[idx,] == "")) {
        return(TRUE)
      } else {
        if(any(dbTypedID %in% ref_cmpdlistp[idx,])) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }, logical(1L));
    print(paste0("Total of ",length(which(!keepRow.idx)),"/", length(keepRow.idx), " Empirical Compounds has been filtered from ESI+ Mode!"))
    matched_res <- matched_res[keepRow.idx,];
    
  } else {
    # ESI - mode
    matched_resn <- matched_resn$as.list();
    if(is.null(unlist(matched_resn))){
      msg.vec <<- "No compound matches from upload peak list!"
      return(0)
    }
    
    matched_resn <- data.frame(matrix(unlist(matched_resn), nrow=length(matched_resn), byrow=T), stringsAsFactors = FALSE);
    matched_res <- matched_resn;
    print("Empirical Compounds Filtration based on MS/MS results..")
    # an extra filtration based on MS2 results
    matched_IDs <- matched_res[,2];
    matched_ID_typed <- vapply(matched_IDs, function(x){
      idx <- cpd.lib$id == x;
      cpd.lib$ms2IDs[idx][1]
    }, FUN.VALUE = character(1L));
    
    keepRow.idx <- vapply(1:nrow(matched_res), function(i){
      id <- matched_res$X2[i];
      idx <- which(cpd.lib[["id"]]==id)
      dbTypedID <- cpd.lib$ms2IDs[idx];
      if(dbTypedID == "") {
        return(TRUE)
      } else if(all(ref_cmpdlistn[idx,] == "")) {
        return(TRUE)
      } else {
        if(any(dbTypedID %in% ref_cmpdlistn[idx,])) {
          return(TRUE)
        } else {
          return(FALSE)
        }
      }
    }, logical(1L));
    print(paste0("Total of ",length(which(!keepRow.idx)),"/", length(keepRow.idx), " Empirical Compounds has been filtered from ESI- Mode!"))
    matched_res <- matched_res[keepRow.idx,];
  }
  
  # re-order columns for output
  matched_res <- matched_res[, c(3,2,7,8,4,5)];
  colnames(matched_res) <- c("Query.Mass", "Matched.Compound", "Matched.Form", "Mass.Diff", "Retention.Time", "RT.Rank");
  
  if(!mSetObj$paramSet$mumRT & version=="v2"){
    matched_res <- matched_res[,-(5:6)]
  }
  
  #print(paste0(length(unique(matched_res[,2])), " matched compounds! cpd2mz"))
  
  # now create empirical compounds if necessary!
  # 1 compound matches to multiple m/z, filter by RT 
  if(mSetObj$paramSet$mumRT & version=="v2"){
    start <- Sys.time()
    # mz, ion
    empirical.cpd.list <- split(matched_res[,c(1,3,5,6)], matched_res[,2]); # split mz, ion and rt by compound
    empirical.cpds2cpds <- vector(length=(length(empirical.cpd.list)), "list")
    names(empirical.cpds2cpds) <- names(empirical.cpd.list)
    
    # for each compound, if multiple matches, split into ECpds if > RT tolerance - rt_tol
    for(i in 1:length(empirical.cpd.list)){
      
      mzs <- empirical.cpd.list[[i]]$Query.Mass
      ions <- empirical.cpd.list[[i]]$Matched.Form
      rts <- empirical.cpd.list[[i]]$Retention.Time
      rt.rank <- empirical.cpd.list[[i]]$RT.Rank
      cpds <- names(empirical.cpd.list)[i]
      
      # first, for each compound, determine ECs among matched ions
      if(length(mzs)>1){ # if multiple ECs per compound
        
        # first group together to create empirical cpds by rt
        rts <- as.numeric(rts)
        names(rts) <- paste0(mzs, ";", ions, ";", rts, ";", cpds)
        rts <- sort(rts)
        
        # second, group together to create empirical cpds by rt rank
        rt.ranks <- as.numeric(rt.rank)
        names(rt.ranks) <- paste0(mzs, ";", ions, ";", rts, ";", cpds)
        rt.ranks <- sort(rt.ranks)
        
        split.inx <- c(0, cumsum(Reduce("&", list(abs(diff(rts)) > rt_tol, abs(diff(rt.ranks)) > rt_tol_rank) )))
        
        # need to deal w. multiple rts but only 1 EC
        if(length(unique(split.inx)) > 1){
          e.cpds <- split(rts, split.inx)
          empirical.cpds2cpds[[i]] <- lapply(e.cpds, names)
        }else{
          empirical.cpds2cpds[[i]] <- paste0(names(rts), collapse="__")
        }
        
      }else{ # if only 1 EC per compound
        empirical.cpds2cpds[[i]] <- paste0(mzs, ";", ions, ";", rts, ";", cpds)
      }
    }
    
    initial_ecs <- unlist(empirical.cpds2cpds, recursive=FALSE)
    names(initial_ecs) <- paste0("EC", 1:length(initial_ecs))
    print(paste0(length(initial_ecs), " initial ECs created!"))
    
    # second, merge ECs if same m/z and form - append compounds
    try <- melt(initial_ecs)
    try2 <- strsplit(as.character(try[,1]), split="__", fixed=TRUE) # deals with multiple rts belonging to 1 EC
    try2.df <- data.frame(value=unlist(try2), L1 = rep(try$L1, sapply(try2, length)))
    
    info <- strsplit(as.character(try2.df[,1]), split=";")
    df_ecs <- data.frame(ec=as.character(try2.df[,2]), mz=sapply(info, `[[`, 1), form=sapply(info, `[[`, 2), rt = sapply(info, `[[`, 3), cpd = sapply(info, `[[`, 4), stringsAsFactors = F)
    df_ecs$str_row_inx <- paste(df_ecs$mz, df_ecs$form, df_ecs$rt, sep = "___")
    qs::qsave(df_ecs, "initial_ecs.qs")
    merged_ecs <- aggregate(. ~ str_row_inx, df_ecs, paste, collapse=";")
    
    # cleaning the df
    # merged_ecs$ec <- sapply(strsplit(merged_ecs$ec, ";", fixed=TRUE), function(x) unlist(x)[1]) - keep as long name
    merged_ecs$mz <- sapply(strsplit(merged_ecs$mz, ";", fixed=TRUE), function(x) unique(unlist(x)))
    merged_ecs$form <- sapply(strsplit(merged_ecs$form, ";", fixed=TRUE), function(x) unique(unlist(x)))
    merged_ecs$rt <- sapply(strsplit(merged_ecs$rt, ";", fixed=TRUE), function(x) unique(unlist(x)))
    print(paste0(length(unique(merged_ecs$ec)), " merged ECs identified!"))
    
    # third, check if primary ion is present
    # needs to be per EC!
    if(mSetObj$dataSet$primary_ion=="yes"){
      
      ecs <- unique(merged_ecs$ec);
      
      # function to group ECs and verify if contains primary ion
      new_info <- lapply(ecs, function(x) { 
        new_info <- merged_ecs[which(merged_ecs$ec == x),] # subset merged_ecs to rows containing ECx
        primary.inx <- length(intersect(new_info$form, primary_ions))
        
        if(primary.inx>0){
          new_info <- new_info
        }else{
          new_info <- NULL
        }
        new_info
      })  
      
      final_ecs <- do.call(args=new_info, what=rbind)[,-1]
      
    }else{
      final_ecs <- merged_ecs[,-1]
    }
    
    colnames(final_ecs) <- c("Empirical.Compound", "Query.Mass", "Matched.Form", "Retention.Time", "Matched.Compound")
    
    # transform to long format
    cpd_split <- strsplit(as.character(final_ecs$Matched.Compound), ";", fixed=TRUE)
    reps <- pmax(lengths(cpd_split))
    df2 <- final_ecs[rep(1:nrow(final_ecs), reps), 1:4]
    df2$Matched.Compound <- unlist(mapply(function(x,y) c(x, rep(NA, y)), cpd_split, reps-lengths(cpd_split)))
    
    matched_res <- merge(matched_res, df2)
    matched_res <- matched_res[,-6] #rm rt rank
    matched_res[,6] <- as.character(matched_res[,6])
    
    # now deal with the fact that if at least one EC overlap, need to count as same EC per compound...
    my_final_cpds <- aggregate(. ~ Matched.Compound, matched_res, paste, collapse="___")
    my_final_cpds_list <- lapply(split(my_final_cpds$Empirical.Compound, my_final_cpds$Matched.Compound), unlist)
    
    cpd2ec1 <- lapply(seq_along(my_final_cpds_list), function(x) { # function used to make grouping of ecs per cpd
      
      ecs <- unlist(strsplit(my_final_cpds_list[[x]], "___", fixed=TRUE))
      
      if(length(ecs) > 1){
        ecs.list <- as.list(strsplit(ecs, ";", fixed=TRUE))
        library(igraph)
        m = sapply(ecs.list, function(x) sapply(ecs.list, function(y) length(intersect(x,y))>0))
        g = igraph::groups(components(graph_from_adjacency_matrix(m)))
        ecs <- paste0(sapply(g, function(z) paste0(ecs[z], collapse = "|") ), collapse = "___")
      }
      ecs
    })
    
    names(cpd2ec1) <- names(my_final_cpds_list)
    
    update_ecs <- lapply(seq_along(cpd2ec1), function(z) {
      
      ecs.old <- unlist(strsplit(my_final_cpds_list[[z]], "___", fixed=TRUE))
      ecs.new <- unlist(strsplit(cpd2ec1[[z]], "___", fixed=TRUE))
      
      for(i in seq_along(ecs.new)){
        pattern <- ecs.new[i]
        pattern_vec <- unlist(strsplit(pattern, "\\|"))
        up.pattern <- paste0(unique(pattern_vec), collapse = "|")
        ecs.old[ ecs.old %in% pattern_vec  ] <- up.pattern
      }
      
      ecs.old <- paste0(ecs.old, collapse = "___")
      ecs.old
    })
    
    updated_ecs <- do.call(rbind, update_ecs)
    my_final_cpds$Empirical.Compound <- updated_ecs
    
    new_dt <- data.table::data.table(my_final_cpds)
    new_dt <- new_dt[, list(Query.Mass = unlist(strsplit(as.character(Query.Mass), "___", fixed=TRUE)), 
                            Matched.Form = unlist(strsplit(as.character(Matched.Form), "___", fixed=TRUE)),
                            Retention.Time = unlist(strsplit(as.character(Retention.Time), "___", fixed=TRUE)),
                            Mass.Diff = unlist(strsplit(as.character(Mass.Diff), "___", fixed=TRUE)),
                            Empirical.Compound = unlist(strsplit(as.character(Empirical.Compound), "___", fixed=TRUE))),
                     by = Matched.Compound]
    
    matched_res <- data.frame(Query.Mass = new_dt$Query.Mass, Matched.Compound = new_dt$Matched.Compound, Matched.Form = new_dt$Matched.Form,
                              Retention.Time = new_dt$Retention.Time, Mass.Diff = new_dt$Mass.Diff, Empirical.Compound = new_dt$Empirical.Compound, stringsAsFactors = FALSE)
    
    # make EC names
    ec <- matched_res$Empirical.Compound
    ec.unique <- unique(matched_res$Empirical.Compound)
    
    for(i in seq_along(ec.unique)){
      ec <- replace(ec, grep(paste0("\\b", ec.unique[i], "\\b"), ec, perl=TRUE), paste0("EC000", i))
    }
    
    matched_res$Empirical.Compound <- gsub("\\|.*", "", ec)
    end <- Sys.time()
    totaltime <- end-start
    print(paste0(length(unique(matched_res$Empirical.Compound)), " empirical compounds identified in ", totaltime, " seconds."))
  }
  
  fast.write.csv(matched_res, file="mummichog_matched_compound_all.csv", row.names=FALSE);
  qs::qsave(matched_res, "mum_res.qs");
  
  # now update expr. profile
  matched_mz <- matched_res[,1];
  matched_ts <- mSetObj$dataSet$expr_dic[matched_mz];
  
  if(mSetObj$paramSet$mumRT & version=="v2") { # RT need to be in EC space
    # first create ecpd to expression dict
    ec.exp.mat <- data.frame(key=matched_res[,6], 
                             value=as.numeric(matched_ts), 
                             stringsAsFactors = F)
    ec_exp_dict <- Convert2Dictionary(ec.exp.mat);
    ec.exp.vec <- unlist(lapply(ec_exp_dict, max));
    
    # also need to make cpd_exp_dict for KEGG network view
    exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    
    # ecpd to cpd dict
    cpd_ecpd_dict <- Convert2Dictionary(matched_res[,c(2,6)])
    ecpd_cpd_dict <- Convert2Dictionary(matched_res[,c(6,2)])
    
    # now mz 2 ecpd dict
    mz2cpd_dict <- Convert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
    mz2ec_dict <- Convert2Dictionary(matched_res[,c(1,6)])
    ec2mz_dict <- Convert2Dictionary(matched_res[,c(6,1)])
    
    # save to mSetObj
    mSetObj$ec_exp_dict <- ec_exp_dict
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$ec_exp <- ec.exp.vec
    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$mz2ec_dict <- mz2ec_dict
    mSetObj$ec2mz_dict <- ec2mz_dict
    mSetObj$ecpd_cpd_dict <- ecpd_cpd_dict
    mSetObj$cpd_ecpd_dict <- cpd_ecpd_dict
    mSetObj$cpd_ecpd_counts <- cpd2ec1
    
    # now do matching to identify significant input_ecpdlist
    refmz <- names(mz2ec_dict)
    hits.index <- which(refmz %in% as.character(mSetObj$dataSet$input_mzlist));
    ec1 <- unique(unlist(mz2ec_dict[hits.index]));
    mSetObj$input_ecpdlist <- ec1;
    mSetObj$total_matched_ecpds <- unique(as.vector(matched_res$Empirical.Compound));
    
  } else {
    # get the expression profile for each 
    exp.mat <- data.frame(key=matched_res[,2], value=as.numeric(matched_ts));
    cpd_exp_dict <- Convert2Dictionary(exp.mat);
    # create average exp
    exp.vec <- unlist(lapply(cpd_exp_dict, mean));
    
    # now need to get the mapping from mz to compound id (one mz can have 0, 1, or more id hits)
    mz2cpd_dict <- Convert2Dictionary(matched_res[,c(1,2)]); #indexed/named by mz
    cpd2mz_dict <- Convert2Dictionary(matched_res[,c(2,1)]); # indexed/named by id
    
    # now do matching to identify significant input_cpdlist
    refmz <- names(mz2cpd_dict)
    hits.index <- which(refmz %in% as.character(mSetObj$dataSet$input_mzlist));
    cpd1 <- unique(unlist(mz2cpd_dict[hits.index]));

    if(.on.public.web){
        currency_tmp <- currency;
    } else {
        currency_tmp <- currency_r;
    }

    cpd1 <- cpd1[!(cpd1 %in% currency_tmp)];
    
    mSetObj$mz2cpd_dict <- mz2cpd_dict;
    mSetObj$cpd_exp_dict <- cpd_exp_dict;
    mSetObj$cpd_exp <- exp.vec;
    mSetObj$cpd2mz_dict <- cpd2mz_dict;
    mSetObj$input_cpdlist <- cpd1;
    mSetObj$total_matched_cpds <- unique(as.vector(matched_res$Matched.Compound));
  }
  
  form.mat <- cbind(matched_res[,2], matched_res[,3]);
  cpd_form_dict <- Convert2Dictionary(form.mat);
  mSetObj$cpd_form_dict <- cpd_form_dict;
  
  return(mSetObj);
}

#'@export
Read.PeakMS2ListData <- function(mSetObj=NA, 
                                 msfile = NA, 
                                 msmsfile = NA,
                                 meta.anal = FALSE,
                                 method = "pvalue") {
  
  mSetObj <- .get.mSet(mSetObj);
  
  file_name <- tools::file_path_sans_ext(basename(msfile)) 
  mumDataContainsPval = 1; #whether initial data contains pval or not
  input <- as.data.frame(.readDataTable(msfile));
  user_cols <- gsub("[^[:alnum:]]", "", colnames(input));
  mummi.cols <- c("m.z", "p.value", "t.score", "r.t");
  
  filems2_name <- tools::file_path_sans_ext(basename(msmsfile))
  cmpd_input <- as.data.frame(.readDataTable(msmsfile));
  cmpd_input <- as.data.frame(apply(cmpd_input, 2, function(x){x[is.na(x)] <- ""; x}));
  
  if(nrow(cmpd_input) == nrow(input)){
    colnames(cmpd_input) <- paste0("CMPD_", seq(ncol(cmpd_input)));
    mSetObj$dataSet$cmpd.orig <-cmpd_input
  } else if (colnames(cmpd_input)[1] == "index") {
    cmpd_ncol <- ncol(cmpd_input)-1
    new_cmpd_input <- as.data.frame(matrix("", ncol = cmpd_ncol, nrow = nrow(input)))
    for(uu in 1:nrow(cmpd_input)){
      idx_sub <- as.integer(cmpd_input[uu, 1])
      new_cmpd_input[idx_sub, ] <- cmpd_input[uu, -1]
    }
    new_cmpd_input -> cmpd_input
    colnames(cmpd_input) <- paste0("CMPD_", seq(ncol(cmpd_input)));
    mSetObj$dataSet$cmpd.orig <-cmpd_input
  } else {
    AddErrMsg("Peak table and compound candidate table have different rows. Please correct.");
    stop("Peak table and compound candidate table have different rows. Please correct.")
  }
  
  if(meta.anal & method %in% c("es", "both")){
    #mummi.cols <- c(mummi.cols, "effect.size", "lower.ci", "upper.ci")
    stop("Functional meta analysis is not available yet.")
  }
  
  # first check if mode included
  mumDataContainsMode <- "mode" %in% user_cols;
  
  if(mumDataContainsMode){
    mode.info <- input$mode  
    input <- subset(input, select=-mode)
    user_cols <- gsub("[^[:alnum:]]", "", colnames(input))
  }
  
  # next check what column names are there
  hit <- "mz" %in% user_cols;
  
  if(sum(hit) < 1){
    AddErrMsg("Missing information, data must contain a 'm.z' column!");
    return(0);
  }
  
  if(length(colnames(input) %in% mummi.cols) == 1){
    peakFormat <- mSetObj$paramSet$peakFormat;
  }else{
    # subset to what's needed for ms peaks
    # then rename columns
    hits2 <- match(gsub("[^[:alnum:]]", "", mummi.cols), user_cols)
    input <- input[, na.omit(hits2)]  
    user_cols <- user_cols[na.omit(hits2)]
    hits.colnames <- match(user_cols, gsub("[^[:alnum:]]", "", mummi.cols))
    user.cols <- mummi.cols[na.omit(hits.colnames)]
    peakFormat <- paste0(substr(sort(user.cols), 1, 1), collapse = "")
    colnames(input) <- user.cols
  }
  
  rt <- rt.hit <- "r.t" %in% colnames(input)
  mSetObj$paramSet$ContainsMS2 <- TRUE;
  
  qs::qsave(input, "mum_raw.qs");
  qs::qsave(cmpd_input, "cmpd_raw.qs");
  
  if(!"p.value" %in% colnames(input)){
    mumDataContainsPval <- 0;
    input[,'p.value'] <- rep(0, length=nrow(input))
  }
  
  if(!"t.score" %in% colnames(input)){
    input[,'t.score'] <- rep(0, length=nrow(input))
  }

  if(rt){
    mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score, input$r.t);
    colnames(mSetObj$dataSet$mummi.orig) = c("p.value", "m.z", "t.score", "r.t")
  }else{
    mSetObj$dataSet$mummi.orig <- cbind(input$p.value, input$m.z, input$t.score);
    colnames(mSetObj$dataSet$mummi.orig) = c("p.value", "m.z", "t.score")
  }
  
  if(meta.anal & method %in% c("es", "both")){
    # mSetObj$dataSet$mummi.orig <- cbind(mSetObj$dataSet$mummi.orig, effect.size=input$effect.size, 
    #                                     lower.ci=input$lower.ci, upper.ci=input$upper.ci);
    stop("Functional meta analysis is not available yet.")
  }
  
  if (mSetObj$dataSet$mode == "positive") {
    mSetObj$dataSet$pos_inx <- rep(TRUE, nrow(mSetObj$dataSet$mummi.orig))
  } else if (mSetObj$dataSet$mode == "negative") {
    mSetObj$dataSet$pos_inx <- rep(FALSE, nrow(mSetObj$dataSet$mummi.orig) )
  } else { # mixed
    mSetObj$dataSet$pos_inx <- mode.info == "positive"
  }
  
  mSetObj$paramSet$mumRT = rt
  mSetObj$dataSet$mum.type = "list";
  mSetObj$msgSet$read.msg <- c(paste("A total of", length(input$p.value), 
                                   "m/z features were found in your uploaded data."),
                               paste(length(unique(as.character(cmpd_input))),
                                   "compounds found in your uploaded data."));
  mSetObj$dataSet$fileName <- file_name;
  mSetObj$paramSet$mumDataContainsPval <- mumDataContainsPval;
  mSetObj$paramSet$peakFormat <- peakFormat;
  mSetObj$dataSet$meta.info <- as.matrix(1); # Define a value to avoid bug
  
  return(.set.mSet(mSetObj));
}

#'@export
SetMS2IDType <- function(mSetObj=NA, IDtype = "hmdb_ids"){
  mSetObj <- .get.mSet(mSetObj);
  if(IDtype %in% c("hmdb_ids", "pubchem_cids", "pubchem_sids", "inchikeys", "smiles")){
    mSetObj$paramSet$ms2id.type <- IDtype;
  } else {
    stop("IDtype must be one of 'hmdb_ids', 'pubchem_cids', 'pubchem_sids', 'inchikeys', 'smiles'.")
  }
  return(.set.mSet(mSetObj))
}

formatfunresult <- function(){
  setwd("/tank/islets_samples/case_study/guest8509032390121001213tmp/")
  df <- read.csv("peaks_ms1_islets.txt")
  df_cmpd <- read.csv("compound_msn_results.csv")
  dfx_list <- list()
  for(i in 1:nrow(df)){
    cat("==>", i, "\n")
    if(any((df$mz[i]>df_cmpd$mzmin) & (df$mz[i] < df_cmpd$mzmax) & 
           (df$rt[i]>df_cmpd$rtmin) & (df$rt[i] < df_cmpd$rtmax))){
      idx <- which((df$mz[i]>df_cmpd$mzmin) & (df$mz[i] < df_cmpd$mzmax) & 
                     (df$rt[i]>df_cmpd$rtmin) & (df$rt[i] < df_cmpd$rtmax))
      dfx <- cbind(i, df_cmpd[idx[1], c("InchiKey_1", "InchiKey_2", "InchiKey_3", "InchiKey_4", "InchiKey_5")])
      dfx_list <- c(dfx_list, list(dfx))
    }
  }
  
  dfx <- do.call(rbind, dfx_list)
  colnames(dfx)[1] <- "index"
  write.csv(dfx, file = "compound_ms2_islet.csv", row.names = F)
  
}
