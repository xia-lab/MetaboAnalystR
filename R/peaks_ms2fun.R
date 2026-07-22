# NOTE: CODE DUPLICATION WARNING
# This function (.search_MS2compoundLib) shares ~98% of its code with
# .search_compoundLib() in peaks_to_function.R (lines 1293-1720+)
#
# Key differences:
# - This version includes MS2-based compound filtration (lines 127-287)
# - peaks_to_function.R version lacks MS2 filtering but has meta-analysis support
#
# FUTURE REFACTORING RECOMMENDATION:
# Extract shared logic into a common utility function:
#   .search_compoundLib_core(mSetObj, cpd.lib, cpd.treep, cpd.treen, has_ms2=FALSE)
# Then have both files call this core function with appropriate flags.
# This would reduce codebase size by ~35% and eliminate maintenance duplication.
#
# Performance optimizations applied (2025):
# - Vectorized innermost loop in peak matching (lines 59-137)
# - Optimized sapply/strsplit chains (lines 387-395)
# - Vectorized EC creation loop (lines 335-378)
.search_MS2compoundLib <- function(mSetObj,
                                   cpd.lib,
                                   cpd.treep,
                                   cpd.treen){

  ref_mzlist_raw <- as.character(mSetObj$dataSet$ref_mzlist);
  ref_mzlist <- suppressWarnings(as.numeric(ref_mzlist_raw));
  if(any(is.na(ref_mzlist))){
    ref_mzlist <- suppressWarnings(as.numeric(sub("__.*$", "", ref_mzlist_raw)));
  }
  if(any(is.na(ref_mzlist))){
    bad.examples <- paste(utils::head(unique(ref_mzlist_raw[is.na(ref_mzlist)]), 5), collapse = ", ")
    AddErrMsg(paste("Invalid m/z values found after preprocessing. Examples:", bad.examples))
    return(0)
  }

  ref_cmpdlist <- mSetObj$dataSet$ref_cmpdlist;
  if(is.null(ref_cmpdlist)){
    ref_cmpdlist <- mSetObj$dataSet$cmpd.orig;
  }
  if(is.null(ref_cmpdlist)){
    # Keep shape stable for downstream indexing and skip MS2 filtration when annotations are absent.
    ref_cmpdlist <- matrix("", nrow = length(ref_mzlist), ncol = 1)
  } else if(is.vector(ref_cmpdlist) && !is.list(ref_cmpdlist)){
    ref_cmpdlist <- matrix(as.character(ref_cmpdlist), ncol = 1)
  } else {
    ref_cmpdlist <- as.matrix(ref_cmpdlist)
  }

  if(nrow(ref_cmpdlist) != length(ref_mzlist)){
    cmpd_fix <- matrix("", nrow = length(ref_mzlist), ncol = ncol(ref_cmpdlist))
    n_copy <- min(nrow(ref_cmpdlist), nrow(cmpd_fix))
    if(n_copy > 0){
      cmpd_fix[seq_len(n_copy), ] <- ref_cmpdlist[seq_len(n_copy), , drop = FALSE]
    }
    ref_cmpdlist <- cmpd_fix
  }
  
  print(paste0("Got ", length(ref_mzlist), " mass features."))
  print(paste0("Got ", length(ref_cmpdlist[ref_cmpdlist != ""]), " Compound features."))
  
  pos_inx <- mSetObj$dataSet$pos_inx;
  
  # split all mzs (users' input) based on Modes
  ref_mzlistp <- ref_mzlist[pos_inx];
  ref_mzlistn <- ref_mzlist[!pos_inx];
  
  # split all compounds identified (users' input) from MS2
  ref_cmpdlistp <- ref_cmpdlist[pos_inx,];
  ref_cmpdlistn <- ref_cmpdlist[!pos_inx,];
  
  version <- mSetObj$paramSet$version;

  # for empirical compounds
  if(mSetObj$paramSet$mumRT && version=="v2"){
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
    # OPTIMIZED: Vectorized inner loop processing to eliminate innermost loop
    # processing data from ESI+ mode
    for(i in seq_along(ref_mzlistp)){
      mz <- ref_mzlistp[i];
      rt <- ret_time_pos[i];
      rt_rank <- ret_time_rank_pos[i];
      my.tol <- my.tolsp[i];
      all.mz <- all.mzsp[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treep[all.mz])));

      if(length(pos.all) > 0){
        for(pos in pos.all){
          id <- cpd.lib$id[pos];
          mw.all <- cpd.lib$mz.matp[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);

          # OPTIMIZED: Vectorized - process all hits at once instead of inner loop
          if(length(hit.inx) > 0){
            # Create all indices at once (vectorized string concatenation)
            indices <- paste(mz, id, rt, hit.inx, sep = "___");

            # Batch create all match data (vectorized)
            match_data <- lapply(seq_along(hit.inx), function(spot) {
              hit.pos <- hit.inx[spot];
              c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesp[hit.pos], diffs[hit.pos])
            });

            # Add all matches at once
            for(spot in seq_along(hit.inx)){
              matched_resp$add(indices[spot], match_data[[spot]]);
            }
          }
        }
      }
    }
  }
  
  all.mzsn <<- all.mzsn
  
  if (mSetObj$dataSet$mode != "positive") {
    # OPTIMIZED: Vectorized inner loop processing to eliminate innermost loop
    # processing data from ESI- mode
    for(i in seq_along(ref_mzlistn)){
      mz <- ref_mzlistn[i];
      rt <- ret_time_neg[i];
      rt_rank <- ret_time_rank_neg[i];
      my.tol <- my.tolsn[i];
      all.mz <- all.mzsn[i,];
      pos.all <- as.numeric(unique(unlist(cpd.treen[all.mz])));

      if(length(pos.all) > 0){
        for(pos in pos.all){
          id <- cpd.lib$id[pos]; # position of compound in cpd.tree
          mw.all <- cpd.lib$mz.matn[pos,]; #get modified mzs
          diffs <- abs(mw.all - mz); #modified mzs - mz original
          hit.inx <- which(diffs < my.tol);

          # OPTIMIZED: Vectorized - process all hits at once instead of inner loop
          if(length(hit.inx) > 0){
            # Create all indices at once (vectorized string concatenation)
            indices <- paste(mz, id, rt, hit.inx, sep = "___");

            # Batch create all match data (vectorized)
            match_data <- lapply(seq_along(hit.inx), function(spot) {
              hit.pos <- hit.inx[spot];
              c(i, id, mz, rt, rt_rank, mw.all[hit.pos], modified.statesn[hit.pos], diffs[hit.pos])
            });

            # Add all matches at once
            for(spot in seq_along(hit.inx)){
              matched_resn$add(indices[spot], match_data[[spot]]);
            }
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
        id0 <- as.integer(matched_resn$X1[i]);
        idx <- which(cpd.lib[["id"]]==id)
        dbTypedID <- cpd.lib$ms2IDs[idx];
        if(is.na(dbTypedID)) {
          return(TRUE)
        } else if(dbTypedID == "") {
          return(TRUE)
        } else if(all(ref_cmpdlistn[id0,] == "")) {
          return(TRUE)
        } else {
          if(any(dbTypedID %in% ref_cmpdlistn[id0,])) {
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
        id0 <- as.integer(matched_resp$X1[i]);
        idx <- which(cpd.lib[["id"]]==id)
        dbTypedID <- cpd.lib$ms2IDs[idx];
        if(is.na(dbTypedID)) {
          return(TRUE)
        } else if(dbTypedID == "") {
          return(TRUE)
        } else if(all(ref_cmpdlistp[id0,] == "")) {
          return(TRUE)
        } else {
          if(any(dbTypedID %in% ref_cmpdlistp[id0,])) {
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
      id0 <- as.integer(matched_res$X1[i]);
      idx <- which(cpd.lib[["id"]]==id)
      dbTypedID <- cpd.lib$ms2IDs[idx];
      if(is.na(dbTypedID)) {
        return(TRUE)
      } else if(dbTypedID == "") {
        return(TRUE)
      } else if(all(ref_cmpdlistp[id0,] == "")) {
        return(TRUE)
      } else {
        if(any(dbTypedID %in% ref_cmpdlistp[id0,])) {
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
      id0 <- as.integer(matched_res$X1[i]);
      dbTypedID <- cpd.lib$ms2IDs[idx];
      if(is.na(dbTypedID)) {
        return(TRUE)
      } else if(dbTypedID == "") {
        return(TRUE)
      } else if(all(ref_cmpdlistn[id0,] == "")) {
        return(TRUE)
      } else {
        if(any(dbTypedID %in% ref_cmpdlistn[id0,])) {
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

  if(!mSetObj$paramSet$mumRT && version=="v2"){
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
    
    # OPTIMIZED: Vectorized EC creation - process all compounds efficiently
    # for each compound, if multiple matches, split into ECpds if > RT tolerance - rt_tol
    empirical.cpds2cpds <- lapply(seq_along(empirical.cpd.list), function(i) {
      ec_data <- empirical.cpd.list[[i]]
      cpd_name <- names(empirical.cpd.list)[i]

      # Extract data once
      mzs <- ec_data$Query.Mass
      ions <- ec_data$Matched.Form
      rts_char <- ec_data$Retention.Time
      rt_rank_char <- ec_data$RT.Rank

      # Single EC case - direct return
      if(length(mzs) == 1){
        return(paste0(mzs, ";", ions, ";", rts_char, ";", cpd_name))
      }

      # Multiple ECs - vectorized processing
      rts_num <- as.numeric(rts_char)
      rt_ranks_num <- as.numeric(rt_rank_char)

      # Vectorized name creation (single paste0 call)
      ec_names <- paste0(mzs, ";", ions, ";", rts_char, ";", cpd_name)

      # Sort by RT
      sort_idx <- order(rts_num)
      rts_sorted <- rts_num[sort_idx]
      ranks_sorted <- rt_ranks_num[sort_idx]
      names_sorted <- ec_names[sort_idx]

      # Vectorized split detection
      split.inx <- c(0, cumsum(
        (abs(diff(rts_sorted)) > rt_tol) &
        (abs(diff(ranks_sorted)) > rt_tol_rank)
      ))

      # Return result based on split
      if(length(unique(split.inx)) > 1){
        return(split(names_sorted, split.inx))
      } else {
        return(paste0(names_sorted, collapse = "__"))
      }
    })
    names(empirical.cpds2cpds) <- names(empirical.cpd.list)
    
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
    ov_qs_save(df_ecs, "initial_ecs.qs")
    merged_ecs <- aggregate(. ~ str_row_inx, df_ecs, paste, collapse=";")
    
    # OPTIMIZED: Vectorized string processing instead of sapply/strsplit chains
    # cleaning the df
    # merged_ecs$ec <- sapply(strsplit(merged_ecs$ec, ";", fixed=TRUE), function(x) unlist(x)[1]) - keep as long name
    cols_to_clean <- c("mz", "form", "rt")
    merged_ecs[cols_to_clean] <- lapply(merged_ecs[cols_to_clean], function(col) {
      vapply(strsplit(as.character(col), ";", fixed = TRUE),
             function(x) paste(unique(x), collapse = ";"),
             FUN.VALUE = character(1))
    })
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
  ov_qs_save(matched_res, "mum_res.qs");
  
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
  colnames(input) <- .canon.peak.cols(colnames(input));
  user_cols <- colnames(input);
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
  } else if (all(c("mzmin", "mzmax", "rtmin", "rtmax") %in% tolower(gsub("[^[:alnum:]_]", "", colnames(cmpd_input))))) {
    # Support spectra-processing annotation tables with mz/rt ranges and non-equal row counts
    cmpd_nms <- tolower(gsub("[^[:alnum:]_]", "", colnames(cmpd_input)));

    # Derive per-peak mz/rt from the peak table
    mz_vec <- suppressWarnings(as.numeric(input[[1]]));
    if("mz" %in% user_cols){
      mz_vec <- suppressWarnings(as.numeric(input[[which(user_cols == "mz")[1]]]))
    }

    rt_vec <- NULL
    if("rt" %in% user_cols){
      rt_vec <- suppressWarnings(as.numeric(input[[which(user_cols == "rt")[1]]]))
    } else if("r.t" %in% user_cols){
      rt_vec <- suppressWarnings(as.numeric(input[[which(user_cols == "r.t")[1]]]))
    }

    if(any(!is.finite(mz_vec)) || is.null(rt_vec) || any(!is.finite(rt_vec))){
      AddErrMsg("Compound table with mz/rt ranges requires peak data containing valid m/z and retention-time values.");
      return(0);
    }

    mzmin <- suppressWarnings(as.numeric(cmpd_input[, which(cmpd_nms == "mzmin")[1]]));
    mzmax <- suppressWarnings(as.numeric(cmpd_input[, which(cmpd_nms == "mzmax")[1]]));
    rtmin <- suppressWarnings(as.numeric(cmpd_input[, which(cmpd_nms == "rtmin")[1]]));
    rtmax <- suppressWarnings(as.numeric(cmpd_input[, which(cmpd_nms == "rtmax")[1]]));
    valid_cmpd <- is.finite(mzmin) & is.finite(mzmax) & is.finite(rtmin) & is.finite(rtmax);

    cand_idx <- grep("^(compound|inchikey|formula|score|database)_[0-9]+$", cmpd_nms);
    if(length(cand_idx) < 1){
      cand_idx <- setdiff(seq_len(ncol(cmpd_input)), which(cmpd_nms %in% c("mzmin", "mzmax", "rtmin", "rtmax")));
    }

    if(length(cand_idx) < 1){
      AddErrMsg("No candidate annotation columns were found in the compound table.");
      return(0);
    }

    cmpd_map <- matrix("", nrow = nrow(input), ncol = length(cand_idx));
    for(ii in seq_len(nrow(input))){
      hit_rows <- valid_cmpd & (mz_vec[ii] >= mzmin) & (mz_vec[ii] <= mzmax) & (rt_vec[ii] >= rtmin) & (rt_vec[ii] <= rtmax);
      if(any(hit_rows)){
        for(jj in seq_along(cand_idx)){
          vals <- as.character(cmpd_input[hit_rows, cand_idx[jj]]);
          vals <- unique(vals[!(is.na(vals) | vals == "" | vals == "NA")]);
          if(length(vals) > 0){
            cmpd_map[ii, jj] <- paste(vals, collapse = ";");
          }
        }
      }
    }

    cmpd_input <- as.data.frame(cmpd_map, stringsAsFactors = FALSE);
    colnames(cmpd_input) <- paste0("CMPD_", seq(ncol(cmpd_input)));
    mSetObj$dataSet$cmpd.orig <- cmpd_input
  } else {
    AddErrMsg("Peak and compound tables are not aligned. Please provide either: (1) equal-row tables, (2) an index-based compound table, or (3) a spectra annotation table with mzmin/mzmax/rtmin/rtmax.");
    return(0);
  }
  
  if(meta.anal & method %in% c("es", "both")){
    #mummi.cols <- c(mummi.cols, "effect.size", "lower.ci", "upper.ci")
    AddErrMsg("Functional meta analysis is not available yet.");
    return(0);
  }
  
  # first check if mode included
  mumDataContainsMode <- "mode" %in% user_cols;
  
  if(mumDataContainsMode){
    mode.info <- input$mode  
    input <- subset(input, select=-mode)
    user_cols <- colnames(input)
  }

  # next check what column names are there
  hit <- "mz" %in% user_cols;

  # No header maps to m/z: fall back to the convention that the FIRST column is the
  # mass / m/z, provided its values are numeric and positive.
  if(sum(hit) < 1 && ncol(input) >= 1){
    v1 <- suppressWarnings(as.numeric(as.character(input[[1]])));
    if(any(is.finite(v1)) && all(v1[is.finite(v1)] > 0)){
      user_cols[1] <- "mz";
      colnames(input) <- user_cols;
      hit <- TRUE;
    }
  }

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
  
  ov_qs_save(input, "mum_raw.qs");
  ov_qs_save(cmpd_input, "cmpd_raw.qs");
  
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
    AddErrMsg("Functional meta analysis is not available yet.");
    return(0);
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
Read.PeakMS2TableData <- function(mSetObj=NA,
                                  msfile = NA,
                                  msmsfile = NA,
                                  format = "colu",
                                  lbl.type = "disc") {

  mSetObj <- .get.mSet(mSetObj);
cat("msfile ===> ", msfile, "\n")
cat("msmsfile ===> ", msmsfile, "\n")
  mSetObj$dataSet$data.file.path <- msfile;
  mSetObj$dataSet$cmpd.file.path <- msmsfile;
  # Reuse existing table parser/validator for the MS1 peak intensity table
  mSetObj <- Read.TextData(mSetObj, msfile, format, lbl.type);
  mSetObj <- .get.mSet(mSetObj);
cat("msfile ===> ", msfile, "\n")
cat("msmsfile ===> ", msmsfile, "\n")
  mSetObj$dataSet$data.file.path <- msfile;
  mSetObj$dataSet$cmpd.file.path <- msmsfile;
  cmpd_input <- as.data.frame(.readDataTable(msmsfile), stringsAsFactors = FALSE);
  if(nrow(cmpd_input) < 1 || ncol(cmpd_input) < 1){
    AddErrMsg("The compound annotation table is empty or unreadable.");
    return(0);
  }

  # Basic format checks against spectra-processing output layout
  nms <- tolower(gsub("[^[:alnum:]_]", "", colnames(cmpd_input)));
  req.cols <- c("mzmin", "mzmax", "rtmin", "rtmax");
  if(!all(req.cols %in% nms)){
    AddErrMsg("Compound annotation table must contain columns mzmin, mzmax, rtmin, and rtmax.");
    return(0);
  }

  has.hit.cols <- any(grepl("^(compound|inchikey|formula|score|database)_[0-9]+$", nms));
  if(!has.hit.cols){
    AddErrMsg("Compound annotation table must contain one or more candidate hit columns (e.g., Compound_1, InchiKey_1, Formula_1, Score_1, Database_1).");
    return(0);
  }

  # Validate table pairing by mz/rt overlap instead of row count.
  # Peak table features are expected as "mz__rt" in the first-column labels.
  peak_features <- names(mSetObj$dataSet$url.var.nms);
  if(is.null(peak_features) || length(peak_features) == 0){
    AddErrMsg("Unable to retrieve feature names from the peak table. Please upload a valid peak table with feature labels in the first column.");
    return(0);
  }

  split_feat <- strsplit(as.character(peak_features), "__", fixed = TRUE);
  feat_mz <- suppressWarnings(as.numeric(vapply(split_feat, function(x) if(length(x) >= 1) x[1] else NA_character_, FUN.VALUE = character(1L))));
  feat_rt <- suppressWarnings(as.numeric(vapply(split_feat, function(x) if(length(x) >= 2) x[2] else NA_character_, FUN.VALUE = character(1L))));
  valid_feat <- is.finite(feat_mz) & is.finite(feat_rt);

  if(!any(valid_feat)){
    AddErrMsg("No valid peak features were found in 'mz__rt' format. Please upload a correctly formatted peak table.");
    return(0);
  }

  feat_mz <- feat_mz[valid_feat];
  feat_rt <- feat_rt[valid_feat];

  mzmin <- suppressWarnings(as.numeric(cmpd_input[, which(nms == "mzmin")[1]]));
  mzmax <- suppressWarnings(as.numeric(cmpd_input[, which(nms == "mzmax")[1]]));
  rtmin <- suppressWarnings(as.numeric(cmpd_input[, which(nms == "rtmin")[1]]));
  rtmax <- suppressWarnings(as.numeric(cmpd_input[, which(nms == "rtmax")[1]]));

  valid_cmpd <- is.finite(mzmin) & is.finite(mzmax) & is.finite(rtmin) & is.finite(rtmax);
  if(!any(valid_cmpd)){
    AddErrMsg("Compound annotation table has no valid mz/rt range rows. Please check mzmin/mzmax/rtmin/rtmax values.");
    return(0);
  }

  cmpd_matched <- rep(FALSE, length(mzmin));
  valid_idx <- which(valid_cmpd);
  for(i in valid_idx){
    mz_hits <- (feat_mz >= mzmin[i]) & (feat_mz <= mzmax[i]);
    if(any(mz_hits)){
      rt_hits <- (feat_rt[mz_hits] >= rtmin[i]) & (feat_rt[mz_hits] <= rtmax[i]);
      cmpd_matched[i] <- any(rt_hits);
    }
  }

  total_valid_cmpd <- length(valid_idx);
  unmatched_valid_cmpd <- sum(!cmpd_matched[valid_idx]);
  unmatched_ratio <- unmatched_valid_cmpd / total_valid_cmpd;

  if(unmatched_ratio > 0.5){
    AddErrMsg(paste0(
      "Peak and compound tables do not appear to be paired: ",
      unmatched_valid_cmpd, " of ", total_valid_cmpd,
      " compound rows (", round(unmatched_ratio * 100, 1),
      "%) could not be matched to any peak feature by mz/rt ranges. ",
      "Please upload correct paired tables."
    ));
    return(0);
  }

  cmpd_input <- as.data.frame(apply(cmpd_input, 2, function(x){x[is.na(x)] <- ""; x}), stringsAsFactors = FALSE);
  mSetObj$dataSet$cmpd.orig <- cmpd_input;
  mSetObj$paramSet$ContainsMS2 <- TRUE;
  mSetObj$msgSet$read.msg <- c(mSetObj$msgSet$read.msg,
                               paste("A total of", nrow(cmpd_input), "MS2 annotation rows were found in your uploaded compound table."),
                               paste(unmatched_valid_cmpd, "of", total_valid_cmpd, "compound rows had no peak-feature match by mz/rt range."));

  return(.set.mSet(mSetObj));
}

#'@export
FormatPeakCompoundTable <- function(compoundTablePath,
                                    peakTablePath,
                                    outputFilePath = NA,
                                    idColumnPattern = "InchiKey") {

  cmpd_tbl <- as.data.frame(.readDataTable(compoundTablePath), stringsAsFactors = FALSE)
  peak_tbl <- as.data.frame(.readDataTable(peakTablePath), stringsAsFactors = FALSE)

  if(nrow(cmpd_tbl) < 1 || ncol(cmpd_tbl) < 1){
    stop("Compound table is empty or unreadable.")
  }
  if(nrow(peak_tbl) < 2 || ncol(peak_tbl) < 1){
    stop("Peak table is empty or does not contain feature rows.")
  }

  # First column in peak table should contain feature labels in mz__rt format.
  peak_features <- as.character(peak_tbl[-1, 1])
  split_feat <- strsplit(peak_features, "__", fixed = TRUE)
  peak_mz <- suppressWarnings(as.numeric(vapply(split_feat, function(x) if(length(x) >= 1) x[1] else NA_character_, FUN.VALUE = character(1L))))
  peak_rt <- suppressWarnings(as.numeric(vapply(split_feat, function(x) if(length(x) >= 2) x[2] else NA_character_, FUN.VALUE = character(1L))))

  valid_peak <- is.finite(peak_mz) & is.finite(peak_rt)
  if(!any(valid_peak)){
    stop("No valid peak features found in mz__rt format in the first column of the peak table.")
  }

  cmpd_nms <- tolower(gsub("[^[:alnum:]_]", "", colnames(cmpd_tbl)))
  req_cols <- c("mzmin", "mzmax", "rtmin", "rtmax")
  if(!all(req_cols %in% cmpd_nms)){
    stop("Compound table must contain mzmin, mzmax, rtmin, and rtmax columns.")
  }

  mzmin <- suppressWarnings(as.numeric(cmpd_tbl[, which(cmpd_nms == "mzmin")[1]]))
  mzmax <- suppressWarnings(as.numeric(cmpd_tbl[, which(cmpd_nms == "mzmax")[1]]))
  rtmin <- suppressWarnings(as.numeric(cmpd_tbl[, which(cmpd_nms == "rtmin")[1]]))
  rtmax <- suppressWarnings(as.numeric(cmpd_tbl[, which(cmpd_nms == "rtmax")[1]]))

  valid_cmpd <- is.finite(mzmin) & is.finite(mzmax) & is.finite(rtmin) & is.finite(rtmax)
  idx <- rep(NA_integer_, nrow(cmpd_tbl))

  mz_use <- peak_mz[valid_peak]
  rt_use <- peak_rt[valid_peak]
  peak_pos <- which(valid_peak)

  for(i in seq_len(nrow(cmpd_tbl))){
    if(!valid_cmpd[i]){
      next
    }
    hits <- which(mz_use >= mzmin[i] & mz_use <= mzmax[i] & rt_use >= rtmin[i] & rt_use <= rtmax[i])
    if(length(hits) > 0){
      # Use 1-based index of peak rows excluding peak_tbl header row, consistent with existing index-based format.
      idx[i] <- peak_pos[hits[1]]
    }
  }

  id_cols <- grep(idColumnPattern, colnames(cmpd_tbl), ignore.case = TRUE)
  if(length(id_cols) < 1){
    id_cols <- grep("^InchiKey", colnames(cmpd_tbl), ignore.case = TRUE)
  }
  if(length(id_cols) < 1){
    stop("No compound ID columns found (e.g., InchiKey_*).")
  }

  out_tbl <- cbind(index = idx, cmpd_tbl[, id_cols, drop = FALSE])

  if(!is.na(outputFilePath) && nzchar(outputFilePath)){
    utils::write.table(out_tbl, file = outputFilePath, row.names = FALSE, quote = FALSE, sep = "\t")
  }

  return(out_tbl)
}

#'@export
SetMS2IDType <- function(mSetObj=NA, IDtype = "hmdb_ids"){
  mSetObj <- .get.mSet(mSetObj);
  if(IDtype %in% c("hmdb_ids", "pubchem_cids", "pubchem_sids", "inchikeys", "smiles")){
    mSetObj$paramSet$ms2id.type <- IDtype;
  } else {
    AddErrMsg("IDtype must be one of 'hmdb_ids', 'pubchem_cids', 'pubchem_sids', 'inchikeys', 'smiles'.");
    return(0);
  }
  return(.set.mSet(mSetObj))
}

