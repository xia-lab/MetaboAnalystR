my.update.JSFHistory <- function(projectDbPath = "", targetPrj_TK = ""){
  
  toolName <- "MetaboAnalyst";
  # projectDbPath <- "/mnt/disks/launchpad/omicsquare_account.sqlite"
  # targetPrj_TK <- "9813b5b333c63540c273c6d223aa4ab205ea0899a75522902cdcb6ded5e588c8"
  # 
  
  print(paste0("now the 330 ==> ", targetPrj_TK))
  print(paste0("now the targetPrj_TK ==> ", targetPrj_TK))
  print(paste0("now the projectDbPath ==> ", projectDbPath))
  
  require(RSQLite)
  require(DBI)
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  
  transactionStarted <- FALSE
  
  on.exit({
    if(transactionStarted) {
      dbRollback(con)
    }
    dbDisconnect(con)
  }, add = TRUE)
  
  tryCatch({
    dbBegin(con)
    transactionStarted <- TRUE
    
    query <- "SELECT javaHistory FROM project WHERE toolName = ? AND partialToken = ? AND projectType = 'batch_project'"
    res <- dbGetQuery(con, query, params = list(toolName, targetPrj_TK));
    
    if (nrow(res) == 0) {
      return("")  # Return NULL if no matches
    }
    
    # update the java history
    batch_info <- qs::qread("RBatchTemplate.qs")
    fun_dict <- batch_info$fun_dict
    fnms_new <- names(batch_info$fun_set)
    fun_set <- batch_info$fun_set
    readerFUNname <- fun_dict$Function_name[fun_dict$FunType == "reader"]

    if(length(readerFUNname)==1){
      if(readerFUNname == "Read.TextData"){
        ii <- which(fnms_new == readerFUNname)
        newDataFile_name <- fun_set[["Read.TextData"]][[1]][[3]][[3]]
      }
    } else if(length(readerFUNname)>1) {
      if("Read.TextDataTs" %in% readerFUNname){ # this is for stat multifactor module
        ki <- which("Read.TextDataTs" == readerFUNname)
        ii <- which(fnms_new == readerFUNname[ki])
        newDataFile_name <- fun_set[["Read.TextDataTs"]][[1]][[3]][[3]]
      }
    } else {
      stop("Now we need to add more if conditions <------")
    }
    
    # import batch_fun_set and JSF history
    batch_fun_set <- qs::qread("batch_cmds_set.qs")
    jsfh <- qs::qread("original_batch_jsfhistory.qs")
    str0 <- jsfh$javaHistory
    jhs0 <- rjson::fromJSON(str0)
    
    jsession0 <- rjson::fromJSON(jhs0[["NA.dummy.SessionBean1"]])
    IntegProcessBean0 <- rjson::fromJSON(jhs0[["NA.dummy.IntegProcessBean"]])
    IntegResBean0 <- rjson::fromJSON(jhs0[["NA.dummy.IntegResBean"]])
    MsetBean0 <- rjson::fromJSON(jhs0[["NA.dummy.MsetBean"]])
    PathBean0 <- rjson::fromJSON(jhs0[["NA.dummy.PathBean"]])
    UtilsBean0 <- rjson::fromJSON(jhs0[["NA.dummy.UtilsBean"]])
    MetaPathStatBean0 <- rjson::fromJSON(jhs0[["NA.dummy.MetaPathStatBean"]])
    MnetResBean0 <- rjson::fromJSON(jhs0[["NA.dummy.MnetResBean"]])
    MultifacBean0 <- rjson::fromJSON(jhs0[["NA.dummy.MultifacBean"]])
    MummiAnalBean0 <- rjson::fromJSON(jhs0[["NA.dummy.MummiAnalBean"]])
    RocAnalBean0 <- rjson::fromJSON(jhs0[["NA.dummy.RocAnalBean"]])
    PowerAnalBean0 <- rjson::fromJSON(jhs0[["NA.dummy.PowerAnalBean"]])
    
    str1 <- res$javaHistory[1]
    jhs <- rjson::fromJSON(str1)
    
    jsession <- rjson::fromJSON(jhs[["NA.dummy.SessionBean1"]])
    IntegProcessBean <- rjson::fromJSON(jhs[["NA.dummy.IntegProcessBean"]])
    IntegResBean <- rjson::fromJSON(jhs[["NA.dummy.IntegResBean"]])
    MsetBean <- rjson::fromJSON(jhs[["NA.dummy.MsetBean"]])
    PathBean <- rjson::fromJSON(jhs[["NA.dummy.PathBean"]])
    UtilsBean <- rjson::fromJSON(jhs[["NA.dummy.UtilsBean"]])
    MetaPathStatBean <- rjson::fromJSON(jhs[["NA.dummy.MetaPathStatBean"]])
    MnetResBean <- rjson::fromJSON(jhs[["NA.dummy.MnetResBean"]])
    MultifacBean <- rjson::fromJSON(jhs[["NA.dummy.MultifacBean"]])
    MummiAnalBean <- rjson::fromJSON(jhs[["NA.dummy.MummiAnalBean"]])
    RocAnalBean <- rjson::fromJSON(jhs[["NA.dummy.RocAnalBean"]])
    PowerAnalBean <- rjson::fromJSON(jhs[["NA.dummy.PowerAnalBean"]])

    oldDataFile_name <- NA; # TODO: may need to update file name [here] for future case

    # update IntegProcessBean [directly]
    IntegProcessBean <- IntegProcessBean0
    jhs[["NA.dummy.IntegProcessBean"]] <- rjson::toJSON(IntegProcessBean)
    
    # update expressbean [directly]
    IntegResBean <- IntegResBean0;
    jhs[["NA.dummy.IntegResBean"]] <- rjson::toJSON(IntegResBean)
    
    # update MsetBean [directly]
    MsetBean <- MsetBean0;
    jhs[["NA.dummy.MsetBean"]] <- rjson::toJSON(MsetBean)
    
    # update PathBean [directly]
    PathBean <- PathBean0;
    jhs[["NA.dummy.PathBean"]] <- rjson::toJSON(PathBean)
    
    # update UtilsBean [directly]
    UtilsBean <- UtilsBean0;
    jhs[["NA.dummy.UtilsBean"]] <- rjson::toJSON(UtilsBean)
    
    # update MetaPathStatBean [directly]
    MetaPathStatBean <- MetaPathStatBean0;
    jhs[["NA.dummy.MetaPathStatBean"]] <- rjson::toJSON(MetaPathStatBean)
    
    # update MnetResBean [directly]
    MnetResBean <- MnetResBean0;
    jhs[["NA.dummy.MnetResBean"]] <- rjson::toJSON(MnetResBean)
    
    # update MultifacBean [directly]
    MultifacBean <- MultifacBean0;
    jhs[["NA.dummy.MultifacBean"]] <- rjson::toJSON(MultifacBean)
    
    # update MummiAnalBean [directly]
    MummiAnalBean <- MummiAnalBean0;
    jhs[["NA.dummy.MummiAnalBean"]] <- rjson::toJSON(MummiAnalBean)

    # update RocAnalBean [directly]
    RocAnalBean <- RocAnalBean0;
    jhs[["NA.dummy.RocAnalBean"]] <- rjson::toJSON(RocAnalBean)
    
    # update PowerAnalBean [directly]
    PowerAnalBean <- PowerAnalBean0;
    jhs[["NA.dummy.PowerAnalBean"]] <- rjson::toJSON(PowerAnalBean)
    
    # update sessionbean 1
    jsession[["shareToken"]] <- targetPrj_TK;
    jsession[["isBatchProject"]] <- TRUE;
    
    if(!is.na(oldDataFile_name)){
      for(i in 1:length(jsession[["graphicsMap"]])){
        cmdx <- jsession[["graphicsMap"]][[i]];
        jsession[["graphicsMap"]][[i]] <-
          gsub(oldDataFile_name, newDataFile_name, cmdx)
      }
    }

    jsession[["org"]] <- jsession0[["org"]]
    
    jhs[["NA.dummy.SessionBean1"]] <- rjson::toJSON(jsession)
    
    # update the entire json file
    res$javaHistory <- rjson::toJSON(jhs)
    
    # correction for special case
    if(grepl("algOpts\\\\\":\\\\\"mum\\\\\"",res$javaHistory)){
      print("correcting special mum string... ")
      res$javaHistory <- sub("algOpts\\\\\":\\\\\"mum\\\\\"",
                             "algOpts\\\\\":\\[\\\\\"mum\\\\\"\\]",
                             res$javaHistory)
    }
    
    
    # commit the java history
    query <- "UPDATE project SET javaHistory=? WHERE partialToken=? AND projectType = 'project'"
    dbExecute(con, query, list(res$javaHistory, targetPrj_TK))

    # delete the batch_project record after updating
    query <- paste0("DELETE FROM project WHERE partialToken = '", targetPrj_TK, "' AND projectType = 'batch_project'")
    dbExecute(con, query)

    dbCommit(con)
    transactionStarted <- FALSE
  }, error = function(e) {
    stop("An error occurred while fetching data: ", e)
  })
  # print("Reaching here ---------- 372")
  # save(res, file = "res___373.rda")
  strx <- res$javaHistory[1]
  # jhs <- rjson::fromJSON(str1)
  # jsession <- rjson::fromJSON(jhs[["SessionBean1"]])
  print(paste0("now the 458 ==> ", projectDbPath))
  return(strx)
}

my.generate.SLURMbash <- function(anaType, newDataCmds, template_path, homeDir, scriptPath, sh4Slurm = TRUE){
  
  save(anaType, newDataCmds, template_path, homeDir, scriptPath, sh4Slurm, file = "generateSLURMbash.rda")
  print("---------------")
  print(getwd())
  print("---------------")
  if(!file.exists(template_path)){
    return(-1)
  } else {
    cmd_tmp <- qs::qread(template_path)
  }
  
  if(newDataCmds == ""){
    newcmds <- readLines("Rhistory.R")
    cmds_set <- lapply(newcmds, function(x){
      parse(text = x)
    })
  } else {
    newDataCmdsx <- strsplit(newDataCmds, ";")
    cmds_set <- lapply(newDataCmdsx, function(x){
      parse(text = x)
    })
  }
  
  fnms_new <- vapply(cmds_set, FUN = function(x){
    if((x[[1]][[1]] == "<-") | (x[[1]][[1]] == "=")){
      return(as.character(x[[1]][[3]][1][1]))
    } else {
      return(as.character(x[[1]][[1]]))
    }
  }, FUN.VALUE = character(1L), USE.NAMES = F)
  
  if(anaType == "stat"){
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "stat", ]
  } else if(anaType == "mf") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "mf", ]
  } else if(anaType == "roc") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "roc", ]
  } else if(anaType == "msetora") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "msetora", ]
  } else if(anaType == "msetqea") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "msetqea", ]
  } else if(anaType == "pathora") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "pathora", ]
  } else if(anaType == "pathqea") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "pathqea", ]
  } else if(anaType == "mummichog") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "mummichog", ]
  } else if(anaType == "power") {
    fun_dict <- cmd_tmp[["fun_dict"]]
    fun_dict <- fun_dict[fun_dict$Module == "power", ]
  }  else {
    # TODO: for other module
  }
  fun_set <- cmd_tmp[["fun_set"]];
  
  readerFUNname <- fun_dict$Function_name[fun_dict$FunType == "reader"]
  if(length(readerFUNname)==1){
    if(readerFUNname == "Read.TextData"){
      oldDataFile_name <- fun_set[["Read.TextData"]][[1]][[3]][[3]]
      ii <- which(fnms_new == readerFUNname)
      newDataFile_name <- cmds_set[[ii]][[1]][[3]][[3]]
    } else {
      newDataFile_name <- oldDataFile_name <- "";
    }
  } else if(length(readerFUNname)>1) {
    if("Read.TextDataTs" %in% readerFUNname){ # this is for stat multifactor module
      oldDataFile_name <- fun_set[["Read.TextDataTs"]][[1]][[3]][[3]]
      ki <- which("Read.TextDataTs" == readerFUNname)
      ii <- which(fnms_new == readerFUNname[ki])
      newDataFile_name <- cmds_set[[ii]][[1]][[3]][[3]]
    }
    if("Read.PeakListData" %in% readerFUNname){
      fun_nms <- names(fun_set)
      if(any("Read.PeakListData" == fun_nms)){
        oldDataFile_name <- fun_set[["Read.PeakListData"]][[1]][[3]][[3]]
        ki <- which("Read.PeakListData" == readerFUNname)
        ii <- which(fnms_new == readerFUNname[ki])
        newDataFile_name <- cmds_set[[ii]][[1]][[3]][[3]]
      }
    }
    if("Read.TextData" %in% readerFUNname){
      fun_nms <- names(fun_set)
      if(any("Read.TextData" == fun_nms)){
        oldDataFile_name <- fun_set[["Read.TextData"]][[1]][[3]][[3]]
        ki <- which("Read.TextData" == readerFUNname)
        ii <- which(fnms_new == readerFUNname[ki])
        newDataFile_name <- cmds_set[[ii]][[1]][[3]][[3]]
      }
    }
    
  } else {
    stop("Now we need to add more if conditions <------")
  }
  
  fun_set1 <- lapply(fun_set, function(x){
    if((x[[1]][[1]] == "<-") | (x[[1]][[1]] == "=")){
      fnm <- as.character(x[[1]][[3]][1])
    } else {
      fnm <- as.character(x[[1]][[1]])
    }
    if(fnm %in% fnms_new){
      idx <- which(fnm == fnms_new)
      return(cmds_set[[idx]])
    } else {
      res <- x
    }
    res <- update2newFun(res,oldDataFile_name, newDataFile_name)
  })
  
  fun_set2 <- funRM(fun_set1, fun_dict)
  fun_set_done <- paramUpdate4analyserFun(fun_set2, fun_dict, anaType);
  qs::qsave(fun_set_done, file = "batch_cmds_set.qs")
  
  if(sh4Slurm){
    sweaveSLURMbash(2, fun_set_done, anaType, homeDir, scriptPath)
  } else {
    sweaveTestbash(2, fun_set_done, anaType, homeDir, scriptPath)
  } 
}

funRM <- function(Funset, fun_dict){
  # this function is used to remove the ones NOT allowed based on function dictionary
  fun2rm <- fun_dict$Function_name[fun_dict$InBatch == 0]
  idx_vec <- vector()
  for(i in 1:length(Funset)){
    if((Funset[[i]][[1]][[1]] == "<-") | (Funset[[1]][[1]] == "=")){
      fnm <- Funset[[i]][[1]][[3]][[1]]
    } else {
      fnm <- Funset[[i]][[1]][[1]]
    }
    if(any(fnm == fun2rm)){
      idx_vec <- c(idx_vec, i)
    }
  }
  Funset[idx_vec] <- NULL;
  return(Funset)
}

paramUpdate4analyserFun <- function(Funset, fun_dict, anaType){
  
  if((anaType == "mf") & file.exists("info4batch.qs")){
    
    batchinfo <- qs::qread("info4batch.qs")
    meta_type <- vapply(1:ncol(batchinfo), function(x) {
      is.numeric(batchinfo[,x])
    }, FUN.VALUE = logical(1L))
    # meta_type: FALSE = "CATEGORICAL"; TRUE = "continuous"
    
    meta_res <- colnames(batchinfo);
    
    fun_dictx <- fun_dict[!is.na(fun_dict$Params2customize_inx),]
    nms <- names(Funset)
    meta_res0 <- meta_res[!meta_type];
    
    for(i in 1:length(Funset)){
      if(nms[i] %in% fun_dictx$Function_name){
        str_params <- fun_dictx$Params2customize_inx[nms[i] == fun_dictx$Function_name]
        idxs <- as.integer(strsplit(str_params, "; ")[[1]])
        # Here, we have to perform function specific correction on the parameters
        # PlotPCAPairSummaryMeta
        if(nms[i] == "PlotPCAPairSummaryMeta"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- meta_res0[1];
          Funset[[i]][[1]][[3]][[idxs[2]+1]] <- meta_res0[2];
        }
        # iPCA.Anal
        if(nms[i] == "iPCA.Anal"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- meta_res0[1];
          Funset[[i]][[1]][[3]][[idxs[2]+1]] <- meta_res0[2];
        }
        # CovariateScatter.Anal
        if(nms[i] == "CovariateScatter.Anal"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- meta_res0[1];
          Funset[[i]][[1]][[3]][[idxs[2]+1]] <- meta_res0[2];
        }
        # FeatureCorrelationMeta
        if(nms[i] == "FeatureCorrelationMeta"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- meta_res0[1];
        }
        # RF.AnalMeta
        if(nms[i] == "RF.AnalMeta"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- meta_res0[1];
        }
      } else if(nms[i] == "<<-"){
        # To define global variables for mf module
        # meta.vec.hm2/sort.vec.hm2/adj.vec/meta.vec.rf/meta.vec.aov/meta.vec.asca
        if((Funset[[i]][[1]][[2]] == "meta.vec.hm2") || 
           (Funset[[i]][[1]][[2]] == "sort.vec.hm2") || 
           (Funset[[i]][[1]][[2]] == "adj.vec") || 
           (Funset[[i]][[1]][[2]] == "meta.vec.rf") || 
           (Funset[[i]][[1]][[2]] == "meta.vec.aov") || 
           (Funset[[i]][[1]][[2]] == "meta.vec.asca")){
          len_ele <- length(Funset[[i]][[1]][[3]])
          if((len_ele-1)>length(meta_res0)){
            len_ele <- length(meta_res0)+1
          }
          if(len_ele == 1) {
            Funset[[i]][[1]][[3]][[2]] <- "";
          } else if(len_ele == 2){
            Funset[[i]][[1]][[3]][[2]] <- meta_res0[1]
          } else {
            for(nn in 2:len_ele){
              Funset[[i]][[1]][[3]][[nn]] <- meta_res0[nn-1]
            }
          }
        }
      }
    }
    
    return(Funset)
  } else if((anaType == "roc")){
    
    fun_dictx <- fun_dict[!is.na(fun_dict$Params2customize_inx),]
    nms <- names(Funset);
    nn = 0;
    for(i in 1:length(Funset)){
      
      if(nms[i] %in% fun_dictx$Function_name){
        str_params <- fun_dictx$Params2customize_inx[nms[i] == fun_dictx$Function_name]
        idxs <- as.integer(strsplit(str_params, "; ")[[1]])
        # Here, we have to perform function specific correction on the parameters;
        # Perform.UnivROC
        if(nms[i] == "Perform.UnivROC"){
          nn <- nn + 1;
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- as.symbol(paste0("rownames(feat.rank.mat)[", nn, "]"));
        }
        # PlotRocUnivBoxPlot
        if(nms[i] == "PlotRocUnivBoxPlot"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- as.symbol(paste0("rownames(feat.rank.mat)[", nn, "]"));
        }
        # PrepareROCDetails
        if(nms[i] == "PrepareROCDetails"){
          Funset[[i]][[1]][[3]][[idxs[1]+1]] <- as.symbol(paste0("rownames(feat.rank.mat)[", nn, "]"));
        }
      }
      
      if(nms[i] == "c"){
        # selected.cmpds
        if(Funset[[i]][[1]][[2]] == "selected.cmpds"){
          idx_vec <- seq(2, length(Funset[[i]][[1]][[3]]))
          for(kk in idx_vec){
            Funset[[i]][[1]][[3]][[kk]] <- as.symbol(paste0("rownames(feat.rank.mat)[", kk-1, "]"));
          }
        }
        # selected.smpls
        if(Funset[[i]][[1]][[2]] == "selected.smpls"){
          if(length(Funset[[i]][[1]][[3]]) >1){
            idx_vec <- seq(2, length(Funset[[i]][[1]][[3]]))
            for(kk in idx_vec){
              Funset[[i]][[1]][[3]][[kk]] <- as.symbol("NULL");
            }
          }
        }
      }
    }
    
  } else if(anaType == "msetqea"){
    save(Funset, fun_dict, file = "paramUpdate4analyserFun___285.rda")
  }
  
  return(Funset)
}

sweaveSLURMbash <- function(ncores, cmdlines, anaType, homeDir, scriptPath){
  # basic backbone
  framework <- c("#!/bin/bash",
    "#",
    paste0("#SBATCH --job-name=",anaType,"_Processing"),
    "#",
    "#SBATCH --ntasks=1",
    "#SBATCH --time=20:00",
    "#SBATCH --mem-per-cpu=4G",
    paste0("#SBATCH --cpus-per-task=",ncores),
    paste0("#SBATCH --output=", homeDir,"/slurm_exe_res.out"),
    "\n",
    "srun R -e '")
  
  cat(framework, file = "exe_batch.sh",sep = "\n")

  src0 <- paste0("setwd(\"", homeDir, "\");")
  cat(src0, file = "exe_batch.sh",sep = "\n", append = T);
  
  src1 <- paste0("source(\"", scriptPath, "\");")
  cat(src1, file = "exe_batch.sh",sep = "\n", append = T);
  
  src2 <- paste0("LoadRscripts(\"", anaType, "\");")
  cat(src2, file = "exe_batch.sh",sep = "\n", append = T)
  
  progs_intv <- ceiling(100/(length(cmdlines)+1))
  
  for(i in 1:length(cmdlines)){
    prog_value <- progs_intv*i
    if(prog_value>99){prog_value <- 99}
    cmdx <- paste0("generateProgressMarker(",prog_value, "); ")
    cat(cmdx, file = "exe_batch.sh",sep = "\n", append = T)
    
    cmd <- as.character(cmdlines[[i]])
    cmd <- gsub("mSet <- ","",cmd)
    cmd <- gsub("`","", cmd)
    cmd <- paste0("try(", cmd, ", silent = T)")
    cat(cmd, file = "exe_batch.sh",sep = "\n", append = T)
  }
  
  cmdy <- "save.image(file = \"Rload_batch.RData\")"
  cat(cmdy, file = "exe_batch.sh",sep = "\n", append = T)
  
  cmdz <- paste0("generateProgressMarker(100); ")
  cat(cmdz, file = "exe_batch.sh",sep = "\n", append = T)
  
  cmd_done <- "print(\"Everything has been finished Successfully !\")"
  cat(cmd_done, file = "exe_batch.sh",sep = "\n", append = T)
  
  cat("'", file = "exe_batch.sh",sep = "\n", append = T)
  
}

sweaveTestbash <- function(ncores, cmdlines, anaType, homeDir, scriptPath){
  
  src0 <- paste0("setwd(\"", homeDir, "\");")
  cat(src0, file = "exe_batch.R",sep = "\n", append = T);
  
  src1 <- paste0("source(\"", scriptPath, "\");")
  cat(src1, file = "exe_batch.R",sep = "\n", append = T);
  
  src2 <- paste0("LoadRscripts(\"", anaType, "\");")
  cat(src2, file = "exe_batch.R",sep = "\n", append = T)
  
  progs_intv <- ceiling(100/length(cmdlines))
  
  for(i in 1:length(cmdlines)){
    prog_value <- progs_intv*i
    if(prog_value>100){prog_value <- 100}
    cmdx <- paste0("generateProgressMarker(",prog_value, "); ")
    cat(cmdx, file = "exe_batch.R",sep = "\n", append = T)
    
    cmd <- as.character(cmdlines[[i]])
    cmd <- gsub("mSet <- ","",cmd)
    cmd <- gsub("`","", cmd)
    cmd <- paste0("try(", cmd, ", silent = T)")
    cat(cmd, file = "exe_batch.R",sep = "\n", append = T)
  }
  
  cmdy <- "save.image(file = \"Rload_batch.RData\")"
  cat(cmdy, file = "exe_batch.R",sep = "\n", append = T)
  
  cmd_done <- "print(\"Everything has been finished Successfully !\")"
  cat(cmd_done, file = "exe_batch.R",sep = "\n", append = T)
}

update2newFun <- function(Fun, old_str, new_replace){
  if(old_str=="" & new_replace==""){
    return(Fun)
  }
  # input data file name needs to be updated for all possible functions
  if((Fun[[1]][[1]] == "<-") | (Fun[[1]][[1]] == "=")){
    if(length(Fun[[1]][[3]])<2){return(Fun)}
    for(i in 2:length(Fun[[1]][[3]])){
      if(is.symbol(Fun[[1]][[3]][[i]])){
        if(Fun[[1]][[3]][[i]] == old_str){
          Fun[[1]][[3]][[i]] <- new_replace
          break;
        }
      }
    }
  } else {
    for(i in 2:length(Fun[[1]])){
      if(is.symbol(Fun[[1]][[i]])){
        if(Fun[[1]][[i]] == old_str){
          Fun[[1]][[i]] <- new_replace
          break;
        }
      }
    }
  }
  return(Fun)
}