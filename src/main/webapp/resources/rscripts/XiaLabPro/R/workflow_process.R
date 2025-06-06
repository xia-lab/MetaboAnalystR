## A collection of common functions used for pro tools
## other functions can be in lazy mode

.load.pro.scripts.on.demand <- function(fileName=""){
  relPath <- paste0("rscripts/XiaLabPro/R/", fileName)
  
  complete.path <- paste0("../../", relPath)
  
  compiler::loadcmp(complete.path)
  
  mSetObj <- .get.mSet(mSetObj)
  
  if(is.null(mSetObj$paramSet$loaded.scripts)){
    mSetObj$paramSet$loaded.scripts <- list()
  }
  
  # Store only the relative path to ensure uniqueness and consistency
  if(!relPath %in% mSetObj$paramSet$loaded.scripts) {
    mSetObj$paramSet$loaded.scripts <- c(mSetObj$paramSet$loaded.scripts, relPath)
  }
  
  .set.mSet(mSetObj)
}


## This script is mainly designed to process data processing in a batch mode
## It includes 1). convert current Rhistory.R into an template file;
##             2). organize R command lines into a format for SLURM;
##             3). update new file as input for SLURM template (.sh);
##             4). execute the SLURM shell script to finish the batch processing;

InitializeBatchTemplate <- function(){
  if(file.exists("../../libs/func_dict.qs")){
    fun_dict <- qs::qread("../../libs/func_dict.qs")
  } else if(file.exists("/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/libs/func_dict.qs")) {
    fun_dict <- qs::qread("/tmp/payaramicro_unpack/applications/MetaboAnalyst/resources/libs/func_dict.qs")
  } else if(file.exists("/home/glassfish/payara5/glassfish/domains/domain1/applications/MetaboAnalyst-Pro/resources/libs/func_dict.qs")) {
    fun_dict <- qs::qread("/home/glassfish/payara5/glassfish/domains/domain1/applications/MetaboAnalyst-Pro/resources/libs/func_dict.qs")
  } else if(file.exists("/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/libs/func_dict.qs")){
    fun_dict <- qs::qread("/home/glassfish/payara6_micro/MetaboAnalyst_dir/applications/MetaboAnalyst/resources/libs/func_dict.qs")
  }
  bt <- list();
  bt$fun_dict <- fun_dict;
  bt$fun_set <- list();
  qs::qsave(bt, file = "RBatchTemplate.qs")
}

UpdateBatchTemplate <- function(rCommand){
  if(!file.exists("RBatchTemplate.qs")){
    InitializeBatchTemplate();
  }
  bt <- qs::qread("RBatchTemplate.qs");
  # TODO: to do some filtration based on the dictionary; and write down the part which is truly needed
  rCommand_exp <- parse(text = rCommand)
  if(rCommand_exp[[1]][[1]] == "<-" | rCommand_exp[[1]][[1]] == "="){
    func_name <- rCommand_exp[[1]][[3]][[1]]
  } else {
    func_name <- rCommand_exp[[1]][[1]]
  }
  
  numi <- length(bt$fun_set)
  bt$fun_set <- c(bt$fun_set, list(rCommand_exp))
  names(bt$fun_set)[numi + 1] <- as.character(func_name)
  #bt$fun_set[[func_name]] <- rCommand_exp
  qs::qsave(bt, file = "RBatchTemplate.qs")
}

updateJSFHistory <- function(projectDbPath = "", targetPrj_TK = ""){
    if(!exists("my.update.JSFHistory")){ 
      .load.pro.scripts.on.demand("util_workflow.Rc");    
    }
    return(my.update.JSFHistory(projectDbPath, targetPrj_TK)); 
}

# This function will summarize all functions and generate a .sh file to submit to SLURM scheduler for executing
# anaType is used to specify the analysis type
# newDataCmds is used to absorb new command lines used for new data importing and sanity checking => replace the ones in template
# template_path is used to specify the path of template file

generateSLURMbash <- function(anaType, newDataCmds, template_path, homeDir, scriptPath, sh4Slurm = TRUE){
      if(!exists("my.generate.SLURMbash")){ 
      .load.pro.scripts.on.demand("util_workflow.Rc");    
    }
    return(my.generate.SLURMbash(anaType, newDataCmds, template_path, homeDir, scriptPath, sh4Slurm)); 
}


generateProgressMarker <- function(value){
  if(file.exists("progress_value")){
    write.table(value, file = "progress_value", quote = F, row.names = F, col.names = F)
  } else {
    write.table(value, file = "progress_value", quote = F, row.names = F, col.names = F)
  }
}

transferJSFHistory <- function(projectDbPath = "", template_TK, batchPrj_TK){
  
  print(paste0("projectDbPath ==> ", projectDbPath))
  print(paste0("template_TK   ==> ", template_TK))
  print(paste0("batchPrj_TK   ==> ", batchPrj_TK))
  
  # template_TK <- "13f246ea95bd44ad9f9e54c17837376feefd13d8c76e91da02b02581987667da"
  # batchPrj_TK <- "2d2c23871c820b336901295573cd9e3ae60b2959659af523ecf3442b2dacf8ea"
  # projectDbPath <- "/mnt/disks/launchpad/omicsquare_account.sqlite"
  toolName <- "MetaboAnalyst"
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
    
    query <- "SELECT javaHistory FROM project WHERE toolName = ? AND partialToken = ?"
    res0 <- dbGetQuery(con, query, params = list(toolName, batchPrj_TK))
    qs::qsave(res0, file = "original_batch_jsfhistory.qs")
    
    query <- "SELECT javaHistory FROM project WHERE toolName = ? AND partialToken = ?"
    res <- dbGetQuery(con, query, params = list(toolName, template_TK))
    if (nrow(res) == 0) {
      return(NULL)  # Return NULL if no matches
    }
    
    query <- "UPDATE project SET javaHistory=? WHERE partialToken=?"
    dbExecute(con, query, list(res$javaHistory, batchPrj_TK))
    
    dbCommit(con)
    transactionStarted <- FALSE
  }, error = function(e) {
    stop("An error occurred while fetching data: ", e)
  })

}


extractGraphicsMap <- function(projectDbPath = "", targetPrj_TK = ""){
  print(paste0("now the 380 ==> ", targetPrj_TK))
  toolName <- "MetaboAnalyst"
  print(paste0("now the targetPrj_TK ==> ", targetPrj_TK))
  print(paste0("now the targetPrj_TK ==> ", projectDbPath))
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
  print(paste0("now the 396 ==> ", targetPrj_TK))
  tryCatch({
    dbBegin(con)
    transactionStarted <- TRUE
    
    query <- "SELECT javaHistory FROM project WHERE toolName = ? AND partialToken = ? AND projectType = 'project'"
    res <- dbGetQuery(con, query, params = list(toolName, targetPrj_TK));
    
    if (nrow(res) == 0) {
      print(paste0("now the 405 ==> ", res))
      return(NULL)  # Return NULL if no matches
    }

    dbCommit(con)
    transactionStarted <- FALSE
  }, error = function(e) {
    stop("An error occurred while fetching data: ", e)
  })
  print(paste0("now the 412 ==> ", targetPrj_TK))
  str1 <- res$javaHistory[1]
  jhs <- rjson::fromJSON(str1)
  jsession <- rjson::fromJSON(jhs[["NA.dummy.SessionBean1"]])
  str2 <- jsession$graphicsMap
  print(paste0("now the 417 ==> ", targetPrj_TK))
  cmd_nms <- names(str2)
  cmds <- unname(str2)
  return(paste(cmd_nms, cmds, sep = "\t\t\t"))
}

extractImgMap <- function(projectDbPath = "", targetPrj_TK = ""){
  
  require(RSQLite)
  require(DBI)
  con <- DBI::dbConnect(RSQLite::SQLite(), projectDbPath)
  toolName <- "MetaboAnalyst"
  transactionStarted <- FALSE
  
  on.exit({
    if(transactionStarted) {
      dbRollback(con)
    }
    dbDisconnect(con)
  }, add = TRUE)
  print(paste0("now the 475 ==> ", targetPrj_TK))
  tryCatch({
    dbBegin(con)
    transactionStarted <- TRUE
    
    query <- "SELECT javaHistory FROM project WHERE toolName = ? AND partialToken = ? AND projectType = 'project'"
    res <- dbGetQuery(con, query, params = list(toolName, targetPrj_TK));
    
    if (nrow(res) == 0) {
      print(paste0("now the 484 ==> ", res))
      return(NULL)  # Return NULL if no matches
    }
    
    dbCommit(con)
    transactionStarted <- FALSE
  }, error = function(e) {
    stop("An error occurred while fetching data: ", e)
  })
  
  print("RUnning into 494 --------")
  jhs <- rjson::fromJSON(json_str = res$javaHistory[1])
  jsession <- rjson::fromJSON(jhs[["NA.dummy.SessionBean1"]])
  print("RUnning into 497 --------")
  return(paste(names(jsession[["imgMap"]]), unname(jsession[["imgMap"]]), sep = "\t\t\t"))
  
}

{
  ## functional utils 
  
  
}

{
  # This is a function to update function dictionary, which is used to prepare SLURM template
  template_prepare <- function(){
    fun_dict <- read.csv("~/Downloads/Functions_batch_dict_metaboanalyst.csv")
    qs::qsave(fun_dict, file = "/home/qiang/NetBeansProjects/OmicsPros/MetaboAnalyst/src/main/webapp/resources/libs/func_dict.qs")
  }

}



