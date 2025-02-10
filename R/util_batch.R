my.batch.correct <- function(mSetObj=NA, imgName=NULL, Method=NULL, center=NULL){
  
  mSetObj <- .get.mSet(mSetObj);
  
  start.time <- Sys.time()
  
  if(class(mSetObj[["dataSet"]][["batch.cls"]])=="list"){
    
    nms <- names(mSetObj$dataSet$batch);
    nm.list <- vector(length=length(mSetObj$dataSet$batch), mode="list");
    cls.lbls <- batch.lbls <- NULL; # record all batch labels
    
    for (i in 1:length(mSetObj$dataSet$batch)){
      batch <- mSetObj$dataSet$batch[[i]];
      cls <- as.character(mSetObj$dataSet$batch.cls[[i]]);
      nm.list[[i]] <- colnames(batch);
      cls.lbls <- c(cls.lbls,cls); 
      batch.lbls <- c(batch.lbls, rep(nms[i], length=nrow(batch)));
    }
    
    cm.nms <- Reduce(intersect, nm.list); # get common names
    
    # now align all the batches
    mSetObj$dataSet$batch <- lapply(mSetObj$dataSet$batch, function(x){x[,cm.nms]});
    mSetObj[["dataSet"]][["table"]] <- commonMat2 <- do.call(rbind, mSetObj$dataSet$batch);
    mSetObj[["dataSet"]][["batch.cls"]] <- batch.lbl2 <- factor(batch.lbls,levels=names(mSetObj$dataSet$batch), ordered=T);
    mSetObj[["dataSet"]][["class.cls"]] <- class.lbl2 <- factor(cls.lbls);
    
    working_mode <- "file";
    
  } else if (class(mSetObj[["dataSet"]][["batch.cls"]])=="factor") {
    
    commonMat2 <- mSetObj[["dataSet"]][["table"]];
    batch.lbl2 <- mSetObj[["dataSet"]][["batch.cls"]];
    class.lbl2 <- mSetObj[["dataSet"]][["class.cls"]];
    order.lbl2 <- mSetObj[["dataSet"]][["order.cls"]];
    
    working_mode <- "table";
    
  } else {
    AddErrMsg("There is something wrong with your data !")
    AddErrMsg("Most possible cause: absence of batch infromation !")
    return(F)
  }
  
  
  if (is.null(Method)){
    Method<-"auto"
  }
  
  if (is.null(center)){
    center<-""
  }
  
  if (is.null(imgName)){
    imgName<-"image_BC"
  }
  
  ## Extract the information from the mSetObj Object
  
  #order.lbl2 <- mSetObj[["dataSet"]][["order.cls"]];
  QCs<-grep("QC",as.character(class.lbl2));
  
  pheno2 <- data.frame(cbind(class.lbl2, batch.lbl2));
  print(paste("Correcting using the", Method, "method !"))
  modcombat2 <- model.matrix(~1, data=pheno2);
  
  if (all(is.na(commonMat2)) | all(is.null(commonMat2))){
    AddErrMsg(paste0("Your data seems like empty !"))
    return(F)
  }
  if(all(is.infinite(commonMat2))){
    AddErrMsg(paste0("Your data contains too many infinite values!"))
    return(F)
  } else if(any(is.infinite(commonMat2))){
    commonMat2[is.infinite(commonMat2)]<- 0
  }
  try(
    if (Method=="auto"){
      #### QCs Independent------------
      # Correction Method 1 - Combat
 
      if (all(!is.na(as.character(unique(batch.lbl2)))) & !is.null(batch.lbl2)){
        print("Correcting with Combat...");
        Combat_edata<-combat(commonMat2,batch.lbl2,modcombat2);
        mSetObj$dataSet$Combat_edata<-Combat_edata;
      }

      # Correction Method 2 - WaveICA
      if(!.on.public.web){
        if (all(!is.na(as.character(unique(batch.lbl2)))) & !is.null(batch.lbl2) & 
            all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
            print("Correcting with WaveICA...");#require(WaveICA)
            WaveICA_edata<-WaveICA(commonMat2,batch.lbl2,class.lbl2);
            mSetObj$dataSet$WaveICA_edata<-WaveICA_edata;
        }
     }

      # Correction Method 3 - Eigens MS
      if (all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
        print("Correcting with EigenMS...");

        EigenMS_edata<-suppressWarnings(suppressMessages(EigenMS(commonMat2,class.lbl2)));
        mSetObj$dataSet$EigenMS_edata<-EigenMS_edata;
        if (all(is.na(as.character(unique(batch.lbl2)))) | is.null(batch.lbl2)){  
          mSetObj$dataSet$batch.cls <- factor(rep(1,length(mSetObj$dataSet$batch.cls)));
        }
      }

      #### QCs (Quality Control Sample) Dependent---------
      ## Run QCs dependent methods
      if (!identical(QCs,integer(0))){
        #Correction Method 1 - QC-RLSC             # Ref:doi: 10.1093/bioinformatics/btp426
        
        if (working_mode == "table" & (.on.public.web == F | as.numeric(object.size(mSetObj[["dataSet"]][["table"]])/1024/1024) < 0.82)){
          
          if (all(!is.na(as.character(unique(batch.lbl2)))) & !is.null(batch.lbl2) & 
              all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2) &
              !(is.null(order.lbl2) | all(is.na(as.character(unique(order.lbl2)))) | any(is.na(order.lbl2)))){
            print("Correcting with QC-RLSC...");

            QC_RLSC_edata<-QC_RLSC(commonMat2,batch.lbl2,class.lbl2,order.lbl2,QCs);

            mSetObj$dataSet$QC_RLSC_edata <- QC_RLSC_edata;
          }
        }
        # Correction Method 2 - QC-SVRC or QC-RFSC  # Ref:https://doi.org/10.1016/j.aca.2018.08.002
        # Future Options to make this script more powerful
        
        # Correction Method 4 - ANCOVA              # Ref:doi: 10.1007/s11306-016-1015-8
        if (all(!is.na(as.character(unique(batch.lbl2)))) & !is.null(batch.lbl2)){
          print("Correcting with ANCOVA...");
          ANCOVA_edata<-ANCOVA(commonMat2, batch.lbl2, QCs);
          if(!all(is.na(ANCOVA_edata))) {mSetObj$dataSet$ANCOVA_edata <- ANCOVA_edata;}          
        }
      }
      
      #### QCm (Quality Control Metabolites) Dependent---------
      QCms<-grep("IS",colnames(commonMat2));
      if (!identical(QCms,integer(0))){
        # Correction Method 1 - RUV_random          # Ref:doi/10.1021/ac502439y
        print("Correcting with RUV-random...");
        RUV_random_edata<-RUV_random(commonMat2);
        mSetObj$dataSet$RUV_random_edata <- RUV_random_edata;
        
        # Correction Method 2 - RUV2                 # Ref:De Livera, A. M.; Bowne, J. Package metabolomics for R, 2012.
        if (all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
          print("Correcting with RUV-2...");
          RUV_2_edata<-RUV_2(commonMat2,class.lbl2);
          mSetObj$dataSet$RUV_2_edata <- RUV_2_edata;
        }
        # Correction Method 3.1 - RUV_sample        # Ref:https://www.nature.com/articles/nbt.2931
        if (all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
          print("Correcting with RUVs...");
          RUV_s_edata<-RUVs_cor(commonMat2,class.lbl2);
          mSetObj$dataSet$RUV_s_edata <- RUV_s_edata;
        }
        # Correction Method 3.2 - RUVSeq_residual   # Ref:https://www.nature.com/articles/nbt.2931
        if (all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
          print("Correcting with RUVr...");require(edgeR)
          RUV_r_edata<-suppressPackageStartupMessages(RUVr_cor(commonMat2,class.lbl2));
          mSetObj$dataSet$RUV_r_edata <- RUV_r_edata;
        }
        # Correction Method 3.3 - RUV_g             # Ref:https://www.nature.com/articles/nbt.2931
        print("Correcting with RUVg...");
        RUV_g_edata<-RUVg_cor(commonMat2);
        mSetObj$dataSet$RUV_g_edata <- RUV_g_edata;
      }
      
      #### Internal standards based dependent methods--------
      ISs <- grep("IS",colnames(commonMat2));
      if (!identical(ISs,integer(0))){
        
        # Correction Method 1 - NOMIS
        print("Correcting with NOMIS...")
        NOMIS_edata <- NOMIS(commonMat2)
        mSetObj$dataSet$NOMIS_edata <- NOMIS_edata;
        
        # Correction Method 2 - CCMN
        if (all(!is.na(as.character(unique(class.lbl2)))) & !is.null(class.lbl2)){
          print("Correcting with CCMN...")
          CCMN_edata <- CCMN2(commonMat2,class.lbl2)
          mSetObj$dataSet$CCMN_edata <- CCMN_edata;
        }
      }
      
      ###################
      
      nms<-names(mSetObj$dataSet)
      nms<-nms[grepl("*edata",nms)]
      nms<- c("table",nms)
      
      interbatch_dis<-sapply(nms, FUN=.evaluate.dis, mSetObj=mSetObj, center=center)

      mSetObj$dataSet$interbatch_dis <- interbatch_dis
      best.choice<-names(which(min(interbatch_dis)==interbatch_dis))
      best.table <- mSetObj$dataSet[[best.choice]];
      mSetObj$dataSet$adjusted.mat <- best.table;
      
      Method <- sub(pattern = "_edata",x = best.choice, replacement = "")

      print(paste("Best results generated by ", Method, " !"))
      #=======================================================================/
    } else if (Method=="Combat"){
      
      if(any(is.na(batch.lbl2)) | is.null(batch.lbl2)){
        AddErrMsg(paste0("batch inforamtion is required for ",Method," !"))
        return(F)
      }
      
      Combat_edata<-combat(commonMat2,batch.lbl2,modcombat2);
      mSetObj$dataSet$adjusted.mat<-mSetObj$dataSet$Combat_edata <- Combat_edata;
      
    } else if (Method=="WaveICA"){
      
      if(any(is.na(batch.lbl2)) | is.null(batch.lbl2)){
        AddErrMsg(paste0("batch inforamtion is required for ",Method," !"))
        return(F)
      }
      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      WaveICA_edata<-WaveICA(commonMat2,batch.lbl2,class.lbl2);
      mSetObj$dataSet$adjusted.mat<-mSetObj$dataSet$WaveICA_edata <- WaveICA_edata;
      
    } else if (Method=="EigenMS"){
      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      EigenMS_edata<-EigenMS(commonMat2,class.lbl2);
      mSetObj$dataSet$adjusted.mat<-mSetObj$dataSet$EigenMS_edata <- EigenMS_edata;
      
    } else if (Method=="QC_RLSC"){
      
      if(any(is.na(batch.lbl2)) | is.null(batch.lbl2)){
        AddErrMsg(paste0("batch inforamtion is required for ",Method," !"))
        return(F)
      }
      
      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      
      if(any(is.na(order.lbl2)) | is.null(order.lbl2)){
        AddErrMsg(paste0("order inforamtion is required for ",Method," !"))
        return(F)
      }
   
      if(any(is.na(QCs)) | is.null(QCs) | identical(QCs, integer(0))){
        AddErrMsg(paste0("QC inforamtion is required for ",Method," !"))

        return(F)
      }
      
      QC_RLSC_edata<-suppressWarnings(suppressMessages(QC_RLSC(commonMat2,batch.lbl2,class.lbl2,order.lbl2,QCs)));
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$QC_RLSC_edata <- QC_RLSC_edata;
      
    } else if (Method=="ANCOVA"){
      
      if(any(is.na(batch.lbl2)) | is.null(batch.lbl2)){
        AddErrMsg(paste0("batch inforamtion is required for ",Method," !"))
        return(F)
      }
      
      if(any(is.na(QCs)) | is.null(QCs) | identical(QCs, integer(0))){
        AddErrMsg(paste0("QC inforamtion is required for ",Method," !"))
        return(F)
      }
      
      ANCOVA_edata<-ANCOVA(commonMat2,batch.lbl2,QCs);
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$ANCOVA_edata <- ANCOVA_edata;
      
    } else if (Method=="RUV_random"){
      
      QCms<-grep("IS",colnames(commonMat2));
      
      if(any(is.na(QCms)) | is.null(QCms) | identical(QCms, integer(0))){
        AddErrMsg(paste0("Quality Control Metabolites inforamtion is required for ",Method," !"))
        return(F)
      }

      RUV_random_edata<-RUV_random(commonMat2);
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$RUV_random_edata <- RUV_random_edata;
      
    } else if (Method=="RUV_2"){
      
      QCms<-grep("IS",colnames(commonMat2));      
      if(any(is.na(QCms)) | is.null(QCms) | identical(QCms, integer(0))){
        AddErrMsg(paste0("Quality Control Metabolites inforamtion is required for ",Method," !"))
        return(F)
      }
      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      
      RUV_2_edata<-RUV_2(commonMat2,class.lbl2);
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$RUV_2_edata <- RUV_2_edata;
      
    } else if (Method=="RUV_s"){
      
      QCms<-grep("IS",colnames(commonMat2));
      
      if(any(is.na(QCms)) | is.null(QCms) | identical(QCms, integer(0))){
        AddErrMsg(paste0("Quality Control Metabolites inforamtion is required for ",Method," !"))
        return(F)
      }

      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      
      RUV_s_edata<-RUVs_cor(commonMat2,class.lbl2);
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$RUV_s_edata <- RUV_s_edata;
      
    } else if (Method=="RUV_r"){
      
      QCms<-grep("IS",colnames(commonMat2));
      
      if(any(is.na(QCms)) | is.null(QCms) | identical(QCms, integer(0))){
        AddErrMsg(paste0("Quality Control Metabolites inforamtion is required for ",Method," !"))
        return(F)
      }

      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      
      RUV_r_edata<-suppressPackageStartupMessages(RUVr_cor(commonMat2,class.lbl2));
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$RUV_r_edata <- RUV_r_edata;
      
    } else if (Method=="RUV_g"){
      
      QCms<-grep("IS",colnames(commonMat2));
      
      if(any(is.na(QCms)) | is.null(QCms) | identical(QCms, integer(0))){
        AddErrMsg(paste0("Quality Control Metabolites inforamtion is required for ",Method," !"))
        return(F)
      }


      RUV_g_edata<-RUVg_cor(commonMat2);
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$RUV_g_edata <- RUV_g_edata;
      
    } else if (Method=="NOMIS"){
        
      ISs <- grep("IS",colnames(commonMat2));      
      if(any(is.na(ISs)) | is.null(ISs) | identical(ISs, integer(0))){
        AddErrMsg(paste0("Internal Standards inforamtion is required for ",Method," !"))
        return(F)
      }

      NOMIS_edata <- NOMIS(commonMat2)
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$NOMIS_edata <- NOMIS_edata;
      
    } else if (Method=="CCMN") {
      
      if(any(is.na(class.lbl2)) | is.null(class.lbl2)){
        AddErrMsg(paste0("class inforamtion is required for ",Method," !"))
        return(F)
      }
      
      ISs <- grep("IS",colnames(commonMat2));      
      if(any(is.na(ISs)) | is.null(ISs) | identical(ISs, integer(0))){
        AddErrMsg(paste0("Internal Standards inforamtion is required for ",Method," !"))
        return(F)
      }

      CCMN_edata <- CCMN2(commonMat2,class.lbl2)
      mSetObj$dataSet$adjusted.mat <- mSetObj$dataSet$CCMN_edata <- CCMN_edata;      
    }    
    ,silent=T)

  if (is.null(mSetObj$dataSet$adjusted.mat) | all(is.na(mSetObj$dataSet$adjusted.mat))){
    AddErrMsg(paste0("Your data or format is not valid! Please double check!"));
    return(F);
  }

  if (Method != "auto"){
    
    nms<-names(mSetObj$dataSet)
    nms<-nms[grepl("*edata",nms)]
    nms<- c("table",nms)
    
    interbatch_dis<-sapply(nms,FUN=.evaluate.dis,mSetObj=mSetObj,center=center)
    mSetObj$dataSet$interbatch_dis <- interbatch_dis
    
  }
  mSetObj <- PlotPCA.overview(mSetObj, imgName, method=Method);
  Plot.sampletrend(mSetObj,paste0(imgName,"Trend"),method=Method);
  plot_dist(mSetObj,paste0(imgName,"dist"))
  best.table <- mSetObj$dataSet$adjusted.mat
  
  # save the meta-dataset
  res <- data.frame(colnames(t(best.table)), class.lbl2, batch.lbl2, best.table);
  colnames(res) <- c('NAME', 'CLASS', 'Dataset', colnames(best.table));
  write.table(res, sep=",", file="MetaboAnalyst_batch_data.csv", row.names=F, quote=FALSE);
  
  if(.on.public.web){
    .set.mSet(mSetObj)
    return("T");
  }
  
  end.time <- Sys.time()
  return(.set.mSet(mSetObj));
}
