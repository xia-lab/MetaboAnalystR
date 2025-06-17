#############################################
## Perform API calls, used by local MetaboAnalystR
## Jeff Xia (jeff.xia@xialab.ca) 
## 09/08/022
#######################

my.ora.kegg <- function(endpoint="/pathwayora"){
  
    call <- paste(api.base, endpoint, sep="");  
    mSetObj <- .do.api.call(call);
    
    # check if result is there
    if(is.null(mSetObj) || nrow(mSetObj$analSet$ora.mat) == 0){
      AddErrMsg("Failed to perform pathway analysis!")
      return(0)
    }
    
    fast.write.csv(mSetObj$analSet$ora.mat, file="pathway_results.csv");
    return(.set.mSet(mSetObj))
}

my.hyperscore.kegg <- function(endpoint="/msetora"){
    
    call <- paste(api.base, endpoint, sep="");
    mSetObj <- .do.api.call(call);
    
    # parse json response from server to results
    if(is.null(mSetObj) || is.null(mSetObj$analSet$ora.mat)){
      AddErrMsg("Error! Mset ORA via xialab.ca/api unsuccessful!")
      return(0)
    }

    print("Mset ORA via xialab.ca/api successful!")
    
    fast.write.csv(mSetObj$analSet$ora.mat, file="msea_ora_result.csv");
    return(.set.mSet(mSetObj));
}

my.qea.kegg <- function(endpoint="/msetqea"){

    call <- paste(api.base, endpoint, sep="");
    mSetObj <- .do.api.call(call);
    
    if(is.null(mSetObj) || is.null(mSetObj$analSet$qea.mat)){
      AddErrMsg("Error! QEA Pathway Analysis via xialab.ca/api unsuccessful!")
      return(0)
    }

    print("Enrichment QEA via xialab.ca/api successful!")
    
    fast.write.csv(mSetObj$analSet$qea.mat, file="msea_qea_result.csv");
    return(.set.mSet(mSetObj));
}

my.pathway.qea <- function(endpoint="/pathwayqea"){

    call <- paste(api.base, endpoint, sep="");
    mSetObj <- .do.api.call(call);
    
    if(is.null(mSetObj) || is.null(mSetObj$analSet$qea.mat)){
      AddErrMsg("Error! Pathway QEA via xialab.ca/api unsuccessful!")
      return(0)
    }

    print("Pathway QEA via xialab.ca/api successful!")
    
    fast.write.csv(mSetObj$analSet$qea.mat, file="pathway_results.csv");
    return(.set.mSet(mSetObj));
}

my.integ.kegg <- function(endpoint="/jointpath"){
    
    call <- paste(api.base, endpoint, sep="");
    mSetObj <- .do.api.call(call);
    
    if(is.null(mSetObj) || is.null(mSetObj$dataSet$path.mat)){
      AddErrMsg("Error! Joint Pathway Analysis via xialab.ca/api unsuccessful!")
      return(0)
    }
    
    print("Joint Pathway Analysis via xialab.ca/api successful!")
    
    rownames(mSetObj$dataSet$path.mat) <- mSetObj$dataSet$jointpa.pathnames
    fast.write.csv(mSetObj$dataSet$path.mat, file="MetaboAnalyst_result_pathway.csv", row.names=TRUE);
    qs::qsave(mSetObj$dataSet$pathinteg.impTopo, file = "pathinteg.impTopo.qs")
    #mSetObj$dataSet$pathinteg.impMat <- impMat;    
    return(.set.mSet(mSetObj));
 }

my.namemap.api <- function(endpoint="/internal_mapcompounds"){
  
    call <- paste(api.base, endpoint, sep="");
    request <- .do.api.call(call);

    if(is.null(request) ||is.null(request$name.map)){
        AddErrMsg("Error! Compound name mapping via xialab.ca/api unsuccessful!")
        return(0)
    }

    mSetObj <- request;
    print("Compound name mapping via xialab.ca/api successful!");
    return(.set.mSet(mSetObj));
}

my.kegg.plot <- function(endpoint="/pathway_kegg_plot", 
                         format = "png", width = 8, height = 8, dpi = default.dpi){
  
    call <- paste(api.base, endpoint, sep="");
    mSetObj <- .do.api.call(call);

    if(is.null(mSetObj)){
        AddErrMsg("Error! Call xialab.ca/api unsuccessful!")
        return(0)
    }
    
    # second create image from g object from server
    if(mSetObj$api$analType == "pathinteg"){
      
      # joint pathway
      g <- mSetObj$api$inmex.plot
      
      pathName <- gsub("\\s","_", mSetObj$api$inmex.pathname);
      pathName <- gsub(",","", pathName);
      
      dpi <- default.dpi
      
      imgName = paste(pathName, "_dpi", dpi, ".", format, sep="");
      
      Cairo::Cairo(file = imgName, dpi=dpi, width=width, height=height, type=format, bg="white");
      par(mai=rep(0,4));
      plotGraph(g, vertex.label=V(g)$plot_name, vertex.color=mSetObj$api$inmex.plot.bg.cols,
                vertex.frame.color=mSetObj$api$inmex.plot.line.cols);
      dev.off();
      
    }else{
      
      imgName <- mSetObj$api$imgName;
      # pathway
      if(!exists("dpi")){dpi <- default.dpi;}
      if(!exists('width')){w <- h <- width <- height <- 8}
      if(is.null(dpi)){
        Cairo::Cairo(file=imgName, width=width, height=height, type="png", bg="white");
      }else{
        if(is.na(width)){
          width <- 8;
          w <- h <- width;
        }
        Cairo::Cairo(file = imgName, dpi=dpi, width=width, height=height, type=format, bg="white");
      }
      
      par(mai=rep(0,4));
      plot(mSetObj$api$metpa.plot)
      dev.off();
    }
    
    print(paste0(mSetObj$api$imgName, " saved to current working directory!"))    
    return(.set.mSet(mSetObj));
}

# internal call
.do.api.call <- function(call, file.send = "tosend.rds"){

    load_httr();
    request <- httr::POST(url = call, 
                          body = list(rds = upload_file(file.send, "application/octet-stream")),
                          encode = "multipart")
    
    # check if successful
    if(request$status_code != 200){
      AddErrMsg("Failed to connect to the API Server!")
      return(NULL)
    }
    
    return(unserialize(httr::content(request, "raw")));
}