my.approx.match <- function(mSetObj=NA, q, lipid){

  mSetObj <- .get.mSet(mSetObj);

  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    cmpd.db <- .get.my.lib("lipid_compound_db.qs");
  }else if(anal.type == "utils"){
    cmpd.db <- .get.my.lib("master_compound_db.qs");
  }else{
    cmpd.db <- .get.my.lib("compound_db.qs");
  }

  if(anal.type %in% c("msetora", "msetssp", "msetqea") & lipid){
    syn.db <- .get.my.lib("lipid_syn_nms.qs")
  }else if(anal.type == "utils"){
    syn.db <- .get.my.lib("master_syn_nms.qs")
  }else{
    syn.db <- .get.my.lib("syn_nms.qs")
  }
  
  if(!lipid){
    # only for none lipids
    nonLipidInx <- cmpd.db$lipid == 0;
    com.nms <- cmpd.db$name[nonLipidInx];
    
    syns.vec <- syn.db$syns.vec[nonLipidInx];
    syns.list <- syn.db$syns.list[nonLipidInx];
    
    matched.dist <- NULL;
    q.length <- nchar(q);
    s <- c(0, 0.1, 0.2);
    
    # init withno hits, then see if any hits
    mSetObj$dataSet$candidates <- NULL;
    
    for (j in s) {
      new.q <- q;
      if(q.length > 32){ # note: agrep fail for exact match when length over 32 characters
        new.q<-substr(q, 1, 32);
      }
      matched <- FALSE;
      matched.inx <- agrep(new.q, syns.vec, ignore.case=T, max.distance=j, useBytes=T);
      
      if(length(matched.inx) > 0) {
        # record all the candidates,
        # don't use cbind, since all will be converted to character mode
        # for data.frame specify "stringsAsFactors" to prevent convert value col into factor
        candidates <- data.frame(index=vector(mode = "numeric", length=length(matched.inx)),
                                 value=vector(mode = "character", length=length(matched.inx)),
                                 score=vector(mode = "numeric", length=length(matched.inx)),
                                 stringsAsFactors = FALSE);
        
        for(n in 1:length(matched.inx)){
          nm.vec<-syns.list[[matched.inx[n]]];
          # try approximate match, note: in some cases, split into element will break match using whole string
          hit3.inx <- agrep(q,nm.vec,ignore.case=T, max.distance=j, useBytes=T);
          if(length(hit3.inx)>0){
            hit3.nm <- vector(mode = "character", length=length(hit3.inx));
            hit3.score <- vector(mode = "numeric", length=length(hit3.inx));
            for(k in 1:length(hit3.inx)){
              idx <- hit3.inx[k];
              hit3.nm[k] <- nm.vec[idx];
              hit3.score[k] <- j + abs(nchar(nm.vec[idx])-nchar(q))/(10*nchar(q));
            }
            
            # now get the best match, the rule is that the first two character should matches
            # first check if first two character are digits or characters, otherwise will cause error
            matches2 <- c();
            if(length(grep("^[1-9a-z]{2}", q, ignore.case=T))>0){
              matches2 <- grep(paste("^", substr(q, 1, 2), sep=""), hit3.nm);
            }else if (length(grep("^[1-9a-z]", q, ignore.case=T))>0){
              matches2 <- grep(paste("^", substr(q, 1, 1), sep=""), hit3.nm);
            }
            
            if(length(matches2)>0){
              hit3.score[matches2] <- hit3.score[matches2] - 0.05;
            }
            
            best.inx<-which(hit3.score==min(hit3.score))[1];
            candidates[n,1]<-matched.inx[n];
            #    candidates[n,2]<-hit3.nm[best.inx]; # show matched syn names
            candidates[n,2]<-com.nms[matched.inx[n]] # show common names
            candidates[n,3]<-hit3.score[best.inx];
          }      
        }
        
        rm.inx <- is.na(candidates[,2]) | candidates[,2]=="NA" | candidates[,2]=="";
        mSetObj$dataSet$candidates<-candidates[!rm.inx, ];  
        mSetObj$dataSet$candidates<-candidates[order(candidates[,3], decreasing=F), , drop=F];    
        
        if(nrow(candidates) > 10){
          mSetObj$dataSet$candidates<-candidates[1:10,];
        }
        return(.set.mSet(mSetObj));
      }
    }
  }else{
    
    mSetObj$dataSet$candidates <- NULL;
    
    new.q <- CleanLipidNames(q)
    syns.vec <- syn.db$syns.vec;
    com.nms <- cmpd.db$name
    
    matched.inx <- agrep(new.q, syns.vec, ignore.case=T, max.distance=0, useBytes=T);
    
    if(length(matched.inx) == 0){
      matched.inx <- agrep(new.q, syns.vec, ignore.case=T, max.distance=0.1, useBytes=T);
    }
    
    if(length(matched.inx) > 0){

      candidates <- data.frame(index=vector(mode = "numeric", length=length(matched.inx)),
                               value=vector(mode = "character", length=length(matched.inx)),
                               score=vector(mode = "numeric", length=length(matched.inx)),
                               stringsAsFactors = FALSE);
      
      for(n in seq_along(matched.inx)){
        candidates[n,1] <- matched.inx[n];
        candidates[n,2] <- com.nms[matched.inx[n]] # show common names
        candidates[n,3] <- min(as.numeric(adist(new.q, unlist(strsplit(syns.vec[matched.inx[1]], "; ")) )))
      }
      
      
      candidates <- candidates[order(candidates[,3]),]
      
      if(nrow(candidates) > 10){
        matched.inx <- candidates[1:10, ]
        candidates <- candidates[1:10, ]
      }
      
      mSetObj$dataSet$candidates <- candidates    
    }
  }
  return(.set.mSet(mSetObj));
}
