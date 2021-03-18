
my.clean.lipid <- function(qvec){
  
  qvec <- qvec;
  
  # first remove A/B 
  qvec <- gsub("/A", "", qvec, fixed = TRUE)
  qvec <- gsub("_A", "", qvec, fixed = TRUE)
  qvec <- gsub("/B", "", qvec, fixed = TRUE)
  qvec <- gsub("_B", "", qvec, fixed = TRUE)
  
  dash <- paste(c("-1", "-2", "_1", "_2", "_3", "_4", "_5"), collapse = "|")
  qvec <- gsub(dash, "", qvec)
  
  # second remove any RT and adduct info
  load_stringr()
  
  rt.pat <- paste0(c("@", "\\[M"), collapse = "|")
  at.inx <- str_detect(qvec, rt.pat)
  
  if(sum(at.inx) > 0){
    qvec[at.inx] <- gsub("[;].*", "", qvec[at.inx], fixed = TRUE)
  }
  
  # third remove everything inside + including square brackets 
  square.pattern <- paste0(c("\\[", "\\]"), collapse = "|") 
  square.inx <- str_detect(qvec, square.pattern)
  
  if(sum(square.inx)>0){
    qvec <- gsub("\\[.*?\\]", "", qvec, fixed = TRUE)
  }
  
  # fourth remove ending parentheses with (Z, Z...)
  qvec <- trimws(gsub("\\(Z|,Z\\)-|,Z|n-|\\(Z\\)-", "", qvec))
  
  # fifth remove plus sign
  qvec <-trimws(gsub("^\\+", "", qvec))
  
  # sixth remove parantheses containing OH or C
  qvec <- trimws(gsub("\\h*\\([^)]*\\b(?:OH||C)\\b[^)]*\\)", "", qvec))
  
  # fix up non-standard acronyms
  coq.inx <- str_detect(qvec, "Co\\(Q")
  if(sum(coq.inx) > 0){
    qvec[coq.inx] <-  trimws(gsub("[()]", " ", gsub("Co", "Coenzyme", qvec[coq.inx], fixed = TRUE)))
  }
  
  acca.inx <- str_detect(qvec, "AcCa")
  if(sum(acca.inx) > 0){
    qvec[acca.inx] <-  trimws(gsub("[()]", " ", gsub("AcCa", "CAR", qvec[acca.inx], fixed = TRUE)))
  }
  
  ac.inx <- str_detect(qvec, "AC\\(")
  if(sum(ac.inx) > 0){
    qvec[ac.inx] <-  trimws(gsub("[()]", " ", gsub("AC", "CAR", qvec[ac.inx], fixed = TRUE)))
  }
  
  a.dash.inx <- str_detect(qvec, "\\(a-")
  if(sum(a.dash.inx) > 0){
    qvec[a.dash.inx] <- trimws(gsub("[()]", " ", gsub("a-", "", qvec[a.dash.inx], fixed = TRUE)))
  }
  
  pa.inx <- str_detect(qvec, fixed("Plasmanyl-", ignore_case=TRUE))
  if(sum(pa.inx) > 0){
    qvec[pa.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[pa.inx], fixed("Plasmanyl-", ignore_case=TRUE), "")))
  }
  
  pe.inx <- str_detect(qvec, fixed("Plasmenyl-", ignore_case=TRUE))
  if(sum(pe.inx) > 0){
    qvec[pe.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[pe.inx], fixed("Plasmenyl-", ignore_case=TRUE), "")))
  }
  
  foura.inx <- str_detect(qvec, fixed("aaaa-", ignore_case=TRUE))
  if(sum(foura.inx) > 0){
    qvec[foura.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[foura.inx], fixed("aaaa-", ignore_case=TRUE), "")))
  }
  
  twoa.inx <- str_detect(qvec, fixed("aaaa-", ignore_case=TRUE))
  if(sum(twoa.inx) > 0){
    qvec[twoa.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[twoa.inx], fixed("aa-", ignore_case=TRUE), "")))
  }
  
  phy.inx <- str_detect(qvec, fixed("Phytocer", ignore_case=TRUE))
  if(sum(phy.inx) > 0){
    qvec[phy.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[phy.inx], fixed("Phytocer", ignore_case=TRUE), "Cer")))
  }
  
  dec.inx <- str_detect(qvec, fixed("deoxy-Cer", ignore_case=TRUE))
  if(sum(dec.inx) > 0){
    qvec[dec.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[dec.inx], fixed("deoxy-Cer", ignore_case=TRUE), "1-DeoxyCer")))
  }
  
  hex.inx <- str_detect(qvec, fixed("Hex-Cer", ignore_case=TRUE))
  if(sum(hex.inx) > 0){
    qvec[hex.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[hex.inx], fixed("Hex-Cer", ignore_case=TRUE), "HexCer")))
  }
  
  cerp.inx <- str_detect(qvec, fixed("CerP", ignore_case=TRUE))
  if(sum(cerp.inx) > 0){
    qvec[cerp.inx] <- trimws(gsub(";O2", "", gsub("[()]", " ", str_replace(qvec[cerp.inx], " ", " d"))))
  }
  
  lpc.inx <- str_detect(qvec, fixed("LPC", ignore_case=TRUE))
  if(sum(lpc.inx) > 0){
    qvec[lpc.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[lpc.inx], fixed("LPC", ignore_case=TRUE), "LysoPC")))
  }
  
  lpe.inx <- str_detect(qvec, fixed("LPE", ignore_case=TRUE))
  if(sum(lpe.inx) > 0){
    qvec[lpe.inx] <- trimws(gsub("[()]", " ", str_replace(qvec[lpe.inx], fixed("LPE", ignore_case=TRUE), "LysoPE")))
  }
  
  # last replace . _ ; to slash if no instances of slash in qvec
  
  slash.inx <- str_detect(qvec, "/")
  
  if(sum(slash.inx) == 0){
    
    period.inx <- str_detect(qvec, "[.]")
    under.inx <- str_detect(qvec, "[_]")
    semi.inx <- str_detect(qvec, "[;]")
    
    if(sum(period.inx) > 0){
      qvec <- gsub(".", "/", qvec, fixed = TRUE)
    }
    
    if(sum(under.inx) > 0){
      qvec <- gsub("_", "/", qvec, fixed = TRUE)
    }
    
    if(sum(semi.inx) > 0){
      
      qvec <- lapply(seq_along(qvec), function(i) {
        
        change <- semi.inx[i]
        lipid <- qvec[i]
        
        if(change & nchar(lipid) > 25){
          lipid <- gsub(".*[;]", "", lipid, fixed = TRUE)
        }else{
          lipid <- gsub(";", "/", lipid, fixed = TRUE)
        }
        lipid
      } )    
      
      qvec <- unlist(qvec)
    }
  }
  return(trimws(qvec))
}
