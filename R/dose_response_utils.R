###########################################
# Basic functions defining models 
# for plot, fit, BMD calculation
###########################################

#library(dplyr)
#library(data.table)
#library(boot)
####### functions for drcfit ########
# define formulas for non-linear models, and functions to find the starting parameter values for nonlinear models.
### Hill model and starting values
formHill <- as.formula(signal ~ c + (d - c) / (1 + (dose/e)^b ) )
startvalHillnls2 <- function(x, y, xm, ym, increase) # requires the definition of increase from min and max values
  # which is the first one
  # inputs
  # - x values of the dose
  # - y values the corresponding signal
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  # 
{
  maxi <- max(y, na.rm = TRUE)
  mini <- min(y, na.rm = TRUE)
  ampl <- maxi - mini
  # inflate maxi and mini so as all values are strictly inside the interval [mini; maxi]
  maxi <- maxi + 0.001 * ampl
  mini <- mini - 0.001 * ampl
  # initial value of c
  c <- ifelse(increase, maxi, mini) 
  # initial value of d
  d <-ifelse(increase, mini, maxi) 
  # initial value of e and b from regression
  yreg <- log((d - c) / (y[x!=0] - c) - 1)
  xreg <- log(x[x!=0])
  reg <- lm(yreg ~ xreg)
  b <- reg$coefficients[2]
  e <- reg$coefficients[1] / (-b)
  startval <- list(b = b, c = c, d = d, e = e)
}


### Exp2 model
# define the model formula
formExp2 <- as.formula(signal ~ e*exp(b*dose))
# get starting values
startvalExp2 <- function(xm, ym)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of a
  e <- ym[1]
  
  # transform y for regression
  yreg <- log(ym[xm != 0])
  reg <- lm(yreg ~ xm[xm != 0])
  
  # estimate slope from regression
  b <- coef(reg)[2]
  
  startval <- list(e = e, b = b)
}


### Exp3 model
# define the model formula
formExp3 <- as.formula(signal ~ e*(exp(sign(b)*(abs(b)*dose)^d)))

# get starting values
startvalExp3 <- function(xm, ym)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # transform y for regression
  yreg <- log(ym[xm != 0])
  reg <- lm(yreg ~ xm[xm != 0])
  
  # estimate b and d from regression
  b <- coef(reg)[2]
  
  d <- (exp(coef(reg)[1]))/e
  
  startval <- list(e = e, b = b, d = d)
}


### Exp4 model
formExp4 <- as.formula(signal ~ e*(c - (c-1)*exp((-1)*b*dose)))

# get starting values
startvalExp4 <- function(xm, ym, ad.dir)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # initial value of c (since the asymptote is always to the right, we can calculate c based on a and the max/min val)
  if(ad.dir == TRUE){
    c <- max(ym)/e + 0.001*(max(ym)/e)
  }else{
    c <- min(ym)/e - 0.001*(min(ym)/e)
  }
  
  # initial value of b
  yreg <- log((ym - e*c)/(e-e*c))
  reg <- lm(yreg ~ xm)
  
  b <- abs(coef(reg)[2])
  
  startval <- list(e = e, b = b, c = c)
}


#### Exp5 ####
formExp5 <- as.formula(signal ~ e*(c - (c-1)*exp((-1)*(b*dose)^d)))

# get starting values
startvalExp5 <- function(xm, ym, ad.dir)
  # inputs
  # - xm unique values of the dose (sorted by dose)
  # - ym means of the signal at each value of xm (sorted by dose)
  
{
  # initial value of e
  e <- ym[1]
  
  # initial value of c
  if(ad.dir){
    c <- max(ym)/e + 0.001*(max(ym)/e)
  }else{
    c <- min(ym)/e - 0.001*(min(ym)/e)
  }
  
  # initial value of b and d
  yreg <- log((ym - e*c)/(e-e*c))
  reg <- lm(yreg ~ xm)
  
  b <- abs(coef(reg)[2])
  d <- exp(coef(reg)[1])
  
  startval <- list(e = e, b = b, c = c, d = d)
}


#### power ####
formPow <- as.formula(signal ~ e + b*(dose^c))

# get starting values
startvalPow <- function(xm, ym, ad.dir, dset){
  
  require(dplyr)
  
  if(ad.dir){
    
    e <- min(ym) - 0.001*min(ym)
    
    yreg <- log(ym[xm!=0] - e)[-1]
    xreg <- log(xm[xm!=0])[-1]
    
    reg <- lm(yreg ~ xreg)
    
    c <- max(c(coef(reg)[2],1))
    b <- exp(coef(reg)[1])
    
  } else {
    
    e <- max(ym) + 0.001*max(ym)
    
    yreg <- log((ym[xm!=0] - e)*(-1))
    xreg <- log(xm[xm!=0])
    
    reg <- lm(yreg ~ xreg)
    
    c <- max(c(coef(reg)[2],1))
    b <- exp(coef(reg)[1])*(-1)
  }
  
  start.1 <- list(e = e, b = b, c = c)
  
  Pow <- suppressWarnings(try(nls(formPow, start = start.1, data = dset,
                                  lower = c(1, -Inf, 0.999), control = nls.control(maxiter = 500),
                                  upper = c(Inf, Inf, 18), algorithm = "port",
                                  weights = 1/(signal^2)), silent = TRUE))
  
  if (!inherits(Pow, "try-error")){
    startval <- coef(Pow) %>% as.list()
  } else {
    startval <- list(e = e, b = b, c = c)
  }
  
}

#### function to get bmd results
bmdres <- function(fit){
  
  bmd.ci <- suppressMessages(confint(fit, "bmd"))
  bmd.mean <- coef(fit)[1] # get bmd estimate from fit
  c(bmd.mean, bmd.ci)
  
}

#### function for bootstrapping the median
fun.boot <- function(x, i) {
  median(x[i])
}

###########################################
# Functions for gene set overrrepresentation analysis
###########################################
##This is the central function for argument checking
paraCheck <- function(name, para) {
  if(name=="normCellHTSobject") {
    if(!is(para,"cellHTS"))
      stop("The argument 'cellHTSobject/normCellHTSobject' should be a cellHTS object")
    if(!state(para)["configured"]) 
      stop("The cellHTS object should be configured to perform the statistical tests")
    if(!state(para)["normalized"]) 
      warning("Your cellHTS object has not been normalized, this could impact the results of these tests",immediate.=TRUE)
    if(state(para)["scored"]) 
      stop("This cellHTS object has been scored; the statistical analysis should be performed on the normalized signal intensities",immediate.=TRUE)
    if(!state(para)["annotated"]) 
      stop("This cellHTS object has not been annotated",immediate.=TRUE)
  }
  if(name=="scoreSign") {
    if(!is.character(para) || length(para)!=1 || !(para %in% c("+","-")))
      stop("'scoreSign' should be either '+' or '-'!\n")
  }
  if(name=="scoreMethod") {
    if(!is.character(para) || length(para)!=1 || !(para %in% c("none","zscore","NPI")))
      stop("'scoreMethod' should be either 'none', 'zscore' or 'NPI'!\n")
  }
  if(name=="summarizeMethod") {
    if(!is.character(para) || length(para)!=1 || !(para %in% c("min","mean","median","max","rms","closestToZero","FurthestFromZero")))
      stop("'summarizeMethod' should be either 'min', 'mean', 'median', 'max', 'rms', 'closestToZero' or 'FurthestFromZero'!\n")
  }
  if(name=="annotationColumn") {
    if(!is.character(para) || length(para)!=1 )
      stop("'annotationColumn' should be a character value!\n")
  }
  if(name=="cutoffHitsEnrichment") {
    if(length(para) != 1 || (!is.integer(para) && !is.numeric(para))) 
      stop("'cutoffHitsEnrichment' should be a single integer! \n ")
  }
  if(name=="nwStatsControls") {
    if(!(is.character(para) && length(para)==1)) 
      stop("'controls/nwStatsControls' should be a character value!\n ")
  }
  if(name=="nwStatsAlternative") {
    if(!(is.character(para) && length(para)==1 && (para %in% c("two.sided","less","greater")) )) 
      stop("'alternative/nwStatsAlternative' should be one in 'two.sided','less' and 'greater'!\n ")
  }
  if(name=="nwStatsTests") {
    if(!is.character(para) || length(para)==0 || !(para %in% c("T-test","MannWhitney","RankProduct")))
      stop("'tests/nwStatsTests' should be one or more in 'T-test', 'MannWhitney' and 'RankProduct'!\n ")
  }
  if(name=="nwAnalysisOrder") {
    if(!(is.numeric(para) || is.integer(para)) || length(para)!=1 || para<0)
      stop("'order/nwAnalysisOrder' should be a positive numeric/integer value!\n")
  }
  if(name=="nwStatsColumns") {
    if(!is.character(para) || length(para)==0) 
      stop("'nwStatsColumns' should be a character vector with length > 0!\n ")
  }
  if(name=="gscs") {
    if(!is.list(para))
      stop("'listOfGeneSetCollections' must be a list of gene set collections!\n")
    if(is.null(names(para)))
      stop("'listOfGeneSetCollections' must be a list of named gene set collections!\n")
    if(!all(unlist(lapply(para,is.list))))
      stop("Each gene set collection in 'listOfGeneSetCollections' must be a list of gene sets!\n")
    if(any(unlist(lapply(para,length))==0))
      stop("Empty gene set collection(s) in 'listOfGeneSetCollections'!\n")
  }
  if(name=="gsc") {
    if(!is.list(para))
      stop("A gene set collection must be a list of gene sets!\n")
    if(length(para)==0)
      stop("No gene set found in input gene set collection!\n")
  }
  if(name=="gs") {
    if(!is.character(para) || length(para)==0 || any(is.na(para)) || any(para==""))
      stop("'geneSet/GeneSet' should be a character vector with length > 0, without NA or empty names!\n")
  }
  if(name=="gs.single") {
    if(!is.character(para) || length(para)!=1 || is.na(para) || para=="")
      stop("'geneSet/GeneSet' should be single character!\n")		
  }
  if(name=="gscs.names") {
    if(!is.character(para) || length(para)==0) 
      stop("'gscs' should be a character! \n")
  }
  if(name=="gsc.name") {
    if(!is.character(para) || length(para)!=1) 
      stop("'gsc' should be a single character! \n")
  }
  if(name=="keggGSCs") {
    if(!is.character(para) || length(para)==0)
      stop("'keggGSCs' should be a character!\n")
  }
  if(name=="goGSCs") {
    if(!is.character(para) || length(para)==0)
      stop("'goGSCs' should be a character!\n")
  }
  if(name=="genelist") {
    if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
      stop("'geneList' should be a named numeric or integer vector with length > 0!\n")
    #if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)=="")) 
  }
  if(name=="universe") {
    if(!is.character(para) || any(is.na(para)) || any(para=="")) 
      stop("'universe' should be a character vector without any NA or empty values!\n")
  }
  if(name=="genelist.general") {
    if(is.matrix(para)) {
      if(is.null(rownames(para)) || any(is.na(rownames(para))) || any(rownames(para)=="")) 
        stop("geneList should be a matrix with rownames!\n")		
    } else {
      if(!(is.numeric(para) || is.integer(para)) || length(para)==0)
        stop("'geneList' should be a numeric or integer vector with length > 0!\n")
      if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)=="")) 
        stop("'geneList' should be a named numeric or integer vector!\n")
    }
  }
  if(name=="hits") {
    if(!is.character(para) || length(para)==0)
      stop("'hits' should be a character vector with length > 0!\n")
  }
  if(name=="gsca.para") {
    if(missing(para))
      stop("'para' should be provided as a list!\n")
    ##default parameters
    para.default<-list(pValueCutoff = 0.05,pAdjustMethod = "BH", nPermutations = 1000, minGeneSetSize = 15,exponent = 1)
    ##check if input parameters are supported
    if(length(setdiff(names(para),names(para.default)))>0) 
      stop("Some parameters in 'para' are not supported. Check the right format of para!\n")
    ##fill out default parameters for non-specified ones
    para.unspecified<-setdiff(names(para.default),names(para))
    if(length(para.unspecified)>0)
      for(i in 1:length(para.unspecified)) {
        para[[para.unspecified[i]]]<-para.default[[para.unspecified[i]]]
      }
    ##check data type in para
    if(!(is.integer(para$pValueCutoff) || is.numeric(para$pValueCutoff)) || length(para$pValueCutoff)!=1 || para$pValueCutoff>1)
      stop("'pValueCutoff' should be an integer or numeric value <=1!\n")
    if(!is.character(para$pAdjustMethod) || length(para$pAdjustMethod)!=1 || 
       !(para$pAdjustMethod %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
      stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
    if(!(is.integer(para$nPermutations) || is.numeric(para$nPermutations)) || length(para$nPermutations)!=1 || para$nPermutations<1)
      stop("'nPermutations' should be an integer >=1 !\n'")
    if(!(is.integer(para$minGeneSetSize) || is.numeric(para$minGeneSetSize)) || length(para$minGeneSetSize)!=1 || para$minGeneSetSize<1)
      stop("'minGeneSetSize' should be an integer >=1 !\n'")
    if(!(is.integer(para$exponent) || is.numeric(para$exponent)) || length(para$pValueCutoff)!=1 || para$exponent<1)
      stop("'exponent' should be an integer or numeric value >=1 !\n")
    return(para)
  }
  if(name=="pAdjustMethod") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none")))
      stop("'pAdjustMethod' should be any one of 'holm', 'hochberg', 'hommel', 'bonferroni', 'BH', 'BY', 'fdr' and 'none'!\n")
  }
  if(name=="pValueCutoff") {
    if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para>1 || para<0)
      stop("'pValueCutoff' should be an integer or numeric value <=1 and >=0!\n")
  }
  if(name=="nPermutations") {
    if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
      stop("'nPermutations' should be an integer >=1 !\n'")
  }
  if(name=="minGeneSetSize") {
    if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
      stop("'minGeneSetSize' should be an integer >=1 !\n'")
    
  }
  if(name=="exponent") {
    if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<1)
      stop("'exponent' should be an integer or numeric value >=1 !\n")
  }
  if(name=="species") {
    if(!is.character(para) || length(para) != 1) 
      stop("'species' should be a character!\n")
    if(!(para %in% c("Dm","Hs","Rn","Mm","Ce"))) {
      stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Dm' ('Drosophila_melanogaster'), 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus'), 'Ce' ('Caenorhabditis_elegans')")
    }	
  }
  if(name=="mam.species") {
    if(!is.character(para) || length(para) != 1) 
      stop("'species' should be a character!\n")
    if(!(para %in% c("Hs","Rn","Mm"))) {
      stop("'species' does not match any of the names recognized by this function, please provide one of the following character strings: 'Hs' ('Homo_sapiens'), 'Rn' ('Rattus_norvegicus'), 'Mm' ('Mus_musculus')")
    }	
  }
  if(name=="initialIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
      stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'! \n")
  }
  if(name=="finalIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt","wormbase")))
      stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt','wormbase'!\n")
  }	
  if(name=="dro.initialIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
      stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'! \n")
  }
  if(name=="dro.finalIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","Flybase","FlybaseCG","FlybaseProt")))
      stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','Flybase','FlybaseCG','FlybaseProt'!\n")
  }
  if(name=="cel.initialIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
      stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'! \n")
  }
  if(name=="cel.finalIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank","wormbase")))
      stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank','wormbase'!\n")
  }	
  if(name=="mam.initialIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
      stop("initialIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
  }
  if(name=="mam.finalIDs") {
    if(!is.character(para) || length(para)!=1 || 
       !(para %in% c("Ensembl.transcript","Ensembl.prot","Ensembl.gene","Entrez.gene","RefSeq","Symbol","GenBank")))
      stop("finalIDs should be one of 'Ensembl.transcript','Ensembl.prot','Ensembl.gene','Entrez.gene','RefSeq','Symbol','GenBank'! \n")
    
  }
  if(name=="keepMultipleMappings") {
    if(!is.logical(para) || length(para)!=1)
      stop("keepMultipleMappings should be a logical value!\n")
  }
  if(name=="duplicateRemoverMethod") {
    if(!is.character(para) || length(para) != 1 || !(para %in% c("max","min","average","fold.change.average")))
      stop("'duplicateRemoverMethod' should be only one of the following character strings: 'max', 'min', 'average', 'fc.avg(fold change average)'")
  }
  if(name=="orderAbsValue") {
    if(!is.logical(para) || length(para)!=1) 
      stop("'orderAbsValue' should be a logical value!\n")
  }
  if(name=="verbose") {
    if(!is.logical(para) || length(para)!=1)
      stop("'verbose' should be a logical value!\n")
  }
  if(name=="filepath") {
    if(!is.character(para) || length(para)!=1)
      stop("'filepath' should be a character!\n")
  }
  if(name=="dataDirectory") {
    if(!is.character(para) || length(para)!=1)
      stop("'dataDirectory' should be a character!\n")
  }
  if(name=="filename") {
    if(!is.character(para) || length(para)!=1)
      stop("'filename' should be a character!\n")
  }
  if(name=="ntop") {
    if(!(is.integer(para) || is.numeric(para)) || length(para)!=1 || para<=0)
      stop("'ntop' should be a integer or numeric value >0 ! \n")
  }
  if(name=="allSig") {
    if(!is.logical(para) || length(para)!=1)
      stop("'allSig' should be a logical value!\n")
  }
  if(name=="gsNameType") {
    if(!is.character(para) || length(para)!=1 || !(para%in% c("id","term","none")))
      stop("'gsNameType' should be a single character value: 'id', 'term' or 'none'!\n")		
  }
  if(name=="displayEdgeLabel") {
    if(!is.logical(para) || length(para)!=1)
      stop("'displayEdgeLabel' should be a logical value!\n")		
  }
  if(name=="layout") {
    if(!is.character(para) || length(para)!=1 || !(para%in%c("layout.fruchterman.reingold", "layout.spring", "layout.circle", "layout.kamada.kawai")))
      stop("'layout' must be one of 'layout.fruchterman.reingold', 'layout.spring', 'layout.circle' and 'layout.kamada.kawai'!\n")
  }
  if(name=="resultName") {
    if(!is.character(para) || length(para)!=1)
      stop("'resultName' should be a character!\n")
  }
  if(name=="pvalues") {
    if(!is.numeric(para) || length(para)==0 || is.null(names(para)))
      stop("'pvalues' should be a named numeric vector with length > 0!\n")
  }
  if(name=="phenotypes") {
    if(!(is.numeric(para) || is.integer(para)) || length(para)==0 || is.null(names(para)))
      stop("'phenotypes/phenotypeVector' should be a named numeric vector with length > 0!\n")
    #if(is.null(names(para)) || any(is.na(names(para))) || any(names(para)==""))
  }
  if(name=="interactome") {
    if(!is(para,"graphNEL") && (numNodes(para)==0 || numEdges(para)==0))
      stop("Input 'interactome/graph' should be a graphNEL object with node and edge No > 0!\n")
  }
  if(name=="fdr") {
    if(!is.numeric(para) || para>1) 
      stop("'fdr' should be <=1 ! \n")
  }
  if(name=="interactionMatrix") {
    #If a data matrix is specified, check that it contains the right columns
    if(!is.matrix(para)) 
      stop("'interactionMatrix' should be a matrix")
    if(!all(c("InteractionType","InteractorA","InteractorB") %in% colnames(para))) 
      stop("'interactionMatrix' should contain the following named columns: 'InteractionType','InteractorA','InteractorB'")
  }
  if(name=="link") {
    if(!is.character(para) || length(para)!=1)
      stop("'link' should be a character!\n")
  }
  if(name=="reportDir") {
    if(!is.character(para) || length(para)!=1)
      stop("'reportDir' should be a character!\n")
  }
  if(name=="genetic") {
    if(!is.logical(para) || length(para)!=1)
      stop("'genetic' should be a logical value!\n")
  }
  if(name=="what.nwa") {
    if(!any(para %in% c("ALL","Pval","Phenotype","Interactome","Para","Result")) || !is.character(para))
      stop("Wrong what input! Please input \"ALL\"(all summary information), \"Pval\"(p-values), \"Phenotype\", \"Interactome\", \"Para\"(parameters for analysis) and \"Result\"\n")
    
  }
  if(name=="what.gsca") {
    if(!is.character(para) || !any(para %in% c("ALL","GSC","GeneList","Hits","Para","Result")))
      stop("Wrong what input! Please input \"ALL\"(all summary information), \"GSC\"(gene set collection), \"GeneList\", \"Hits\", \"Para\"(parameters for analysis) and \"Result\"\n")
    
  }
  if(name=="experimentName") {
    if(!is.character(para) || length(para)!=1)
      stop("'experimentName' should be a character!\n ")
  }
  if(name=="ontologies") {
    if(!is.character(para) || length(para)==0 || !all(para %in% c("BP","MF","CC"))) 
      stop("'ontologies' should be a character vector containing any non redundant combination of 'BP','MF','CC'!\n")
  }
  if(name=="output") {
    if(!is.character(para) || length(para)!=1 || !all(para %in% c("png","pdf")))
      stop("'output' should be 'png' or 'pdf'!\n")
  }
  if(name=="gseaScore.mode") {
    if(!is.character(para) || length(para)!=1 || !(para %in% c("graph", "score"))) {
      stop("'mode' should be 'graph' or 'score'!\n")
    }
  }
  if(name=="cutoffHitsEnrichment") {
    if(length(para) != 1 || (!is.integer(para) && !is.numeric(para))) 
      stop("'cutoffHitsEnrichment' should be a single integer!\n ")
  }
  if(name=="doGSOA" || name=="doGSEA") {
    if(length(para) != 1 || !is.logical(para))
      stop("'doGSOA' and 'doGSEA' should be a single logical value!\n ")
  }
}

##This function takes in a single gene set (GeneSet), a vector 
##(GeneList) of gene symbols for all tested genes, a vector of "hits" 
##(hits), and a p-value adjustment method. It outputs a vector 
##containing the size of the gene universe, the size of the gene set 
##within this universe (i.e. how many genes from the universe map to 
##this gene set), the total number of hits, the number of hits expected 
##to occur in the gene set, the actual hits observed in the gene set, 
##and the pvalue from a hypergeometric test.
hyperGeoTest <- function(geneSet, universe, hits) {
  ##number of genes in universe
  N <- length(universe)
  ##remove genes from gene set that are not in universe			
  geneSet <- intersect(geneSet[[1]], universe) 
  ##size of gene set	
  m <- length(geneSet) 							
  Nm <- N-m
  ##hits in gene set
  overlap <- intersect(geneSet, hits) 	
  ##number of hits in gene set		
  k <- length(overlap) 							
  n <- length(hits)	
  HGTresults <- phyper(k-1, m, Nm, n, lower.tail = F)
  ex <- (n/N)*m
  if(m == 0) HGTresults <- NA
  hyp.vec <- c(N, m, n, ex, k, HGTresults)
  names(hyp.vec) <- c("Universe Size", "Gene Set Size", "Total Hits", 
                      "Expected Hits", "Observed Hits", "Pvalue")
  return(hyp.vec)
}

##This function performs hypergeometric tests for over-representation 
##of hits, on a list of gene sets. This function applies the 
##hyperGeoTest function to an entire list of gene sets and returns a 
##data frame.
multiHyperGeoTest <- function(collectionOfGeneSets, universe, hits, 
                              minGeneSetSize = 15, pAdjustMethod = "BH", verbose = FALSE) {
  ##check arguments
  paraCheck("gsc", collectionOfGeneSets)
  paraCheck("universe", universe)
  paraCheck("hits", hits)
  paraCheck("minGeneSetSize", minGeneSetSize)
  paraCheck("pAdjustMethod", pAdjustMethod)
  paraCheck("verbose", verbose)
  l.GeneSet <- length(collectionOfGeneSets)
  geneset.size <- unlist(
    lapply(
      lapply(collectionOfGeneSets, intersect, y = universe), length
    )
  )
  if(all(geneset.size < minGeneSetSize))
    stop(paste("The largest number of overlapped genes of gene ",
               "sets with universe is: ", max(geneset.size), ", which is < ", 
               minGeneSetSize, "!\n", sep = ""))
  geneset.filtered <- which(geneset.size >= minGeneSetSize)
  ##if verbose, create a progress bar to monitor computation progress
  if(verbose) 
    pb <- txtProgressBar(style=3)
  results <- t(
    sapply(geneset.filtered, 
           function(i) {
             if(verbose) 
               setTxtProgressBar(pb, i/l.GeneSet)		
             hyperGeoTest(collectionOfGeneSets[i], universe, hits)
           }
    )
  )

  results.genes <- sapply(geneset.filtered, 
    function(i) {
      idx <- match(unlist(collectionOfGeneSets[i]), hits);
      if(sum(is.na(idx)) < length(idx)){
        idx <- idx[!is.na(idx)]
        val <- hits[idx]
      }
    }
  )
  if(verbose) 
    close(pb)
  if(length(results) > 0) {
    ##Adjust pvalues
    adjPvals <- p.adjust(results[, "Pvalue"], method = pAdjustMethod)
    results <- cbind(results, adjPvals)
    colnames(results)[ncol(results)] <- "Adjusted.Pvalue"
    results <- results[order(results[, "Adjusted.Pvalue"]), , drop=FALSE]		
  } else {
    colnames(results) <- c("Universe Size", "Gene Set Size", 
                           "Total Hits", "Expected Hits", "Observed Hits", "Pvalue", 
                           "Adjusted.Pvalue")
  }
  
  results.test <- list()
  results.test[['results']] = results
  results.test[['genes']] = results.genes
  
  return(results.test)
}

#### function for running gene set analysis
gsoa.fun <- function(file, universe, hits, bmd.res, pval = 1.0, FDR = FALSE){
  
  # create gene set library object
  gs.rds <- readRDS(file);
  if(is.null(names(gs.rds))){ # some go lib does not give names
    names(gs.rds) <- c("link", "term", "sets");
  }

  gs.names <- data.frame(name = gs.rds$term, id = names(gs.rds$sets))
  gs.lib <- gs.rds[["sets"]]
  
  # hypergeometic test
  results.test <- multiHyperGeoTest(gs.lib, universe, hits)
  names(results.test$genes) <- as.vector(gs.names$name[match(names(results.test$genes), gs.names$id)])
  results <- as.data.frame(results.test$results)
  results$id <- rownames(results)
  rownames(results) <- NULL
  results <- as.data.table(merge(results, gs.names, by = "id"))
  
  if(nrow(results) > 50){
    results <- results[order(Pvalue)]
    results <- results[1:50]
  }


  # filter for significance
  if(FDR){
    results <- results[Adjusted.Pvalue < pval]
  } else {
    results <- results[Pvalue < pval]
  }
  
  # get BMDs from each enriched gene set
  gs.id <- as.list(results$id)
  bmds <- lapply(gs.id, function(x){bmd.res[bmd.res$id %in% gs.lib[[x]], 3]})
  
  # bootstrap median BMD for each gene set
  bmd.med <- lapply(bmds, function(x){boot(x, statistic = fun.boot, R = 10)$t0})
  results$bmd.med <- unlist(bmd.med)
  results.summary <- list()
  results.summary[['results']] <- results
  results.summary[['genematches']] <- results.test$genes
  return(results.summary) 
}


#######################################################
## set of functions to init/read details of ItemSelect 
##  object needed for DR curve fitting
#######################################################

####################ItemSelec object###################
GetItemSelectModels <- function(){
  # Checks
  # TODO: return error message to API response
  if(!exists("models")){
    print("Could not find models vector!");
    return(0);
  }
  
  return(models)
}

GetItemSelectData <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # double matrix
  return(itemselect$data);
}

GetItemSelectDataRowNms <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(rownames(itemselect$data));
}

GetItemSelectDataColNms <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(colnames(itemselect$data));
}

GetItemSelectDose <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # numeric vector
  return(itemselect$dose)
}

GetItemSelectItems <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # numeric vector
  return(itemselect$item)
}

GetItemSelectDataMean <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # double matrix
  return(itemselect$data.mean);
}

GetItemSelectDataMeanColNms <- function(){
  itemselect <- dataSet$itemselect;
  # Checks
  # TODO: return error message to API response
  if (!inherits(itemselect, "itemselect"))
    stop("Use only with 'itemselect' objects, created with the function itemselect")
  
  # character vector
  return(colnames(itemselect$data.mean));
}

InitItemSelect <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  if(!exists("data.select") || !exists("dose") || !exists("item") || !exists("data.mean") || !exists("data.mean.colnames")){
    print("Could not find data vector!");
    return(0);
  }

  rownames(data.select) <- item
  colnames(data.mean) <- data.mean.colnames
  reslist <- list(data = data.select, dose = dose,
                  item = item, data.mean = data.mean)  
  dataSet$itemselect <- structure(reslist, class = "itemselect")
  RegisterData(dataSet);
  return(1);
}

####################DrcFit object###################
GetNumberDoses <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  print(dataSet$itemselect$dose);
    return(length(unique(dataSet$itemselect$dose))-1) # number of doses minus 0
}

GetDrcFitAllRes <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  res <- res[,-c(1,2,ncol(res))]
  res <- as.matrix(res)
  
  return(res)
}

GetDrcFitAllResColNms <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all

  return(colnames(res))
}

GetDrcFitAllGeneIds <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,1])
}

GetDrcFitAllModNms <- function(){
  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,2])
}

GetDrcFitAllInvStatus <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  f.drc <- dataSet$drcfit.obj;
  
  # Checks
  if (!inherits(f.drc, "drcfit"))
    stop("Use only with 'drcfit' objects, created with the function drcfit")
  
  res <- f.drc$fitres.all
  return(res[,ncol(res)])
}

InitDrcFitObj <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  if(!exists("drc.res.modelnms") || !exists("drc.res.gene.ids") || !exists("drc.res") || !exists("drc.res.colnms") || !exists("drc.res.invstatus")){
    print("Could not find drc meta objects!");
    return(0);
  }
  drc.res.invstatus <- as.logical(drc.res.invstatus)
  dres <- data.frame(drc.res.gene.ids, drc.res.modelnms, drc.res, drc.res.invstatus)
  colnames(dres) <- drc.res.colnms
  
  #TODO: check if some needed objects are missing
  reslist <- list(fitres.all = dres, fitres.filt = data.frame(), data = dataSet$itemselect$data,
                  dose = dataSet$itemselect$dose, data.mean = dataSet$itemselect$data.mean, 
                  item = dataSet$itemselect$item) 
  
  dataSet$drcfit.obj <- structure(reslist, class = "drcfit")
  
  RegisterData(dataSet);
  return(1);
}


FilterBMDResults <- function(dataSet){
  obj.bmd <- dataSet$bmdcalc.obj
  if (!inherits(obj.bmd, "bmdcalc"))
    stop("Use only with 'bmdcalc' objects, created with the function bmdcalc")

  bmdcalc.obj <- obj.bmd
  if(bmd.pass.option == "conv"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$conv.pass, ]
  }else if(bmd.pass.option == "hd"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$hd.pass, ]
  }else if(bmd.pass.option == "ci"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$CI.pass, ]
  }else if(bmd.pass.option == "ld"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$ld.pass, ]
  }else{
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$all.pass, ]
  }
  return(bmdcalc.res)
}

PerformDREnrichment<-function(file.nm, fun.type, xMin, xMax){
    paramSet <- readSet(paramSet, "paramSet");
    dataSet <- readDataset(paramSet$dataName);
    bmdcalc.res <- FilterBMDResults(dataSet);
    gene.vec <- as.matrix(bmdcalc.res[which(bmdcalc.res$bmd > xMin & bmdcalc.res$bmd < xMax),1])
    sym.vec <- doEntrez2SymbolMapping(gene.vec);
    names(gene.vec) <- sym.vec;
    res <- PerformEnrichAnalysis(file.nm, fun.type, gene.vec);
    enr1.mat <<- enr.mat
    return(res);
}
GetDRRes <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);
  obj.bmd <- dataSet$bmdcalc.obj
  
  bmdcalc.obj <- obj.bmd
  if(bmd.pass.option == "conv"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$conv.pass, ]
  }else if(bmd.pass.option == "hd"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$hd.pass, ]
  }else if(bmd.pass.option == "ci"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$CI.pass, ]
  }else if(bmd.pass.option == "ld"){
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$ld.pass, ]
  }else{
    bmdcalc.res <- bmdcalc.obj$bmdcalc.res[bmdcalc.obj$bmdcalc.res$all.pass, ]
  }
  
  curv.num <- dim(obj.bmd$bmdcalc.res)[1]
  bmd.num <- dim(bmdcalc.res)[1]
  
  res <- c(curv.num, bmd.num)
  
  #update sig genes
  dataSet$sig.mat <- dataSet$comp.res[which(rownames(dataSet$comp.res) %in% bmdcalc.res$id),];
  RegisterData(dataSet);
  return(res)  
  
}

interpFits <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

    fitres.filt <- dataSet$drcfit.obj$fitres.filt
    resp.list <- vector(mode = "list", length = nrow(fitres.filt))
    max.dose <- max(dataSet$bmdcalc.obj$dose)
    dose = seq(from = 0, to = max.dose, length.out = 100)

    for(i in c(1:nrow(fitres.filt))){
      params <- fitres.filt[i,]
      model.nm <- as.vector(params$mod.name)
      b <- as.numeric(as.vector(params$b))
      c <- as.numeric(as.vector(params$c))
      d <- as.numeric(as.vector(params$d))
      e <- as.numeric(as.vector(params$e))
      f <- as.numeric(as.vector(params$f))

      if(is.nan(d)){
        d <- 1
      }
      e <- as.numeric(as.vector(params$e))
      if(is.nan(e)){
        e <- 1
      }


      if(model.nm == "Poly2"){
        response <- b + c*dose + d*I(dose^2)
      }else if(model.nm == "Poly3"){
        response <- b + c*dose + d*I(dose^2) + e*I(dose^3)
      }else if(model.nm == "Poly4"){
        response <- b + c*dose + d*I(dose^2) + e*I(dose^3) + f*I(dose^4)
      }else if(model.nm == "Exp2"){
        response <- e * exp(b * dose)
      }else if(model.nm == "Exp3"){
        response <- e * (exp(sign(b) * (abs(b) * dose)^d))
      }else if(model.nm == "Exp4"){
        response <- e * (c - (c - 1) * exp((-1) * b * dose))
      }else if(model.nm == "Exp5"){
        response <- e * (c - (c - 1) * exp((-1) * (b * dose)^d))
      }else if(model.nm == "Lin"){
        response <- b * dose + d
      }else if(model.nm == "Hill"){
        response <- c + (d - c)/(1 + (dose/e)^b)
      }else if(model.nm == "Power"){
        response <- e + b * (dose^c)
      }

      resp.min <- min(response)
      response <- response - resp.min
      resp.max <- max(response)
      response <- response/resp.max

      resp.list[[i]] <- response
    }

    df <- as.data.frame(do.call(rbind, resp.list))
    colnames(df) <- as.character(dose)
    rownames(df) <- fitres.filt$gene.id

    return(df);

}

# create request for dose response API

PerformAPIDRFit <- function(){
  paramSet <- readSet(paramSet, "paramSet");
  dataSet <- readDataset(paramSet$dataName);

  dataSetWeb <- dataSet
  models <- models
  cpus <- cpus
  
  toSend <- list(dataSet = dataSetWeb,
                 models = models,
                 cpus = cpus)
  
  # rds file to be sent to server
  library(httr)
  base <- paramSet$api.base
  endpoint <- "/fitcurves"
  call <- paste(base, endpoint, sep="")
  print(call)
  
  saveRDS(toSend, "tosend.rds")
  request <- suppressWarnings(try(httr::POST(url = call, 
                        body = list(rds = upload_file("tosend.rds", "application/octet-stream"))), silent = TRUE))
  
  # check if successful
  if(inherits(request, "try-error")){
    PerformDRFit()
    current.msg <<- c("Failed to connect to Xia Lab API Server!")
    return(0)
  }
  
  # now process return
  request <- httr::content(request, "raw")
  request <- base::unserialize(request)
  if(!is.null(request$drcfit.obj)){
    dataSet <<- request
    RegisterData(dataSet);
  }

  return(1)
}

#### use the pureErrorAnova function from alr3. alr3 is now deprecated, so extracted these 
# lines from the alr3 source code in the CRAN archive
pureErrorAnova <- function(mod){UseMethod("pureErrorAnova")}
pureErrorAnova.lm <- function(mod) {
  if (is.null(mod$model)) mod <- update(mod, model=TRUE)
  p <- dim(mod$model)[2] -1
  mod$model$Lack.of.Fit <-
    factor(randomLinComb(model.matrix(mod), 101319853))
  aov1 <- anova(mod)
  #set.seed(save.seed) # restore random number seed
  if (length(levels(mod$model$Lack.of.Fit)) == length(mod$model$Lack.of.Fit))
    aov1 else {
      aov2 <- anova(lm(mod$model[ , 1]~mod$model$Lack.of.Fit, weights=weights(mod)))
      rrow <- dim(aov1)[1]
      aov2[1, 1] <- aov1[rrow, 1]-aov2[2, 1]
      aov2[1, 2] <- aov1[rrow, 2]-aov2[2, 2]
      aov2[1, 3] <- aov2[1, 2]/aov2[1, 1]
      aov1[1:(rrow-1), 4] <- aov1[1:(rrow-1), 3]/aov2[2, 3]
      aov2[1, 4] <- aov2[1, 3]/aov2[2, 3]
      row.names(aov2) <- c(" Lack of fit", " Pure Error")
      aov <- rbind(aov1, aov2)
      aov[ , 5] <- pf(aov[ , 4], aov[ , 1], aov2[2, 1], lower.tail=FALSE)
      aov
    }}


randomLinComb <- function(X, seed=NULL) {UseMethod("randomLinComb")}

randomLinComb.default <- function(X, seed=NULL) {
  if(!is.null(seed)) set.seed(seed)
  std <- function(x){
    s <- sd(x)
    if( s > 0) (x-mean(x))/s else x}
  as.vector(apply(X, 2, std)%*% as.vector(2*rnorm(dim(X)[2])-1) )
}

randomLinComb.lm <- function(X, ...) {
  randomLinComb(model.matrix(X), ...)}

randomLinComb.lm <- function(X, seed=NULL) {
  if(is.null(X$model)) X <- update(X, model=TRUE)
  randomLinComb(X$model[ , -1], seed=seed)}





