### Generic DataClass Defination----------
# 1. This xcmsSet is defined as refereced to XCMS for CAMERA annotation but not completely same
##' @references Smith, C.A., Want, E.J., O'Maille, G., Abagyan,R., Siuzdak, G. (2006). 
##' "XCMS: Processing mass spectrometry data for metabolite profiling using nonlinear peak alignment, 
##' matching and identification." Analytical Chemistry, 78, 779-787.
setClass("xcmsSet",
         representation = representation(peaks = "matrix",
                                         groups = "matrix",
                                         groupidx = "list",
                                         filled="numeric",
                                         phenoData = "data.frame",
                                         rt = "list",
                                         filepaths = "character",
                                         profinfo = "list",
                                         dataCorrection="numeric",
                                         polarity = "character",
                                         progressInfo = "list",
                                         progressCallback="function",
                                         mslevel = "numeric",
                                         scanrange = "numeric"),
         prototype = prototype(peaks = matrix(nrow = 0, ncol = 0),
                               groups = matrix(nrow = 0, ncol = 0),
                               groupidx = list(),
                               filled = integer(0),
                               phenoData = data.frame(),
                               rt = list(),
                               filepaths = character(0),
                               profinfo = vector("list"),
                               dataCorrection=integer(0),
                               polarity = character(0),
                               progressInfo = list(),
                               mslevel = numeric(0),
                               scanrange= numeric(0),
                               progressCallback = function(progress) NULL),
         validity = function(object) {
           msg <- character()
           ## Check if all slots are present.
           slNames <- slotNames(object)
           missingSlots <- character()
           for (i in 1:length(slNames)) {
             if (!.hasSlot(object, slNames[i]))
               missingSlots <- c(missingSlots, slNames[i])
           }
           if (length(missingSlots) > 0)
             msg <- c(msg, paste0("This xcmsSet lacks slot(s): ",
                                  paste(missingSlots, collapse = ","),
                                  ". Please update the object using",
                                  " the 'updateObject' method."))
           ## Check the .processHistory slot.
           
           if (length(msg))
             return(msg)
           return(TRUE)
         })

# 2. ruleSet were set for CAMERA processing.
##' @references Kuhl C, Tautenhahn R, Boettcher C, Larson TR, Neumann S (2012). 
##' "CAMERA: an integrated strategy for compound spectra extraction and annotation of 
##' liquid chromatography/mass spectrometry data sets." Analytical Chemistry, 84, 283-289. 
##' http://pubs.acs.org/doi/abs/10.1021/ac202450g.

setClass("ruleSet",
         representation(ionlistfile="character",
                        ionlist="data.frame", 
                        neutrallossfile="character",
                        neutralloss="data.frame", 
                        neutraladditionfile="character",
                        neutraladdition="data.frame",
                        maxcharge="numeric",
                        mol="numeric",
                        nion="numeric",
                        nnloss="numeric",
                        nnadd="numeric",
                        nh="numeric",
                        polarity="character",
                        rules="data.frame",
                        lib.loc="character"),        
         contains=c("Versioned"),
         prototype=prototype(
           ionlistfile="",
           ionlist=data.frame(),
           neutrallossfile="",
           neutralloss=data.frame(),           
           neutraladditionfile="",
           neutraladdition=data.frame(),
           maxcharge=numeric(),
           mol=numeric(),
           nion=numeric(),
           nnloss=numeric(),
           nnadd=numeric(),
           nh=numeric(),
           polarity=NULL,
           rules=data.frame(),
           lib.loc=NULL,
           new("Versioned", versions=c(ruleSet="0.1.1"))),
         validity=function(object) {
           TRUE
         })

