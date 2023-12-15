
##################################################
## R script for mGWAS-Explorer
## Description: GO/Pathway ORA
## Author: Yannan, yannan.fan@mail.mcgill.ca
###################################################

# need to obtain the full path to convert (from imagemagik) for cropping images
GetBashFullPath<-function(){
    path <- system("which bash", intern=TRUE);
    if((length(path) == 0) && (typeof(path) == "character")){
        print("Could not find bash in the PATH!");
        return("NA");
    }
    return(path);
}

cleanMem <- function(n=10) { for (i in 1:n) gc() }

# new range [a, b]
rescale2NewRange <- function(qvec, a, b){
    q.min <- min(qvec);
    q.max <- max(qvec);
    if(length(qvec) < 50){
        a <- a*2;
    }
    if(q.max == q.min){
        new.vec <- rep(1.5, length(qvec));
    }else{
        coef.a <- (b-a)/(q.max-q.min);
        const.b <- b - coef.a*q.max;
        new.vec <- coef.a*qvec + const.b;
    }
    return(new.vec);
}

`%fin%` <- function(x, table) {
  fmatch(x, table, nomatch = 0L) > 0L
}

###########
# improved list of objects
.ls.objects <- function (pos = 1, pattern, order.by,
                        decreasing=FALSE, head=FALSE, n=5) {
    napply <- function(names, fn) sapply(names, function(x)
                                         fn(get(x, pos = pos)))
    names <- ls(pos = pos, pattern = pattern)
    obj.class <- napply(names, function(x) as.character(class(x))[1])
    obj.mode <- napply(names, mode)
    obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
    obj.prettysize <- napply(names, function(x) {
                           capture.output(format(utils::object.size(x), units = "auto")) })
    obj.size <- napply(names, object.size)
    obj.dim <- t(napply(names, function(x)
                        as.numeric(dim(x))[1:2]))
    vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
    obj.dim[vec, 1] <- napply(names, length)[vec]
    out <- data.frame(obj.type, obj.size, obj.prettysize, obj.dim)
    names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
    if (!missing(order.by))
        out <- out[order(out[[order.by]], decreasing=decreasing), ]
    if (head)
        out <- head(out, n)
    out
}

# shorthand
ShowMemoryUse <- function(..., n=40) {
    library(pryr);
    sink(); # make sure print to screen
    print(mem_used());
    print(sessionInfo());
    print(.ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n));
    print(warnings());
}

color_scale <- function(c1="grey", c2="red") {
  pal <- colorRampPalette(c(c1, c2))
  colors <- pal(100)
  return(colors)
}


ComputeColorGradient <- function(nd.vec, background="black", centered){
    require("RColorBrewer");

    minval = min(nd.vec, na.rm=TRUE);
    maxval = max(nd.vec, na.rm=TRUE);
    res = maxval-minval;

    if(res == 0){
        return(rep("#FF0000", length(nd.vec)));
    }
    color <- GetColorGradient(background, centered);
    breaks <- generate_breaks(nd.vec, length(color), center = centered);
    return(scale_vec_colours(nd.vec, col = color, breaks = breaks));
}

generate_breaks = function(x, n, center = F){
    if(center){
        m = max(abs(c(min(x, na.rm = T), max(x, na.rm = T))))
        res = seq(-m, m, length.out = n + 1)
    }
    else{
        res = seq(min(x, na.rm = T), max(x, na.rm = T), length.out = n + 1)
    }
    return(res)
}

scale_vec_colours = function(x, col = rainbow(10), breaks = NA){
    breaks <- sort(unique(breaks));
    return(col[as.numeric(cut(x, breaks = breaks, include.lowest = T))])
}

GetColorGradient <- function(background, center){
    if(background == "black"){
        if(center){
            return(c(colorRampPalette(c("#31A231", "#5BC85B", "#90EE90", "#C1FFC1"))(50), colorRampPalette(c("#FF9DA6", "#FF7783", "#E32636", "#BD0313"))(50)));
        }else{
            return(colorRampPalette(rev(heat.colors(9)))(100));
        }
    }else{ # white background
        if(center){
            return(c(colorRampPalette(c("#137B13", "#31A231", "#5BC85B", "#90EE90"))(50), colorRampPalette(c("#FF7783", "#E32636", "#BD0313", "#96000D"))(50)));
        }else{
           # return(colorRampPalette(c("grey", "orange", "red", "darkred"))(100));
           # return(colorRampPalette(c("#80d0f0", rainbow(8, start=0.8, end=1)))(100));
            return(colorRampPalette(hsv(h = seq(0.72, 1, 0.035), s = 0.72, v = 1))(100));
        }
    }
}

rescale <- function(x, from, to){
  (x - min(x)) / max(x - min(x)) * (to - from) + from
}

# parse two-column list as a string input from text area in web page
.parseListData <- function(my.input){
  if(grepl("\n", my.input[1])) {
    lines <- strsplit(my.input, "\r|\n|\r\n")[[1]];
  }else{
    lines <- my.input;
  }
  my.lists <- strsplit(lines, "\\s+");
  my.mat <- do.call(rbind, my.lists);
  if(dim(my.mat)[2] == 1){ # add *
    my.mat <- cbind(my.mat, rep("*", nrow(my.mat)));
  }else if(dim(my.mat)[2] > 2){
    my.mat <- my.mat[,1:2];
    current.msg <- "More than two columns found in the list. Only first two columns will be used.";
    print(currret.msg);
  }
  return(my.mat);
}

QueryMet2Dis <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  statement <- paste("SELECT * FROM ", table.nm, " 
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  mir.dic <- .query.sqlite(mir.db, statement);
  return(mir.dic);

}

Query.mGWASDB <- function(db.path, q.vec, table.nm, col.nm, biofluid="all", population="all", db.opt="kegg"){
  require('RSQLite');
   #db.path<<-db.path;
   #q.vec<<-q.vec;
   #table.nm<<-table.nm;
   #col.nm<<-col.nm;
   #biofluid<<-biofluid;
   #population<<-population;
   #db.opt <<- db.opt;
   #save.image("Query.mGWASDB.RData")

  db.path <- paste0(db.path, ".sqlite");
 
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(table.nm=="snp_annot"){
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN entrez
                     ON entrez.symbol=",table.nm,".symbol
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");

  }else if(table.nm %in% c("snp2met_csf","snp2met_saliva", "snp2met_urine","snp2met_blood", "snp2met_all","snp2met")){
    table1.nm <- "snp2met";
    col.nm <- "rsid"; # temporary for now
    table2.nm <- "snp";
    statement <- .get_statement(table1.nm, table2.nm, col.nm, query, biofluid, population);
  }else if(table.nm=="met2snp"){
    table1.nm <- "snp2met";
    table2.nm <- "metabolites";
    statement <- .get_statement(table1.nm, table2.nm, col.nm, query, biofluid, population);
  }else if(table.nm=="exposure"){
    # table1.nm <- "snp2met";
    # table2.nm <- "metabolites";
    # statement <- .get_statement(table1.nm, table2.nm, col.nm, query, biofluid, population);
    statement <- paste("SELECT * FROM snp2met LEFT JOIN snp
                    ON snp.snp_orig=snp2met.snp_orig
                     LEFT JOIN metabolites
                     ON metabolites.metabolite_id=snp2met.metabolite_id
                     LEFT JOIN snp_af
                     ON snp_af.rsid=snp.rsid AND snp_af.ea=snp2met.ea 
                     LEFT JOIN study
                     ON study.pmid=snp2met.pmid
                     WHERE metabolites.", col.nm," IN (", query, ")", sep="");
  }else if(table.nm=="gene2snp"){
    statement <- paste("SELECT * FROM snp LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                     WHERE gene.", col.nm," IN (", query, ")", sep="");
    
  }else if(table.nm=="snp2met_study"){
    table.nm <- "snp2met";
    if(query=="'Viñuela_medRxiv_2021_targeted'"){
      query <- "'Viñuela_medRxiv_2021'";
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                      AND (note = 'targeted')", sep="");
    } else if(query=="'Viñuela_medRxiv_2021_untargeted'"){
      query <- "'Viñuela_medRxiv_2021'";
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                      AND (note = 'untargeted')", sep="");
    }else{
      statement <- paste("SELECT *  FROM ", table.nm,
                         " LEFT JOIN metabolites
                       ON metabolites.metabolite_id=",table.nm,".metabolite_id
                     LEFT JOIN snp
                     ON snp.snp_orig=",table.nm,".snp_orig
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                       WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
    }
  }else if(table.nm %in% c("met2gene","gene2met")){
    if(db.opt=="kegg"){
      db.opt <- "KEGG Enzyme"
    }else if(db.opt=="tcdb"){
      db.opt <- "TCDB Transporter"
    }else if(db.opt=="recon3d"){
      db.opt <- "Recon3D"
    }
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN metabolites
                        ON metabolites.hmdb=",table.nm,".hmdb 
                        LEFT JOIN gene
                        ON gene.symbol=",table.nm,".symbol
                       WHERE ",table.nm,".", col.nm," IN (", query, ")
                       AND type = ('" ,db.opt,"')
                       ", sep="");
    
    
  }else if(table.nm=="met2dis"){
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN metabolites
                        ON metabolites.hmdb=",table.nm,".hmdb 
                       WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  }else{
    statement <- paste("SELECT * FROM ", table.nm, " LEFT JOIN entrez
                     ON entrez.symbol=",table.nm,".symbol
                     LEFT JOIN metabolites
                     ON metabolites.hmdb=",table.nm,".hmdb
                     WHERE ",table.nm,".", col.nm," IN (", query, ")", sep="");
  }

    mir.dic <- .query.sqlite(mir.db, statement);
  # remove duplicates
  if(table.nm=="snp2met" || table.nm=="snp2met_merge"|| table.nm=="met2snp" || table.nm=="exposure" ){
    dup.inx <- duplicated(mir.dic$snp2met_id);
  }else if(table.nm %in% c("met2gene","gene2met")){
    dup.inx <- duplicated(mir.dic$met2gene_id);
  }else if(table.nm=="met2dis"){
    dup.inx <- duplicated(mir.dic$met2dis_id);
  }else if(table.nm=="gene2snp"){
    # one snp is mapped to one gene
    dup.inx <- duplicated(mir.dic$rsid);
  }else{
    dup.inx <- duplicated(mir.dic$mgwas);
  }
  mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

Query.DisGeNETDB <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  #save.image("Query.DisGeNETDB.RData")

  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      #print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(anal.type=="dis2snp"){
    statement <- paste("SELECT * FROM ", table.nm,
                       " LEFT JOIN variantAttributes
                     ON variantAttributes.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=variantDiseaseNetwork.diseaseNID
                     LEFT JOIN variantGene
                     ON variantGene.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=variantGene.geneNID",
                       " WHERE ", col.nm," IN (", query, ")", sep="");

  }else{
    if(table.nm=="geneDiseaseNetwork"){
      statement <- paste("SELECT * FROM ", table.nm,
                         " LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=geneDiseaseNetwork.geneNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=geneDiseaseNetwork.diseaseNID
                     WHERE ", col.nm," IN (", query, ")", sep="");
    }else{
      statement <- paste("SELECT * FROM ", table.nm,
                         " LEFT JOIN variantAttributes
                     ON variantAttributes.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN diseaseAttributes
                     ON diseaseAttributes.diseaseNID=variantDiseaseNetwork.diseaseNID
                     LEFT JOIN variantGene
                     ON variantGene.variantNID=variantDiseaseNetwork.variantNID
                     LEFT JOIN geneAttributes
                     ON geneAttributes.geneNID=variantGene.geneNID",
                         " WHERE ", col.nm," IN (", query, ")", sep="");
    }
  }

  mir.dic <- .query.sqlite(mir.db, statement);
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

# Load httr
load_httr <- function(){
  suppressMessages(library(httr))
}

# Load reshape, necessary for graphics
load_reshape <- function(){
  suppressMessages(library(reshape))
}

overlap_ratio <- function(x, y) {
  x <- unlist(x)
  y <- unlist(y)
  length(intersect(x, y))/length(unique(c(x,y)))
}

# Load ggplot2
load_ggplot <- function(){
  suppressMessages(library(ggplot2))
}

hex2rgb <- function(cols){
  return(apply(sapply(cols, col2rgb), 2, function(x){paste("rgb(", x[1], ",", x[2], ",", x[3], ")", sep="")}));
}

`%notin%` <- Negate(`%in%`)

gg_color_hue <- function(grp.num, filenm=NULL) {
  grp.num <- as.numeric(grp.num)
  pal18 <- c("#3cb44b", "#f032e6", "#ffe119", "#e6194B", "#f58231", "#bfef45", "#fabebe", "#469990", "#e6beff", "#9A6324", "#800000", "#aaffc3", "#808000", "#ffd8b1", "#42d4f4","#000075", "#ff4500");
  if(grp.num <= 18){ # update color and respect default
    colArr <- pal18[1:grp.num];
  }else{
    colArr <- colorRampPalette(pal18)(grp.num);
  }
  if(is.null(filenm)){
    return(colArr);
  }else{
    sink(filenm);
    cat(toJSON(colArr));
    sink();
    return(filenm);
  }
}

Prepare3DManhattanJSON <- function(pmid){
  qs.dir <- "../../data/manhattan/";
  qs.filenm <- paste0(qs.dir, pmid, ".qs");
  json.res <- qs::qread(qs.filenm);
  library(RJSONIO)
  json.mat <- toJSON(json.res, .na='null');
  file.nm <- "manhattan_0.json";
  sink(file.nm)
  cat(json.mat);
  sink();
  if(.on.public.web){
    return(1);
  }else{
    return(paste("JSON files are downloaded!"))
  }
}

PrepareMiamiJSON <- function(){
  #save.image("PrepareMiamiJSON.RData")
  qs.dir <- "../../data/phemr_miami";
  
  #qs.dir <- "../../data/phemr_heatmap";
  
  
  qs.filenm <- paste0(qs.dir, ".qs");
  json.res <- qs::qread(qs.filenm);
  library(RJSONIO)
  json.mat <- toJSON(json.res, .na='null');
  file.nm <- "miami_0.json";
  sink(file.nm)
  cat(json.mat);
  sink();
  if(.on.public.web){
    return(1);
  }else{
    return(paste("JSON files are downloaded!"))
  }
}

.manhattan <- function(x, chr="CHR", bp="BP", p="P", snp="SNP", met="Metabolite", Consequence="Consequence", rsID="rsID",Gene="Gene",Metabolite="Metabolite",Chr="Chr",super_class="super_class",chr_bp="chr_bp",
                       col=c("gray10", "gray60"), chrlabs=NULL,
                       suggestiveline=-log10(1e-5), genomewideline=-log10(5e-8),
                       highlight=NULL, logp=TRUE, annotatePval = NULL, annotateTop = TRUE, ...) {
  x<<-x;
  #save.image("manhattan.RData")
  # https://github.com/stephenturner/qqman
  # Not sure why, but package check will warn without this.
  CHR=BP=P=index=NULL

  # Check for sensible dataset
  ## Make sure you have chr, bp and p columns.
  if (!(chr %in% names(x))) stop(paste("Column", chr, "not found!"))
  if (!(bp %in% names(x))) stop(paste("Column", bp, "not found!"))
  if (!(p %in% names(x))) stop(paste("Column", p, "not found!"))
  ## warn if you don't have a snp column
  if (!(snp %in% names(x))) warning(paste("No SNP column found. OK unless you're trying to highlight."))
  ## make sure chr, bp, and p columns are numeric.
  if (!is.numeric(x[[chr]])) stop(paste(chr, "column should be numeric. Do you have 'X', 'Y', 'MT', etc? If so change to numbers and try again."))
  if (!is.numeric(x[[bp]])) stop(paste(bp, "column should be numeric."))
  if (!is.numeric(x[[p]])) stop(paste(p, "column should be numeric."))

  # Create a new data.frame with columns called CHR, BP, and P.
  # d=data.frame(CHR=x[[chr]], BP=x[[bp]], P=x[[p]], pos = NA, index = NA) # with millions of SNPs, create dataframe at once
  #  rather than dynamically allocated(see line 72-73, and remove line 87 and line 91 )

  # If the input data frame has a SNP column, add it to the new data frame you're creating.
  if (!is.null(x[[snp]])) d = data.frame(CHR=x[[chr]], BP=x[[bp]], P=x[[p]], pos = NA, index = NA ,SNP=x[[snp]],met=x[[met]], Consequence=x[[Consequence]],
                                         rsID=x[[rsID]], Gene=x[[Gene]], Metabolite=x[[Metabolite]], Chr=x[[Chr]], super_class=x[[super_class]], chr_bp=x[[chr_bp]], stringsAsFactors = FALSE) else
                                           d = data.frame(CHR=x[[chr]], BP=x[[bp]], P=x[[p]], pos = NA, index = NA, Consequence=x[[Consequence]], met=x[[met]],
                                                          rsID=x[[rsID]], Gene=x[[Gene]], Metabolite=x[[Metabolite]], Chr=x[[Chr]], super_class=x[[super_class]], chr_bp=x[[chr_bp]])


                                         # Set positions, ticks, and labels for plotting
                                         ## Sort and keep only values where is numeric.
                                         #d <- subset(d[order(d$CHR, d$BP), ], (P>0 & P<=1 & is.numeric(P)))
                                         #  d <- subset(d, (is.numeric(CHR) & is.numeric(BP) & is.numeric(P)))       ## unused, all three variables are numeric, line:63-65
                                         d <- d[order(d$CHR, d$BP), ]
                                         #d$logp <- ifelse(logp, yes=-log10(d$P), no=d$P)
                                         if (logp) {
                                           d$logp <- -log10(d$P)
                                         } else {
                                           d$logp <- d$P
                                         }
                                         # d$pos=NA


                                         # Fixes the bug where one chromosome is missing by adding a sequential index column.
                                         # d$index=NA
                                         # ind = 0
                                         # for (i in unique(d$CHR)){
                                         #     ind = ind + 1
                                         #     d[d$CHR==i,]$index = ind
                                         # }
                                         d$index = rep.int(seq_along(unique(d$CHR)), times = tapply(d$SNP,d$CHR,length))  # replcace the for loop of line 92-96 to improve efficiency

                                         # This section sets up positions and ticks. Ticks should be placed in the
                                         # middle of a chromosome. The a new pos column is added that keeps a running
                                         # sum of the positions of each successive chromsome. For example:
                                         # chr bp pos
                                         # 1   1  1
                                         # 1   2  2
                                         # 2   1  3
                                         # 2   2  4
                                         # 3   1  5
                                         nchr = length(unique(d$CHR))
                                         if (nchr==1) { ## For a single chromosome
                                           ## Uncomment the next two linex to plot single chr results in Mb
                                           #options(scipen=999)
                                           #d$pos=d$BP/1e6
                                           d$pos=d$BP
                                           #  ticks=floor(length(d$pos))/2+1          ## unused, from code line: 169
                                           xlabel = paste('Chromosome',unique(d$CHR),'position')
                                           #  labs = ticks          ## unused, from code line: 169
                                         } else { ## For multiple chromosomes
                                           lastbase=0
                                           ticks=NULL
                                           for (i in unique(d$index)) {
                                             if (i==1) {
                                               d[d$index==i, ]$pos=d[d$index==i, ]$BP
                                             } else {
                                               ## chromosome position maybe not start at 1, eg. 9999. So gaps may be produced.
                                               lastbase = lastbase +max(d[d$index==(i-1),"BP"])   # replace line 128
                                               d[d$index == i,"BP"] = d[d$index == i,"BP"]-min(d[d$index==i,"BP"]) +1
                                               d[d$index == i, "pos"] = d[d$index == i,"BP"] + lastbase    # replace line 129
                                               # lastbase=lastbase+tail(subset(d,index==i-1)$BP, 1)
                                               # d[d$index==i, ]$pos=d[d$index==i, ]$BP+lastbase

                                             }
                                             # Old way: assumes SNPs evenly distributed
                                             # ticks=c(ticks, d[d$index==i, ]$pos[floor(length(d[d$index==i, ]$pos)/2)+1])
                                             # New way: doesn't make that assumption
                                             # ticks = c(ticks, (min(d[d$index == i,]$pos) + max(d[d$index == i,]$pos))/2 + 1)  # see line 136, to reduce the burden of for loop
                                           }
                                           ticks <-tapply(d$pos,d$index,quantile,probs=0.5)   # replace line 135
                                           xlabel = 'Chromosome'
                                           #labs = append(unique(d$CHR),'') ## I forgot what this was here for... if seems to work, remove.
                                           labs <- unique(d$CHR)
                                         }

                                         return(d);
}

.get_statement <- function(table1.nm,table2.nm, col.nm, query, biofluid, population){
  statement1 <- paste("SELECT * FROM ", table1.nm,
                      " LEFT JOIN snp
                    ON snp.snp_orig=",table1.nm,".snp_orig
                     LEFT JOIN metabolites
                     ON metabolites.metabolite_id=",table1.nm,".metabolite_id
                     LEFT JOIN gene
                     ON gene.embl=snp.nearest_gene_50kb
                     LEFT JOIN study
                     ON study.pmid=",table1.nm,".pmid", sep="");
  if(biofluid=="all" && population=="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ")", sep="");
  }else if(biofluid=="all" && population!="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND pop_code=('",population,"')", sep="");
  }else if(biofluid!="all" && population=="all"){
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND biofluid = ('" ,biofluid,"')", sep="");
  }else(
    statement2 <- paste("WHERE ",table2.nm,".", col.nm," IN (", query, ") AND biofluid = ('" ,biofluid,"') AND pop_code=('",population,"')", sep="")
  )
  statement <- paste(statement1, statement2)
}

.parse_snp2met <- function(res){
  res <- res[complete.cases(res[ , 7]),];
  res <- res[complete.cases(res[ , 1]),];
  res <- res[res$p_value != 0,];
  res <- res[res$p_value != "NA",];
  res <- res[order(res$p_value),];
  res$p_value <- format(res$p_value, digits = 4);
  return(res);
}

.parse_snp2dis <- function(res){
  res <- res[complete.cases(res[ , 10]),];
  res <- res[order(res$score, decreasing = TRUE),];
  return(res);
}

.parse_snp2prot <- function(res){
  res <- res[res$Pvalue != 0,];
  res <- res[order(res$Pvalue, decreasing = FALSE),];
  return(res);
}

.get_snp2met_aggregate <- function(res){
  res$`paste(rsid, name)` <- paste(res$rsid, res$name);
  agg.pmid <- aggregate(pmid ~ paste(rsid,name), data = res, paste, collapse = "|");
  agg.pval <- aggregate(p_value ~ paste(rsid,name), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(rsid,name), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(rsid, name)");
  agg.res <- merge(agg.res, agg.pval, by="paste(rsid, name)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(rsid, name)");
}

.get_snp2dis_aggregate <- function(res){
  res$`paste(variantId, diseaseId)` <- paste(res$variantId, res$diseaseId);
  agg.pmid <- aggregate(pmid ~ paste(variantId, diseaseId), data = res, paste, collapse = "|");
  agg.source <- aggregate(source ~ paste(variantId, diseaseId), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(variantId, diseaseId), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(variantId, diseaseId)");
  agg.res <- merge(agg.res, agg.source, by="paste(variantId, diseaseId)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(variantId, diseaseId)");
  
}

.get_snp2prot_aggregate <- function(res){
  res$`paste(rsid, Mapped_gene)` <- paste(res$rsid, res$Mapped_gene);
  agg.pmid <- aggregate(PMID ~ paste(rsid, Mapped_gene), data = res, paste, collapse = "|");
  agg.pval <- aggregate(Pvalue ~ paste(rsid, Mapped_gene), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(PMID ~ paste(rsid, Mapped_gene), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(rsid, Mapped_gene)");
  agg.res <- merge(agg.res, agg.pval, by="paste(rsid, Mapped_gene)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(rsid, Mapped_gene)");
  
}

.get_gene2dis_aggregate <- function(res){
  res$`paste(geneId, diseaseId)` <- paste(res$geneId, res$diseaseId);
  agg.pmid <- aggregate(pmid ~ paste(geneId, diseaseId), data = res, paste, collapse = "|");
  agg.source <- aggregate(source ~ paste(geneId, diseaseId), data = res, paste, collapse = "|");
  agg.n.pmid <- aggregate(pmid ~ paste(geneId, diseaseId), data = res, FUN = length);
  agg.res <- merge(res, agg.pmid, by="paste(geneId, diseaseId)");
  agg.res <- merge(agg.res, agg.source, by="paste(geneId, diseaseId)");
  agg.res <- merge(agg.res, agg.n.pmid, by="paste(geneId, diseaseId)");
  
}

QueryVEP <- function(q.vec,vepDis,content_type="application/json" ){
  library(httr)
  #library(jsonlite)
  #library(xml2)
  server <- "http://rest.ensembl.org"
  ext <- "/vep/human/id/"
  r=list()
  resvep = list()
  vepDis = as.numeric(vepDis)*1000
  
  for(i in 1:length(q.vec)){
    r[[i]] <- GET(paste(server, ext, q.vec[i],"?distance=",vepDis,sep = ""),  accept(content_type))
    stop_for_status(r[[i]])
    resvep[[i]] = content(r[[i]])[[1]][["transcript_consequences"]]
    # resvep[[i]] =lapply( resvep[[i]] , function(x) {c(x,q.vec[i])})
  }
  names(resvep)=q.vec
  
  #saveRDS(resvep,"~/Documents/omicsnet/omicsnet-microbiome/resvep.rds")
  resvep2 = lapply(resvep,function(s) {lapply(s, function(x) {
    list(
      gene_symbol=ifelse(length(x[["gene_symbol"]])!=0,x[["gene_symbol"]],"NA"),
      gene_id=ifelse(length(x[["gene_id"]]) !=0, x[["gene_id"]],'NA'),
      hgnc_id=ifelse(length(x[["hgnc_id"]]) !=0, x[["hgnc_id"]],'NA'),
      transcript_id=ifelse(length(x[["transcript_id"]]) !=0, x[["transcript_id"]],'NA'),
      consequence_terms=ifelse(length(x[["consequence_terms"]]) !=0, paste(unlist(x[["consequence_terms"]]),collapse = ";"),'NA'),
      distance=ifelse(length(x[["distance"]])!=0, x[["distance"]],'NA'))
  })
  }
  )
  resvep3 = do.call(rbind,lapply(resvep2,function(s) {
    do.call(rbind.data.frame,s)
  } )
  )
  resvep3$rsid = gsub("\\.[0-9]*","",rownames(resvep3))
  
  row.names(resvep3)=NULL
  return(resvep3)
}

phenoscanner <- function(snpquery=NULL, genequery=NULL, regionquery=NULL, catalogue="GWAS", pvalue=1E-5, proxies="None", r2=0.8, build=37){
  cat("PhenoScanner V2\n")
  if(is.null(snpquery) & is.null(regionquery) & is.null(genequery)) stop("no query has been requested")
  if((length(snpquery[1])+length(regionquery[1])+length(genequery[1]))>1) stop("only one query type allowed")
  if(!(catalogue=="None" | catalogue=="GWAS" | catalogue=="eQTL" | catalogue=="pQTL" | catalogue=="mQTL" | catalogue=="methQTL")) stop("catalogue has to be one of None, GWAS, eQTL, pQTL, mQTL or methQTL")
  if(!(proxies=="None" | proxies=="AFR" | proxies=="AMR" | proxies=="EAS" | proxies=="EUR" | proxies=="SAS")) stop("proxies has to be one of None, AFR, AMR, EAS, EUR or SAS")
  if(length(snpquery)>100) stop("a maximum of 100 SNP queries can be requested at one time")
  if(length(genequery)>10) stop("a maximum of 10 gene queries can be requested at one time")
  if(length(regionquery)>10) stop("a maximum of 10 region queries can be requested at one time")
  if(!(pvalue>0 & pvalue<=1)) stop("the p-value threshold has to be greater than 0 and less than or equal to 1")
  if(!(r2>=0.5 & r2<=1)) stop("the r2 threshold has to be greater than or equal to 0.5 and less than or equal to 1")
  if(!(build==37 | build==38)) stop("the build has to be equal to 37 or 38")
  if(!is.null(regionquery)){
    ub <- as.numeric(sub(".*-", "", sub(".*:", "",regionquery)))
    lb <- as.numeric(sub("-.*", "", sub(".*:", "",regionquery)))
    dist <- ub - lb
    if(any(dist>1000000)) stop("region query can be maximum of 1MB in size")
  }
  if(length(snpquery)>0){
    results <- data.frame()
    snps <- data.frame()
    n_queries <- length(snpquery) %/% 10
    if((length(snpquery) %% 10)>0){n_queries <- n_queries + 1}
    for(i in 1:n_queries){
      if(i < n_queries){qsnps <- paste0(snpquery[(1+10*(i-1)):(10*i)], collapse="+")}else{qsnps <- paste0(snpquery[(1+10*(i-1)):length(snpquery)], collapse="+")}
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?snpquery=",qsnps,"&catalogue=",catalogue,"&p=",pvalue,"&proxies=",proxies,"&r2=",r2,"&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$snps)==0){
        print(paste0("Error: ",json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          if(length(snpquery)==1){print(paste0(snpquery," -- queried"))}else{print(paste0(i," -- chunk of 10 SNPs queried"))}
        }else{if(length(snpquery)==1){print(paste0("Warning: no results found for ",snpquery))}else{print(paste0("Warning: no results found for chunk ",i))}}
      }
      if(length(json_data$snps)>0){
        fields_snps <- json_data$snps[[1]]; json_data$snps[[1]] <- NULL
        if(length(json_data$snps)>0){
          tables_snps <- as.data.frame(matrix(unlist(json_data$snps), ncol=length(fields_snps), byrow=T), stringsAsFactors=F)
          names(tables_snps) <- fields_snps
          snps <- rbind(snps,tables_snps)
          if(length(json_data$results)==0){if(length(snpquery)==1){print(paste0(snpquery," -- queried"))}else{print(paste0(i," -- chunk of 10 SNPs queried"))}}
        }else{if(length(json_data$results)==0){if(length(snpquery)==1){print(paste0("Warning: no results found for ",snpquery))}else{print(paste0("Warning: no results found for chunk ",i))}}}
      }
    }
    output <- list(snps=snps, results=results)
  }
  if(length(genequery)>0){
    results <- data.frame()
    genes <- data.frame()
    n_queries <- length(genequery)
    for(i in 1:n_queries){
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?genequery=",genequery[i],"&catalogue=",catalogue,"&p=",pvalue,"&proxies=None&r2=1&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$genes)==0){
        print(paste0("Error: ", json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          print(paste0(genequery," -- queried"))
        }else{print(paste0("Warning: no results found for ",genequery))}
      }
      if(length(json_data$genes)>0){
        fields_genes <- json_data$genes[[1]]; json_data$genes[[1]] <- NULL
        if(length(json_data$genes)>0){
          tables_genes <- as.data.frame(matrix(unlist(json_data$genes), ncol=length(fields_genes), byrow=T), stringsAsFactors=F)
          names(tables_genes) <- fields_genes
          genes <- rbind(genes,tables_genes)
        }
      }
    }
    output <- list(genes=genes, results=results)
  }
  if(length(regionquery)>0){
    results <- data.frame()
    regions <- data.frame()
    n_queries <- length(regionquery)
    for(i in 1:n_queries){
      json_file <- paste0("http://www.phenoscanner.medschl.cam.ac.uk/api/?regionquery=",regionquery[i],"&catalogue=",catalogue,"&p=",pvalue,"&proxies=None&r2=1&build=",build)
      json_data <- getApiResult(json_file);
      if(length(json_data$results)==0 & length(json_data$locations)==0){
        print(paste0("Error: ",json_data$error))
        next
      }
      if(length(json_data$results)>0){
        fields <- json_data$results[[1]]; json_data$results[[1]] <- NULL
        if(length(json_data$results)>0){
          tables <- as.data.frame(matrix(unlist(json_data$results), ncol=length(fields), byrow=T), stringsAsFactors=F)
          names(tables) <- fields
          results <- rbind(results,tables)
          print(paste0(regionquery," -- queried"))
        }else{print(paste0("Warning: no results found for ",regionquery))}
      }
      if(length(json_data$locations)>0){
        fields_regions <- json_data$locations[[1]]; json_data$locations[[1]] <- NULL
        if(length(json_data$locations)>0){
          tables_regions <- as.data.frame(matrix(unlist(json_data$locations), ncol=length(fields_regions), byrow=T), stringsAsFactors=F)
          names(tables_regions) <- fields_regions
          regions <- rbind(regions,tables_regions)
        }
      }
    }
    output <- list(regions=regions, results=results)
  }
  if(is.null(output)) stop("there is no output")
  cat("PhenoScanner done\n")
  return(output)
}

QueryHaploreg <- function(query=NULL, file=NULL,
                          study=NULL,
                          ldThresh=0.8, 
                          ldPop="EUR", 
                          epi="vanilla", 
                          cons="siphy", 
                          genetypes="gencode",
                          url=Haploreg.settings[["base.url"]],
                          timeout=100,
                          encoding="UTF-8",
                          querySNP=FALSE,
                          fields=NULL,
                          verbose=FALSE) {
  
  if(!is.null(query) & !is.null(file))
  {
    stop("'query' and 'file' can not be supplied simultaneously")
  }
  parameters <- list(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                     epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                     encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
  
  recTable <- data.frame(matrix(nrow=0, ncol=35))
  if(!is.null(query))
  {
    if(grepl("chr", query[1])) 
    {
      res <- lapply(1:length(query), function(i) {
        simpleQuery(query=query[i], file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                    epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                    encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      })
      
      recTable <- ldply(res, data.frame)
      recTable <- recTable[!duplicated(recTable), ]
    } else {
      
      recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                              epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                              encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
    }
  } else if(!is.null(file)) 
  {
    con <- file(file)
    lines <- readLines(con)
    close(con)
    if(grepl("chr", lines[1]))
    {
      res <- lapply(1:length(lines), function(i) {
        simpleQuery(query=lines[i], file=NULL, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                    epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                    encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      })
      
      recTable <- ldply(res, data.frame)
      recTable <- recTable[!duplicated(recTable), ]
    } 
    else 
    {
      recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                              epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                              encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
      
    }
    
  } else if(!is.null(study))
  {
    
    recTable <- simpleQuery(query=query, file=file, study=study, ldThresh=ldThresh, ldPop=ldPop, 
                            epi=epi, cons=cons, genetypes=genetypes, url=url, timeout=timeout,
                            encoding=encoding, querySNP=querySNP, fields=fields, verbose=verbose)
    
  } else 
  {
    stop("Parameters 'query', 'study' and 'file' are NULL.")
  }
  #ifelse(!is.null(recTable), as_tibble(recTable), NULL)
  #library(tibble);
  if(is.null(recTable)){
  return(recTable);
  }else{
  return(as.data.frame(recTable));
  }
}

simpleQuery <- function(query=NULL, file=NULL,
                        study=NULL,
                        ldThresh=0.8, 
                        ldPop="EUR", 
                        epi="vanilla", 
                        cons="siphy", 
                        genetypes="gencode",
                        url=Haploreg.settings[["base.url"]],
                        timeout=100,
                        encoding="UTF-8",
                        querySNP=FALSE,
                        fields=NULL,
                        verbose=FALSE)
{
  trunc <- 1000 # can be 0 2 3 4 5 1000
  oligo <- 1000 # can be 1, 6, 1000
  output <- "text"
  
  if(!is.null(study)) {
    if(class(study) == "list") {
      gwas_idx <- study$id
    } else {
      stop("Parameter study is not a list with 
           study id and study name.")
    }
  } else {
    gwas_idx <- "0"
  }
  
  
  if(!is.null(file)) {
    query <- upload_file(file)
    body <- list(upload_file=query, 
                 gwas_idx=gwas_idx,
                 ldThresh=ifelse(!is.na(ldThresh), as.character(ldThresh), "1.1"), 
                 ldPop=ldPop, epi=epi, 
                 cons=cons, 
                 genetypes=genetypes,
                 trunc=as.character(trunc),
                 oligo=as.character(oligo),
                 output=output)
    
  } else {
    query <- paste(query, collapse = ',') 
    body <- list(query=query, 
                 gwas_idx=gwas_idx,
                 ldThresh=ifelse(!is.na(ldThresh), as.character(ldThresh), "1.1"), 
                 ldPop=ldPop, epi=epi, 
                 cons=cons, 
                 genetypes=genetypes,
                 trunc=as.character(trunc),
                 oligo=as.character(oligo),
                 output=output)
    
  }
  
  
  res.table <- data.frame()
  # Form encoded: multipart encoded
  r <- getHaploregData(url, body, timeout)

  if(is.null(r)) {
    return(r)
  }
  
  dat <- content(r, "text", encoding=encoding)
  sp <- strsplit(dat, '\n')
  res.table <- data.frame(matrix(nrow=length(sp[[1]])-1, ncol=length(strsplit(sp[[1]][1], '\t')[[1]])), stringsAsFactors = FALSE)
  colnames(res.table) <- strsplit(sp[[1]][1], '\t')[[1]]
  
  for(i in 2:length(sp[[1]])) {
    res.table[i-1,] <- strsplit(sp[[1]][i], '\t')[[1]]
  }
  
  #Convert numeric-like columns to actual numeric #
  for(i in 1:dim(res.table)[2]) {
    col.num.conv <- suppressWarnings(as.numeric(res.table[,i]))
    #na.rate <- length(which(is.na(col.num.conv)))/length(col.num.conv)
    if(!any(is.na(col.num.conv))) {
      res.table[,i] <- col.num.conv
    }
  }
  
  
  if(querySNP) {
    res.table <- res.table[which(res.table$is_query_snp == 1), ]
  }
  
  if(!is.null(fields)) {
    res.table <- res.table[, fields]
  }
  
  # Removing blank rows:
  res.table <- res.table[, colSums(is.na(res.table)) <= 1] 
  # Adding additional columns: 
  user.agent <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"
  body$output <- "html"
  library(httr)
  request.data <- tryCatch(
    {
      POST(url=url, body = body, encode="multipart",  timeout(timeout),user_agent(user.agent))
    },
    error=function(e) {
      library(RCurl)
      if(url.exists(url)) {
        message(paste("URL does not seem to exist:", url))
      }
      message("Here's the original error message:")
      message(e$message)
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(e) {
      message(e$message)
      # Choose a return value in case of warning
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      return(NULL)
    }
  )
  # this is the original, does not catch error
  #request.data <- POST(url=url, body=body, encode="multipart",  timeout(timeout), user_agent(user.agent))
  html.content <- content(request.data, useInternalNodes=TRUE, encoding="ISO-8859-1",as="text")
  library(XML);
  tmp.tables <- readHTMLTable(html.content)
  html.table <- NULL
  for(i in 4:length(tmp.tables)) {
    tmp.table <- tmp.tables[[i]]
    if(is.null(tmp.table))
    {
      next
    }
    
    n.col <- dim(tmp.table)[2]
    n.row <- dim(tmp.table)[1]
    
    if(n.col <= 6) 
    {
      next
    } 
    
    if(n.col < 23) {
      while(n.col < 23) {
        tmp.col <- data.frame(replicate(n.row, ""), stringsAsFactors = FALSE)
        colnames(tmp.col) <- paste("V",n.col+1, sep="")
        tmp.table <- cbind(tmp.table, tmp.col)
        n.col <- dim(tmp.table)[2]
      }
    }
    
    if(is.null(html.table)) {
      html.table <- tmp.table
    } else {   
      #print(head(tmp.table))
      colnames(html.table) <- colnames(tmp.table)
      #print("*****")
      #print(head(html.table))
      #print(dim(html.table))
      #print("=====")
      #print(head(tmp.table))
      #print(dim(tmp.table))
      html.table <- rbind(html.table, tmp.table)
    }
  }
  
  if(!is.null(html.table)) {
    tmp.table <- data.frame(html.table[, c(5,13:14)], stringsAsFactors = FALSE)
    tmp.table <- tmp.table[!duplicated(tmp.table), ]
    if("variant" %in% colnames(tmp.table)) {
      data.merged <- merge(res.table, tmp.table, by.x="rsID", by.y="variant")
    } else {
      data.merged <- merge(res.table, tmp.table, by.x="rsID", by.y="V5")
    }
    
    data.merged1 <- cbind(data.merged[["chr"]],
                          data.merged[["pos_hg38"]],
                          data.merged[["r2"]],
                          data.merged[["D'"]],
                          data.merged[["is_query_snp"]],
                          data.merged[["rsID"]],
                          data.merged[["ref"]],
                          data.merged[["alt"]],
                          data.merged[["AFR"]],
                          data.merged[["AMR"]],
                          data.merged[["ASN"]],
                          data.merged[["EUR"]],
                          data.merged[["GERP_cons"]],
                          data.merged[["SiPhy_cons"]],
                          data.merged[["Chromatin_States"]],
                          data.merged[["Chromatin_States_Imputed"]],
                          data.merged[["Chromatin_Marks"]],
                          data.merged[["DNAse"]],
                          data.merged[["Proteins"]],
                          data.merged[["eQTL"]],
                          data.merged[["gwas"]],
                          data.merged[["grasp"]],
                          data.merged[["Motifs"]],
                          data.merged[["GENCODE_id"]],
                          data.merged[["GENCODE_name"]],
                          data.merged[["GENCODE_direction"]],
                          data.merged[["GENCODE_distance"]],
                          data.merged[["RefSeq_id"]],
                          data.merged[["RefSeq_name"]],
                          data.merged[["RefSeq_direction"]],
                          data.merged[["RefSeq_distance"]],
                          data.merged[["dbSNP_functional_annotation"]],
                          data.merged[["query_snp_rsid"]])
    data.merged <- data.frame(data.merged1, data.merged[,34:35], stringsAsFactors = FALSE)
    #data.merged <- cbind(data.merged1, data.merged[,34:35])
    
    colnames(data.merged) <- c("chr", "pos_hg38", "r2", "D'", "is_query_snp", 
                               "rsID", "ref", "alt", "AFR", "AMR", 
                               "ASN", "EUR", "GERP_cons", "SiPhy_cons", 
                               "Chromatin_States",
                               "Chromatin_States_Imputed", "Chromatin_Marks", 
                               "DNAse", "Proteins", "eQTL",
                               "gwas", "grasp", "Motifs", "GENCODE_id", 
                               "GENCODE_name",
                               "GENCODE_direction", "GENCODE_distance", "RefSeq_id", 
                               "RefSeq_name", "RefSeq_direction",
                               "RefSeq_distance", "dbSNP_functional_annotation", 
                               "query_snp_rsid", "Promoter_histone_marks", 
                               "Enhancer_histone_marks")
    
  }
  
  # Make important columns to be numeric
  data.merged[["chr"]] <- as.num(data.merged[["chr"]])
  data.merged[["r2"]] <- as.num(data.merged[["r2"]])
  data.merged[["D'"]] <- as.num(data.merged[["D'"]])
  data.merged[["is_query_snp"]] <- as.num(data.merged[["is_query_snp"]])
  data.merged[["AFR"]] <- as.num(data.merged[["AFR"]])
  data.merged[["AMR"]] <- as.num(data.merged[["AMR"]])
  data.merged[["ASN"]] <- as.num(data.merged[["ASN"]])
  data.merged[["EUR"]] <- as.num(data.merged[["EUR"]])
  
  
  return(data.merged)
}

getHaploregData <- function(url, body, timeout) {
  # Form encoded: multipart encoded
  library(httr)
  r <- tryCatch(
    {
      POST(url=url, body = body, encode="multipart",  timeout(timeout))
    },
    error=function(e) {
      library(RCurl)
      if(url.exists(url)) {
        message(paste("URL does not seem to exist:", url))
      }
      message("Here's the original error message:")
      message(e$message)
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";

      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(e) {
      message(e$message)
      # Choose a return value in case of warning
      current.msg <<- "Timeout was reached for HaploReg API call, please try again later or try another option.";
      return(NULL)
    }
  )
  return(r)
}

as.num <- function(x, na.strings = "NA") {
  stopifnot(is.character(x))
  na = x %in% na.strings
  x[na] = 0
  x = as.numeric(x)
  x[na] = NA_real_
  x
}

QueryPpiSQLiteZero <- function(table.nm, q.vec, requireExp, min.score){
  #table.nm<<-table.nm;
  #q.vec<<-q.vec;
  #requireExp<<-requireExp;
  #min.score<<-min.score;
  #save.image("QueryPpiSQLiteZero.RData")
  require('RSQLite')
  db.path <- paste(url.pre, "ppi.sqlite", sep="");
  if(.on.public.web){
    ppi.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name);
    }
    ppi.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste(shQuote(q.vec),collapse=",");
  
  if(grepl("string$", table.nm)){
    if(requireExp){
      statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))  AND combined_score >=", min.score, " AND experimental > 0", sep="");
    }else{
      statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))  AND combined_score >=", min.score, sep="");
    }
  }else{
    statement <- paste("SELECT * FROM ",table.nm, " WHERE ((id1 IN (", query, ")) OR (id2 IN (", query, ")) OR (name1 IN (", query, "))OR (name2 IN (", query, ")))", sep="");
  }
  
  ppi.res <- .query.sqlite(ppi.db, statement);
  hit.inx1 <- ppi.res[,1] %in% q.vec
  # hit.inx2 <- ppi.res[,2] %in% q.vec
  ppi.res1 <- ppi.res[(hit.inx1),]
  # 
  hit.inx3 <- ppi.res[,3] %in% q.vec
  # hit.inx4 <- ppi.res[,4] %in% q.vec
  ppi.res2 <- ppi.res[(hit.inx3),]
  ppi.res = rbind(ppi.res1,ppi.res2)
  
  return(ppi.res);
}
  
getApiResult <- function(url="NA", init=TRUE){
  json_data = tryCatch({
    rjson::fromJSON(file=url);
  }, warning = function(w) {
    print(w);
    if(init){
      print("API call failed, calling again!");
      Sys.sleep(5);
      getApiResult(url, F);
    }else{
          current.msg <<- "PhenoScanner API call failed, please try again later or try another option.";
      return(0);
    }
  }, error = function(e) {
    if(init){
      print(e);
      print("API call failed, calling again2!");
      Sys.sleep(5);
      getApiResult(url, F);
    }else{
          current.msg <<- "PhenoScanner API call failed, please try again later or try another option.";
      return(0);
    }
  }, finally = {
    
  })
  return(json_data)
}

GetCompleteMetList <- function(){
  #save.image("GetCompleteMetList.RData");
  # get pre-saved metabolite name with beta and se
  met.nms <- .get.my.lib("mr_met_nms.qs");
  return(met.nms);
}

GetCompletePheMRMetDisList <- function(){
  #save.image("GetCompletePheMRMetDisList.RData");
  met.nms <- .get.my.lib("phemr_browse_met_nms.qs");
  dis.nms <- .get.my.lib("phemr_browse_dis_nms.qs")
  return(c(met.nms, dis.nms));
}

GetCompleteDisList <- function(){
  # get disease from ieugwasr
  #save.image("GetCompleteDisList.RData")
  ieugwas.db <- .get.my.lib("ieugwas_202210.qs");
  return(sort(paste(ieugwas.db$trait, ieugwas.db$id, sep = " | ")));

}

.parse_snp2met_exposure <- function(res){
  res <- res[complete.cases(res[ , 10]),]; # beta
  res <- res[complete.cases(res[ , 19]),]; # se
  res <- res[res$beta != 0,];
  res <- res[res$se != 0,];
  res <- res[res$beta != "NA",];
  res <- res[res$se != "NA",];
  res <- res[order(res$p_value),];
  res$p_value <- format(res$p_value, digits = 4);
  return(res);
}

#'Replace infinite numbers
#'@description Replace -Inf, Inf to 99999 and -99999
#'@param bdata Input matrix to clean numbers
#'@author Jeff Xia\email{jeff.xia@mcgill.ca}
#'McGill University, Canada
#'License: MIT License
#'
CleanNumber <-function(bdata){
  if(sum(bdata==Inf)>0){
    inx <- bdata == Inf;
    bdata[inx] <- NA;
    bdata[inx] <- 999999;
  }
  if(sum(bdata==-Inf)>0){
    inx <- bdata == -Inf;
    bdata[inx] <- NA;
    bdata[inx] <- -999999;
  }
  bdata;
}

SetEqtlTissue <- function(){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$eqtlTissue <- nms.vec;
  .set.mSet(mSetObj);
  if(.on.public.web){
    return(1);
  }else{
    return (paste("eQTL mapping tissue options were entered!"))
  }
}

Query.eQTLDB <- function(db.path, q.vec, table.nm, col.nm){
  require('RSQLite');
  # db.path <<- db.path;
  # q.vec<<-q.vec;
  # table.nm<<-table.nm;
  # col.nm<<-col.nm;
  # save.image("Query.eQTLDB.RData")
  
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  statement <- paste("SELECT * FROM ", table.nm, 
                     " LEFT JOIN snp_conv 
                     ON snp_conv.variant_id=", table.nm, ".variant_id",
                     " WHERE ", col.nm," IN (", query, ")", sep="");
  
  
  mir.dic <- .query.sqlite(mir.db, statement);
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

Query.pQTLDB <- function(db.path, q.mat){
  require('RSQLite');
  #db.path<<-db.path;
  #q.mat<<-q.mat;
  #save.image("Query.pQTLDB.RData")
  
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      #print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.mat[,"chr_hg19"]),collapse=",");
  statement <- paste("SELECT * FROM pqtl WHERE chr_hg19 IN (", query, ")", sep="");
  mir.dic <- .query.sqlite(mir.db, statement);
  mir.dic <- unique(merge(mir.dic, as.data.frame(q.mat), by = "chr_hg19"))
  
  # # remove duplicates
  # dup.inx <- duplicated(mir.dic$mgwas);
  # mir.dic <- mir.dic[!dup.inx, ];
  return(mir.dic);
}

.map2chrposhg19 <- function(snp.vec){
  #snp.vec<<-snp.vec;
  #save.image("map2chrposhg19.RData")
  snp.res.chr <- myvariant::getVariants(snp.vec, scopes="dbsnp.rsid", fields="dbsnp.chrom");
  snp.res.hg19.start <- myvariant::getVariants(snp.vec, scopes="dbsnp.rsid", fields="dbsnp.hg19.start");
  chr_hg19 <- paste(snp.res.chr@listData[["dbsnp.chrom"]], snp.res.hg19.start@listData[["dbsnp.hg19.start"]], sep = ":")
  rsid <- snp.res.chr@listData[["query"]];
  snp.res.mat <- unique(cbind(rsid, chr_hg19));
  return(snp.res.mat);
}

query_epigraphdb <- function(route, params = NULL,
                             mode = c("raw", "table"),
                             method = c("GET", "POST"),
                             retry_times = 3,
                             retry_pause_min = 1) {
  #print("query_epigraphdb====")
  # route<<-route;
  # params<<-params;
  # print(mode);
  # print(method);
  # retry_times<<-retry_times;
  # retry_pause_min<<-retry_pause_min;
  # save.image("query_epigraphdb.RData")
  mode <- match.arg(mode)
  method <- match.arg(method)
  if (method == "GET") {
    method_func <- api_get_request
  } else if (method == "POST") {
    method_func <- api_post_request
  }
  res <- api_request(
    route = route, params = params, mode = mode, method = method_func,
    retry_times = retry_times, retry_pause_min = retry_pause_min
  )
  res
}

api_get_request <- function(route, params,
                            retry_times, retry_pause_min) {
  #print("api_get_request====")
  #route<<-route;
  #params<<-params;
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("api_get_request.RData")
  api_url <- "https://api.epigraphdb.org";
  url <- glue::glue("{api_url}{route}")
  library(magrittr) # for pipe operation %>% 
  epigraphdb.ci = Sys.getenv(x = "CI", unset = c(CI = "false")) %>%
    as.logical()
  is_ci <- getOption("epigraphdb.ci") %>%
    as.character() %>%
    tolower()
  config <- httr::add_headers(.headers = c("client-type" = "R", "ci" = is_ci))
  response <- httr::RETRY(
    "GET",
    url = url, query = params, config = config,
    times = retry_times, pause_min = retry_pause_min
  )
  stop_for_status(response = response, context = list(params = params, url = url))
  response
}

api_post_request <- function(route, params,
                             retry_times, retry_pause_min) {
  #route<<-route;
  #params<<-params;
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("api_post_request.RData")
  #api_url <- "https://api.epigraphdb.org";
  api_url <- "https://melodi-presto.mrcieu.ac.uk/api"
  url <- glue::glue("{api_url}{route}");
  library(magrittr) # for pipe operation %>% 
  # Are the requests for CI usage
  epigraphdb.ci = Sys.getenv(x = "CI", unset = c(CI = "false")) %>%
    as.logical()
  is_ci <- getOption("epigraphdb.ci") %>%
    as.character() %>%
    tolower()
  config <- httr::add_headers(.headers = c("client-type" = "R", "ci" = is_ci))
  # body <- jsonlite::toJSON(params, auto_unbox = TRUE) # this is for epigraphdb query
  body <- jsonlite::toJSON(params);
  response <- httr::RETRY(
    "POST",
    url = url, body = body, config = config,
    times = retry_times, pause_min = retry_pause_min
  )
  stop_for_status(response, context = list(params = params, url = url))
  response
}

api_request <- function(route, params,
                        mode = c("table", "raw"),
                        method = method,
                        retry_times, retry_pause_min) {
  #print("api_request====")  
  # route<<-route;
  # params<<-params;
  # print(mode)
  # print(method);
  # retry_times<<-retry_times;
  # retry_pause_min<<-retry_pause_min;
  # save.image("api_request.RData")
  mode <- match.arg(mode)
  response <- do.call(method, args = list(
    route = route, params = params,
    retry_times = retry_times, retry_pause_min = retry_pause_min
  ))
  if (mode == "table") {
    return(flatten_response(response))
  }
  library(magrittr) # for pipe operation %>% 
  response %>% httr::content(as = "parsed", encoding = "utf-8") 
}

stop_for_status <- function(response, context) {
  #print("stop_for_status====")
  
  if (httr::status_code(response) < 300) {
    return(invisible(response))
  }
  
  stop(httr::http_condition(response, context))
}

flatten_response <- function(response, field = "results") {
  library(magrittr) # for pipe operation %>% 
  response %>%
    httr::content(as = "text", encoding = "utf-8") %>%
    jsonlite::fromJSON(flatten = TRUE) %>%
    purrr::pluck(field) %>%
    tibble::as_tibble()
}

query_melodipresto <- function(route, params = NULL,
                             mode = c("raw", "table"),
                             method = c("GET", "POST"),
                             retry_times = 3,
                             retry_pause_min = 1) {
  #route<<-route;
  #params<<-params;
  #print(mode);
  #print(method);
  #retry_times<<-retry_times;
  #retry_pause_min<<-retry_pause_min;
  #save.image("query_melodipresto.RData")
  mode <- match.arg(mode)
  method <- match.arg(method)
  if (method == "GET") {
    method_func <- api_get_request
  } else if (method == "POST") {
    method_func <- api_post_request
  }
  res <- api_request(
    route = route, params = params, mode = mode, method = method_func,
    retry_times = retry_times, retry_pause_min = retry_pause_min
  )
  l <- list();
  library(magrittr)
  for (i in 1:length(res$data)){
    l[[i]] <- res$data[[i]] %>% 
      tibble::as_tibble()
  }
  df <- do.call("rbind", l);
}

clump_data_local_ld <- function (dat, clump_kb = 10000, clump_r2 = 0.001, clump_p1 = 1, 
                                 clump_p2 = 1, pop = "EUR") 
{
  pval_column <- "pval.exposure"
  if (!is.data.frame(dat)) {
    stop("Expecting data frame returned from format_data")
  }
  if ("pval.exposure" %in% names(dat) & "pval.outcome" %in% 
      names(dat)) {
    message("pval.exposure and pval.outcome columns present. Using pval.exposure for clumping.")
  }
  else if (!"pval.exposure" %in% names(dat) & "pval.outcome" %in% 
           names(dat)) {
    message("pval.exposure column not present, using pval.outcome column for clumping.")
    pval_column <- "pval.outcome"
  }
  else if (!"pval.exposure" %in% names(dat)) {
    message("pval.exposure not present, setting clumping p-value to 0.99 for all variants")
    dat$pval.exposure <- 0.99
  }
  else {
    pval_column <- "pval.exposure"
  }
  if (!"id.exposure" %in% names(dat)) {
    dat$id.exposure <- random_string(1)
  }
  d <- data.frame(rsid = dat$SNP, pval = dat[[pval_column]], 
                  id = dat$id.exposure)
  ########## use a local LD reference panel, faster
  out <- ieugwasr::ld_clump(d, clump_kb = clump_kb, clump_r2 = clump_r2, 
                            clump_p = clump_p1, pop = pop,
                            plink_bin = genetics.binaRies::get_plink_binary(),
                            bfile = paste0(plink.path, pop)
                            )
  keep <- paste(dat$SNP, dat$id.exposure) %in% paste(out$rsid, 
                                                     out$id)
  return(dat[keep, ])
}

Query.PheMRDB <- function(db.path, q.vec, col.nm, table.nm){
  require('RSQLite');
  #db.path<<-db.path;
  #q.vec<<-q.vec;
  #col.nm<<-col.nm;
  #table.nm<<-table.nm
  #save.image("Query.PheMRDB.RData")
  db.path <- paste0(db.path, ".sqlite");
  if(.on.public.web){
    mir.db <- dbConnect(SQLite(), db.path);
  }else{
    msg <- paste("Downloading", db.path);
    db.name <- gsub(sqlite.path, "", db.path);
    if(!file.exists(db.name)){
      print(msg);
      download.file(db.path, db.name, mode = "wb");
    }
    mir.db <- dbConnect(SQLite(), db.name);
  }
  query <- paste (shQuote(q.vec),collapse=",");
  if(table.nm == "mr_results" | table.nm == "mr_single" ){
    statement <- paste("SELECT * FROM ", table.nm,"  
                     WHERE ", table.nm,".", col.nm," IN (", query, ")", sep="");
  }else if(table.nm == "mr_sensitivity"){
    statement <- paste("SELECT mr_direct.exposure,  mr_direct.outcome_nm, mr_direct.correct_causal_direction, mr_direct.steiger_pval, 
    mr_hetero.method, mr_hetero.Q_pval, mr_pleio.pval
    FROM mr_direct
    LEFT JOIN mr_hetero
    ON (mr_direct.exposure || mr_direct.outcome) = (mr_hetero.exposure || mr_hetero.outcome)
    LEFT JOIN mr_pleio
    ON (mr_direct.exposure || mr_direct.outcome) = (mr_pleio.exposure || mr_pleio.outcome)
    WHERE mr_direct.", col.nm," IN (", query, ")", sep="")
  }
  mir.dic <- .query.sqlite(mir.db, statement);
  return(mir.dic);
  
}

QueryMyVariant <- function(query, scope){
  # query<<-query;
  # scope<<-scope;
  # save.image("QueryMyVariant.RData")
  res <- myvariant::getVariants(query, scopes="dbsnp.rsid", fields="all");
  rsid <- res@listData[["query"]];
  chr <- res@listData[["chrom"]];
  position_hg19 <- res@listData[["dbsnp.hg19.start"]];
  ref <- res@listData[["dbsnp.ref"]];
  alt <- res@listData[["dbsnp.alt"]];
  consequence <- res@listData[["snpeff.ann.effect"]];
  if(is.null(consequence)){
    snpeff.ann <- res@listData[["snpeff.ann"]];
    consequence <- sapply(snpeff.ann, function(x) {
      if (is.data.frame(x)) {
        if (length(x$effect) > 0) {
          return(x$effect[1])
        } else {
          return(NA)  # return NA if x$effect has length 0
        }
      } else {
        if (length(x$effect) > 0) {
          return(x$effect)
        } else {
          return(NA)  # return NA if x$effect has length 0
        }
      }
    })
  }
  symbol <- res@listData[["snpeff.ann.gene_id"]]
  if(is.null(symbol)){
    symbol <- sapply(snpeff.ann, function(x) {
      if (is.data.frame(x)) {
        if (length(x$gene_id) > 0) {
          return(x$gene_id[1])
        } else {
          return(NA)  # return NA if x$gene_id has length 0
        }
      } else {
        if (length(x$gene_id) > 0) {
          return(x$gene_id)
        } else {
          return(NA)  # return NA if x$gene_id has length 0
        }
      }
    })
  }
  mygene.res <- mygene::queryMany(symbol, scopes = "symbol", fields = "entrezgene", species = "human");
  entrez <- mygene.res$entrezgene
  output.df <- data.frame(
    query_snp_rsid = rsid,
    chr = chr,
    position_hg19 = position_hg19,
    ref = ref,
    alt = alt,
    consequence = unlist(consequence),
    symbol = unlist(symbol),
    entrez = entrez,
    stringsAsFactors = FALSE
  )
  library(dplyr);
  # only keep the first row when there are multiple alternative alleles
  output.df <- output.df %>%
        distinct(query_snp_rsid, .keep_all = TRUE)
  return(output.df);

}

QueryLDlink <- function(){
  library(LDlinkR)
  # Your token is: 9a329febb2bf
  # this is an example
  LDhap(snps = c("rs3", "rs4", "rs148890987"), 
        pop = "EUR", 
        token = "9a329febb2bf",
        genome_build = "grch38")
  my_proxies <- LDproxy(snp = "rs657152", 
                        pop = "EUR", 
                        r2d = "r2", 
                        token = "9a329febb2bf",
                        genome_build = "grch38"
  )
  
}

# code adapted from gwasvcf R package
get_ld_proxies <- function(rsid, bfile, searchspace=NULL, tag_kb=5000, tag_nsnp=5000, tag_r2=0.6, threads=1, out=paste0(getwd(),"/"))
{
  stopifnot(gwasvcf::check_plink())
  searchspacename <- paste0(out, ".searchspace")
  targetsname <- paste0(out, ".targets")
  outname <- paste0(out, ".targets.ld.gz")
  utils::write.table(rsid, file=targetsname, row.names = FALSE, col.names = FALSE, quote = FALSE)
  if(!is.null(searchspace))
  {
    stopifnot(is.character(searchspace))
    
    utils::write.table(unique(c(rsid, searchspace)), file=searchspacename, row.names = FALSE, col.names = FALSE, quote = FALSE)
    extract_param <- paste0(" --extract ", searchspacename)
  } else {
    extract_param <- " " 
  }
  cmd <- paste0(options()[["tools_plink"]],
                " --bfile ", bfile, 
                extract_param,
                " --keep-allele-order ",
                " --r in-phase with-freqs gz",
                " --ld-snp-list ", targetsname,
                " --ld-window-kb ", tag_kb,
                " --ld-window-r2 ", tag_r2,
                " --ld-window ", tag_nsnp,
                " --out ", targetsname,
                " --threads ", threads,
                " 2>&1 > /dev/null"
  )

  print("Finding proxies...")
  system(cmd)
  
  if (Sys.info()["sysname"] == "Windows") {
    stop("Currently, this function only works on macOS and Linux")
  }
  if (!file.exists(outname)) {
    ld <- data.frame(CHR_A = integer(), BP_A = integer(), SNP_A = character(), MAF_A = double(), CHR_B = integer(), BP_B = integer(), 
                     SNP_B = character(), PHASE = character(), MAF_B = double(), R = double())
    print("Index SNP not found in the reference panel")
    return(ld)
  }

  library(magrittr);
  system(paste0("gunzip -c ", outname, " > out.txt"));
  ld <- data.table::fread("out.txt", header=TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::filter(.data[["R"]]^2 > tag_r2) %>%
    dplyr::filter(.data[["SNP_A"]] != .data[["SNP_B"]]) %>%
    dplyr::mutate(PHASE=gsub("/", "", .data[["PHASE"]])) %>%
    dplyr::filter(nchar(.data[["PHASE"]]) == 4)

  unlink(searchspacename)
  unlink(targetsname)
  unlink(paste0(targetsname, c(".log", ".nosex")))
  unlink(outname)
  if(nrow(ld) == 0)
  {
    print("No proxies found")
    return(ld)
  }

  temp <- do.call(rbind, strsplit(ld[["PHASE"]], "")) %>% dplyr::as_tibble(.data, .name_repair="minimal")
  names(temp) <- c("A1", "B1", "A2", "B2")
  ld <- cbind(ld, temp) %>% 
    dplyr::as_tibble( .name_repair="minimal")
  # ld <- dplyr::arrange(ld, desc(abs(R))) %>%
  # 	dplyr::filter(!duplicated(SNP_A))
  ld <- dplyr::arrange(ld, dplyr::desc(abs(.data[["R"]])))
  print(paste0("Found ", nrow(ld), " proxies"))

  return(ld)
}



#'Record R Commands
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@param cmd Commands 
#'@export
RecordRCommand <- function(mSetObj, cmd){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$cmdSet <- c(mSetObj$cmdSet, cmd);
  .set.mSet(mSetObj)
  return(1);
}

SaveRCommands <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  cmds <- paste(mSetObj$cmdSet, collapse="\n");
  pid.info <- paste0("# PID of current job: ", Sys.getpid());
  cmds <- c(pid.info, cmds);
  write(cmds, file = "Rhistory.R", append = FALSE);
}

#'Export R Command History
#'@param mSetObj Input the name of the created mSetObj (see InitDataObjects)
#'@export
GetRCommandHistory <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  if(length(mSetObj$cmdSet) == 0){
    return("No commands found");
  }
  return(mSetObj$cmdSet);
}

ClearRCommandHistory <- function(mSetObj){
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$cmdSet$cmdVec <- c();
  .set.mSet(mSetObj)
}