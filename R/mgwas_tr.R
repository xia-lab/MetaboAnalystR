##################################################
## R script for Triangulation of MR results 
## Author: Jeff Xia, jeff.xia@mcgill.ca
###################################################


QueryLiteratureMelodiPresto <- function(exposure, outcome) {
  .init.multilist();
  # exposure<<-exposure;
  # outcome<<-outcome;
  # save.image("QueryLiteratureMelodi.RData")
  mSetObj <- .get.mSet(mSetObj);
  endpoint <- "/overlap/"
  params <- list(
    x = exposure,
    y = outcome
  )
  # print(params)

  lit_df <- query_melodipresto(route = endpoint, params = params, mode = "raw", method = "POST")

  if(is.null(lit_df)){
    return(0);
  }
  hit.num <- nrow(lit_df);
  if (hit.num == 0) {
    current.msg <<- "No hits found in the literature evidence database.";
    print(current.msg);
    return(0);
  } else{
    # Q1 term    set_x
    # Q1 subject    subject_name_x
    # Q1 predicate   predicate_x
    # Q1 object    object_name_x **********
    # Q1 pval       pval_x
    # Q1 pmid       pmids_x
    # Q2 subject    subject_name_y **********
    # Q2 predicate    predicate_y
    # Q2 object    object_name_y
    # Q2 pval    pval_y
    # Q2 pmid    pmids_y
    # Q2 term    set_y
    #An overlap is taken to be cases where the object of a triple from the set of ‘x’ queries overlaps with a subject from the set of ‘y’ queries    
    res <- as.data.frame(lit_df[ , c("set_x","subject_name_x", "predicate_x", "pval_x","pmids_x" , "object_name_x", "predicate_y","object_name_y","pval_y","pmids_y","set_y")]);
    res$pval_x <- signif(res$pval_x, digits = 5);
    res$pval_y <- signif(res$pval_y, digits = 5);

    colnames(res) <- c("Exposure","Exposure_Subject","Exposure_Predicate", "Exposure_Pval","Exposure_PMIDs",  "Overlap", "Outcome_Predicate", "Outcome_Object", "Outcome_Pval", "Outcome_PMIDs","Outcome");
    fast.write.csv(res, file="mr_lit_evidence.csv", row.names=FALSE);
    res <- res[order(res$Outcome_Pval),];
    mSetObj$dataSet$mr2lit <- res; # for table display
    # 4 types of edges
    #exposure -> s1 subject
    edge1 <- data.frame(Name1=(res$Exposure), ID1=(paste(res$Exposure,"exposure", sep="_")), Name2=res$Exposure_Subject, ID2=paste(res$Exposure_Subject, "e_subject", sep="_"), Predicate=rep("", nrow(res)),  pmid=res$Exposure_PMIDs, stringsAsFactors = FALSE);
    exp.ids <<- edge1[,"ID1"];
    expsbj.ids <<- edge1[,"ID2"];
    #s2 object -> outcome
    edge2 <- data.frame(Name1=res$Outcome_Object, ID1=paste(res$Outcome_Object, "o_object", sep="_"), Name2=res$Outcome, ID2=paste(res$Outcome,"outcome",sep="_"), Predicate=rep("", nrow(res)),pmid=res$Outcome_PMIDs, stringsAsFactors = FALSE);
    outobj.ids <<- edge2[,"ID1"]
    out.ids <<- edge2[,"ID2"];
    #s1 subject - s1 predicate -> s1 object
    edge3 <- data.frame(Name1=res$Exposure_Subject, ID1=paste(res$Exposure_Subject,"e_subject", sep="_"), Name2=res$Overlap, ID2=paste(res$Overlap,"overlap",sep="_"), Predicate=res$Exposure_Predicate, pmid=res$Exposure_PMIDs, stringsAsFactors = FALSE);
    expsbj.ids <<- edge3[,"ID1"];
    overlap.ids <<- edge3[,"ID2"];
    # s2 subject - s2 predicate -> s2 object
    edge4 <- data.frame(Name1=res$Overlap, ID1=paste(res$Overlap, "overlap", sep="_"), Name2=res$Outcome_Object, ID2=paste(res$Outcome_Object, "o_object", sep="_"), Predicate=res$Outcome_Predicate,pmid=res$Outcome_PMIDs, stringsAsFactors = FALSE);
    outobj.ids <<- edge4[,"ID2"]
    edges.all <- list(mir.resu, edge1, edge2, edge3, edge4);
    #edges.all <- list(mir.resu, edge3, edge4);
    mir.resu <- do.call("rbind", edges.all);
   

my.edges <- as.data.frame(mir.resu[, c(1,3,5,6)])
colnames(my.edges)[1:4] <- c("from", "to", "predicate", "pmid")

# Create the graph with edge attributes
mir.graph <- simplify(
  graph_from_data_frame(
    my.edges,
    directed = TRUE,
    vertices = NULL
  ),
  edge.attr.comb = "first"  # keep first value for duplicated edges
)

from = unique(res$Exposure)
to= unique(res$Outcome)
paths <- get.all.shortest.paths(mir.graph, from, to)$res;

if(length(paths) == 0){
return(0)
  return (paste("No connection between the two nodes!"));
}


path_info <- lapply(paths, function(path) {
  nodes <- names(path)  # vertex names
  # Get edges along the path as pairs (i to i+1)
  edge_ids <- sapply(seq_along(nodes)[-length(nodes)], function(i) {
    get.edge.ids(mir.graph, vp = c(nodes[i], nodes[i+1]), directed = TRUE)
  })
  # Get all pmids from those edges
  pmids <- E(mir.graph)$pmid[edge_ids]
  # Collapse edge list into path string
  path_str <- paste(nodes, collapse = " → ")
  # Collapse pmids (optionally unique or sorted)
  pmid_str <- paste(unique(unlist(strsplit(pmids, " "))), collapse = ", ")
  
  data.frame(
    path = path_str,
    pmids = pmid_str,
    stringsAsFactors = FALSE
  )
})

# Combine all into one data frame
path_df <- do.call(rbind, path_info)


mSetObj$dataSet$path <- path_df

    .set.mSet(mSetObj);
    if(.on.public.web){
      return(1);
    }else{
      return(current.msg);
    }
  }
}


.init.multilist <- function(){
  mSetObj <- .get.mSet(mSetObj);
  anal.type <<- "multilist"
  net.info <<- list();
  mir.mappedu <<- matrix();
  mir.resu <<- data.frame();
  m2g.resu <<- data.frame();
  snp2met <<- data.frame();
  snp2gene <<- data.frame();
  gene2snp <<- data.frame();
  dis2snp <<- data.frame();
  snp2dis <<- data.frame();
  snp2prot <<- data.frame();
  gene2dis <<- data.frame();
  met2snp <<- data.frame();
  met2gene <<- data.frame();
  gene2met <<- data.frame();
  met2dis <<- data.frame();
  gene2dis <<- data.frame();
  drug2met<<-data.frame();
  snp2egene <<- data.frame();
  mr_results <<- data.frame();
  mr_sensitivity <<- data.frame();
  mr_single <<- data.frame();
  protein2protein <<- data.frame();
  mirtargetu <<- vector();
  mirtableu <<- vector();
  seedsu <<- vector();
  mSetObj$dataSet$directionInx <-vector()
  mSetObj$dataSet$regDirection <-vector()
  mSetObj$dataSet$tfTargetType <-vector()
  edgeNumU <<- vector();
  edgeu.res <<- data.frame();
  nodeu.ids <<- vector();
  nodeu.nms <<- vector();
  mir.nmsu <<- vector();
  snp.nmsu <<- vector();
  tf.nms <<- vector();
  gene.nms <<- vector();
  met.nms<<-vector();
  drug.nms<<-vector();
  dis.nms<<-vector();
  prot.nms<<-vector();
  snp.nms<<-vector();
  egene.nms<<-vector();
  .set.mSet(mSetObj);

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



.set.net.names <- function(input.type){
  
    net.info$exp.ids = exp.ids;
    net.info$out.ids = out.ids;
    #net.info$lit.ids = c(overlap.ids, expsbj.ids, outobj.ids)
    net.info$overlap.ids = overlap.ids;
    net.info$expsbj.ids = expsbj.ids;
    net.info$outobj.ids = outobj.ids;
 
 
  return(net.info)
}



CreateGraph <- function(mSetObj=NA, net.type){
  
  net.type<<-net.type; # necessary for table stats
  mSetObj <- .get.mSet(mSetObj);
  query.type <- mSetObj$analSet$type;
  #save.image("CreateGraph.RData")
  if(.on.public.web){
    load_igraph()
  }
  
  res <- mSetObj$dataSet$mir.res;
    my.edges <- res[,c(1,3)];
    my.edges <- as.data.frame(my.edges);
    nd.nms <- c(res[, 1], res[, 3]);
    nd.ids <- c(res[, 2], res[, 4]);
    names(nd.ids) <- nd.nms;
    dups <- duplicated(nd.ids); #note using unique will lose the names attribute
    node.anot <<- nd.ids[!dups];
    colnames(my.edges) = c("from", "to");
 
    my.edges <- as.data.frame(res[, c(1,3,5)]); # name1, name2, and predicate
    mir.graph <-simplify( graph_from_data_frame(my.edges, directed=TRUE, vertices=NULL), edge.attr.comb="first");
   substats <- DecomposeGraph(mir.graph, 2);
 
  if(!is.null(substats)){
    mir.graph <<- mir.graph;
    mir.query <- nrow(mSetObj$dataSet$mir.mapped);
    #mir.query <- nrow(dataSet$mir.orig); #original query
    mir.count <- length(unique(my.edges[,1]));#matched mir
    tgt.count <- length(unique(my.edges[,2]));#matched target
    if(.on.public.web){
      mSetObj <- .get.mSet(mSetObj);
      return(c(mir.query, mir.count, tgt.count, ecount(mir.graph), length(mir.nets), substats));
    }else{
      return(0)
    }
  }else{
    return(.set.mSet(mSetObj));
  }
  
}

DecomposeGraph <- function(gObj, minNodeNum = 2){
  mSetObj <- .get.mSet(mSetObj);
   #save.image("DecomposeGraph.RData")
  comps <-decompose.graph(gObj, min.vertices=minNodeNum);
  
  if(length(comps) == 0){
    current.msg <<- paste("No connected nodes found after this filtering!");
    return(NULL);
  }
  
  # first get stats
  queries <- unique(mSetObj$dataSet$seeds);
  net.stats <- as.data.frame(matrix(0, ncol = 3, nrow = length(comps)));
  mSetObj$dataSet$query.nums <- vector()
  mSetObj$dataSet$type.nums <- vector()
  res.list <- list();
  for(i in 1:length(comps)){
    g <- comps[[i]];
    if(vcount(g) > 0){
      my.stat <- GetNetStatByType(g);
      res.list[[i]] <- list()
      res.list[[i]][["type.nums"]] <- my.stat$node.num;
      res.list[[i]][["query.nums"]] <- my.stat$query.num;
      net.stats[i,] <- c(
        vcount(g),
        ecount(g),
        sum(queries %in% V(g)$name)
      );
    }
  }
  
  # now sort graph based on node size and add names
  ord.inx <- order(net.stats[,1], decreasing=TRUE);
  net.stats <- net.stats[ord.inx,];
  res.list <- res.list[ord.inx];
  mSetObj$dataSet$type.nums <- unlist(lapply(res.list, function(x) x[["type.nums"]]))
  mSetObj$dataSet$query.nums <- unlist(lapply(res.list, function(x) x[["query.nums"]]))

  comps <- comps[ord.inx];
  names(comps) <- rownames(net.stats) <- paste("mgwas", 1:length(comps), sep="");
  net.stats <- cbind(rownames(net.stats), net.stats);
  colnames(net.stats) <- c("Name", "Node", "Edge", "Query");
  
  # note, we report stats for all nets (at least 2 nodes);
  # but only contruct at least min node
  hit.inx <- net.stats$Node >= minNodeNum;
  comps <- comps[hit.inx];
  sub.stats <- NULL;
  json.res <- rep(list(list()), length(comps));
  i <- 0;
  for(nm in names(comps)){
    sub.stats <- c(sub.stats, vcount(comps[[nm]]));
  }
  
  # now save the components
  mir.nets <<- comps;
  net.stats <<- net.stats[,-1];  # remove the first name col
  
  # update the mir.res edge table
  # both side of the edge must present in all.nodes
  all.nodes <- V(gObj)$name;
  res <- mSetObj$dataSet$mir.res;
  hit.inx <- (res[, 1] %in% all.nodes) & (res[, 2] %in% all.nodes);

  mSetObj$dataSet$mir.filtered <- res[hit.inx, ];
  .set.mSet(mSetObj);
  return(sub.stats);
}


GetNetStatByType <- function(g){
   # qs::qsave(g,"g.qs")
   # save.image("GetNetStatByType.RData")
  mSetObj <- .get.mSet(mSetObj);
  nd.queries <- V(g)$name;
  uniq.ins <- unique(rownames(mSetObj$dataSet$mir.mapped));
  sd.queries <- uniq.ins[uniq.ins %in% nd.queries];

  my.stat <- list(
    query.num = length(sd.queries),
    node.num = length(V(g)$name)
  );
  return(my.stat);
}

GetNetNames <- function(){
  rownames(net.stats);
}


GetNetStats <- function(){
  as.matrix(net.stats);
}

GetQueryNum <-function(){
  mSetObj <- .get.mSet(mSetObj); 
  return(mSetObj$dataSet$query.nums)
}

GetTableNames <- function(){
  #save.image("GetTableNames.RData")
  mSetObj <- .get.mSet(mSetObj);
  mSetObj$dataSet$mirtable;
}

GetSeedsColumn <- function(){
  #save.image("GetSeedsColumn.RData")
  mSetObj <- .get.mSet(mSetObj);
  tbls = unique(mSetObj$dataSet$mirtable);
  vec = vector();
  # print(tbls)
  for( i in 1:length(tbls)){
    nms = strsplit(tbls[i], "2")[[1]];
    orignms = nms;
    nms= gsub("lit", "Literature",nms);
    nms= gsub("snp", "SNP",nms)
    nms= gsub("gene_eqtl", "eGene",nms);
    nms= gsub("gene", "Gene",nms)
    nms= gsub("met_study", "Metabolite",nms)
    nms= gsub("met", "Metabolite",nms)
    nms= gsub("dis", "Disease",nms)
    nms= gsub("pos", "Positional Mapping",nms)
    nms= gsub("snp_pos", "Positional Mapping",nms)
    nms= gsub("drug", "Drug",nms);
    nms= gsub("protein", "Protein",nms); # this is for ppi
    nms= gsub("prot", "Protein",nms); # this is for snp2protein
    nms= gsub("mr", "MR",nms);
    
    if(tbls[i] %in% c("protein2protein")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])) + length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])) )
    }else if(tbls[i] %in% c("snp2gene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,9])))
    }else if(tbls[i] %in% c("snp2egene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])))
    }else if(tbls[i] %in% c("snp2prot")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c("gene2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,6])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("snp2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("dis2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c("met2gene")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,5])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])))
    }else if(tbls[i] %in% c("gene2met")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,5])))
    }else if(tbls[i] %in% c("gene2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c("met2dis")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])))
    }else if(tbls[i] %in% c("snp2met")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(tbls[i] %in% c( "met2snp")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,3])))
    }else if(tbls[i] %in% c( "snp2met_study")){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,4])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])))
    }else if(orignms[1] == "dis"){
      vec[i]=paste0(nms[1],":" ,length(unique(mSetObj$dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,2])))
    }else if(tbls[i] %in% c("mr2lit")){
      vec[i]=paste0(nms[1],":" ,length(unique(c(mSetObj$dataSet[tbls[i]][[1]][,1],mSetObj$dataSet[tbls[i]][[1]][,11]))),", ",nms[2],": ",length(unique(mSetObj$dataSet[tbls[i]][[1]][,6])))
    }else{
      vec[i]=paste0(nms[1],":" ,length(unique(dataSet[tbls[i]][[1]][,1])),", ",nms[2],": ",length(unique(dataSet[tbls[i]][[1]][,3])))
    }
  }
  return(vec)
}

PrepareNet <- function(mir.nm, file.nm){
  my.mirnet <- mir.nets[[mir.nm]];
  current.mirnet <<- my.mirnet;
  convertIgraph2JSON(my.mirnet, file.nm);
  if(.on.public.web){
    return(1);
  }else{
    return(paste("Network files are downloaded!"))
  }
}



convertIgraph2JSON <- function(g, filenm){
  #filenm<<-filenm;
  mSetObj <- .get.mSet(mSetObj);
  #qs::qsave(g,"g.qs");

  #save.image("net.RData");
  nms <- V(g)$name;
  nd.ids <- as.character(node.anot[nms]);

  res <- mSetObj$dataSet$mir.res;
  my.nodes <- res[, c(1, 2)];
  
  m <- as.matrix(my.nodes);
  layers = ceiling(match(V(g)$name, m)/nrow(m));
  
  # setup shape (mir square, gene circle)
  shapes <- rep("circle", length(nms));
  
  # get edge data
  edge.mat <- get.edgelist(g);
  edge.pmids <- igraph::edge_attr(g, "Reference");
  edge.p_values <- igraph::edge_attr(g, "EdgeAttr1");
  edge.n_pmids <- igraph::edge_attr(g, "EdgeAttr2");
  edge.type <- igraph::edge_attr(g, "EdgeType");
  edge.predicate <- igraph::edge_attr(g, "Predicate");
  if(!is.null(edge.pmids)){
  edge.sizes <- as.numeric(rescale2NewRange((edge.n_pmids), 0.5, 3));
  }else{
  edge.sizes <- rep("0.5", nrow(edge.mat));
  }

  if(!is.null(edge.pmids)){
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], pmids=edge.pmids, p_values=edge.p_values, 
                      n_pmids=edge.n_pmids, esize=edge.sizes, etype=edge.type);
  }else if(!is.null(edge.predicate)){
    edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], predicate=edge.predicate, esize=edge.sizes);
  }else{
   edge.mat <- cbind(id=1:nrow(edge.mat), source=edge.mat[,1], target=edge.mat[,2], type=rep("arrow", nrow(edge.mat)), esize=edge.sizes);
  }
  
  
  # now get coords
  pos.xy <- PerformLayOut(g, layers, "Default");
  
  node.btw <- as.numeric(betweenness(g));
  node.dgr <- as.numeric(degree(g));
  
  if(anal.type %notin% c("array", "rnaseq", "qpcr")){
    node.exp <- as.character(get.vertex.attribute(g, name="abundance", index = V(g)));
  }else{
    node.exp <- as.numeric(get.vertex.attribute(g, name="abundance", index = V(g)));
  }
  
  if(vcount(g) > 1000){
    minSize = 3;
  }else if(vcount(g) > 300){
    minSize = 4;
  }else{
    minSize = 5;
  }
  node.sizes <- as.numeric(rescale2NewRange((log(node.dgr))^2, minSize, 10));

  if(net.type=="snp2dis" || net.type=="dis2snp"){
    dis.inx <- nms %in% edge.mat[,3];
    met.inx <- NULL
  } else{
    met.inx <- nms %in% edge.mat[,3];
    dis.inx <- NULL
  }

  snp.inx <- nms %in% net.info$snp.nms;
  shapes[snp.inx] <- "square";
  dis.inx <- nms %in% net.info$dis.nms;
  shapes[dis.inx] <- "circle";
  met.inx <- nms %in% net.info$met.nms;
  shapes[met.inx] <- "circle";
  gene.inx <- nms %in% net.info$gene.nms;
  egene.inx <- nms %in% net.info$egene.nms;
  prot.inx <- nms %in% net.info$prot.nms;
  shapes[gene.inx] <- "diamond";
  shapes[egene.inx] <- "diamond";
  shapes[prot.inx] <- "diamond";
  drug.inx <- nms %in% net.info$drug.nms;
  shapes[drug.inx] <- "diamond";
  exp.inx <- nd.ids %in% net.info$exp.ids;
  out.inx <- nd.ids %in% net.info$out.ids;
  overlap.inx <- nd.ids %in% net.info$overlap.ids;
  expsbj.inx <- nd.ids %in% net.info$expsbj.ids;
  outobj.inx <- nd.ids %in% net.info$outobj.ids;
  #lit.inx <- nd.ids %in% net.info$lit.ids;
  exp.final.inx <- exp.inx & !overlap.inx & !expsbj.inx
  out.final.inx <- out.inx & !overlap.inx & !outobj.inx
  

  containsGP <- any(gene.nms %in% prot.nms) ## check if any overlap between gene and protein
  #cat("containsGP = ", containsGP, "\n")

  if(substring(anal.type, 1,3) == "snp"){ #highlight SNP if they are the query
    node.sizes[snp.inx] <- node.sizes[snp.inx] + 1;
  }else{
    node.sizes[snp.inx] <- node.sizes[snp.inx] + 0.4;
  }
  node.sizes[gene.inx] <- node.sizes[gene.inx] + 1;
  node.sizes[egene.inx] <- node.sizes[egene.inx] + 1;
  node.sizes[prot.inx] <- node.sizes[prot.inx] + 1;
  node.sizes[drug.inx] <- node.sizes[drug.inx] + 1;
  # diamond shape is too small, make it slightly bigger
  node.types <- rep("", length(node.dgr));
  
  node.types[snp.inx] <-  paste("SNP", node.types[snp.inx]);
  node.types[met.inx] <-  paste("Metabolite", node.types[met.inx]);
  node.types[gene.inx] <-  paste("Gene", node.types[gene.inx]);
  node.types[egene.inx] <-  paste("eGene", node.types[egene.inx]);
  node.types[prot.inx] <-  paste("Protein", node.types[prot.inx]);
  node.types[dis.inx] <-  paste("Disease", node.types[dis.inx]);
  node.types[drug.inx] <-  paste("Drug", node.types[drug.inx]);
  node.types[exp.final.inx] <-  paste("Exposure", node.types[exp.final.inx]);
  node.types[out.final.inx] <-  paste("Outcome", node.types[out.final.inx]);
  node.types[overlap.inx] <-  paste("Overlap", node.types[overlap.inx]);
  node.types[expsbj.inx] <-  paste("Exposure_Subject", node.types[expsbj.inx]);
  node.types[outobj.inx] <-  paste("Outcome_Object", node.types[outobj.inx]);
  
  n.types <- rep("", length(node.dgr));
  n.types[snp.inx] <- "SNP";
  n.types[met.inx] <- "Metabolite";
  n.types[dis.inx] <- "Disease";
  n.types[gene.inx] <- "Gene";
  n.types[egene.inx] <- "eGene";
  n.types[prot.inx] <- "Protein";
  n.types[drug.inx] <- "Drug";
  n.types[exp.final.inx] <- "Exposure";
  n.types[out.final.inx] <- "Outcome";
  n.types[overlap.inx] <- "Overlap";
  n.types[expsbj.inx] <- "Exposure_Subject";
  n.types[outobj.inx] <- "Outcome_Object";
  
  node.types <-  trimws(node.types);
  # this is the same as types in omicsnet
  #node.types <-  gsub(" ", "_", node.types);
  
  node.cols <- rep("#ff4500", length(node.dgr));
  ntype <- unique(n.types)
  color.vec = gg_color_hue(length(ntype))
  for(i in 1:length(ntype)){
    node.cols[which(n.types ==ntype[i])]=color.vec[i]
  }
  
  node.cols[snp.inx] <- "#306EFF"; # dark blue
  # update mir node color
  topo.colsw <- node.cols;
  node.cols[snp.inx] <- "#98F5FF";
  topo.colsb <- node.cols;
  
  freq <- table(node.types)
  
  duplicated.types <- node.types
  for(i in 1:length(unique(node.types))){
    duplicated.types[duplicated.types == names(freq[i])]=order(freq)[i]
  }
  
  duplicated.types <- as.numeric(duplicated.types)
  V(g)$layers = duplicated.types

  V(g)$group = as.numeric(duplicated.types); #concentric circle
  
  
  node.cols[snp.inx] <- "#306EFF"; # dark blue
  # update mir node color
  topo.colsw <- node.cols;
  node.cols[snp.inx] <- "#98F5FF";
  topo.colsb <- node.cols;
  # color based on expression
  bad.inx <- is.na(node.exp) | node.exp==0;
  if(!all(bad.inx)){
    exp.val <- node.exp;
    node.colsb.exp <- getExpColors(node.exp, c("#78ff4d", "#FA8072", "#ebebeb"));
    node.colsw.exp <- getExpColors(node.exp, c("#269b00", "#b30000", "#333333"));
    node.colsb.exp[bad.inx] <- "#d3d3d3";
    node.colsw.exp[bad.inx] <- "#c6c6c6";
  }else{
    node.colsb.exp <- rep("#d3d3d3",length(node.exp));
    node.colsw.exp <- rep("#c6c6c6",length(node.exp));
  }
  
  topo.colsw[snp.inx] <- "#bcbd22";
  topo.colsw[met.inx] <- "#2ca02c";
  topo.colsw[gene.inx] <- "#1f77b4";
  topo.colsw[prot.inx] <- "#e377c2";
  topo.colsw[egene.inx] <- "#d62728"; 
  topo.colsw[dis.inx] <- "#9467bd"; 

  topo.colsw[exp.final.inx] <- "#bcbd22";
  topo.colsw[out.final.inx] <- "#2ca02c";
  topo.colsw[overlap.inx] <- "#e49444"; # 2
  topo.colsw[expsbj.inx] <- "#5778a4";  # 1
  topo.colsw[outobj.inx] <- "#6a9f58";  # 3
  
  topo.colsb <- topo.colsw;
  colVec  <- unique(topo.colsw);

  # if(containsGP){
  #   topo.colsb[gene.inx] <- "#D3D3D3";
  #   topo.colsw[gene.inx] <- "#D3D3D3";
  #   color.vec <- c("#D3D3D3", "#FF8484", "#39FF14","#00f6ff", "#D3D3D3", "#00ffff", "#ffff00", "#ff9900", "#39FF14");
  # }else{
  #   color.vec <- c("#FF8484", "#FF8484", "#39FF14","#00f6ff", "#D3D3D3", "#00ffff", "#ffff00", "#ff9900", "#39FF14");
  # }
  
  seed.nms <- unique(rownames(mSetObj$dataSet$mir.mapped)[!is.na(rownames(mSetObj$dataSet$mir.mapped))])
  seed.inx <- nms %in% seed.nms;
  seed_arr <- rep("notSeed",length(node.dgr));
  seed_arr[seed.inx] <- "seed";
  
  # now create the json object
  nodes <- vector(mode="list");
  for(i in 1:length(node.sizes)){
    nodes[[i]] <- list(
      id=nms[i],
      label=nms[i],
      size=node.sizes[i],
      molType=node.types[i],
      type=shapes[i],
      seedArr =seed_arr[i],
      url=nd.ids[i],
      colorb=topo.colsb[i],
      colorw=topo.colsw[i],
      color=topo.colsw[i],
      x=pos.xy[i,1],
      y=pos.xy[i,2],
      attributes=list(
        expr = node.exp[i],
        expcolb=node.colsb.exp[i],
        expcolw=node.colsw.exp[i],
        degree=node.dgr[i], # actual degree in orginal network
        between=node.btw[i])
    );
  }
  
  current.mirnet <<- g
  # save node table
  nd.tbl <- data.frame(Id=nms, Label=nms, Degree=node.dgr, Betweenness=node.btw);
  fileNm <- paste("node_table_", substring(filenm, 0, nchar(filenm)-5), ".csv", sep="")
  fast.write.csv(nd.tbl, file=fileNm, row.names=FALSE);
  
  # covert to json
  library(RJSONIO);
  edge.color  <- rep("#d3d3d3",nrow(edge.mat));
  # perhaps show edge color based on beta 
  up.inx <- E(g)$direction == "+";
  down.inx <- E(g)$direction == "-";
  edge.color[up.inx] = "#FF0000" #red
  edge.color[down.inx] = "#11679A" #blue
  
  edge.mat  <- cbind(edge.mat, color=edge.color);
  netData <- list(mirnet=net.type, 
                  nodes=nodes, 
                  edges=edge.mat, 
                  nodeColors = colVec, 
                  prot.nms=prot.nms,
                  gene.nms=gene.nms, 
                  snp.nms=snp.nms, 
                  containsGP=containsGP,
                  nodeTypes=ntype);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  
  # also save to GraphML
  write.graph(g, file="mgwas.graphml", format="graphml");
  
  if(!.on.public.web){
    library(httr);
    r <- POST("localhost:8080/miRNet/faces/R_REQUEST?type=network", body = list(organism = data.org, idtype = "entrez", network = toJSON(netData))) 
    #TO-DO: need to check org and idtype
  }
}


PerformLayOut <- function(g, layers, algo, focus=""){
  vc <- vcount(g);
  if(algo == "Default"){
    if(vc > 1000) {
      # pos.xy <- layout.fruchterman.reingold(g, area=30*vc^2);
      pos.xy <- layout.lgl(g);
    }else if(vc < 100){
      pos.xy <- layout.kamada.kawai(g);
    }else{
      pos.xy <- layout.fruchterman.reingold(g, area=40*vc^2);
    }
  }else if(algo == "FrR"){
    pos.xy <- layout.fruchterman.reingold(g, area=34*vc^2);
  }else if(algo == "circle"){
    pos.xy <- layout.circle(g);
  }else if(algo == "random"){
    pos.xy <- layout.random(g);
  }else if(algo == "lgl"){
    pos.xy <- layout.lgl(g);
  }else if(algo == "gopt"){
    pos.xy <- layout.graphopt(g)
  }else if(algo == "circular_tripartite"){
    library(ggforce)
    l <- layout_with_sugiyama(g, layers = V(g)$group*(vc/3) +30)
    layout <- l$layout
    
    radial <- radial_trans(
      r.range = rev(range(layout[,2])),
      a.range = range(layout[,1]),
      offset = 0
    )
    coords <- radial$transform(layout[,2], layout[,1])
    layout[,1] <- coords$x
    layout[,2] <- coords$y
    pos.xy= layout
  }else if(algo == "tripartite"){
    l <- layout_with_sugiyama(g, layers = V(g)$layers*(vc/4))
    pos.xy <- -l$layout[,2:1]
  }else if(algo == "concentric"){
    library(graphlayouts)
    # the fist element in the list for concentric is the central node.
    if(focus==""){
      inx=1;
    }else{
      inx = which(V(g)$name == focus)
    }
    coords <- layout_with_focus(g,inx)
    pos.xy <- coords$xy
  }else if(algo == "backbone"){
    library(graphlayouts)
    if(length(V(g)$name)<2000){
      coords = layout_with_stress(g)
      pos.xy = coords
    }else{
      coords = layout_with_sparse_stress(g,pivots=100)
      pos.xy = coords
    }
    
  }else if(algo == "mds"){
    library(graphlayouts)
    coords = layout_with_pmds(g,length(V(g)$name)/10)
    pos.xy = coords/100
    rownames(pos.xy) = NULL
  }
  pos.xy;
}

GetNetsNameString <- function(){
  paste(rownames(net.stats), collapse="||");
}

UpdateNetworkLayout <- function(algo, filenm, focus){
  # get layers
  mSetObj <- .get.mSet(mSetObj);
  res <- mSetObj$dataSet$mir.res;
  my.nodes <- res[, c(1, 2)];
  
  m <- as.matrix(my.nodes);
  layers = ceiling(match(V(current.mirnet)$name, m)/nrow(m));
  
  pos.xy <- PerformLayOut(current.mirnet, layers, algo, focus);
  nms <- V(current.mirnet)$name;
  nodes <- vector(mode="list");
  for(i in 1:length(nms)){
    nodes[[i]] <- list(
      id=nms[i],
      x=pos.xy[i,1],
      y=pos.xy[i,2]
    );
  }
  # now only save the node pos to json
  library(RJSONIO);
  netData <- list(nodes=nodes);
  sink(filenm);
  cat(toJSON(netData));
  sink();
  return(filenm);
}

GetShortestPaths <- function(from, to){
  
  paths <- get.all.shortest.paths(current.mirnet, from, to)$res;
  if(length(paths) == 0){
    return (paste("No connection between the two nodes!"));
  }
  
  path.vec <- vector(mode="character", length=length(paths));
  for(i in 1:length(paths)){
    path.inx <- paths[[i]];
    path.ids <- V(current.mirnet)$name[path.inx];
    #path.sybls <- V(current.mirnet)$Label[path.inx];
    path.sybls <- path.ids;
    pids <- paste(path.ids, collapse="->");
    psbls <- paste(path.sybls, collapse="->");
    path.vec[i] <- paste(c(pids, psbls), collapse=";")
  }
  
  if(length(path.vec) > 50){
    path.vec <- path.vec[1:50];
  }
  
  all.paths <- paste(path.vec, collapse="||");
  return(all.paths);
}
