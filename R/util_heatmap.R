# Function to generate interactive heatmaps to display peak intensity table from
# mummichog/PSEA analysis

psea.heatmap.json <- function(mSetObj=NA, libOpt, libVersion, minLib, fileNm, filtOpt,
                              version="v1"){
  mSetObj <- .get.mSet(mSetObj);
  dataSet <- mSetObj$dataSet;
  data <- t(dataSet$norm)
  sig.ids <- rownames(data);

  # Reuse existing stat results if available (from PerformMumTableStat)
  if (!is.null(mSetObj$analSet$tt) && !is.null(mSetObj$analSet$tt$p.value)) {
    pvals <- mSetObj$analSet$tt$p.value[sig.ids]
    tscores <- mSetObj$analSet$tt$t.score[sig.ids]
    res <- data.frame(p.value = pvals, t.score = tscores, row.names = sig.ids)
    message("Heatmap: reusing existing stat results")
  } else {
    res <- PerformFastUnivTests(mSetObj$dataSet$norm, mSetObj$dataSet$cls);
  }

  if(dataSet$mode == "positive"){
    mSetObj$dataSet$pos_inx = rep(TRUE, nrow(data))
  }else if(dataSet$mode == "negative"){
    mSetObj$dataSet$pos_inx = rep(FALSE, nrow(data))
  }

  is.rt <- mSetObj$paramSet$mumRT;

  if(mSetObj$paramSet$mumRT){
    feat_info <- rownames(data)
    sep <- if(any(grepl("__", feat_info, fixed=TRUE))) "__" else if(any(grepl("@", feat_info, fixed=TRUE))) "@" else "__"
    feat_info_split <- matrix(unlist(strsplit(feat_info, sep, fixed=TRUE)), ncol=2, byrow=T)
    colnames(feat_info_split) <- c("m.z", "r.t")

    duplicates <- duplicated(feat_info_split[,1])
    if(any(duplicates)){
      set.seed(123)
      n_dup <- sum(duplicates)
      feat_info_split[duplicates, 1] <- paste0(feat_info_split[duplicates, 1],
                                                sample(1:999, n_dup, replace = FALSE))
    }

    if(mSetObj$paramSet$mumRT.type == "minutes"){
      feat_info_split[,2] <- as.numeric(feat_info_split[,2]) * 60
    }

    new_feats <- paste(feat_info_split[,1], feat_info_split[,2], sep = "__")
    rownames(data) <- make.unique(new_feats)

    rownames(res) <- l <- mSetObj$dataSet$ref_mzlist <- make.unique(feat_info_split[,1]);
    retention_time <- as.numeric(feat_info_split[,2]);
    names(retention_time) <- mSetObj$dataSet$ref_mzlist;
    mSetObj$dataSet$ret_time <- retention_time;

    if(is.na(mSetObj$dataSet$rt_tol)){
      rt_tol <- max(mSetObj$dataSet$ret_time) * mSetObj$dataSet$rt_frac
      print(paste0("Retention time tolerance is ", rt_tol))
      mSetObj$dataSet$rt_tol <- rt_tol
    }
    mSetObj$dataSet$expr_dic= res[,1];
    names(mSetObj$dataSet$expr_dic) = rownames(res)
  }else{
    l <- sapply(rownames(data), function(x) return(unname(strsplit(x,"/")[[1]][1])))
    mSetObj$dataSet$ref_mzlist <- rownames(res) <- l <- as.numeric(unname(unlist(l)))
    mSetObj$dataSet$expr_dic= res[,1];
    names(mSetObj$dataSet$expr_dic) = rownames(data)
  }
  mum.version <- mSetObj$paramSet$version <- version

  stat.pvals <- unname(as.vector(res[,2]));
  t.stat <- unname(as.vector(res[,1]));
  org <- unname(strsplit(libOpt,"_")[[1]][1])
  cls <- data.frame(dataSet$cls);

  # Heavy computation (scaling, distance, clustering) in subprocess
  hm.result <- rsclient_isolated_exec(
    func_body = function(input_data) {
      data <- input_data$data
      stat.pvals <- input_data$stat.pvals
      t.stat <- input_data$t.stat

      # scale each gene
      data <- t(scale(t(data)));

      # sort by p-value
      rankPval <- order(as.vector(stat.pvals));
      stat.pvals <- stat.pvals[rankPval];
      data <- data[rankPval, , drop=FALSE];
      t.stat <- t.stat[rankPval];

      orig.smpl.nms <- colnames(data);
      orig.gene.nms <- rownames(data);

      if(nrow(data) > 1){
        # Gene clustering
        gene.dist <- dist(data);
        gene.ward.rk <- match(orig.gene.nms, orig.gene.nms[hclust(gene.dist, "ward.D")$order]);
        gene.ave.rk <- match(orig.gene.nms, orig.gene.nms[hclust(gene.dist, "ave")$order]);
        gene.single.rk <- match(orig.gene.nms, orig.gene.nms[hclust(gene.dist, "single")$order]);
        gene.complete.rk <- match(orig.gene.nms, orig.gene.nms[hclust(gene.dist, "complete")$order]);
        rm(gene.dist);

        # Sample clustering
        smpl.dist <- dist(t(data));
        smpl.ward.rk <- match(orig.smpl.nms, orig.smpl.nms[hclust(smpl.dist, "ward.D")$order]);
        smpl.ave.rk <- match(orig.smpl.nms, orig.smpl.nms[hclust(smpl.dist, "ave")$order]);
        smpl.single.rk <- match(orig.smpl.nms, orig.smpl.nms[hclust(smpl.dist, "single")$order]);
        smpl.complete.rk <- match(orig.smpl.nms, orig.smpl.nms[hclust(smpl.dist, "complete")$order]);
        rm(smpl.dist);
      } else {
        gene.ward.rk <- gene.ave.rk <- gene.single.rk <- gene.complete.rk <- matrix(1);
        smpl.ward.rk <- smpl.ave.rk <- smpl.single.rk <- smpl.complete.rk <- 1:ncol(data);
      }

      # transform to 30 breaks
      binned <- apply(data, 1, function(x){ as.numeric(cut(x, breaks=30)) });

      list(
        gene.cluster = list(ward = gene.ward.rk, average = gene.ave.rk,
                            single = gene.single.rk, complete = gene.complete.rk,
                            pval = stat.pvals, stat = t.stat),
        sample.cluster = list(ward = smpl.ward.rk, average = smpl.ave.rk,
                              single = smpl.single.rk, complete = smpl.complete.rk),
        binned = binned,
        orig.gene.nms = orig.gene.nms,
        orig.smpl.nms = orig.smpl.nms
      )
    },
    input_data = list(data = data, stat.pvals = stat.pvals, t.stat = t.stat),
    packages = c("stats"),
    timeout = 120,
    output_type = "qs"
  )

  # Unpack subprocess results
  gene.cluster <- hm.result$gene.cluster
  sample.cluster <- hm.result$sample.cluster
  orig.gene.nms <- hm.result$orig.gene.nms
  orig.smpl.nms <- hm.result$orig.smpl.nms

  # Prepare meta info
  meta <- data.frame(dataSet$cls);
  grps <- "Condition"
  nmeta <- meta.vec <- NULL;
  uniq.num <- 0;
  for (i in 1:ncol(meta)){
    cls <- meta[,i];
    grp.nm <- grps[i];
    meta.vec <- c(meta.vec, as.character(cls))
    ncls <- paste(grp.nm, as.numeric(cls));
    nmeta <- c(nmeta, ncls);
  }

  nmeta <- as.numeric(as.factor(nmeta))+99;
  unik.inx <- !duplicated(nmeta)
  meta_anot <- meta.vec[unik.inx];
  names(meta_anot) <- nmeta[unik.inx];

  nmeta <- matrix(nmeta, ncol=ncol(meta), byrow=F);
  colnames(nmeta) <- grps;

  gene.id = orig.gene.nms; if(length(gene.id) ==1) { gene.id <- matrix(gene.id) };
  json.res <- list(
    data.type = dataSet$type,
    gene.id = gene.id,
    gene.entrez = gene.id,
    gene.name = gene.id,
    gene.cluster = gene.cluster,
    sample.cluster = sample.cluster,
    sample.names = orig.smpl.nms,
    meta = data.frame(nmeta),
    meta.anot = meta_anot,
    data = hm.result$binned,
    org = org
  );

  mSetObj$dataSet$hm_peak_names = gene.id
  mSetObj$dataSet$gene.cluster = gene.cluster

  .set.mSet(mSetObj)
  json.mat <- rjson::toJSON(json.res);
  sink(fileNm);
  cat(json.mat);
  sink();
  return(1);
}

rsclient_isolated_exec <- function(func_body, input_data, packages = character(0),
                                   timeout = 180, output_type = "qs") {
  bridge_tmp <- file.path(tempdir(), "rsclient_bridge")
  if (!dir.exists(bridge_tmp)) dir.create(bridge_tmp, recursive = TRUE)
  uid <- paste0(sample(letters, 6), collapse = "")
  input_path <- file.path(bridge_tmp, paste0(uid, "_in.qs"))
  output_path <- file.path(bridge_tmp, paste0(uid, "_out.qs"))
  ov_qs_save(input_data, input_path, preset = "fast"); Sys.sleep(0.02)
  on.exit({ for (p in c(input_path, output_path)) if (file.exists(p)) unlink(p) }, add = TRUE)
  result <- run_func_via_rsclient(
    func = function(input_path, output_path, func_body, pkgs) {
      tryCatch({
        for (pkg in pkgs) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
        res <- func_body(ov_qs_read(input_path))
        ov_qs_save(res, output_path, preset = "fast"); Sys.sleep(0.02)
        list(success = TRUE)
      }, error = function(e) list(success = FALSE, message = e$message))
    },
    args = list(input_path = input_path, output_path = output_path,
                func_body = func_body, pkgs = packages),
    timeout_sec = timeout)
  if (isTRUE(result$success) && file.exists(output_path)) return(ov_qs_read(output_path))
  msg <- if (!is.null(result$message)) result$message else "RSclient subprocess failed"
  message("[rsclient_isolated_exec] ", msg)
  return(list(success = FALSE, message = msg))
}