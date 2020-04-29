PrepareGeneList <- function(path, fileNm="datalist1"){
    data <- readRDS(file=paste0(path, fileNm));
    data <- as.data.frame(data$prot.mat);
    data <- cbind(rownames(data), data)
    colnames(data) <- c("#Entrez", "logFC")
    dest.file <- paste0(path, fileNm, ".txt")
    write.table(data, dest.file, append = FALSE, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE);
    return(dest.file);
}
