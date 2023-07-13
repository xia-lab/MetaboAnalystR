##################################################
## R script for ExpressAnalyst
## Description: plot violin plot for individual gene across metadata
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

# given a gene id, plot its expression profile as violin plot
PlotSelectedGene <-function(dataName="",imageName="", gene.id, type="notvolcano", format="png", dpi=dpi, singleCol = F){
  
  library(see);
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  dataSet <- readDataset(dataName);
  anal.type <- paramSet$anal.type;
  require(ggplot2)
  imgName <- paste(imageName,"dpi",dpi,".",format,sep="");
  require(lattice);
  if(length(dataSet$rmidx)>0){
    data.norm <- dataSet$data.norm[,-dataSet$rmidx]  
  }else{
    data.norm <- dataSet$data.norm;
  }
  if(anal.type == "onedata"){
    ids <- rownames(dataSet$comp.res);
    inx <- which(ids == gene.id);
    cmpdNm <- analSet$sig.genes.symbols[inx]; 
    if(type== "volcano"){
      cmpdNm <- "";
    }    
    if(length(dataSet$sec.cls)==1){
      if(dataSet$comp.type == "custom"){
        Cairo(file = imgName, width=5, height=5, type=format, bg="white", dpi=dpi,unit="in");
        grp.nms <- dataSet$grp.nms;
        if(dataSet$cont.inx[dataSet$analysisVar] |  any(grepl("(^[0-9]+).*", as.character(dataSet$cls)))){
          grp.nms <- gsub(paste0(dataSet$analysisVar,"_"),"",grp.nms)
        } 
        inx <- dataSet$cls %in% grp.nms;
        cls <- dataSet$cls[inx]
        dat <- data.norm[,inx];
      }else{
        Cairo(file = imgName, width=5, height=5, type=format, bg="white", dpi=dpi,unit="in");
        dat <- data.norm
        meta <- dataSet$meta.info[rownames(dataSet$meta.info) %in% colnames(dat),,drop=F]
        cls <- droplevels(meta[match(rownames(meta),colnames(dat)),dataSet$analysisVar])
      }
      
      col <- unique(GetColorSchema(cls));   
      df.norm <- data.frame(value=dat[gene.id,], name = cls);
      if(dataSet$disc.inx[dataSet$analysisVar]){
        p.norm <- ggplot2::ggplot(df.norm, aes(x = name, y = value, fill = name)) +
          geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + 
          geom_jitter(height = 0, width = 0.05, show.legend = FALSE) +
          theme(legend.position = "none") +  xlab(dataSet$analysisVar) +
          stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE) +
          scale_fill_okabeito() + 
          scale_color_okabeito() + 
          ggtitle(cmpdNm) + 
          theme(axis.title.x = element_blank(), plot.title = element_text(size = 11, hjust=0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
          theme_bw()
      }else{
        df.norm$name <- as.numeric(df.norm$name )
        p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value))+
          geom_point(size=2) + theme_bw()  + geom_smooth(method=lm,se=T)+
          xlab(dataSet$analysisVar) +
          theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")+
          ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())+ theme_bw()
      }
      myplot <- p.norm + theme(axis.title.x = element_blank(), plot.title = element_text(size = 11, hjust=0.5), plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10))
    }else{
      out.fac <- dataSet$sec.cls
      in.fac <- dataSet$fst.cls
      xlab <- colnames(dataSet$meta.info[,1]);
      col <- unique(GetColorSchema(in.fac));
      
      img.num <- length(levels(out.fac));
      row.num <- ceiling(img.num/2)
      
      if(row.num == 6){
        layout <- c(row.num, 1);
        h=320;
        w=160*row.num;
      }else{
        rn <- round(row.num/2);
        layout <- c(rn, 2);
        h=500;
        w=160*rn;
      }
      
      p_all <- list()
      
      for(lv in levels(out.fac)){
        inx <- out.fac == lv;
        df.orig <- data.frame(facA = lv, value = data.norm[gene.id, inx], name = in.fac[inx])
        p_all[[lv]] <- df.orig
      }
      Cairo(file <- imgName, width=5, height=5, type=format, bg="white", dpi=dpi,unit="in");
      
      alldata <- do.call(rbind, p_all)
      alldata$facA <- factor(as.character(alldata$facA), levels=levels(out.fac))
      p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + 
        geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + 
        geom_jitter(height = 0, width = 0.05, show.legend = FALSE) + 
        facet_wrap(~facA, nrow = row.num) + 
        theme(axis.title.x = element_blank(), legend.position = "none", axis.text.x = element_text(angle=90, hjust=1),
              plot.title = element_text(size = 11, hjust=0.5, face = "bold"), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
        scale_fill_okabeito() + 
        scale_color_okabeito() + 
        ggtitle(cmpdNm) + 
        ylab("Expression") +
        theme_bw()
      myplot <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
    }
    
  }else{ # metadata
    mdata.all <- paramSet$mdata.all;
    inmex.meta <- qs::qread("inmex_meta.qs");
    if(inmex.meta$id.type == "entrez"){
      cmpdNm <- inmex.meta$gene.symbls[gene.id];
    }else{
      cmpdNm <- gene.id;
    }
    num <- sum(mdata.all == 1);
    # calculate width based on the dateset number
    if(num == 1){
      Cairo(file = imgName, width=5, height=5, type=format, bg="white", dpi=dpi);
      
      col <- unique(GetColorSchema(as.character(inmex.meta$cls.lbl)));   
      df.norm <- data.frame(value=inmex.meta$plot.data[gene.id,], name = as.character(inmex.meta$cls.lbl))
      p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name))  
      p.norm <- p.norm + geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + geom_jitter(height = 0, width = 0.05, show.legend = FALSE)  + theme_bw()
      p.norm <- p.norm + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.norm <- p.norm + stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
      p.norm <- p.norm + scale_fill_manual(values=col) + 
        scale_color_manual(values=col) +
        ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.norm <- p.norm + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.norm <- p.norm + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      myplot <- p.norm + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }else{
      # calculate layout
      if(singleCol){
        layout <- c(1, num);
        height <- 200*num;
        width <- 280;
        row.num <- num;
      }else{

          h=500;
          if(num>5){
            w=100*num;
          }else{
            w=140*num;
          }
      } 
      width = w*dpi/72
      height = h*dpi/72
      
      Cairo(file = imgName, width=width, height=height, type=format, bg="white", dpi=dpi);
      data.lbl <- as.character(inmex.meta$data.lbl);
      data.lbl <- substr(data.lbl, 0, nchar(data.lbl)-4);
      
      # get counts in each data, same order as a levels
      counts <- table(data.lbl);
      # back to factor 
      data.lbl <- factor(data.lbl);
      
      # get new lbls to cut potential long names, and add sample numbers
      nlbls <- data.lbl;
      levels(nlbls) <- abbreviate(levels(nlbls),9);
      nlbls <- paste(levels(nlbls), "( n=", as.vector(counts), ")");
      # update labels
      data.lbl <- factor(data.lbl, labels=nlbls);
      # some time the transformed plot.data can switch class label, use the original data, need to be similar scale
      p_all <- list();
      
      out.fac <- data.lbl;
      in.fac <- inmex.meta$cls.lbl;
      xlab <- colnames(dataSet$meta.info[,1]);
      
      col <- unique(GetColorSchema(as.character(inmex.meta$cls.lbl)));   
      
      for(lv in levels(out.fac)){
        inx <- out.fac == lv;
        df.orig <- data.frame(facA = lv, value = inmex.meta$plot.data[gene.id, inx], name = in.fac[inx])
        p_all[[lv]] <- df.orig
      }
      
      alldata <- do.call(rbind, p_all)
      alldata$Dataset <- factor(as.character(alldata$facA), levels=levels(out.fac))
      colnames(alldata) <- c("Dataset", "value", "Factor")
      
      p.time <- ggplot2::ggplot(alldata, aes(x=Dataset, y=value, fill=Factor)) + geom_boxplot()  + theme_bw()
      p.time <- p.time + scale_fill_manual(values=col) + 
        theme(axis.text.x = element_text(angle=90, hjust=1))
      p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Expression")
      p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      myplot <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
    }
  }
  print(myplot);
  dev.off();
}

UpdateMultifacPlot <-function(dataName="",imgName, gene.id, boxmeta,format="png", dpi=72){

  require(ggplot2);
  require(see);
  require(lattice);

  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  dataSet <- readDataset(dataName);
  anal.type <- paramSet$anal.type;
  imgName <- paste(imgName,"dpi",dpi,".",format,sep="");
  meta <- dataSet$meta.info[dataSet$meta.info[,boxmeta]!="NA",boxmeta,drop=F];
  cls <- droplevels(meta[,boxmeta]);
  data.norm <- dataSet$data.norm[,colnames(dataSet$data.norm) %in% rownames(meta)];

  if(anal.type == "onedata"){
    ids <- rownames(dataSet$comp.res);
    inx <- which(ids == gene.id);
    cmpdNm <- analSet$sig.genes.symbols[inx]; 

        Cairo(file = imgName,  width=320*dpi/72, height=380*dpi/72, type=format, dpi=dpi, bg="white");
        dat <- data.norm
       
      df.norm <- data.frame(value=dat[gene.id,], name = cls);
      if(dataSet$disc.inx[boxmeta]){
        p.norm <- ggplot2::ggplot(df.norm, aes(x = name, y = value, fill = name)) +
          geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + 
          geom_jitter(height = 0, width = 0.05, show.legend = FALSE) +
          theme_bw()+
          theme(legend.position = "none") +  xlab(boxmeta) +
          stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE) +
          scale_fill_okabeito() + 
          scale_color_okabeito() + 
          ggtitle(cmpdNm) + 
          theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 11, hjust=0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank())
      }else{
        df.norm$name <- as.numeric(as.character(df.norm$name ))
        p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value))+
          geom_point(size=2) + theme_bw()  + geom_smooth(method=lm,se=T)+
          xlab(boxmeta) +
          theme(axis.text.x = element_text(angle=90, hjust=1)) + guides(size="none")+
          ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) +
          theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank())
      }
      myplot <- p.norm + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10))
    
    
  }else{ # metadata
    mdata.all <- paramSet$mdata.all;
    inmex.meta <- qs::qread("inmex_meta.qs");
    if(inmex.meta$id.type == "entrez"){
      cmpdNm <- inmex.meta$gene.symbls[gene.id];
    }else{
      cmpdNm <- gene.id;
    }
    num <- sum(mdata.all == 1);
    # calculate width based on the dateset number
    if(num == 1){
      Cairo(file = imgName, width=280*dpi/72, height=320*dpi/72, type=format, dpi=dpi, bg="white");
      
      col <- unique(GetColorSchema(as.character(inmex.meta$cls.lbl)));   
      df.norm <- data.frame(value=inmex.meta$plot.data[gene.id,], name = as.character(inmex.meta$cls.lbl))
      p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name))  
      p.norm <- p.norm + geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + geom_jitter(height = 0, width = 0.05, show.legend = FALSE)  + theme_bw()
      p.norm <- p.norm + theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none")
      p.norm <- p.norm + stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE)
      p.norm <- p.norm + scale_fill_manual(values=col) + 
        scale_color_manual(values=col) +
        ggtitle(cmpdNm) + theme(axis.text.x = element_text(angle=90, hjust=1))
      p.norm <- p.norm + theme(plot.title = element_text(size = 11, hjust=0.5)) 
      p.norm <- p.norm + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      myplot <- p.norm + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10))
    }
  }
  print(myplot);
  dev.off();
}






