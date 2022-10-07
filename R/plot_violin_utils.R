##################################################
## R script for ExpressAnalyst
## Description: plot violin plot for individual gene across metadata
## Authors: 
## Jeff Xia, jeff.xia@mcgill.ca
## Guangyan Zhou, guangyan.zhou@mail.mcgill.ca
###################################################

PlotSelectedGeneLoading<-function(dataName="", gene.id){
  paramSet <- readSet(paramSet, "paramSet");;
  anal.type <- paramSet$anal.type;
  if(anal.type == "metadata"){
    PlotSelectedGene(dataName, gene.id,"notVolcano", T);
  }else{
    PlotSelectedGene(dataName, gene.id, "notVolcano");
  }
}

# given a gene id, plot its expression profile as violin plot
PlotSelectedGene <-function(dataName="", gene.id, type, singleCol = F){
  paramSet <- readSet(paramSet, "paramSet");
  analSet <- readSet(analSet, "analSet");
  dataSet <- readDataset(dataName);
  
  anal.type <- paramSet$anal.type;
  
  require(ggplot2)
  imgName <- paste("Gene_", gene.id, ".png", sep="");
  require(lattice);
  if(anal.type == "onedata"){
    ids <- rownames(dataSet$comp.res);
    inx <- which(ids == gene.id);
    cmpdNm <- analSet$sig.genes.symbols[inx]; 
    if(type== "volcano"){
      cmpdNm <- "";
    }    
    if(length(dataSet$sec.cls)==1){
      if(dataSet$comp.type == "custom"){
        Cairo(file = imgName, width=280, height=320, type="png", bg="white");
        grp.nms <- dataSet$grp.nms;
        inx <- dataSet$cls %in% grp.nms;
        cls <- dataSet$cls[inx]
        dat <- dataSet$data.norm[,inx];
      }else{
        Cairo(file = imgName, width=280, height=320, type="png", bg="white");
        cls <- dataSet$cls
        dat <- dataSet$data.norm
      }
      
      col <- unique(GetColorSchema(cls));   
      df.norm <- data.frame(value=dat[gene.id,], name = cls);
      p.norm <- ggplot2::ggplot(df.norm, aes(x = name, y = value, fill = name)) +
        geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + 
        geom_jitter(height = 0, width = 0.05, show.legend = FALSE) +
        theme(axis.title.x = element_blank(), axis.title.y = element_blank(), legend.position = "none") +
        stat_summary(fun=mean, colour="yellow", geom="point", shape=18, size=3, show.legend = FALSE) +
        scale_fill_manual(values = col) + 
        scale_color_manual(values = col) +
        ggtitle(cmpdNm) + 
        theme(axis.text.x = element_text(angle=90, hjust=1), plot.title = element_text(size = 11, hjust=0.5), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
        theme_bw()
      myplot <- p.norm + theme(plot.margin = margin(t=0.35, r=0.25, b=0.15, l=0.5, "cm"), axis.text = element_text(size=10))
    }else{
      
      out.fac <- dataSet$sec.cls
      in.fac <- dataSet$fst.cls
      xlab <- colnames(dataSet$meta[,1]);
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
        df.orig <- data.frame(facA = lv, value = dataSet$data.norm[gene.id, inx], name = in.fac[inx])
        p_all[[lv]] <- df.orig
      }
      Cairo(file <- imgName, dpi=72, width=320, height=320, type="png", bg="white");
      
      alldata <- do.call(rbind, p_all)
      alldata$facA <- factor(as.character(alldata$facA), levels=levels(out.fac))
      p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + 
        geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + 
        geom_jitter(height = 0, width = 0.05, show.legend = FALSE) + 
        facet_wrap(~facA, nrow = row.num) + 
        theme(axis.title.x = element_blank(), legend.position = "none", axis.text.x = element_text(angle=90, hjust=1),
              plot.title = element_text(size = 11, hjust=0.5, face = "bold"), panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
        scale_fill_manual(values = col) + 
        scale_color_manual(values = col) + 
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
      Cairo(file = imgName, width=280, height=320, type="png", bg="white");
      
      col <- unique(GetColorSchema(as.character(inmex.meta$cls.lbl)));   
      df.norm <- data.frame(value=inmex.meta$plot.data[gene.id,], name = as.character(inmex.meta$cls.lbl))
      p.norm <- ggplot2::ggplot(df.norm, aes(x=name, y=value, fill=name))  
      p.norm <- p.norm + + geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + geom_jitter(height = 0, width = 0.05, show.legend = FALSE)  + theme_bw()
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
        if(num < 6){
          layout <- c(num, 1);
          height=320;
          width=160*num;
        }else{
          rn <- round(num/2);
          layout <- c(rn, 2);
          height=500;
          width=160*rn;
        }
        
        
        row.num <- ceiling(num/2)
        
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
      } 
      
      Cairo(file = imgName, width=width, height=height, type="png", bg="white");
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
      p_all <- list()
      
      out.fac <- data.lbl;
      in.fac <- inmex.meta$cls.lbl;
      xlab <- colnames(dataSet$meta[,1]);
      
      col <- unique(GetColorSchema(as.character(inmex.meta$cls.lbl)));   
      
      for(lv in levels(out.fac)){
        inx <- out.fac == lv;
        df.orig <- data.frame(facA = lv, value = inmex.meta$plot.data[gene.id, inx], name = in.fac[inx])
        p_all[[lv]] <- df.orig
      }
      
      alldata <- do.call(rbind, p_all)
      alldata$facA <- factor(as.character(alldata$facA), levels=levels(out.fac))
      
      p.time <- ggplot2::ggplot(alldata, aes(x=name, y=value, fill=name)) + geom_violin(trim = FALSE, aes(color = name), show.legend = FALSE) + geom_jitter(height = 0, width = 0.05, show.legend = FALSE) + theme_bw()
      p.time <- p.time + facet_wrap(~facA, nrow = row.num) + theme(axis.title.x = element_blank(), legend.position = "none")
      p.time <- p.time + scale_fill_manual(values=col) + 
        scale_color_manual(values=col) +
        theme(axis.text.x = element_text(angle=90, hjust=1))
      p.time <- p.time + ggtitle(cmpdNm) + theme(plot.title = element_text(size = 11, hjust=0.5, face = "bold")) + ylab("Expression")
      p.time <- p.time + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) # remove gridlines
      myplot <- p.time + theme(plot.margin = margin(t=0.15, r=0.25, b=0.15, l=0.25, "cm"), axis.text = element_text(size=10)) 
    }
  }
  print(myplot);
  dev.off();
}
