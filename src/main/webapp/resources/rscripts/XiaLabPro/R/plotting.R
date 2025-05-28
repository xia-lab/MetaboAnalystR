PlotVolcanoCustom <- function(mSetObj=NA, imgName, plotLbl, plotTheme,  format="png", dpi=72, width=NA, labelNum=5,plotStyle=0, interactive=F){

    # make this lazy load
    if(!exists("my.plot.volcano.custom")){ # public web on same user dir
      .load.pro.scripts.on.demand("plot_volcano.Rc");    
    }
    return(my.plot.volcano.custom(mSetObj, imgName, plotLbl, plotTheme, format, dpi,  width, labelNum,plotStyle, interactive));
}

