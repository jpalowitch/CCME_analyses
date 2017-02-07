makePerformancePlot <-  function (fn, meanMat, sdMat,
                                  legPos = "topright",
                                  legCex = 2,
                                  lwd = 1,
                                  cex = 1,
                                  xRange,
                                  ... ) {
  
  # Calculating plot limits
  upperLimit <- max(meanMat + sdMat)
  lowerLimit <- min(meanMat - sdMat)
  
  png(fn, width = 1000, height = 1000)
  
  par(oma= rep(0,4),
      mar=c(12,12,6,4),
      mgp=c(8,3,0),
      mfrow = c(1,1))
  
  plot(0.5, 0.5, col="white", pch='.',
       xlim = xRange,
       ylim = c(lowerLimit, upperLimit),
       ...)
  
  for(j in 1:length(plot_names)){
    
    meth = plot_names[j]
    plotCI(x = paramVec,
           y = meanMat[meth, ],
           uiw = sdMat[meth, ],         
           pch=j,
           pt.bg="black",
           cex=cex,
           lty=1,
           lwd = lwd,
           gap=0,
           type="o",
           sfrac=0.005,
           add=TRUE,
           col = colPal[j])
    
  }
  
  legend(x = legPos,
         legend = plot_names,
         col = colPal,
         lty = 1,
         lwd = lwd,
         pt.cex = cex,
         pch = 1:length(plot_names),
         cex = legCex)
  
  dev.off()
  
  return(NULL)
  
}