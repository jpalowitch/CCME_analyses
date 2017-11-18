tonmi <- function(x, a) {
  (1 / (1 - x + a) - 1 / (1 + a)) / (1 / a - 1 / (1 + a)) 
}

makePerformancePlot <-  function (fn = NULL, meanMat, sdMat = NULL,
                                  legPos = "topright", xvals, tnmi = FALSE,
                                  tnmi_a = 0.05, bw = FALSE,
                                  legCex = 2, pchs = 1:nrow(meanMat),
                                  lwd = 1, legLwd = 2, yRange = NULL,
                                  cex = 1, new = TRUE, doLegend = TRUE,
                                  xRange, plotFile = TRUE, leg_ncol = 1,
                                  ... ) {
  
  # Calculating plot limits
  
  if (!new) {
    upperLimit <- max(meanMat + sdMat)
    lowerLimit <- min(meanMat - sdMat)
  } else {
    upperLimit <- max(meanMat)
    lowerLimit <- min(meanMat)
  }
  if (plotFile) {
    png(fn, width = 1000, height = 1000)
    
    par(oma= rep(0,4),
        mar=c(12,12,6,4),
        mgp=c(8,3,0),
        mfrow = c(1,1))
  }
  
  if (is.null(yRange)) {
    yRange <- c(lowerLimit, upperLimit)
  }
  
  if (tnmi) {
    yRange <- c(0, 1)
    meanMat <- tonmi(meanMat, tnmi_a)
  }
  
  plot(0.5, 0.5, col="white", pch='.',
       xlim = xRange, ylim = yRange, ...)
  
  for(j in 1:length(plot_names)){
    
    if (new) {
      
      if (bw) {
        Col <- "#000000"
      } else {
        Col <- colPal[j]
      }
      
      meth = plot_names[j]
      points(xvals, meanMat[meth, ], pch = pchs[j], cex = cex, col = Col)
      lines(xvals, meanMat[meth, ], lty = j + 1, lwd = lwd, col = Col)
      if (tnmi) {
        abline(h = tonmi(0.95, tnmi_a), lwd = lwd)
        text(y = tonmi(0.95, tnmi_a), pos = 3, cex = cex.lab,
             x = paramVec[round(length(paramVec) * 0.75)],
             labels = c("oNMI = 0.95"))
      }
      
    } else {
    
      meth = plot_names[j]
      plotCI(x = xvals,
             y = meanMat[meth, ],
             uiw = sdMat[meth, ],         
             pch = pchs[j],
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
    
  }
  
  if (doLegend) {
    if (new) {
      
      if (bw) {
        legCols <- "black"
      } else {
        legCols <- colPal
      }
      
      legend(x = legPos, legend = plot_names, lty = 1 + 1:length(plot_names),
             cex = legCex, lwd = legLwd, pch = pchs, col = legCols, ncol = leg_ncol)
      
    } else {
    
    legend(x = legPos,
           legend = plot_names,
           col = colPal,
           lty = 1,
           lwd = lwd,
           pt.cex = cex,
           pch = 1:length(plot_names),
           cex = legCex, ncol = leg_ncol)
      
    }
  }
  
  if (plotFile)
    dev.off()
  
  return(NULL)
  
}