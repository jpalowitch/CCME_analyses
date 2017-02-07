library(ggplot2)
library(reshape)

plotMat = function(Mat, 
                   nKeep = NULL,
                   confMat = FALSE,
                   Title = "", 
                   Xlab = "", 
                   Ylab = "",
                   Limits = c(min(Mat),max(Mat)),
                   Low = "white",
                   High = "black",
                   XStr = NULL,
                   YStr = NULL,
                   valName = "",
                   annotate = FALSE,
                   textCol = "black",
                   valRound = 2,
                   textSize = 4,
                   nullTruth = FALSE){
    if(!is.null(nKeep)){
        chosenR = sort(sample(nrow(Mat),nKeep,replace = FALSE))
        chosenC = sort(sample(ncol(Mat),nKeep,replace = FALSE))
        Mat = Mat[chosenR,chosenC]
    }
    
    if(is.matrix(Mat)==FALSE)
        Mat = as.matrix(Mat)
    xDim = dim(Mat)[2]
    yDim = dim(Mat)[1]
    
    plotDfMat = t(Mat[yDim:1,])
    if(yDim==1)
        plotDfMat = as.matrix(Mat[yDim:1,])
    plotDf = melt(plotDfMat)
    
    if(annotate){
        plotDf$value = round(plotDf$value,valRound)
    }
    
    
    if(confMat){
        Limits = c(0,1)
        valName = "% In Comm"
    }
    
    if(!is.null(XStr)){
        if(length(XStr)!=xDim){
            print("err: XStr not same length as col dim")
            return(NULL)
        }
        newXStr = XStr[plotDf$X1]
        plotDf$X1 = factor(newXStr,levels = XStr)
    }
    
    if(!is.null(YStr)){
        if(length(YStr)!=yDim){
            print("err: YStr not same length as row dim")
            return(NULL)
        }
        newYStr = YStr[plotDf$X2]
        plotDf$X2 = factor(newYStr,levels = YStr)
    }
    

    
    g = ggplot(plotDf,aes(x = X1, y = X2)) + 
        geom_tile(aes(fill = value, color = value, stat = "identity")) + 
        xlab(Xlab) + 
        ylab(Ylab) + 
        scale_fill_gradient(low = Low,high = High,limits = Limits,name = valName) + 
        scale_color_gradient(low = Low,high = High,limits = Limits,name = valName) + 
        ggtitle(Title)
    
    if(annotate == TRUE)
        g = g + geom_text(aes(label = value),color = textCol, size = textSize)
    
    print(g)
    
}

if(FALSE){
plotMat(matrix(c(1,2,3,4,5,6),2,3),
        "testMat",
        "testX",
        "testY",
        c(1,6),
        Low = "yellow",
        High = "blue",
        c("One","Two","Three"),
        c("One","Two"),
        "Number")
    
    
}