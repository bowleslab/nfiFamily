### Plot Expression in violin 
plotViolinExpression <- function(dataExp,nameData,genesData,zeros=TRUE, ylim=FALSE,ylim0=0,ylim1=1){
  
  CellLogcounts = as.matrix(logcounts(dataExp))
  row.names(CellLogcounts) = rowData(dataExp)$Symbol
  
  
  hist(CellLogcounts['Nfix',], freq = FALSE) #Note the zeros.

  CellLogcounts = CellLogcounts[genesData,]

  CellLogcounts = as.data.frame(CellLogcounts)
  
    
  Nfi = list()
  colorF = c("#999999", "#E69F00", "#56B4E9","#FC4E07","#56B4E9")
  count = 1  

  for (i in genesData){
    
      
    Nfi[[i]]  <- as.data.frame( t(CellLogcounts[i,]) )
    row.names(Nfi[[i]] ) = NULL
    Nfi[[i]] $Gene <- i
    names(Nfi[[i]] ) <- c("Expression","Gene")
    Nfi[[i]]$Gene <- as.factor(Nfi[[i]]$Gene)
    if(!zeros)
        Nfi[[i]] <- Nfi[[i]] [Nfi[[i]]$Expression != 0,]
    
    ## Plot violin
    if(ylim)
        p <- ggplot(Nfi[[i]], aes(x=Gene,y=Expression)  ) +  ylim(ylim0,ylim1) +
              geom_violin(trim=FALSE, fill = colorF[count], colour = colorF[count]) +
              theme_minimal() + labs(title="",x="", y = "")
    else
        p <- ggplot(Nfi[[i]], aes(x=Gene,y=Expression)  )  +
              geom_violin(trim=FALSE, fill = colorF[count] , colour = colorF[count]) +
              theme_minimal() + labs(title="",x="", y = "")
      

    
    ggsave(paste("plots/",nameData,i,".png"),width = 4, height = 4)
    

    print(p)
    
    count = 1 + count
  }
}






