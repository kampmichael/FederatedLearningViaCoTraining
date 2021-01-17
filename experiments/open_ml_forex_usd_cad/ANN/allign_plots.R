library(ggplot2)
library(magrittr)
library(reshape2)
library(grid)

global = readRDS("RDS/global_ANN.RDS")
rf = readRDS("RDS/ANN_method.RDS")



rf_ = lapply(rf,function(x){
  x$acc = (x$acc %>% TTR::SMA(.,n=2)) %>% max(.,na.rm = T)
  x$auc = (x$auc %>% TTR::SMA(.,n=2))  %>% max(.,na.rm = T)
  x
})


solo_acc = lapply(rf,function(x){
  x$acc[1]
}) %>% unlist() %>% mean()

solo_auc = lapply(rf,function(x){
  x$auc[1]
}) %>% unlist() %>% mean()

acc_ = lapply(rf_,function(x)x$acc) %>% unlist()
auc_ = lapply(rf_,function(x)x$auc) %>% unlist()



benchmark_acc = list(
  solo = rep(solo_acc,length(acc_)),
  global = rep(mean(global$acc),length(acc_)),
  ref = acc_,
  x = gsub(names(acc_),pattern = "ref_size_",replacement = "") %>% as.numeric()
)

benchmark_auc = list(
  solo = rep(solo_auc,length(acc_)),
  global = rep(mean(global$auc),length(acc_)),
  ref = auc_,
  x = gsub(names(auc_),pattern = "ref_size_",replacement = "") %>% as.numeric()
)



acc = stringi::stri_list2matrix(benchmark_acc)
acc = apply(acc,2,as.numeric)


auc = stringi::stri_list2matrix(benchmark_auc)
auc = apply(auc,2,as.numeric)


acc = as.data.frame(acc)
auc = as.data.frame(auc)

colnames(acc) = c(
  "solo",
  "global",
  "proposed method",
  "Size"
)
colnames(auc) = c(
  "solo",
  "global",
  "proposed method",
  "Size"
)



risi = function(acc11=acc,auc11=auc){
  # names = c(names,"Size")
  tmp1 = melt(acc11,"Size")
  tmp2 = melt(auc11,"Size")
  one =  ggplot(tmp1, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
    geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
    xlab("Ref. Size") + ylab("Accuracy (test)") + ggtitle("ANN 16 units : 20 exchanges (from 200-400 examples per ex.)") +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_log10("Ref. Size", labels = as.character(acc11$Size), breaks = acc11$Size)
  
  
  
  two = ggplot(tmp2, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
    geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
    xlab("Ref. Size") + ylab("AUC (test)") + ggtitle("ANN 16 units: 20 exchanges (from 200-400 examples per ex.)") +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_log10("Ref. Size", labels = as.character(acc11$Size), breaks = acc11$Size)
  
  
  tmp11 = melt(acc11[,-2],"Size")
  tmp22 = melt(auc11[,-2],"Size")
  three =  ggplot(tmp11, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
    geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
    xlab("Ref. Size") + ylab("Accuracy (test)") + ggtitle("") +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_log10("Ref. Size", labels = as.character(acc11$Size), breaks = acc11$Size)
  
  
  
  four = ggplot(tmp22, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
    geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
    xlab("Ref. Size") + ylab("AUC (test)") + ggtitle("") +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_log10("Ref. Size", labels = as.character(acc11$Size), breaks = acc11$Size)
  
  
  multiplot = function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    
    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)
    
    numPlots = length(plots)
    
    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
    }
    
    if (numPlots==1) {
      print(plots[[1]])
      
    } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
        # Get the i,j matrix positions of the regions that contain this subplot
        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
        
        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
      }
    }
  }
  
  multiplot(
    one,two,three,four,cols=2
  )
}



risi()

jpeg(filename = "../results_png/ANN_forex.JPG", pointsize =12, quality = 100,width = 1200,height = 800)
risi()
dev.off()





