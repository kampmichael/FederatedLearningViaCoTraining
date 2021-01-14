library(ggplot2)
library(magrittr)
library(reshape2)
library(grid)

solo = readRDS("RDS/solo_ANN.RDS")
ann = readRDS("RDS/ANN_variable_reference_size_all_reference.RDS")
ann2 = readRDS("RDS/ANN_variable_reference_size_fixed_reference.RDS")
global = readRDS("RDS/global_ANN.RDS")


ann_ = lapply(ann,function(x){
  x$acc = (x$acc %>% TTR::SMA(.,n=4)) %>% max(.,na.rm = T)
  x$auc = (x$auc %>% TTR::SMA(.,n=4))  %>% max(.,na.rm = T)
  x
})

ann_2 = lapply(ann2,function(x){
  x$acc = (x$acc %>% TTR::SMA(.,n=4)) %>% max(.,na.rm = T)
  x$auc = (x$auc %>% TTR::SMA(.,n=4))  %>% max(.,na.rm = T)
  x
})


acc_ = lapply(ann_,function(x)x$acc) %>% unlist()
auc_ = lapply(ann_,function(x)x$auc) %>% unlist()

acc_2 = lapply(ann_2,function(x)x$acc) %>% unlist()
auc_2 = lapply(ann_2,function(x)x$auc) %>% unlist()

benchmark_acc = list(
  solo = rep(mean(solo$acc),length(acc_)),
  global = rep(mean(global$acc),length(acc_)),
  ref = acc_,
  ref_fix = acc_2,
  x = gsub(names(acc_),pattern = "ref_size_",replacement = "") %>% as.numeric()
)

benchmark_auc = list(
  solo = rep(mean(solo$auc),length(acc_)),
  global = rep(mean(global$auc),length(acc_)),
  ref = auc_,
  ref_fix = auc_2,
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
  "reference",
  "ref. 2.25 ratio",
  "Size"
)
colnames(auc) = c(
  "solo",
  "global",
  "reference",
  "ref. 2.25 ratio",
  "Size"
)


# acc2 = melt(acc, "Epoch")
# auc2 = melt(auc,"Epoch")
# 
# acc_all = ggplot(acc2, aes(Epoch,value)) + geom_point(aes(colour = variable),lwd=1.25) +
#   geom_line(aes(x=Epoch,y=value,colour = variable),lwd=0.75) +
#   xlab("Epoch") + ylab("Accuracy") + ggtitle("Accuracy on Unseen data:")
# 
# 
# auc_all = ggplot(auc2, aes(Epoch,value)) + geom_point(aes(colour = variable),lwd=1.25) +
#   geom_line(aes(x=Epoch,y=value,colour = variable),lwd=0.75) +
#   xlab("Epoch") + ylab("Area under Curve") + ggtitle("Area under Curve on Unseen data:")
# 
# 
# projectXcppCode::multiplot(acc_all,auc_all,cols = 2)




imena = names(acc)[1:(ncol(acc)-1)]

risi = function(acc11=acc,auc11=auc){
  # names = c(names,"Size")
  tmp1 = melt(acc11,"Size")
  tmp2 = melt(auc11,"Size")
  one =  ggplot(tmp1, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
      geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
      xlab("Ref. Size") + ylab("Accuracy (test)") + ggtitle("20 hospitals (from 35-65 examples per hosp.):") +
    theme(axis.text.x = element_text(angle = 45)) +
    scale_x_log10("Ref. Size", labels = as.character(acc11$Size), breaks = acc11$Size)
  
  
  
  two = ggplot(tmp2, aes(Size,value)) + geom_point(aes(colour = variable),lwd=1.25) +
      geom_line(aes(x=Size,y=value,colour = variable),lwd=0.75) +
      xlab("Ref. Size") + ylab("AUC (test)") + ggtitle("20 hospitals (from 35-65 examples per hosp.):") +
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
  
  
 list(
   ACC = one,
   AUC = two
 )
}



save.image("~/Desktop/marko_paper/share/for_markdown.RData")


