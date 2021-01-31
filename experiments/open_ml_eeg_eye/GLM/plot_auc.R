d1 = readRDS("data_18_n5.RDS")
d2 = readRDS("data_30_n5.RDS")
d3 = readRDS("data_50_n5.RDS")

library(projectXcppCode)
library(reshape2)


y=d1[[1]]
y = apply(y, 2, function(x)EMA_cpp(x,2))

g1 = ggplot() +
  geom_line(aes(x=1:nrow(y),y=y[,1+5]),col="deeppink",lwd=0.75,) +
  geom_line(aes(x=1:nrow(y),y=y[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(y),y=y[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(y),y=y[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(y),y=y[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][1,2+5],nrow(y))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][1,3+5],nrow(y))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][1,4+5],nrow(y))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][1,5+5],nrow(y))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][1,6+5],nrow(y))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][2,2+5],nrow(y))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][2,3+5],nrow(y))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][2,4+5],nrow(y))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][2,5+5],nrow(y))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y),y=rep(d1[[4]][2,6+5],nrow(y))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("18% reference dataset, 33% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")

y1=d1[[2]]
y1 = apply(y1, 2, function(x)EMA_cpp(x,2))
g2 = ggplot() +
  geom_line(aes(x=1:nrow(y1),y=y1[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(y1),y=y1[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(y1),y=y1[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(y1),y=y1[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(y1),y=y1[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][1,2+5],nrow(y1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][1,3+5],nrow(y1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][1,4+5],nrow(y1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][1,5+5],nrow(y1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][1,6+5],nrow(y1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][2,2+5],nrow(y1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][2,3+5],nrow(y1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][2,4+5],nrow(y1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][2,5+5],nrow(y1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y1),y=rep(d1[[4]][2,6+5],nrow(y1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("18% reference dataset, 66% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")



y2=d1[[3]]
y2 = apply(y2, 2, function(x)EMA_cpp(x,2))
g3 = ggplot() +
  geom_line(aes(x=1:nrow(y2),y=y2[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(y2),y=y2[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(y2),y=y2[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(y2),y=y2[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(y2),y=y2[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][1,2+5],nrow(y2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][1,3+5],nrow(y2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][1,4+5],nrow(y2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][1,5+5],nrow(y2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][1,6+5],nrow(y2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][2,2+5],nrow(y2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][2,3+5],nrow(y2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][2,4+5],nrow(y2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][2,5+5],nrow(y2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(y2),y=rep(d1[[4]][2,6+5],nrow(y2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("18% reference dataset, 100% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")




z=d2[[1]]
z = apply(z, 2, function(x)EMA_cpp(x,2))

g4 = ggplot() +
  geom_line(aes(x=1:nrow(z),y=z[,1+5]),col="deeppink",lwd=0.75,) +
  geom_line(aes(x=1:nrow(z),y=z[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(z),y=z[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(z),y=z[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(z),y=z[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][1,2+5],nrow(z))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][1,3+5],nrow(z))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][1,4+5],nrow(z))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][1,5+5],nrow(z))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][1,6+5],nrow(z))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][2,2+5],nrow(z))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][2,3+5],nrow(z))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][2,4+5],nrow(z))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][2,5+5],nrow(z))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z),y=rep(d2[[4]][2,6+5],nrow(z))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("30% reference dataset, 33% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")

z1=d2[[2]]
z1 = apply(z1, 2, function(x)EMA_cpp(x,2))
g5 = ggplot() +
  geom_line(aes(x=1:nrow(z1),y=z1[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(z1),y=z1[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(z1),y=z1[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(z1),y=z1[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(z1),y=z1[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][1,2+5],nrow(z1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][1,3+5],nrow(z1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][1,4+5],nrow(z1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][1,5+5],nrow(z1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][1,6+5],nrow(z1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][2,2+5],nrow(z1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][2,3+5],nrow(z1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][2,4+5],nrow(z1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][2,5+5],nrow(z1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z1),y=rep(d2[[4]][2,6+5],nrow(z1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("30% reference dataset, 66% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")



z2=d2[[3]]
z2 = apply(z2, 2, function(x)EMA_cpp(x,2))
g6 = ggplot() +
  geom_line(aes(x=1:nrow(z2),y=z2[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(z2),y=z2[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(z2),y=z2[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(z2),y=z2[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(z2),y=z2[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][1,2+5],nrow(z2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][1,3+5],nrow(z2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][1,4+5],nrow(z2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][1,5+5],nrow(z2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][1,6+5],nrow(z2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][2,2+5],nrow(z2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][2,3+5],nrow(z2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][2,4+5],nrow(z2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][2,5+5],nrow(z2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(z2),y=rep(d2[[4]][2,6+5],nrow(z2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("30% reference dataset, 100% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")




w=d3[[1]]
w = apply(w, 2, function(x)EMA_cpp(x,2))

g7 = ggplot() +
  geom_line(aes(x=1:nrow(w),y=w[,1+5]),col="deeppink",lwd=0.75,) +
  geom_line(aes(x=1:nrow(w),y=w[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(w),y=w[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(w),y=w[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(w),y=w[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][1,2+5],nrow(w))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][1,3+5],nrow(w))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][1,4+5],nrow(w))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][1,5+5],nrow(w))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][1,6+5],nrow(w))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][2,2+5],nrow(w))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][2,3+5],nrow(w))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][2,4+5],nrow(w))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][2,5+5],nrow(w))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w),y=rep(d3[[4]][2,6+5],nrow(w))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("50% reference dataset, 33% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")

w1=d3[[2]]
w1 = apply(w1, 2, function(x)EMA_cpp(x,2))
g8 = ggplot() +
  geom_line(aes(x=1:nrow(w1),y=w1[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(w1),y=w1[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(w1),y=w1[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(w1),y=w1[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(w1),y=w1[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][1,2+5],nrow(w1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][1,3+5],nrow(w1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][1,4+5],nrow(w1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][1,5+5],nrow(w1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][1,6+5],nrow(w1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][2,2+5],nrow(w1))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][2,3+5],nrow(w1))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][2,4+5],nrow(w1))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][2,5+5],nrow(w1))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w1),y=rep(d3[[4]][2,6+5],nrow(w1))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("50% reference dataset, 66% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")



w2=d3[[3]]
w2 = apply(w2, 2, function(x)EMA_cpp(x,2))
g9 = ggplot() +
  geom_line(aes(x=1:nrow(w2),y=w2[,1+5]),col="deeppink",lwd=0.75) +
  geom_line(aes(x=1:nrow(w2),y=w2[,2+5]),col="deeppink4",lwd=0.75) +
  geom_line(aes(x=1:nrow(w2),y=w2[,3+5]),col="firebrick",lwd=0.75) +
  geom_line(aes(x=1:nrow(w2),y=w2[,4+5]),col="chocolate3",lwd=0.75) +
  geom_line(aes(x=1:nrow(w2),y=w2[,5+5]),col="goldenrod1",lwd=0.75) +
  
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][1,2+5],nrow(w2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][1,3+5],nrow(w2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][1,4+5],nrow(w2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][1,5+5],nrow(w2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][1,6+5],nrow(w2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][2,2+5],nrow(w2))),lwd=0.75,col="deeppink",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][2,3+5],nrow(w2))),lwd=0.75,col="deeppink4",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][2,4+5],nrow(w2))),lwd=0.75,col="firebrick",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][2,5+5],nrow(w2))),lwd=0.75,col="chocolate3",linetype="dashed",alpha=0.8) +
  geom_line(aes(x=1:nrow(w2),y=rep(d3[[4]][2,6+5],nrow(w2))),lwd=0.75,col="goldenrod1",linetype="dashed",alpha=0.8) +
  
  ylim(0.5,0.75)+
  
  xlab("Iteration of proposed method") + ylab("Agent's test AUC") + ggtitle("50% reference dataset, 100% per iteration",subtitle = "lower dash = solo, higher dash = global, colour = agent")

jpeg(filename = "GLM_eye_test_AUC.JPG", pointsize =12, quality = 100,width = 1800,height = 1500)
multiplot(g1,g4,g7,g2,g5,g8,g3,g6,g9,cols=3)
dev.off()

