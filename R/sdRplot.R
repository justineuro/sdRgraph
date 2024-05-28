#' Create Semantic Differential (SD) Plots
#' 
#' sdRplot is an R function (in the sdRgraph R package) that can be used for creating Semantic Differential (SD) inventory plots.  
#' @param nx  The number of levels in each of the the SD scales in the inventory, e.g., 5
#' @param sl The scale labels given as a list, e.g. c("SA","A","N","A","SA")
#' @param ny The number of descriptor items in the inventory
#' @param x The data matrix with the first column (col1) containing the low-end descriptors of the SD scales, the second column (col2) containing the high-end descriptors of the SD scales, and the third column  and beyond (col3-onwards) containing the mean scale values for each group. 
#' The names of the columns (col 3 and above) of the data matrix (e.g., dimnames(x)[[2]][3:ncol(x)])) are used for labelling the groups in the legend.
#' @return The SD plot of the data summarized in matrix x.
#' @examples
#' low<-c("Serious","Slow","Useless","Tiring","Old","Hard","Long")
#' high<-c("Fun","Fast","Useful","Light","New","Easy","Short")
#' scale<-c("SA","A","N","A","SA")
#' grp1means<-c(4.2,4.6,4.3,4.1,4.5,4.5,4.0)
#' grp2means<-c(3.8,3.9,3.7,4.5,4.4,4.3,4.4)
#' grp3means<-c(4.5,4.7,4.4,4.2,4.6,4.4,3.9)
#' data<-matrix(
#'  cbind(low,high,grp1means,grp2means,grp3means),
#'  nrow=7,ncol=5,byrow=FALSE,
#'  dimnames=list(c("I1","I2","I3","I4","I5","I6","I7"),
#'                c("Low","High","Grp1","Grp2","Grp3"))
#')
#' sdRplot(5,scale,7,data)
#' @export
sdRplot<-function(nx,sl,ny,x){
NSCALE<-nx
NLAB<-ny
NRW<-nrow(x)
NCL<-ncol(x)
NGRP<-ncol(x)-2
GRPNMS<-dimnames(x)[[2]][3:NCL]
yval<-NLAB:1
labvaln<-x[,1]
labvalp<-x[,2]
# plot means of first group (points and line, type="b")
plot(x[,3],yval,axes=F,pch=1,col=1,type="b",
     xlab="Mean SD Scale Value",ylab="Descriptor",
     cex.lab=1.0, # size of xlab, ylab
     xlim=c(0.30,NSCALE+0.95),
     lab=c(NSCALE,NLAB+2,1),mgp=c(3,1,0),xaxs="r",
     mar=c(7,7,1,1)
     )
# add title
title(main="SD Response Profiles",
      sub="",
      cex.sub=0.50
      )
# draw vertical axes
axis(1,tck=1,at=c(1:NSCALE),labels=sl,cex.axis=0.60)
# draw legend with opaque white background
legend(1.25,NLAB-0.5,title="Legend:",GRPNMS,
       title.col="black",
       text.col=1:NGRP,
       pch=1:NGRP,lty=1:NGRP,
       col=1:NGRP,cex=0.60,
       bg="white"
       )
# skip this if there is only 1 group
  if(NGRP>1){
    for(i in 2:NGRP){
      points(x[,i+2],yval,pch=i,col=i)
      lines(x[,i+2],yval,pch=i,col=i,lty=i)
    }
  }
# draw the labels for the tick marks on the axes
  for (i in 1:NLAB)
  {
    text(0.90,yval[i],label=labvaln[i],adj=1,cex=0.70) # cex = size of marks
    text(NSCALE+0.10,yval[i],label=labvalp[i],adj=0,cex=0.70)
  }
}
