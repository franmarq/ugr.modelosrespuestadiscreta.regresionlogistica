hosmerlem.test <- function(y, yhat, g=10, group=F)
{
  colnum<-ncol(y)
  if(group==F)
  {
    cutyhat1 = cut(yhat,breaks =unique(quantile(yhat, probs=seq(0,1, 1/g))), include.lowest=TRUE)
    obs = xtabs(cbind(1 - y[,colnum], y[,colnum]) ~ cutyhat1)
    expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat1)
  }
  else
  {
    y2<-c(rep(seq(0,0,length=nrow(y)),y[,colnum-1]),rep(seq(1,1,length=nrow(y)),y[,colnum]))
    yhat2<-c(rep(yhat,y[,colnum-1]),rep(yhat,y[,colnum]))
    cutyhat1 = cut(yhat2,breaks =unique(quantile(yhat2, probs=seq(0,1, 1/g))), include.lowest=TRUE)
    obs = xtabs(cbind(1 - y2, y2) ~ cutyhat1)
    expect = xtabs(cbind(1 - yhat2, yhat2) ~ cutyhat1)
  }
  chisq.C = sum((obs - expect)^2/expect)
  P.C = 1 - pchisq(chisq.C, g - 2)
  res <- data.frame(c(chisq.C,P.C))
  colnames(res)<- c("Hosmer-Lemeshow Test")
  rownames(res)<- c("X-squared","p.value")
  return(res)  
}