#' Power function for proportion test
#'
#' Plots a power function
#'
#' @param baseCR the base conversion rate
#' @param targetCR the target conversion rate
#' @param nMax max samples
#'
#' @return None
#'
#' @examples
#' pow.curve.AB(0.1,0.15,10000)
#'
#' @export
#' @import pwr



######################################################
############## Power curve function###################
######################################################
pow.curve.AB <- function(baseCR,targetCR,nMax)
{
  effectSize <- pwr::ES.h(baseCR,targetCR)
  nControl <- seq(100,nMax,length.out = 1000)
  #powVals<-sapply(nControl, function (x) pwr.2p2n.test(h=effectSize,n1=x,n2=x/2,sig.level = 0.05,power=NULL,alternative = "less")$power)
  powVals<-sapply(nControl, function (x) pwr::pwr.2p2n.test(h=effectSize,n1=x,n2=x*2,sig.level = 0.05,power=NULL,alternative = "less")$power)
  plot(nControl, powVals, xlab="nControl", ylab="power",main="Power Curve for AB-test",col="red",type="l")

  pow80<-round(nControl[which(abs(powVals-0.8)==min(abs(powVals-0.8)))])
  cat("80% power is reached with", pow80, "control samples. Effect size =",round(abs(effectSize),3))
  #return(message)
}
