#' Uses sample function to construct bootstrap error in xts objects
#' The metric is computed for rep times sizefrac*(size) sized samples and the variance of the metric over these tries is returned
#' This is NOT filtered for NA values outside of filtering in the functions we are passing to. This will be fixed.
#' Can be used as a generalized wrapper for metrics that return numeric values from xts
#' @param x the xts object ( note : no benchmark xts implemented yet ) , fun = the function, such as Sharpe Ratio
#' @param sizefrac = the size of each bootstrap sample as a fraction of total size, rep = repetitions, flag - 0,1,2 for var,mean,median resp.
#' @export

xtsboot <- function(x,fun=NULL,sizefrac=0.5,rep=100,flag=0)

{

if (is.null(fun)) return (NA)

x = checkData(x)

vararray<-c()
meanarray<-c()
medianarray<-c()

for (i in 1:ncol(x)) {
a<-c()
for (j in 1:rep) { a<-c(a,fun(sample(x[,i],floor(sizefrac*nrow(x))))) }
vararray<-c(vararray,var(a))
meanarray<-c(meanarray,mean(a))
medianarray<-c(medianarray,median(a))
}

meanarray<-as.matrix(meanarray)
medianarray<-as.matrix(medianarray)
vararray<-as.matrix(vararray)

colnames(vararray)<-paste("Bootstrapped variance in the metric")
colnames(meanarray)<-paste("Bootstrapped mean in the metric")
colnames(medianarray)<-paste("Bootstrapped median in the metric")

if (flag==0) return(t(vararray))
else if (flag==1) return (t(meanarray))
else if (flag==2) return (t(medianarray))
else {stop("Flag value should be between 0 and 2 inclusive")}

}


