#' Calculates error in the raw Sharpe Ratio
#' The error calculation is sourced from : http://edge-fund.com/Lo02.pdf and is defined as sqrt(1+((SR)^2)/2) divided by N
#' @param Ra, Rb ( default zero )
#' No wrapper used, instead the AdjustedSharpeRatio code is re-used to calculate Sharpe in the function itself
#' Possible modification : Pass a flag variable error ( default zero ) into all sharpe Ratio functions and return error instead of Sharpe if error = 1
#' @export

SharpeError<-function(Ra, Rf = 0, ...)
{

R = checkData(Ra)

    if (ncol(R)==1 || is.null(R) || is.vector(R)) {
       calcul = FALSE
        for (i in (1:length(R))) {
     	     if (!is.na(R[i])) {
     	    	calcul = TRUE
	     }
        }		      
        if(!calcul) {
	  result = NA
	}
	else {
	     period = Frequency(R)
             R = na.omit(R)
       	     n = length(R)
       	     Rp = (prod(1+R/100)^(period/length(R))-1)*100
       	     Sigp = sqrt(sum((R-mean(R))^2)/n)*sqrt(period)
       	     SR = (Rp - Rf) / Sigp
             result = sqrt((1+((SR)^2)/2)/n)
}
return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, SharpeError, Rf = Rf, period = period, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Error in raw Sharpe (Error = ",Rf,")", sep="")
        return(result)
    }
}
