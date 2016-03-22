#' Calculates error in the Adjusted Sharpe Ratio
#' The error calculation is sourced from : http://edge-fund.com/Lo02.pdf and is defined as sqrt(1+((SR)^2)/2) divided by N for the RAW Sharpe
#' For Adjusted Sharpe, we use the error for Kurtosis and Skewness in conjunction with the Sharpe error
#' @param Ra, Rb ( default zero )
#' No wrapper used, instead the AdjustedSharpeRatio code is re-used to calculate Sharpe in the function itself
#' Possible modification : Pass a flag variable error ( default zero ) into all sharpe Ratio functions and return error instead of Sharpe if error = 1
#' @export

AdjustedSharpeError <- function (R, Rf = 0, ...)

{
    R = checkData(R)

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
       	     K = kurtosis(R, method = "moment")
       	     S = skewness(R)
       	     sqerrskew = ((6*n*(n-1))/((n-2)*(n+1)*(n+3)))
             sqerrkurt = 4*sqerrskew*((n^2 - 1)/((n-3)*(n+5)))
             sqerrsh = ((1+((SR)^2)/2)/n)
             varsharpsquare = (4*(sqerrsh)*(SR)^2 + 2*(sqerrsh)^2)
             expsharpsquare = (SR)^2 + sqerrsh
             sqerrsh2 = (varsharpsquare*sqerrskew + sqerrskew*(expsharpsquare^2) + varsharpsquare*(S^2))/36
             varsharpcube = 9*((SR)^4)*sqerrsh + 39*((SR)^2)*(sqerrsh)^2 + 15*(sqerrsh)^3
             expsharpcube = (SR)^3 + 3*SR*sqerrsh
             sqerrsh3 = (sqerrkurt*varsharpcube + sqerrkurt*(expsharpcube)^2 + varsharpcube*(K^2))/576
             sqerrsh4 = varsharpcube/64
             netsqerr = sqerrsh + sqerrsh2 + sqerrsh3 + sqerrsh4
             result = sqrt(netsqerr)  

        }
       return(result)
    }  
    else {
        result = apply(R, MARGIN = 2, AdjustedSharpeError, Rf = Rf, period = period, ...)
        result<-t(result)
        colnames(result) = colnames(R)
        rownames(result) = paste("Adjusted Sharpe Error (Error = ",Rf,")", sep="")
        return(result)
    }
}
