#'multiCor 
#'compute correlation coefficients for one response variable vs multiple predictor (independent) variables. 
#'The output is a dataframe ordered by highest to lowest correlation
#'
#' @param df is dataframe with response variable and predictor variables
#' @param response is a character string that is the name of the response variable in df
#' @param IVs is a vector of character strings that are the independent variables in df
#' @param method is either "spearman" (nonparametric) or "pearson" (parametric)
#' @return z dataframe with the variable name in column 1 and correlation 
#' coefficient in column 2. The dataframe is ordered from greatest correlation
#' to least correlation.
#' @export
#' @examples
#' data <- dfOptical
#' multiCor(data,"logEColi",names(data)[-1],"spearman")
multiCor <- function(df,response,IVs,method="spearman"){
  z <- as.matrix(cor(df[,response],df[,IVs],use="complete.obs",method=method))
  z <- as.matrix(z[,order(abs(z),decreasing = TRUE)])
  return(z)
}

