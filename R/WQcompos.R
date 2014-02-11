#' Combine multiple samples into one composite sample (loads and concentrations)
#' 
#' function to composite samples weighted by the associated volume
#' the result is a volume-weighted concentration and summation of volumes
#'
#' @param df.samples dataframe with sample results and volumes
#' @param sampleID string IDs for compositing samples (multiple samples will have the same ID)
#' @param parms vector Parameters to composite
#' @param volume string, defaults to "Evolume" 
#' @examples
#' flowData <- flowData
#' FIBdata <- FIBdata
#' FIBcomposData <- Hydrovol(dfQ=flowData,Q="Q",time="pdate",df.dates=FIBdata,bdate="SSdate",edate="SEdate")
#' WQcompos(df.samples=FIBcomposData,sampleID="SampleID",parms=c("Ecoli","Enterococci"),volume="event.vol")
#' @return IDdf dataframe
#' @export
WQcompos <- function(df.samples,sampleID,parms,volume="Evolume"){
  
  # 
  ID <- unique(df.samples[,sampleID])
  
  rows <- numeric()
  for (i in 1:length(ID)) rows[i] <- match(x=ID[i], table=df.samples[,sampleID])[1]
  IDdf <- df.samples[rows,]
  numrows <- length(ID)
  
  for (i in 1:numrows){
    subdf <- subset(df.samples,df.samples[,sampleID]==ID[i])
    IDdf[i,volume] <- sum(subdf[,volume])
#    IDdf <- cbind(subIDdf,volume)
    for (j in 1:length(parms)) IDdf[i,parms[j]] <- sum(subdf[,parms[j]]*subdf[,volume])/IDdf[i,volume]
  }
  return(IDdf)
}

