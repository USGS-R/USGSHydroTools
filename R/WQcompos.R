#' Combine multiple samples into one composite sample (loads and concentrations)
#' 
#' function to composite samples weighted by the associated volume
#' the result is a volume-weighted concentration and summation of volumes
#'
#' @param df.samples dataframe with sample results and volumes
#' @param sampleID character variable name for the IDs for compositing samples (multiple samples will have the same ID)
#' @param parms vector Parameters to composite
#' @param volume character variable name for the volume, defaults to "Evolume" 
#' @param bdate character variable name for the beginning of event times for each sample
#' @param edate character variable name for the ending of event times for each sample
#' @param codes a vector of character variable names for the values that should be pasted together into one string when combining samples (lab IDs are common here)
#' @examples
#' flowData <- flowData
#' FIBdata <- FIBdata
#' FIBcomposData <- Hydrovol(dfQ=flowData,Q="Q",time="pdate",
#'        df.dates=FIBdata,bdate="SSdate",edate="SEdate")
#' WQcompos(df.samples=FIBcomposData,sampleID="SampleID",
#'        parms=c("Ecoli","Enterococci"), volume="event.vol",
#'        bdate="SSdate",edate="SEdate",codes="SampleID")
#' @return IDdf dataframe
#' @export

WQcompos <- function(df.samples,sampleID,parms,volume="Evolume",bdate,edate,codes){
  
  # 
  ID <- unique(df.samples[,sampleID])
  
  rows <- numeric()
  for (k in 1:length(ID)) rows[k] <- match(x=ID[k], table=df.samples[,sampleID])[1]
  
  IDdf <- df.samples[rows,]
  numrows <- length(ID)
  
  for (i in 1:numrows){
    subdf <- df.samples[which(df.samples[,sampleID]==ID[i]),]
    numSamples <- dim(subdf)[1]
    IDdf[i,volume] <- sum(subdf[,volume])
    for (j in 1:length(parms)) IDdf[i,parms[j]] <- sum(subdf[,parms[j]]*subdf[,volume])/IDdf[i,volume]
    IDdf[i,volume] <- sum(subdf[,volume])
    IDdf[i,bdate] <- subdf[1,bdate]
    IDdf[i,edate] <- subdf[numSamples,edate]
    for (l in 1:length(codes)) IDdf[i,codes[l]] <- paste(subdf[,codes[l]],collapse="; ")
  }
  return(IDdf)
}
