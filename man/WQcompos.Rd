\name{WQcompos}
\alias{WQcompos}
\title{Combine multiple samples into one composite sample (loads and concentrations)}
\usage{
WQcompos(df.samples, sampleID, parms, volume = "Evolume")
}
\arguments{
  \item{df.samples}{dataframe with sample results and
  volumes}

  \item{sampleID}{string IDs for compositing samples
  (multiple samples will have the same ID)}

  \item{parms}{vector Parameters to composite}

  \item{volume}{string, defaults to "Evolume"}
}
\value{
IDdf dataframe
}
\description{
function to composite samples weighted by the associated
volume the result is a volume-weighted concentration and
summation of volumes
}
\examples{
flowData <- flowData
FIBdata <- FIBdata
FIBcomposData <- Hydrovol(dfQ=flowData,Q="Q",time="pdate",df.dates=FIBdata,bdate="SSdate",edate="SEdate")
WQcompos(df.samples=FIBcomposData,sampleID="SampleID",parms=c("Ecoli","Enterococci"),volume="event.vol")
}

