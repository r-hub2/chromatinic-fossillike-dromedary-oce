% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adv.R
\name{subset,adv-method}
\alias{subset,adv-method}
\title{Subset an adv Object}
\usage{
\S4method{subset}{adv}(x, subset, ...)
}
\arguments{
\item{x}{an \linkS4class{adv} object.}

\item{subset}{a condition to be applied to the \code{data} portion of \code{x}.
See \dQuote{Details}.}

\item{\dots}{ignored.}
}
\value{
A new \linkS4class{adv} object.
}
\description{
Subset an adv (acoustic Doppler profile) object.  This function is somewhat
analogous to \code{\link[=subset.data.frame]{subset.data.frame()}}, except that subsets can only be
specified in terms of \code{time}.
}
\examples{
library(oce)
data(adv)
plot(adv)
plot(subset(adv, time < mean(range(adv[["time"]]))))

}
\seealso{
Other things related to adv data: 
\code{\link{[[,adv-method}},
\code{\link{[[<-,adv-method}},
\code{\link{adv}},
\code{\link{adv-class}},
\code{\link{advSontekAdrFileTrim}()},
\code{\link{applyMagneticDeclination,adv-method}},
\code{\link{beamName}()},
\code{\link{beamToXyz}()},
\code{\link{enuToOther}()},
\code{\link{enuToOtherAdv}()},
\code{\link{plot,adv-method}},
\code{\link{read.adv}()},
\code{\link{read.adv.nortek}()},
\code{\link{read.adv.sontek.adr}()},
\code{\link{read.adv.sontek.serial}()},
\code{\link{read.adv.sontek.text}()},
\code{\link{rotateAboutZ}()},
\code{\link{summary,adv-method}},
\code{\link{toEnu}()},
\code{\link{toEnuAdv}()},
\code{\link{velocityStatistics}()},
\code{\link{xyzToEnu}()},
\code{\link{xyzToEnuAdv}()}

Other functions that subset oce objects: 
\code{\link{subset,adp-method}},
\code{\link{subset,amsr-method}},
\code{\link{subset,argo-method}},
\code{\link{subset,cm-method}},
\code{\link{subset,coastline-method}},
\code{\link{subset,ctd-method}},
\code{\link{subset,echosounder-method}},
\code{\link{subset,lobo-method}},
\code{\link{subset,met-method}},
\code{\link{subset,oce-method}},
\code{\link{subset,odf-method}},
\code{\link{subset,rsk-method}},
\code{\link{subset,sealevel-method}},
\code{\link{subset,section-method}},
\code{\link{subset,topo-method}},
\code{\link{subset,xbt-method}}
}
\author{
Dan Kelley
}
\concept{functions that subset oce objects}
\concept{things related to adv data}
