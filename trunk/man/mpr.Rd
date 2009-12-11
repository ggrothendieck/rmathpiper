\name{mpr}
\alias{mpr.character}
\alias{mpr.Sym}
\alias{as.character.mpr}
\alias{mpr}
\alias{mpr0}
\alias{mprInvokeString}
\alias{mprStart}
\alias{pow}
\title{ R/Mathpiper interface }
\description{
  Interface to the Mathpiper computer algebra system from R.
}
\usage{
\method{mpr}{character}(x, env = parent.frame(), ...)
\method{mpr}{Sym}(x, ...)
}

\arguments{
  \item{x}{ A character string representing an R expression or a \code{Sym}
object.}
  \item{env}{ An environment. }
  \item{\dots}{Additional arguments. }
}
\details{
  Supply a character string representing an R expression or a Sym object
  and it will be translated to Mathpiper's language, sent to Mathpiper.
\code{as.character.mpr} can be used to convert a \code{mpr} class object to
a character string.
}
\value{
  The return value is a \code{"mpr"} class object.  This is a character 
string representing the resulting 
  R expression with the indicated class.}
\references{ \url{http://sage.ssu.portsmouth.oh.us/mathpiper/}, 
\url{https://mathrider.dev.java.net/}, \url{http://yacas.sourceforge.net} }

\keyword{ symbolmath }

\examples{
\dontrun{
x <- Sym("x")
x*x
Integrate(x*x, x)
}
}

