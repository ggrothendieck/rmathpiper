\name{mprRun}
\alias{mprRun}
\title{ Run a mpr session directly. }
\description{
This command runs a Mathpiper session independently of R.  Use the
code{mpr} function instead if you wish to communicate between R and Mathpiper.
If such communication is not needed then this function which
simply launches
Mathpiper with Mathpiper's own interface can be used.
}
\usage{
mprRun(what = c("mpr", "colorconsole", "console", "help"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{what}{ What program to run. }
}
\value{
  No return value.
}
\examples{
\dontrun{
mprRun()
}
}
\keyword{ symbolmath }
