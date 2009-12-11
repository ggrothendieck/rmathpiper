\name{rMathpiper-package}
\alias{rMathpiper-package}
\docType{package}
\title{ R interface to yacas computer algebra package }
\description{
rMathpiper allows one to use the Mathpiper computer algebra
package (a fork of the java version of yacas)
from within R.  
It can be used for symbolic mathematics,
exact arithmetic, ASCII pretty printing and R to TeX
conversions.  

Users employ the 
\code{Sym} function to produce symbolic objects of \code{S3} 
class \code{"Sym"}.  Internally these objects
are character strings containing statements in the
Mathpiper/yacas language.  The \code{print.Sym} method calls Mathpiper to do
processing and displays
the result.   Alternately one may use the \code{mpr} function to process
a \code{Sym} object or an R expression represented as a character string.

The first time Mathpiper is used it will 
automatically be initiated so the user normally does not have to separately
start Mathpiper.

Manipulating the \code{Sym} objects is done using 
these functions and \code{Sym} methods:
\code{"\%Where\%"},
\code{Conjugate},
\code{determinant},
\code{deriv},
\code{determinant},
\code{Expand},
\code{Factor},
\code{Factorial},
\code{Identity},
\code{Integrate},
\code{Inverse},
\code{InverseTaylor},
\code{Limit},
\code{List},
\code{Ops}, 
\code{Math}, 
\code{N},
\code{Newton},
\code{print.Sym},
\code{Set},
\code{Simplify},
\code{Solve},
\code{Subst},
and \code{Taylor}.

Also 
\code{Clear},
\code{Precision}, 
\code{PrettyForm}, \code{PrettyPrinter}, \code{TeXForm} and \code{Ver} are
available.

The various functions listed above 
correspond to Mathpiper functions of the same or nearly the same
name.  \code{mprRun("help")}, which is part of the direct interface described
in the next section, can be used to access the Mathpiper help to learn
more about these.

Beneath the interface discussed above is
a lower level interface to Mathpiper that consists of \code{mprStart}
and \code{mpr0}.  The former starts Mathpiper and the latter takes a character
string argument which represents a 
Mathpiper statement, sends it to Mathpiper for
processing and returns the result.  \code{mprStart} and \code{mpr0} are not
normally accessed by the user.

Aside from the user level and lower level interfaces discussed above the
\code{mprRun} function is available to
launch Mathpiper directly without further involvement of R.   
Color gui, monochrome gui and text consoles are available.  Issue any of 
these: \code{mprRun()}, \code{mprRun("colorconsole")} or 
\code{mprRun("console")} commands.  Also
Mathpiper help can be launched using \code{mprRun("help")}.  These commands
simply spawn a batch or shell session with the requisite java commands to
launch Mathpiper.  
}
\details{
The following are sources of information on \code{"rMathpiper"}.  Not all of these are yet implemented:
\tabular{ll}{
DESCRIPTION file\tab library(help = rMathpiper)\cr
List of demo files\tab demo(package = "rMathpiper")\cr
Demo file\tab demo("rMathpiper")\cr
Demo\tab demo("PrettyPrinter")\cr
Demo\tab demo("Sym")\cr
List Vignettes\tab vignette(package = "rMathpiper")\cr
Vignette\tab vignette("rMathpiper")\cr
This File\tab package?rMathpiper\cr
Other Help files\tab ?mpr, ?Sym\cr
Mathpiper help\tab mprRun("help")\cr
News\tab RShowDoc("NEWS", package = "rMathpiper")\cr
Acknowledgements\tab RShowDoc("THANKS", package = "rMathpiper")\cr
Wish List\tab RShowDoc("WISHLIST", package = "rMathpiper")\cr
Home page\tab http://rMathpiper.googlecode.com\cr
}
}
\examples{
\dontrun{
x <- Sym("x")
x*x
Integrate(1/x, x)
deriv(x3, x)
acos(Sym("1/2"))
}
}
\keyword{programming}
