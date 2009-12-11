\name{Sym}
\alias{Sym}

%- Also NEED an '\alias' for EACH other topic documented here.

\alias{as.Sym}
\alias{as.Sym.mpr}

\alias{as.character.Sym}
\alias{as.expression.Sym}
\alias{deriv.Sym}
\alias{Integrate}
\alias{OpenMath2R}
\alias{Ops.Sym}
\alias{Math.Sym}
\alias{Ops.mpr.symbol}
\alias{print.Sym}
\alias{determinant.Sym}
\alias{print.mpr}
\alias{Sym}
\alias{trans}
\alias{transtab}
\alias{mpr.symbol.value}
\alias{yDeriv}
\alias{yFactorial}
\alias{yIntegrate}
\alias{yLimit}
\alias{yrewrite}
\alias{yUnlist}

\alias{Simplify}
\alias{Factorial}
\alias{List}
\alias{Ver}
\alias{N}
\alias{Pi}
\alias{Clear}
\alias{Factor}
\alias{Expand}
\alias{Taylor}
\alias{InverseTaylor}
\alias{PrettyForm}
\alias{TeXForm}
\alias{Precision}
\alias{Conjugate}
\alias{PrettyPrinter}
\alias{Solve}
\alias{Newton}
\alias{Set}
\alias{Infinity}
\alias{I}
\alias{Limit}
\alias{Inverse}

\alias{Conjugate.default}
\alias{Factorial.default }
\alias{Inverse.Expr }
\alias{Inverse.default }
\alias{InverseTaylor.default }
\alias{Limit.default}
\alias{List.default }
\alias{N.default }
\alias{Newton.default }
\alias{Precision.default }
\alias{PrettyForm.default }
\alias{PrettyPrinter.default }
\alias{Simplify.default}
\alias{Solve.default }
\alias{Taylor.default }
\alias{TeXForm.default }
\alias{Ver.default}
\alias{Identity.default}
\alias{Identity}
\alias{Subst}
\alias{Subst.default}
\alias{\%Where\%}
\alias{\%Where\%.default}
\alias{Clear.default}
\alias{Expand.default}
\alias{Factor.default}
\alias{Integrate.default}

\title{ Sym }
\description{
  The Symbol interface to mpr.
}
\usage{
Sym(...)
}
\arguments{
  \item{\dots}{ Character strings or objects that can be coerced to
a character strings.}
}
\details{
An object of class  \code{"Sym"} is internally a mpr character string.
An object of class \code{"Expr"} is internally an R expression.
One can combine such objects using the Math and Ops R operators (see
help(Math) and help(Ops) for a list).  Also there are methods for a
number of R generics: 
\code{as.character.Sym},
\code{as.expression.Sym},
\code{determinant.Sym},
\code{deriv.Sym} and
\code{print.Sym}
and mpr-oriented functions:
Clear,
Conjugate, 
Expand, 
Factor, 
Factorial, 
I, 
Identity,
Infinity,
Integrate, 
Inverse,
InverseTaylor, 
Limit, 
List, 
N, 
Newton, 
Pi, 
Precision, 
PrettyForm, 
PrettyPrinter, 
Set, 
Simplify, 
Solve, 
Subst,
Taylor, 
TeXForm,
Ver and "%Where%"
all of which have
the same meaning as the corresponding mpr commands.
Try \code{vignette("Sym")} for many examples.
}
\value{
\code{Sym} returns a \code{"Sym"} object and \code{Expr} returns an
\code{"Expr"} object.
}
\note{Currently the only \code{Expr} methods implemented are
as.character.Expr, deriv.Expr, Math.Expr, Ops.Expr and print.Expr.}
\examples{
\dontrun{
x <- Sym("x")
x*x
Integrate(x*x, x)
Sym("%") %Where% list(x = 10)

acos(Sym("1/2"))

y <- Exprq(x)
y*y
deriv(y*y, y)
Exprq(acos(1/2))
}
}
\keyword{ symbolmath }
