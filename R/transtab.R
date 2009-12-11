trans <- function(x, ttab=transtab, from, to) {
   idx <- match(x, ttab[,from], nomatch = 0)
   res <- if (idx > 0) ttab[idx,to] else x
   if (tolower(substr(res, 1, 1)) %in% letters) res
   else paste('"', res, '"', sep="")
}

transtab <- matrix( c(
	#R			OM			mpr
	"pi",		"pi",		"Pi",
	"+",		"plus",		"+",
	"-",		"minus",	"-",
	"*",		"times",	"*",
	"/",		"divide",	"/",
	"/",		"rational",	"/",
	"^",		"power",	"^",
	"%%",		"mod",		"Mod",
	"%/%",		"div",		"Div",
	"root",		"root",		"NthRoot",
	"Inf",		"infinity",	"Infinite",
	"NaN",		"undefined","Undefined",
	"sin",		"Sin",		"Sin",
	"cos",		"Cos",		"Cos",
	"tan",		"Tan",		"Tan",
	"asin",		"arcsin",	"ArcSin",
	"acos",		"arccos",	"ArcCos",
	"atan", 	"arctan", 	"ArcTan",
	"asinh", 	"arcsinh", 	"ArcSinh",
	"acosh", 	"arccosh", 	"ArcCosh",
	"atanh", 	"arctanh", 	"ArcTanh",
	"acsc",		"arccsc",	"ArcCsc",
	"acsch",	"arccsch",	"ArcCsch",
	"asec",		"arcsec",	"ArcSec",
	"asech",	"arcsech",	"ArcSech",
	"acot",		"arccot",	"ArcCot",
	"acoth",	"arccoth",	"ArcCoth",
	"exp", 		"exp", 		"Exp",
	"log", 		"ln", 		"Ln",
	"sqrt", 	"sqrt", 	"Sqrt",
	"choose", 	"bin", 		"Bin",
	"gamma", 	"gamma", 	"Gamma",
	"!",		"not",		"Not",
	"==",		"eq",		"=",
	"==",		"equivalent","=",
	">=",		"geq",		">=",
	">", 		"gt",		">",
	"<=", 		"leq",		"<=",
	"<", 		"lt",		"<",
	"!=", 		"neq",		"!=",
	":", 		"seq",		"sequence",
	":", 		"seq",		"..",
	"factorial","factorial","factorial",
	"factorial","factorial","!",
	"limit", 	"lim", 		"Limit",
	"deriv", 	"deriv", 	"Deriv",
	"integrate","integrate","Integrate",
	"?",		"taylor",	"Taylor",
	"list",		"List", 	"List",
	"TRUE",		"true", 	"True",
	"<-",		"?",		":=",
	"Expr",		"?",		"",
	"Exprq", 	"?",		"",
	"expression", 	"?", 		""
), byrow = TRUE, ncol = 3)
colnames(transtab) <- c("R", "OM", "mpr")

# Used for expressions not handled by R

pow <- function(x, y) {
	(x)^(y)
}
