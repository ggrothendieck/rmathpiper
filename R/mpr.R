
mprInvokeString <- function(what = c("mpr", "colorconsole", "console", "help")) {
	what <- match.arg(what)
	mpr.jar <- system.file("mathpiper.jar", package = "rMathpiper")
	switch(what, 
		mpr = paste("java -jar", mpr.jar),
		colorconsole = paste("java -cp", mpr.jar, 
			"org.mathpiper.ui.gui.consoles.ColorConsole"),
		console = paste("java -cp", mpr.jar,
			"org.mathpiper.ui.text.consoles.Console"),
		help = paste("java -cp", mpr.jar, 
			"org.mathpiper.ui.gui.help.FunctionTreePanel"))
}

mprRun <- function(what = c("mpr", "colorconsole", "console", "help")) {
   what <- match.arg(what)
   cmd <- mprInvokeString(what)
   if (what == "console" && .Platform$OS.type == "windows")
	   shell(paste("start", cmd))
   else system(cmd, wait = FALSE)
}

mprStart <- function() {
	require(rJava)
	mathpiper.jar <- system.file("mathpiper.jar", package = "rMathpiper")
	.jinit(mathpiper.jar)
	# attach( javaImport( "org.mathpiper.interpreters" ) )
	# interpreters <- new( Interpreters )
	interpreters <- .jnew("org.mathpiper.interpreters.Interpreters")
	interpreter <- interpreters$getSynchronousInterpreter()
	assign(".mpr", interpreter, .GlobalEnv)
	# place Mathpiper into R Form (i.e. R output) mode
	mpr0('PrettyPrinterSet("RForm")')
	invisible(interpreter)
}

mpr <- function(x, ...) UseMethod("mpr")

mpr.Sym <- function(x, ...) {
	mpr0(x, ...)
}

mpr.character <- function(x, env = parent.frame(), ...) {
	x <- eval(parse(text = x), env)
	mpr0(x, ...)
}

# lowest level R/mathpiper interface.  Sends character string x to mathpiper.
mpr0 <- function(x, env = parent.frame(), verbose = FALSE, retclass = c("character", "expression", "unquote"), ...) {

	x <- as.character(x)

    retclass <- match.arg(retclass)

	# ???
    # if (retclass == "expression") {
    #	x <- paste("Eval(",x,")")
    # } 
	
    if (!exists(".mpr", .GlobalEnv)) mprStart()

	response <- .mpr$evaluate(x)
	result <- response$getResult()
	attr(result, "msg") <- response$getExceptionMessage()

    class(result) <- "mpr"
    result
}

as.language <- function(x) parse(text=paste(deparse(x)))[[1]]
bodyAsExpression <- function(x) as.expression(as.language(body(x)))
	
as.character.mpr <- function(x, ...) as.character(x[[1]])

if (FALSE) {

mpr.expression <- function(x, ...) {
    x <- deparse(yparse(x), width.cutoff = 200)
    x <- gsub("\"","", x)
    .Class <- "character"
    NextMethod(x, ...)
}

yparse <- function(x) {
    if (!is.expression(x)) return
	old.show.error.messsages <- getOption("show.error.messages")
    options(show.error.messages = FALSE)
	# ynext does all translations, yrewrite special rewriting
    x[[1]] <- yrewrite(ynext(x[[1]]))
    options(show.error.messages = old.show.error.messages)
    x[[1]]
}

ynext <- function(x) {
    if (length(x) == 1) {
	    x <- ysub(x)
#		print(paste("1:", x))
	} else
        for (i in 1:length(x)) {
			if (length(x[[i]]) >= 1) {		
#				print(paste("x[[", i, "]]->", x[[i]]))
#				x[[i]] <- ynext(x[[i]]) 
				# Added yrewrite to make ynext really recursive
				x[[i]] <- yrewrite(ynext(x[[i]]))
#				print(paste("x[[", i, "]]->", x[[i]]))
#				print(paste(length(x), ":", x))
			}
		}
    x
}

ysub <- function(x) 
{
  if (!match(as.character(x), c("-", "+", "/", "^", "*"), nomatch = 0)) 
  {
    if (!typeof(x) == "double") 
    {
                if (match(toString(x), transtab[,"R"], nomatch = 0) >0 ) {
                        x <- trans(toString(x), from="R", to="mpr")
                        if (x == '":="') x <- ":="
                        mode(x) <- "name"
        } else if (typeof(x) == "symbol") 
            {
                try(x <- ynext(eval(x)[[1]]))
            }
    }
  }
  x
}


yrewrite <- function(x) {
    if (length(x) > 1) {
		if (x[[1]] == quote(Integrate)) {
	    	x <- yIntegrate(x)
	    }
		if (x[[1]] == quote(Deriv)) {
	    	x <- yDeriv(x)
	    }
		if (x[[1]] == quote(Limit)) {
	    	x <- yLimit(x)
	    }
		if (x[[1]] == quote(factorial)) {
	    	x <- yFactorial(x)
	    }	    
		if (x[[1]] == quote(sequence)) {
	    	x <- ySequence(x)
	    }	    
		if (x[[1]] == as.name(":=") && length(x) == 3 && 
			length(x[[3]]) > 2 &&
			x[[3]][[1]] == as.name("function")) {
		x <- yAssignFunction(x)
	    }
    }
    x
}

# Used to separatedly parse argument expressions
yUnlist <- function(x) {
	out <- c()
	if (length(x) > 1) {
		out <- paste(out, "UnList({", toString(x), "})", sep="")
	} else
		out <- paste(out, x, sep="")
}

yFactorial <- function(x) {
#	print(paste("factorial:", x))
	paste("Eval(",yUnlist(x[[2]]), ")!", sep="")
}

ySequence <- function(x) {
#	print(paste("sequence:", x))
	paste("Eval(",yUnlist(x[[2]]) ," .. ",yUnlist(x[[3]]) ,")", sep="")
}

yLimit <- function(x) {
	out <- c(); res <- ""
	res <- try(mode(eval(x[[3]])))
	if (res=="numeric") x[[3]] <- eval(x[[3]])
	out <- paste("Apply(", x[[1]], ", {", yUnlist(x[[2]]), ", Eval(",
		 yUnlist(x[[3]]), ")", sep="")
	x <- paste(out, ", ", yUnlist(x[[4]]), "})", sep="")
}

yDeriv <- function(x) {
	# tmp <- yparse(x[2][1])
	out <- c()
	# if just function name specified then add third arg
	if (length(x) == 2) x[[3]] <- "x"
	if (is.name(x[[3]])) {
		x[[3]] <- as.character(x[[3]])
	} else {
		# translate c to List
		if (identical(x[[3]][[1]], as.name("c"))) 
			x[[3]][[1]] <- as.name("List")
		# translate Deriv to D for higher order deriv
		if (identical(x[[3]][[1]], as.name("List"))) 
			x[[1]] <- "D"
		# remove quotes on variables 
		x[[3]] <- gsub('"', '', format(x[[3]]))
	}
	out <- paste("Apply(", x[[1]], ", {", format(x[[3]]), sep="")
	# if only function name specified append (x) to make F(x)
	x <- if (is.name(x[[2]])) 
		paste(out, ", ", x[[2]], "(", x[[3]], ")})", sep="")
	else
		paste(out, ", ", format(x[[2]]), "})", sep="")
}

yIntegrate <- function(x) {
	out <- c()
	if (is.name(x[[2]])) x[[2]] <- paste(x[[2]], "(x)")
	is.x.specified <- length(x) == 3 || length(x) == 5
	out <- if (is.x.specified)
		paste("Apply(", x[[1]], ", {", sep="")
	else
		paste("Apply(", x[[1]], ", {x, ", sep="")
		
	for (i in seq(3, length = length(x) - 2)) {
		if (length(x[[i]]) > 1) {
			out <- paste(out, yUnlist(x[[i]]), sep="")
		} else
			out <- paste(out, x[[i]], sep="")
		out <- paste(out, ", ", sep="")
	}
	out <- paste(out, format(x[[2]]), "})", sep="")
	out
}

	
yAssignFunction <- function(x) {
	paste(x[[2]], 
		"(", 
		paste(names(x[[3]][[2]]), collapse = ","), 
		")", 
		x[[1]], 
		format(body(eval(x[[3]]))), 
		sep = ""
	)
}


mpr.function <- function(x, ...) {
	funname <- deparse(substitute(x))
	a <- paste( "(", paste(names(formals(x)), collapse = ","), ")" )
	b <- format(body(x))
	e <- as.expression(parse(text = b))
	s <- yparse(e)
	x <- paste(funname, a, ":=", format(s), sep = "")
	.Class <- "character"
	NextMethod(x)
}

mpr.formula <- function(x, ...) {
	x <- as.expression(as.language(x[[length(x)]]))
	.Class <- "expression"
	NextMethod(x)
}

mpr.mpr <- function(x, ...) {
	x <- x[[1]]
	stopifnot(is.expression(x))
	.Class <- "expression"
	NextMethod(x)
}

as.Expr.formula <- function(x) as.expression(as.language(x[[length(x)]]))

Eval <- function(x, env = parent.frame(), ...) UseMethod("Eval")

Eval.mpr <- function(x, env = parent.frame(), ...) 
eval(x[[1]], env = env)

as.expression.mpr <- function(x, ...) x[[1]]

} # FALSE
