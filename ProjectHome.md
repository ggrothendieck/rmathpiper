rMathpiper provides an interface that allows R users to access the [Mathpiper](http://mathpiper.googlecode.com) computer algebra system.  (Mathpiper is a fork of the java version of the [yacas](http://yacas.sourceforge.net/homepage.html) computer algebra system whose significant additions are represented by about 1000 commits including the ability to emit R code.)

The rMathpiper package supports a class `"Sym"` that stores symbolic expressions in a `yacas` language string internally.  Printing such expressions passes them through `mpr()` which calls Mathpiper to process them using Mathpiper displaying them in R form.  The Mathpiper jar file is included in rMathpiper (so there is nothing separate to install other than Java itself).  Mathpiper is automatically initialized (i.e. java virtual machine loaded loaded) the first time Mathpiper is invoked in a session so no explicit initialization of Mathpiper is needed:

Sample session from within R:
```
> library(rMathpiper)
Loading required package: rJava
> x <- Sym("x")
> x*x
"x^2"
> Integrate(x^2, x)
"x^3/3"
```


