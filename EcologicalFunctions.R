###################################################
#          ecological functions, graphed          #
#           adapted from B. Bolker, 2008          #
#                                                 #
# 0. print directory, parameter values, and par() #
#         1. piecewise polynomial functions       #
#            2. rational functions                #
#           3. exponential functions              #
#            4. power-law functions               #
#                                                 #
# *note to run 0 before anything                  #
#                                                 #
#                        ~written by isadore nabi #
###################################################

# 0. print directory, parameter values, and par()
	print.dir <- "/Users/christophermoore/Projects/Ecological functions"
	setwd(print.dir)
	a1 <- 1
	a2 <- 2
	s <- 5
	a <- 1
	b <- 1
	c <- -1

	four.panel.par <- par(mfrow = c(2,2) , mar = c(2 , 1 , 1 , 1) , oma = c(2,2,4,2))


# 1. piecewise polynomial functions
	pdf("PiecewisePolynomialFunctions.pdf")
	four.panel.par
	# threshold
	threshold <- function(a1 , a2 , s , x){
		ifelse(x < s , a1 , a2)
		}
	curve(threshold(a1 = a1 , a2 = a2 , s = s , x = x) , from = 0 , to = 10 , n = 10000 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	mtext("Threshold" , side = 3)
	abline(v = s , col = "grey" , lty = 2)
	mtext("s" , side = 1 , at = s , line = 0.25)
	mtext(expression(a[1]) , side = 2 , at = a1 , line = 0.25 , las = 1)
	mtext(expression(a[2]) , side = 4 , at = a2 , line = 0.25 , las = 1)
	legend("bottomright" , legend = expression(f(x)==bgroup("{",atop(a[1]~", if x < s",a[2]~", if x > s"),"")) , bty = "n" , adj = c(0,-.25))
	#hockey stick
	hockeystick <- function(a , s , x){
		ifelse(x < s , a*x , a*s)
		}
	curve(hockeystick(a = a , s = s , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	mtext("Hockey stick" , side = 3)
	abline(v = s , col = "grey" , lty = 2)
	mtext("s" , side = 1 , at = s , line = 0.25)
	mtext(expression(a*s) , side = 4 , at = a*s , line = 0.25 , las = 1)
	legend("bottomright" , legend = expression(f(x)==bgroup("{",atop(a*x~", if x < s",a*s~", if x > s"),"")) , bty = "n" , adj = c(0,-.25))
	#general piecewise linear
	b <- 0.5
	genpiecelin <- function(a , s , b , x){
		ifelse(x < s , a*x , a*s-b*(x-s))
		}
	curve(genpiecelin(a = a , s = s , b = b , x = x) , from = 0 , to = 15 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0 , a*s*1.25))
	mtext("General piecewise linear" , side = 3)
	abline(v = s , col = "grey" , lty = 2)
	mtext("s" , side = 1 , at = s , line = 0.25)
	legend("topright" , legend = expression(f(x)==bgroup("{",atop(a*x~", if x < s",a*s-b*(x-s)~", if x > s"),"")) , bty = "n" , adj = c(0,.2))
	# spline
	x <- c(runif(8 , 0 , 10))
	y <- c(runif(8 , 0 , 10))
	plot(x,y , ylim = c(-10,20) , pch = 16 , xaxt = "n" , yaxt = "n", frame = T , xlab = "" , ylab = "n")
	mtext("Splines" , side = 3)
	lines(spline(x,y))
	legend("topright" , legend = "f(x) is  complicated" , bty = "n" , adj = c(0,.2))
	mtext("Piecewise polynomial functions" , side = 3 , outer = T , line = 1)
	dev.off()

# 2. rational functions
	pdf("RationalFunctions.pdf")
	four.panel.par
	# hyperbolic
	b <- 1
	hyperbolic <- function(a , b , x){
		y <- a/(b + x)
		}
	curve(hyperbolic(a = a , b = b , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	mtext("Hyperbolic" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = b , col = "grey" , lty = 2)
	abline(h = c(a/2 , a/b) , col = "grey" , lty = 2)
	mtext("b" , side = 1 , at = b , line = 0.25)
	mtext(expression(over(a,2*b)) , side = 2 , at = a/2*b , las = 1 , line = 0.25)
	mtext(expression(over(a,b)) , side = 2 , at = a/b , las = 1 , line = 0.25)
	legend("topright" , legend = expression(f(x) == frac(a, b + x)) , bty = "n")
	#michaelis-menten
	michaelismenten <- function(a , b , x){
		y <- (a*x)/(b + x)
		}
	curve(michaelismenten(a = a , b = b , x = x) , from = 0 , to = 20 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0,a) )
	mtext("Michaelis-Menten (Holling type II)" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = b , col = "grey" , lty = 2)
	abline(h = c(a/2 , a) , col = "grey" , lty = 2)
	mtext("b" , side = 1 , at = b , line = 0.25)
	mtext(expression(over(a,2)) , side = 2 , at = a/2 , las = 1 , line = 0.25)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("topright" , legend = expression(f(x) == frac(ax, b + x)) , bty = "n" , adj = c(0,1))
	# holling type iii
	hollingIII <- function(a , b , x){
		y <- (a*x)^2/(b^2 + x^2)
		}
	curve(hollingIII(a = a , b = b , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0,a))
	mtext("Holling type III" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = b , col = "grey" , lty = 2)
	abline(h = c(a/2 , a) , col = "grey" , lty = 2)
	mtext("b" , side = 1 , at = b , line = 0.25)
	mtext(expression(over(a,2)) , side = 2 , at = a/2 , las = 1 , line = 0.25)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("topright" , legend = expression(f(x) == frac(ax^2, b^2 + x^2)) , bty = "n" , adj = c(0,1))
	# holling type iv
	hollingIV <- function(a , b , c , x){
		y <- a*x^2/(b + c*x + x^2)
		}
	curve(hollingIV(a = a , b = b , c = c , x = x) , from = 0 , to = 20 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0 , 1.5*a))
	mtext("Holling type IV" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = (-2*b/c) , col = "grey" , lty = 2)
	abline(h = c(a) , col = "grey" , lty = 2)
	mtext(text = expression(over("-2b",c)) , side = 1 , at = (-2*b/c) , line = 2)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("topright" , legend = expression(f(x) == frac(ax^2, b + cx + x^2)) , bty = "n" , adj = c(0,.25))
	mtext("Rational functions" , side = 3 , outer = T , line = 1)
	dev.off()

# 3. exponential functions
	pdf("ExponentialFunctions.pdf")
	four.panel.par	
	b <- 1
	#negative expoential
	negativeexponential <- function(a , b , x){
		y <- a*exp(-b*x)
		}
	curve(negativeexponential(a = a , b = b , x = x) , from = 0 , to = 5 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	mtext("Negative exponential" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = (1/b) , col = "grey" , lty = 2)
	abline(h = c(a , (a/exp(1))) , col = "grey" , lty = 2)
	mtext(expression(frac(1,b)) , side = 1 , at = (1/b) , line = 2)
	mtext(expression(frac(a,italic(e))) , side = 2 , at = (a/exp(1)) , las = 1 , line = 0.25)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("right" , legend = expression(f(x) == a*italic(e)^-bx) , bty = "n")
	# monomolecular
	monomolecular <- function(a , b , x){
		y <- a*(1-exp(-b*x))
		}
	curve(monomolecular(a = a , b = b , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0,a) )
	mtext("Monomolecular" , side = 3)
	abline(v = 0 , col = "grey")
	abline(h = a , col = "grey" , lty = 2)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("right" , legend = expression(f(x) == a*(1-italic(e)^-bx)) , bty = "n" , adj = c(0,1))
	# ricker
	ricker <- function(a , b , x){
		y <- a*x*exp(-b*x)
		}
	curve(ricker(a = a , b = b , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 )
	mtext("Ricker" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = 1/b , col = "grey" , lty = 2)
	abline(h = a/b/exp(1) , col = "grey" , lty = 2)
	mtext(expression(frac(1,b)) , side = 1 , at = 1/b , line = 2)
	mtext(expression(over(a,b)~italic(e)^-1) , side = 2 , at = a/b/exp(1) , las = 1 , line = 0.25)
	legend("right" , legend = expression(f(x) == a*x*italic(e)^-bx) , bty = "n" , adj = c(0,1))
	# logistic
	logisitic <- function(a , b , x){
		y <- exp(a+(b*x))/(1 + exp(a+(b*x)))
		}
	b <- 1
	curve(logisitic(a = a , b = b , x = x) , from = -5 , to = 5 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0,1))
	mtext("Logistic" , side = 3)
	abline(v = (-a/b) , col = "grey" , lty = 2)
	abline(h = c(0.5,1) , col = "grey" , lty = 2)
	mtext(text = expression(over("-a",b)) , side = 1 , at = (-a/b) , line = 2)
	mtext(expression(frac(1,2)) , side = 2 , at = 0.5 , las = 1 , line = 0.25)
	mtext(1 , side = 2 , at = 1 , line = 0.25 , las = 1)
	legend("bottomright" , legend = expression(f(x) == frac(italic(e)^"a+bx",1+italic(e)^"a+bx")) , bty = "n" , adj = c(0,.25))
	mtext("Exponential functions" , side = 3 , outer = T , line = 1)
	dev.off()


# 4. power-law functions
	setwd(print.dir)
	pdf("Power-LawFunctions.pdf")
	four.panel.par
	# power-law
	powerlaw <- function(a , b , x){
		y <- a*x^b
		}
	b <- -2
	curve(powerlaw(a = a , b = b , x = x) , from = 0 , to = 5 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , lty = 3 , ylim = c(0,5))
	b <- 0.5
	curve(powerlaw(a = a , b = b , x = x) , from = 0 , to = 5 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , add = T , lty = 2)
	b <- 2
	curve(powerlaw(a = a , b = b , x = x) , from = 0 , to = 5 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , add = T)
	mtext("Power laws" , side = 3)
	abline(v = 0 , col = "grey")
	abline(v = 1 , col = "grey" , lty = 2)
	abline(h = a , col = "grey" , lty = 2)
	mtext(1 , side = 1 , at = 1 , line = 0.25)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("topright" , legend = c("b<0","0<b<1","b>1") , bty = "n" , lty = c(3,2,1))
	legend("right" , legend = expression(f(x) == a*x^b) , bty = "n" , adj = c(0.25,0))
	# von bertalanffy
	vonbertalanffy <- function(a , k , d , x){
		y <- a*(1-exp(-k*(a-d)*x))^(1/(1-d))
		}
	a <-1
	d <- 2/3
	k <- .5
	curve(vonbertalanffy(a = a , k = k , d = d , x = x) , from = 0 , to = 30 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , ylim = c(0,a))
	mtext("Von Bertalanffy" , side = 3)
	abline(v = 0 , col = "grey")
	abline(h = a , col = "grey" , lty = 2)
	mtext("a" , side = 2 , at = a , las = 1 , line = 0.25)
	legend("bottomright" , legend = expression(f(x) == a(1-italic(e)^-k(a-d))^frac(1,1-d)) , bty = "n" , adj = c(0,0))
	# shepherd, hassell
	shepherd <- function(a , b , c , x){
		y <- (a*x) / (b+x^c)
		}
	hassel <- function(a , b , c , x){
		y <- (a*x) / (b+x)^c
		}
	c <- 1.5
	curve(shepherd(a = a , b = b , c = c , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	curve(hassel(a = a , b = b , c = c , x = x) , from = 0 , to = 10 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2 , add = T , lty = 2)
	mtext("Shepherd, Hassell" , side = 3)
	abline(v = 0 , col = "grey")
	legend("bottomright" , legend = c(expression(f(x) == frac(a*x , b + x^c)) , expression(f(x) == frac(a*x , (b + x)^c))) , bty = "n" , y.intersp = 1.5)
	# non-rectangular hyperbola
	nonrectangularhyperbola <- function(a , theta , p , x){
		y <- (1/2*theta)*(a*x + p - ((((a*x)+p)^2)-(4*theta*a*x*p))^0.5 )
		}
	p <- 1
	theta <- 0.5
	curve(nonrectangularhyperbola(a = a , theta = theta , p = p , x = x) , from = 0 , to = 100 , xaxt = "n" , xlab = "" , yaxt = "n" , ylab = "" , lwd = 2)
	mtext("Non-rectangular hyperbola" , side = 3)
	abline(v = 0 , col = "grey")
	legend("bottomright" , legend = expression(f(x) == frac(1,2~theta)~(alpha*x + p[max] - sqrt((alpha*x+p[max])^2-4*theta*alpha*x*p[max]))) , bty = "n" , adj = c(0,-0.1) , cex = 0.85)
	mtext("Power-law functions" , side = 3 , outer = T , line = 1)
	dev.off()
