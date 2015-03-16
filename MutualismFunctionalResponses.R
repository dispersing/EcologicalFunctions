#paramters
	alpha <- 1
	delta <-  -2
	beta <- -0.8
	epsilon <- 1 #slope of saturating functions
	zeta <- 2 #difference in slopes for the saturating curves

	min <- 0
	max <- 10
	n <- 101

# functions
	linear <- function(N2 , alpha = 0 , beta = 0  , delta = 0){
		N1 <- alpha*N2 - (beta*N2 + delta)
	}

	saturate <- function(N2 , asymptote , slope){
		N1 <- (asymptote*N2)/(slope + N2)
	}

	hyperbolic <- function(N2 , asymptote , slope){
		N1 <- (asymptote)/(slope + N2)
	}


# figure
	setwd("/Users/christophermoore/Desktop/")
	pdf("MutFuncResp.pdf" , width = 6 , height = 4)
	par(mfrow = c(2,3) , mar = rep(1 , 4) , oma = rep(2 , 4)) # to mimic holland et al. 2002
	# A. NE lineraly increase with constant cost
	curve(linear(N2 = x , alpha = alpha) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out = n),seq(max,min,length.out = n)) , c(linear(N2 = seq(min,max,length.out = n) , alpha = alpha) , linear(N2 = seq(min,max,length.out = n) , delta = delta)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(linear(N2 = x , alpha = alpha) , from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(linear(N2 = x , delta = delta) , from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve(linear(N2 = x , alpha = alpha)-linear(N2 = x , delta = delta) , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)
	legend("topleft" , lwd = c(rep(1,3),NA) , pch = c(rep(NA,3),15) , pt.cex = c(1,1,1,2) , col = c("#0000FF" , "#FF0000" , "#FF00FF" , rgb(255,0,255,50,,255)) , legend = c("benefit (b)" , "cost (c)" , "net effect (ne)" , "b > c") , bty = "n" , seg.len = 1)

	# B. NE lineraly increase
	curve(linear(N2 = x , alpha = alpha) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out = n),seq(max,min,length.out = n)) , c(linear(N2 = seq(min,max,length.out = n) , alpha = alpha) , linear(N2 = seq(max,min,length.out = n) , beta = beta)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(linear(N2 = x , alpha = alpha) , from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(linear(N2 = x , beta = beta) , from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve((linear(N2 = x , alpha = alpha)-linear(N2 = x , beta = beta)) , from = min , to = max , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)

	# C. NE constant
	curve(linear(N2 = x , alpha = alpha) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out = n),seq(max,min,length.out = n)) , c(linear(N2 = seq(min,max,length.out = n) , alpha = alpha) , linear(N2 = seq(max,min,length.out = n) + delta , beta = beta)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(linear(N2 = x , alpha = alpha), from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(linear(N2 = x + delta, beta = beta), from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve(linear(N2 = x , alpha = alpha)-ifelse(linear(N2 = x + delta, beta = beta) <0 , 0 , linear(N2 = x + delta, beta = beta)) , from = min , to = max , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)

	# D. NE saturate
	curve(saturate(N2 = x , asymptote = alpha , slope = epsilon) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out=n),seq(max,min,length.out=n)) , c(saturate(N2 = seq(min,max,length.out = n) , asymptote = alpha, slope = epsilon) , saturate(N2 = seq(max,min,length.out = n) , asymptote = -beta, slope = epsilon)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(saturate(N2 = x , asymptote = alpha, slope = epsilon) , from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(saturate(N2 = x , asymptote = -beta , slope = epsilon) , from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve(saturate(N2 = x , asymptote = alpha , slope = epsilon)-saturate(N2 = x , asymptote = -beta , slope = epsilon) , from = min , to = max , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)

	# E. NE saturate + constant
	curve(saturate(N2 = x , asymptote = 1 , slope = epsilon) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out = n),seq(max,min,length.out = n)) , c(saturate(N2 = seq(min,max,length.out = n) , asymptote = alpha , slope = epsilon) , hyperbolic(N2 = seq(max,min,length.out = n) , asymptote = 1, slope = epsilon)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(saturate(N2 = x, asymptote = 1 , slope = epsilon) , from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(hyperbolic(N2 = x, asymptote = 1 , slope = epsilon) , from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve(saturate(N2 = x, asymptote = 1 , slope = epsilon)-hyperbolic(N2 = x, asymptote = 1 , slope = epsilon) , from = min , to = max , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)

	# F. NE unimodal
	curve(saturate(N2 = x , asymptote = 1 , slope = epsilon) , type = "n" , from = min , to = max , xaxt = "n" , yaxt = "n" , xlab = "" , ylab = "" , lwd = 2 , col = "#0000FF" , yaxs = "i" , xaxs = "i" , frame = F)
	polygon(c(seq(min,max,length.out = n),seq(max,min,length.out = n)) , c(saturate(N2 = seq(min,max,length.out = n) , asymptote = alpha , slope = epsilon) , saturate(N2 = seq(max,min,length.out = n) , asymptote = 1, slope = epsilon*zeta)) , col = rgb(255,0,255,50,,255) , lty = 0)
	curve(saturate(N2 = x, asymptote = 1 , slope = epsilon) , from = min , to = max , add = T ,  lwd = 2 , col = "#0000FF")
	curve(saturate(N2 = x, asymptote = 1 , slope = epsilon*zeta) , from = min , to = max , add = T ,  lwd = 2 , col = "#FF0000")
	curve(saturate(N2 = x, asymptote = 1 , slope = epsilon)-saturate(N2 = x, asymptote = 1 , slope = epsilon*zeta) , from = min , to = max , lwd = 2 , col = "#FF00FF" , add = T)
	abline(v = 0 , h = 0 , col = "#000000" , lwd = 2)

	# looking pretty
	mtext(expression(Population~size~of~N[2]) , side = 1 , outer = T)
	mtext(expression(Effect~on~population~size~of~N[1]) , side = 2 , outer = T)
	mtext(expression(Mutualism~functional~responses~from~Holland~italic(et~al.)~2002) , side = 3 , outer = T)
	dev.off()
