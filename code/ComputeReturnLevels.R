########################################################################################
##### Name: ComputeReturnLevels.R                                                  #####
#####                                                                              #####
##### Description:                                                                 #####
##### This codes is to accompany the Trend and Return Level of Extreme Snow Events #####                   
##### in New York City (TAS-17-202)                                                #####
#####                                                                              #####
##### Purpose:                                                                     #####
##### This code calculates MLE GEV and GP parameters, MLE return levels, and BCa   #####
##### bootstrap confidence intervals for return levels for the model 3.            #####
#####                                                                              #####
##### Last Updated: July 12, 2018                                                  #####
########################################################################################


########################################################################################
##### Setting up variables                                                         #####
########################################################################################

## Starting time
ptm <- proc.time()

## Loading required libraries
library(ismev)
library(quantreg)
library(foreach)
library(doParallel)

## setup parallel backend to use many processors
cores <- detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

## Settings
# T for GEV model and F for GP model
gev <- TRUE
# T for stationary and F for non-stationary
stationary <- FALSE

# T to export bootstrap distribution
export <- FALSE

# Set whether to estimate extremal index for GP models
find.extremal <- TRUE

### Defining Global Variables
# Bootstrap iteration B
iteration <- 10000
# Significance level for confidence interval
significance <- 0.95
# Years to compute return levels
year <- c(25,50,75,100)
# Other global variables
n.stn <- 4
n.yrs <- 56
# Block size for the moving block bootstrap
block.size <- 50
# tau for quantile regression
exceedance <- 0.875

### Setting whether to include data from the snow year 2015
include2015 <- FALSE
if(include2015) {
	n.yrs <- 57
}

########################################################################################
##### Reading Dataset                                                              #####
########################################################################################

### Reading Snowfall dataset
load('SnowfallData.RData')

### Assigning appropriate snowfall observations for GEV or GP
if(gev) {
	snow.1 <- annmax.1
	snow.2 <- annmax.2
	snow.3 <- annmax.3
	snow.4 <- annmax.4
} else {
	snow.1 <- nonzerosnow.1
	snow.2 <- nonzerosnow.2
	snow.3 <- nonzerosnow.3
	snow.4 <- nonzerosnow.4
}

### Excluding the Snow Year 2015 (includes January 2016 Blizzard) if selected
if(!include2015) {
	### GEV Snowfall
	if(gev) {
		snow.1 <- snow.1[-length(snow.1)]
		snow.2 <- snow.2[-length(snow.2)]
		snow.3 <- snow.3[-length(snow.3)]
		snow.4 <- snow.4[-length(snow.4)]
	### GP Snowfall
	} else {
		snow.1 <- snow.1[season.1 < 2015]
		snow.2 <- snow.2[season.2 < 2015]
		snow.3 <- snow.3[season.3 < 2015]
		snow.4 <- snow.4[season.4 < 2015]
		duration.1 <- duration.1[season.1 < 2015]
		duration.2 <- duration.2[season.2 < 2015]
		duration.3 <- duration.3[season.3 < 2015]
		duration.4 <- duration.4[season.4 < 2015]
		days.1 <- days.1[season.1 < 2015]
		days.2 <- days.2[season.2 < 2015]
		days.3 <- days.3[season.3 < 2015]
		days.4 <- days.4[season.4 < 2015]
		season.1 <- season.1[season.1 < 2015]
		season.2 <- season.2[season.2 < 2015]
		season.3 <- season.3[season.3 < 2015]
		season.4 <- season.4[season.4 < 2015]
	}
}

### Length of observations for each station
n.data.1 <- length(snow.1)
n.data.2 <- length(snow.2)
n.data.3 <- length(snow.3)
n.data.4 <- length(snow.4)
n.data.all <- n.data.1 + n.data.2 + n.data.3 + n.data.4
if(!gev) {
	days.1 <- days.1/365.25
	days.2 <- days.2/365.25
	days.3 <- days.3/365.25
	days.4 <- days.4/365.25
}

########################################################################################
##### Defining functions                                                           #####
########################################################################################

### Defining Functions to Compute Return Levels for GEV based on selected cases
## Case 1: mu.1=mu.2=mu.3, mu.4 (snowfall)
fit.gev <- function(snow.123,snow.4,time.123=NA,time.4=NA) {
	time.123 <- time.123[!is.na(snow.123)]
	time.4 <- time.4[!is.na(snow.4)]
	snow.123 <- snow.123[!is.na(snow.123)]
	snow.4 <- snow.4[!is.na(snow.4)]
	total.obs <- length(c(snow.123,snow.4))

	gev.lik <- function(a) {
		mu.123 <- a[1]
		mu.4 <- a[2]
		sc <- a[3]
		xi <- a[4]
		if(!stationary) { mu.t <- a[5] }

		if(stationary) {
			y123 <- 1 + xi * (snow.123 - mu.123)/sc
			y4 <- 1 + xi * (snow.4 - mu.4)/sc
		} else {
			y123 <- 1 + xi * (snow.123 - (mu.123 + mu.t*time.123))/sc
			y4 <- 1 + xi * (snow.4 - (mu.4 + mu.t*time.4))/sc
		}

		if (any(y123 <= 0) || any(y4 <= 0) || any(sc <= 0)) {
			nllh <- 10^6
		} else {
			nllh <- total.obs*log(sc) + sum(y123^(-1/xi)) + sum(y4^(-1/xi)) + sum(log(y123) * (1/xi + 1)) + sum(log(y4)*(1/xi + 1))
		}
		nllh
	}
	
	siginit <- sqrt(6 * var(c(snow.123,snow.4)))/pi
	mu123init <- mean(snow.123) - 0.57722 * siginit 
	mu4init <- mean(snow.4) - 0.57722 * siginit 
	shinit <- 0.1

	if(stationary) {
		init <- c(mu123init, mu4init, siginit, shinit)
	} else {
		init <- c(mu123init, mu4init, siginit, shinit, 0)
	}

	fit <- optim(init, gev.lik, hessian = TRUE, method = "Nelder-Mead")	
	if(stationary) {
		mu.t <- 0
	} else {
		mu.t <- fit$par[5]
	} 
	return <- matrix(0,n.stn,length(year))
	return[1,] <- fit$par[1] + mu.t*(year + n.yrs)/10 - (fit$par[3]/fit$par[4])*(1-(-log(1-(1/year)))^(-fit$par[4]))
	return[2,] <- fit$par[1] + mu.t*(year + n.yrs)/10 - (fit$par[3]/fit$par[4])*(1-(-log(1-(1/year)))^(-fit$par[4]))
	return[3,] <- fit$par[1] + mu.t*(year + n.yrs)/10 - (fit$par[3]/fit$par[4])*(1-(-log(1-(1/year)))^(-fit$par[4]))
	return[4,] <- fit$par[2] + mu.t*(year + n.yrs)/10 - (fit$par[3]/fit$par[4])*(1-(-log(1-(1/year)))^(-fit$par[4]))
	return(list(returns=return, trend=mu.t, output=fit))
}

### Perform Smith's Correction Method for GEV
smith.method.gev <- function(fitted,snow.1,snow.2,snow.3,snow.4) {
	H<-fitted$hessian

	mua.MLE<-fitted$par[1]
	mub.MLE<-fitted$par[2]
	sig.MLE<-fitted$par[3]
	xi.MLE<-fitted$par[4]
	mut.MLE<-0
	nu.MLE<-0

	if(!stationary) {
		mut.MLE<-fitted$par[5]
	}

	se.old<-sqrt(diag(solve(H)))

	# MLE and SE estimation by the new method
	mua.year<-numeric()
	mub.year<-numeric()
	sig.year<-numeric()
	xi.year<-numeric()
	mut.year<-numeric()

	for (i in 1:n.yrs) {
	  	mu1.MLE<-mua.MLE
	  	mu2.MLE<-mua.MLE
	  	mu3.MLE<-mua.MLE
	  	mu4.MLE<-mub.MLE

		diff.1<-snow.1[i]-mu1.MLE-mut.MLE*c(1:n.yrs/10)[i]
		diff.2<-snow.2[i]-mu2.MLE-mut.MLE*c(1:n.yrs/10)[i]
		diff.3<-snow.3[i]-mu3.MLE-mut.MLE*c(1:n.yrs/10)[i]
		diff.4<-snow.4[i]-mu4.MLE-mut.MLE*c(1:n.yrs/10)[i]
		diff<-c(diff.1, diff.2, diff.3, diff.4)
		diff<-diff[!is.na(diff)]

		diffxisc.1<-1+xi.MLE*diff.1/sig.MLE
		diffxisc.2<-1+xi.MLE*diff.2/sig.MLE
		diffxisc.3<-1+xi.MLE*diff.3/sig.MLE
		diffxisc.4<-1+xi.MLE*diff.4/sig.MLE
		diffxisc<-c(diffxisc.1, diffxisc.2, diffxisc.3, diffxisc.4)
		diffxisc<-diffxisc[!is.na(diffxisc)]

		# partial derivative of log likelihood function w.r.t. mu.t parameter
		if(!stationary) {
			mut.year[i]<--sum(diffxisc^(-1-1/xi.MLE))*c(1:n.yrs/10)[i]/sig.MLE+
				c(1:n.yrs/10)[i]*(1+xi.MLE)*sum(1/(xi.MLE*diff+sig.MLE))
		} else {
			mut.year[i]<-0
		}

		# partial derivatives of log likelihood function w.r.t. mu parameters
		mu1<--sum(diffxisc.1^(-1-1/xi.MLE))/sig.MLE+sum((1+xi.MLE)/(xi.MLE*diff.1+sig.MLE))
  		mu2<--sum(diffxisc.2^(-1-1/xi.MLE))/sig.MLE+sum((1+xi.MLE)/(xi.MLE*diff.2+sig.MLE))
  		mu3<--sum(diffxisc.3^(-1-1/xi.MLE))/sig.MLE+sum((1+xi.MLE)/(xi.MLE*diff.3+sig.MLE))
  		mu4<--sum(diffxisc.4^(-1-1/xi.MLE))/sig.MLE+sum((1+xi.MLE)/(xi.MLE*diff.4+sig.MLE))
		# Combining partial derivatives as needed
		mua.year[i]<-mu1+mu2+mu3
  		mub.year[i]<-mu4

  		# partial derivative of log likelihood function w.r.t. scale parameter
  		sig.year[i]<--length(diff)/sig.MLE-sum(diff*(diffxisc^(-1-1/xi.MLE)))/sig.MLE^2+
			(1+xi.MLE)*sum(diff/(xi.MLE*sig.MLE*diff+sig.MLE^2))

  		# partial derivative of log likelihood function w.r.t. shape parameter
  		xi.year[i]<-sum(log(diffxisc)/xi.MLE^2-(1+1/xi.MLE)*diff/(sig.MLE*diffxisc))-
			sum(diffxisc^(-1/xi.MLE)*(log(diffxisc)/xi.MLE^2-diff/(xi.MLE*sig.MLE*diffxisc)))
	}

	# Setting up the gradient vector
	if(sum(mut.year) == 0) {
		matrix<-cbind(mua.year,mub.year,sig.year,xi.year)
	} else {
		matrix<-cbind(mua.year,mub.year,sig.year,xi.year,mut.year)
	}

	# Calculating the V matrix
	if(any(is.na(matrix))) {
		V <- cov(matrix,use='pairwise.complete.obs')*n.yrs
	} else {
		V <- cov(matrix)*n.yrs
	}

	# Calculating the 'corrected' SE
	new <- (solve(H) %*% V) %*% solve(H)
	se.new <- sqrt(diag(new))

	return(list(old=se.old, new=se.new))
}

### Moving Block Bootstrap resampling
moving.block <- function(size,num.total,seed) {
	set.seed(seed)
	num.blocks <- num.total - size + 1
	blocks.out <- floor(num.total/size)
	block.index <- sample(1:num.blocks,size=blocks.out,replace=T)
	index <- numeric()
	for(i in 1:blocks.out) {
		index[(1+size*(i-1)):(size*i)] <- seq(from=block.index[i],to=block.index[i]+size-1)
	}
	index
}

### Obtaining Thresholds for GP Models
get.threshold <- function(exceedance=0.875,snow.1,snow.2,snow.3,snow.4,time.1=NA,time.2=NA,time.3=NA,time.4=NA) {
	# all inputs
	all.input <- c(snow.1,snow.2,snow.3,snow.4)
	# intercepts for quantile regression
	intercept.1 <- rep(0,length(all.input))
	intercept.2 <- rep(0,length(all.input))
	intercept.3 <- rep(0,length(all.input))
	intercept.4 <- rep(0,length(all.input))
	intercept.1[1:length(snow.1)] <- 1
	intercept.2[(length(snow.1)+1):(length(snow.1)+length(snow.2))] <- 1
	intercept.3[(length(snow.1)+length(snow.2)+1):(length(snow.1)+length(snow.2)+length(snow.3))] <- 1
	intercept.4[(length(snow.1)+length(snow.2)+length(snow.3)+1):length(all.input)] <- 1
	# slope term for non-stationary case only
	if(!stationary) {
		qr.slope<- numeric(0)
		qr.slope[1:length(snow.1)] <- time.1
		qr.slope[(length(snow.1)+1):(length(snow.1)+length(snow.2))] <- time.2
		qr.slope[(length(snow.1)+length(snow.2)+1):(length(snow.1)+length(snow.2)+length(snow.3))] <- time.3
		qr.slope[(length(snow.1)+length(snow.2)+length(snow.3)+1):length(all.input)] <- time.4
		qr.slope <- qr.slope/10
	}
	# fitting quantile regression models for original data
	if(stationary && exceedance == 0.875) {
		intercept.123 <- intercept.1 + intercept.2 + intercept.3
		qr.fit <- rq(all.input ~ -1 + intercept.123 + intercept.4, tau = exceedance)
		threshold.1 <- qr.fit$coef[1]
		threshold.2 <- qr.fit$coef[1]
		threshold.3 <- qr.fit$coef[1]
		threshold.4 <- qr.fit$coef[2]
		pars.out <- c(qr.fit$coef[1],qr.fit$coef[1],qr.fit$coef[1],qr.fit$coef[2],0)
	} else if (!stationary && exceedance == 0.875) {
		intercept.234 <- intercept.2 + intercept.3 + intercept.4
		qr.fit <- rq(all.input ~ -1 + intercept.1 + intercept.234 + qr.slope, tau = exceedance)
		threshold.1 <- qr.fit$coef[1] + time.1/10*qr.fit$coef[3]
		threshold.2 <- qr.fit$coef[2] + time.2/10*qr.fit$coef[3]
		threshold.3 <- qr.fit$coef[2] + time.3/10*qr.fit$coef[3]
		threshold.4 <- qr.fit$coef[2] + time.4/10*qr.fit$coef[3]
		pars.out <- c(qr.fit$coef[1],qr.fit$coef[2],qr.fit$coef[2],qr.fit$coef[2],qr.fit$coef[3])
	} else if (!stationary && exceedance == 0.825) {
		intercept.123 <- intercept.1 + intercept.2 + intercept.3
		qr.fit <- rq(all.input ~ -1 + intercept.123 + intercept.4 + qr.slope, tau = exceedance)
		threshold.1 <- qr.fit$coef[1] + time.1/10*qr.fit$coef[3]
		threshold.2 <- qr.fit$coef[1] + time.2/10*qr.fit$coef[3]
		threshold.3 <- qr.fit$coef[1] + time.3/10*qr.fit$coef[3]
		threshold.4 <- qr.fit$coef[2] + time.4/10*qr.fit$coef[3]
		pars.out <- c(qr.fit$coef[1],qr.fit$coef[1],qr.fit$coef[1],qr.fit$coef[2],qr.fit$coef[3])
	} else if (!stationary && exceedance == 0.925) {
		intercept.12 <- intercept.1 + intercept.2
		intercept.34 <- intercept.3 + intercept.4
		qr.fit <- rq(all.input ~ -1 + intercept.12 + intercept.34 + qr.slope, tau = exceedance)
		threshold.1 <- qr.fit$coef[1] + time.1/10*qr.fit$coef[3]
		threshold.2 <- qr.fit$coef[1] + time.2/10*qr.fit$coef[3]
		threshold.3 <- qr.fit$coef[2] + time.3/10*qr.fit$coef[3]
		threshold.4 <- qr.fit$coef[2] + time.4/10*qr.fit$coef[3]
		pars.out <- c(qr.fit$coef[1],qr.fit$coef[1],qr.fit$coef[2],qr.fit$coef[2],qr.fit$coef[3])
	}
	if(stationary) {
		threshold.1 <- rep(threshold.1,length(snow.1))
		threshold.2 <- rep(threshold.2,length(snow.2))
		threshold.3 <- rep(threshold.3,length(snow.3))
		threshold.4 <- rep(threshold.4,length(snow.4))
	}
	set.seed(55455)
	std.error <- summary.rq(qr.fit,se='boot')$coef[,2]
	return(list(t1 = threshold.1, t2 = threshold.2, t3 = threshold.3, t4 = threshold.4, pars = as.numeric(pars.out), std.error = as.numeric(std.error)))
}

### Fitting GP model
get.gp.mle <- function(data.all,threshold.all) {
	# Negative log likelihood funciton for GP with xi=0
	data <- data.all[data.all > threshold.all]
	threshold <- threshold.all[data.all > threshold.all]
	GPD.nloglik <- function(a) {
		sc<-a[1]
		y <- (data - threshold)/sc
		if (sc <= 0) {
			nloglik<-10^6
		} else {
			if (any(min(y) <= 0)) {
				nloglik<-10^6
			} else {
	  			nloglik<-length(data)*log(sc)+sum(y)
	    		}
	  	}
		nloglik
	}
	# Obtaining MLE of sigma
	fit <- optim(0,fn=GPD.nloglik,method="Brent",hessian=TRUE,lower=-10,upper=10)
	return(list(sig=fit$par, hessian=fit$hessian, loglik=-fit$value))
}

# Obtaining exceedance rates
get.exd.rate <- function(snow.1,snow.2,snow.3,snow.4,threshold.1,threshold.2,threshold.3,threshold.4,duration.1=NA,duration.2=NA,duration.3=NA,duration.4=NA) {
	# Obtaining number of obs and number of exceeding obs
	if(stationary && exceedance == 0.875) {
		# number of all observations
		n.obs.1 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.2 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.3 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.4 <- sum(duration.4)
		# number of exceeding observations
		n.exd.1 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.2 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.3 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.4 <- sum(duration.4[snow.4 > threshold.4])
	} else if (!stationary && exceedance == 0.875) {
		# number of all observations
		n.obs.1 <- sum(duration.1)
		n.obs.2 <- sum(c(duration.2,duration.3,duration.4))/3
		n.obs.3 <- sum(c(duration.2,duration.3,duration.4))/3
		n.obs.4 <- sum(c(duration.2,duration.3,duration.4))/3
		# number of exceeding observations
		n.exd.1 <- sum(duration.1[snow.1 > threshold.1])
		n.exd.2 <- sum(c(duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3],duration.4[snow.4 > threshold.4]))/3
		n.exd.3 <- sum(c(duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3],duration.4[snow.4 > threshold.4]))/3
		n.exd.4 <- sum(c(duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3],duration.4[snow.4 > threshold.4]))/3
	} else if (!stationary && exceedance == 0.825) {
		# number of all observations
		n.obs.1 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.2 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.3 <- sum(c(duration.1,duration.2,duration.3))/3
		n.obs.4 <- sum(duration.4)
		# number of exceeding observations
		n.exd.1 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.2 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.3 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2],duration.3[snow.3 > threshold.3]))/3
		n.exd.4 <- sum(duration.4[snow.4 > threshold.4])
	} else if (!stationary && exceedance == 0.925) {
		# number of all observations
		n.obs.1 <- sum(c(duration.1,duration.2))/2
		n.obs.2 <- sum(c(duration.1,duration.2))/2
		n.obs.3 <- sum(c(duration.3,duration.4))/2
		n.obs.4 <- sum(c(duration.3,duration.4))/2
		# number of exceeding observations
		n.exd.1 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2]))/2
		n.exd.2 <- sum(c(duration.1[snow.1 > threshold.1],duration.2[snow.2 > threshold.2]))/2
		n.exd.3 <- sum(c(duration.3[snow.3 > threshold.3],duration.4[snow.4 > threshold.4]))/2
		n.exd.4 <- sum(c(duration.3[snow.3 > threshold.3],duration.4[snow.4 > threshold.4]))/2
	}

	# returning exceedance rates
	return(list(exd=c(n.exd.1/n.obs.1,n.exd.2/n.obs.2,n.exd.3/n.obs.3,n.exd.4/n.obs.4), obs=c(n.obs.1,n.obs.2,n.obs.3,n.obs.4)))
}

# Estimating extremal index
get.extremal.index <- function(times.A,times.B,find.extremal = TRUE) {
	# Estimating extremal index using intervals estimator by Ferro and Segers (2003)
	eiA<-min(2*sum(times.A - 1)^2/(length(times.A)*sum((times.A-1)*(times.A-2))),1)
	eiB<-min(2*sum(times.B - 1)^2/(length(times.B)*sum((times.B-1)*(times.B-2))),1)
	# estimated extremal index
	if(stationary && exceedance == 0.875) {
		ei <- c(eiA, eiA, eiA, eiB)
	} else if (!stationary && exceedance == 0.875) {
		ei <- c(eiA, eiB, eiB, eiB)
	} else if (!stationary && exceedance == 0.825) {
		ei <- c(eiA, eiA, eiA, eiB)
	} else if (!stationary && exceedance == 0.925) {
		ei <- c(eiA, eiA, eiB, eiB)
	}
	# return extremal index
	if(find.extremal) {
		ei
	} else {
		c(1,1,1,1)
	}
}

# Bootstrap sampling of extremal index
extremal.boot <- function(iat,index,seed) {
	set.seed(i)
	intra.boot <- sample(iat[which(index == 0)],replace=T)
	inter.index <- sample(1:max(index),replace=T)
	count <- 1
	iat.boot <- numeric()
	for(i in 1:max(index)) {
		iat.boot[count:(count+length(iat[index==inter.index[i]]))] <- c(iat[index==inter.index[i]],intra.boot[i])
		count <- count + length(iat[index==inter.index[i]]) + 1
	}
	iat.boot[!is.na(iat.boot)]
}
cluster.code <- function(input,extremal) {
	cutoff <- c(sort(input,decreasing=T))[ceiling(extremal*length(input))]
	for(i in 1:length(input)) {
		if(length(which(input==cutoff))==1) {
			break
		} else {
			cutoff <- c(sort(input,decreasing=T))[ceiling(extremal*length(input))-i]
		}
	}
	output <- rep(0,length(input))
	output[input <= cutoff] <- 1
	num <- 1
	for(i in 1:length(input)) {
		if(output[i] == 1) { 
			output[i] <- num
		} else {
			num <- num + 1
		}
	}
	output
}


### Perform Smith's Correction Method for GP
smith.method.gp <- function(fitted,input0.1,input0.2,input0.3,input0.4,threshold.1,threshold.2,threshold.3,threshold.4,season0.1,season0.2,season0.3,season0.4) {
	H <- fitted$hessian
	sig.MLE <- fitted$sig
	se.old <- sqrt(diag(solve(H)))
	sig.year<-numeric()

	start_year <- min(season.1,season.2,season.3,season.4)
	for (i in 1:n.yrs) {
		diff.1 <- input0.1[season0.1==(i+start_year-1)] - threshold.1[season0.1==(i+start_year-1)]
		diff.2 <- input0.2[season0.2==(i+start_year-1)] - threshold.2[season0.2==(i+start_year-1)]
		diff.3 <- input0.3[season0.3==(i+start_year-1)] - threshold.3[season0.3==(i+start_year-1)]
		diff.4 <- input0.4[season0.4==(i+start_year-1)] - threshold.4[season0.4==(i+start_year-1)]

		diff<-c(diff.1, diff.2, diff.3, diff.4)
		diff<-diff[!is.na(diff)]
		diff<-diff[diff>0]

		# partial derivative of log likelihood function w.r.t. scale parameter
		sig.year[i]<--length(diff)/sig.MLE + sum(diff)/sig.MLE^2
	}

	# Setting up the gradient vector
	matrix<-cbind(sig.year)

	# Calculating the V matrix
	V <- cov(matrix)*n.yrs

	# Calculating the 'corrected' SE
	new <- (solve(H) %*% V) %*% solve(H)
	se.new <- sqrt(diag(new))

	return(list(old=se.old, new=se.new))
}

### Compute Return Levels for GP
return.gp <- function(sigma,exd.rates,intercept,slope=0,extremal=c(1,1,1,1)) {
	return <- matrix(0,n.stn,length(year))
	return[1,] <- intercept[1] + (n.yrs+year-0.5)/10*slope + sigma*log((year-0.5)*exd.rates$obs[1]/n.yrs*exd.rates$exd[1]*extremal[1])
	return[2,] <- intercept[2] + (n.yrs+year-0.5)/10*slope + sigma*log((year-0.5)*exd.rates$obs[2]/n.yrs*exd.rates$exd[2]*extremal[2])
	return[3,] <- intercept[3] + (n.yrs+year-0.5)/10*slope + sigma*log((year-0.5)*exd.rates$obs[3]/n.yrs*exd.rates$exd[3]*extremal[3])
	return[4,] <- intercept[4] + (n.yrs+year-0.5)/10*slope + sigma*log((year-0.5)*exd.rates$obs[4]/n.yrs*exd.rates$exd[4]*extremal[4])
	return(list(returns=return, trend=slope))
}

########################################################################################
##### Computing MLE model parameters with associated standard errors               #####
########################################################################################

### Fitting appropriate GEV models
if(gev) {
	if(stationary) {
		gev.model <- fit.gev(c(snow.1,snow.2,snow.3),snow.4)
		std.errors <- smith.method.gev(gev.model$output,snow.1,snow.2,snow.3,snow.4)
	} else {
		years <- 1:n.yrs/10
		gev.model <- fit.gev(c(snow.1,snow.2,snow.3),snow.4,c(years,years,years),years)
		std.errors <- smith.method.gev(gev.model$output,snow.1,snow.2,snow.3,snow.4)
	}
	model_parameters <- gev.model$output$par
}

### Fitting appropriate GP models
if(!gev) {
	# obtaining thresholds
	threshold.out <- get.threshold(exceedance,snow.1,snow.2,snow.3,snow.4,days.1,days.2,days.3,days.4)
	threshold.1 <- threshold.out$t1
	threshold.2 <- threshold.out$t2
	threshold.3 <- threshold.out$t3
	threshold.4 <- threshold.out$t4
	parameters <- threshold.out$pars

	# Calculating inter-arrival times for extremal index calculation
	# Exceedance times
	exd.times.1 <- c(days.1[snow.1>0])[c(snow.1[snow.1>0]) > threshold.1]*365.25
	exd.times.2 <- c(days.2[snow.2>0])[c(snow.2[snow.2>0]) > threshold.2]*365.25
	exd.times.3 <- c(days.3[snow.3>0])[c(snow.3[snow.3>0]) > threshold.3]*365.25
	exd.times.4 <- c(days.4[snow.4>0])[c(snow.4[snow.4>0]) > threshold.4]*365.25
	# Inter-arrival times
	iat.1 <- diff(exd.times.1)
	iat.2 <- diff(exd.times.2)
	iat.3 <- diff(exd.times.3)
	iat.4 <- diff(exd.times.4)
	# Find extremal index
	if(stationary && exceedance == 0.875) {
		extremal.index <- get.extremal.index(c(iat.1,iat.2,iat.3),iat.4,find.extremal)
	} else if (!stationary && exceedance == 0.875) {
		extremal.index <- get.extremal.index(iat.1,c(iat.2,iat.3,iat.4),find.extremal)
	} else if (!stationary && exceedance == 0.825) {
		extremal.index <- get.extremal.index(c(iat.1,iat.2,iat.3),iat.4,find.extremal)
	} else if (!stationary && exceedance == 0.925) {
		extremal.index <- get.extremal.index(c(iat.1,iat.2),c(iat.3,iat.4),find.extremal)
	}
	# Finding MLE GP fit
	fitted <- get.gp.mle(c(snow.1,snow.2,snow.3,snow.4),c(threshold.1,threshold.2,threshold.3,threshold.4))
	loglik <- fitted$loglik
	sig.MLE <- fitted$sig

	# exceedance rates
	exd.rates <- get.exd.rate(snow.1,snow.2,snow.3,snow.4,threshold.1,threshold.2,threshold.3,threshold.4,duration.1,duration.2,duration.3,duration.4)

	# Finding standard errors for sigma
	std.errors.sig <- smith.method.gp(fitted,snow.1[snow.1>0],snow.2[snow.2>0],snow.3[snow.3>0],snow.4[snow.4>0],threshold.1,threshold.2,threshold.3,threshold.4,season.1[snow.1>0],season.2[snow.2>0],season.3[snow.3>0],season.4[snow.4>0])
	
	# standard errors
	std.errors <- list(threshold=threshold.out$std.error, sigma_old=std.errors.sig$old, sigma_new=std.errors.sig$new)
}
model_parameters <- list(threshold=parameters, sigma=sig.MLE, theta=extremal.index, loglik=loglik)


########################################################################################
##### Computing standard errors of extremal index                                  #####
########################################################################################

### Bootstrap sampling for extremal index
if(!gev && find.extremal) {
	cluster.index.1 <- cluster.code(iat.1,extremal.index[1])
	cluster.index.2 <- cluster.code(iat.2,extremal.index[2])
	cluster.index.3 <- cluster.code(iat.3,extremal.index[3])
	cluster.index.4 <- cluster.code(iat.4,extremal.index[4])
	extremal.boots <- foreach(i = 1:iteration) %dopar% {
		iat.boot.1 <- extremal.boot(iat.1,cluster.index.1,i*2)
		iat.boot.2 <- extremal.boot(iat.2,cluster.index.2,i*3)
		iat.boot.3 <- extremal.boot(iat.3,cluster.index.3,i*4)
		iat.boot.4 <- extremal.boot(iat.4,cluster.index.4,i*5)
		get.extremal.index(iat.boot.1,c(iat.boot.2,iat.boot.3,iat.boot.4))
	}
	extremal.boot.1 <- numeric(0)
	extremal.boot.2 <- numeric(0)
	extremal.boot.3 <- numeric(0)
	extremal.boot.4 <- numeric(0)
	for(i in 1:iteration) {
		extremal.boot.1[i] <- extremal.boots[[i]][1]
		extremal.boot.2[i] <- extremal.boots[[i]][2]
		extremal.boot.3[i] <- extremal.boots[[i]][3]
		extremal.boot.4[i] <- extremal.boots[[i]][4]
	}
	std.errors.extremal <- c(sd(extremal.boot.1),sd(extremal.boot.2),sd(extremal.boot.3),sd(extremal.boot.4))
}
if(!gev && !find.extremal) {
	std.errors.extremal <- c(0,0,0,0)
}

########################################################################################
##### Computing MLE return levels                                                  #####
########################################################################################

### Extracting Return Levels from the fitted GEV model
if(gev) {
	return.MLE <- gev.model$returns
}

### Computing Return Levels from MLE Parameters for GP
if(!gev) {
	return.MLE <- return.gp(sig.MLE,exd.rates,parameters[1:4],parameters[5],extremal.index)$returns
}

########################################################################################
##### Computing bootstrap return levels                                            #####
########################################################################################

### Computing Bootstrap Return Level Samples for B iterations for GEV
if (gev) {
	boots <- foreach(i = 1:iteration,.packages="quantreg") %dopar% {
		set.seed(i)
		# resampling indexes
		index.1 <- sample(1:n.data.1,replace=T)
		index.2 <- sample(1:n.data.2,replace=T)
		index.3 <- sample(1:n.data.3,replace=T)
		index.4 <- sample(1:n.data.4,replace=T)
		# resampling snow data
		resample.1 <- snow.1[index.1]
		resample.2 <- snow.2[index.2]
		resample.3 <- snow.3[index.3]
		resample.4 <- snow.4[index.4]
		# This resamples years for non-stationary GEV case
		if(!stationary) {
			boot.year.1 <- index.1/10
			boot.year.2 <- index.2/10
			boot.year.3 <- index.3/10
			boot.year.4 <- index.4/10
		}
		# Calculating bootstrap return levels based on settings
		if(stationary) {
			return.boot <- fit.gev(c(resample.1,resample.2,resample.3),resample.4)
		} else {
			return.boot <- fit.gev(c(resample.1,resample.2,resample.3),resample.4,c(boot.year.1,boot.year.2,boot.year.3),boot.year.4)
		}	
	}
} 

### Computing Bootstrap Return Level Samples for B iterations for GP Snowfall
if (!gev) {
	boots <- foreach(i = 1:iteration,.packages="quantreg") %dopar% {
		set.seed(i)
		# This resamples days for non-stationary GP case
		index.1 <- moving.block(block.size,n.data.1,i*2)
		index.2 <- moving.block(block.size,n.data.2,i*3)
		index.3 <- moving.block(block.size,n.data.3,i*4)
		index.4 <- moving.block(block.size,n.data.4,i*5)
		# This resamples data using paired bootstrap
		resample.1 <- snow.1[index.1]
		resample.2 <- snow.2[index.2]
		resample.3 <- snow.3[index.3]
		resample.4 <- snow.4[index.4]
		# This resamples days for non-stationary case
		if(!stationary) {
			boot.days.1 <- days.1[index.1]
			boot.days.2 <- days.2[index.2]
			boot.days.3 <- days.3[index.3]
			boot.days.4 <- days.4[index.4]
		}
		# This resamples duration for each snowfall
		boot.duration.1 <- duration.1[index.1]
		boot.duration.2 <- duration.2[index.2]
		boot.duration.3 <- duration.3[index.3]
		boot.duration.4 <- duration.4[index.4]
		# obtaining thresholds
		if(stationary) {
			boot.threshold.out <- get.threshold(exceedance,resample.1,resample.2,resample.3,resample.4)
		} else {
			boot.threshold.out <- get.threshold(exceedance,resample.1,resample.2,resample.3,resample.4,boot.days.1,boot.days.2,boot.days.3,boot.days.4)
		}
		boot.threshold.1 <- boot.threshold.out$t1
		boot.threshold.2 <- boot.threshold.out$t2
		boot.threshold.3 <- boot.threshold.out$t3
		boot.threshold.4 <- boot.threshold.out$t4
		boot.parameters <- boot.threshold.out$pars
		# Finding MLE Return Levels
		boot.sig.MLE <- get.gp.mle(c(resample.1,resample.2,resample.3,resample.4),c(boot.threshold.1,boot.threshold.2,boot.threshold.3,boot.threshold.4))$sig
		boot.exd.rates <- get.exd.rate(resample.1,resample.2,resample.3,resample.4,boot.threshold.1,boot.threshold.2,boot.threshold.3,boot.threshold.4,boot.duration.1,boot.duration.2,boot.duration.3,boot.duration.4)
		return.boot <- return.gp(boot.sig.MLE,boot.exd.rates,boot.parameters[1:4],boot.parameters[5],extremal.index)
	}
}

# Distributing bootstrap output into each station
return.boot.1 <- matrix(0,iteration,length(year))
return.boot.2 <- matrix(0,iteration,length(year))
return.boot.3 <- matrix(0,iteration,length(year))
return.boot.4 <- matrix(0,iteration,length(year))
trends.boot <- matrix(0,iteration,1)
for(i in 1:iteration) {
	return.boot.1[i,] <- boots[[i]]$returns[1,]
	return.boot.2[i,] <- boots[[i]]$returns[2,]
	return.boot.3[i,] <- boots[[i]]$returns[3,]
	return.boot.4[i,] <- boots[[i]]$returns[4,]
	trends.boot[i] <- boots[[i]]$trend
}

# Exporting bootstrap distributions to text file
if (export) {
	export.data <- list()
	export.data[[1]] <- return.boot.1
	export.data[[2]] <- return.boot.2
	export.data[[3]] <- return.boot.3
	export.data[[4]] <- return.boot.4
	export.data[[5]] <- trends.boot
	write.csv(export.data,"export.csv")
}

### Calculating bias-correction constant, zBC
zBC <- matrix(0,n.stn,length(year))
for(i in 1:length(year)) {
	zBC[1,i] <- qnorm(sum(return.boot.1[,i] < return.MLE[1,i])/iteration,mean=0,sd=1)
	zBC[2,i] <- qnorm(sum(return.boot.2[,i] < return.MLE[2,i])/iteration,mean=0,sd=1)
	zBC[3,i] <- qnorm(sum(return.boot.3[,i] < return.MLE[3,i])/iteration,mean=0,sd=1)
	zBC[4,i] <- qnorm(sum(return.boot.4[,i] < return.MLE[4,i])/iteration,mean=0,sd=1)
}

### Computing delete-1 Jackknife Return Level Samples
if(!stationary) { all.yrs <- 1:n.yrs/10 }
if (gev) {
	jacks <- foreach(i = 1:n.yrs,.packages="quantreg") %dopar% {
		jack.sample.1 <- snow.1[-i]
		jack.sample.2 <- snow.2[-i]
		jack.sample.3 <- snow.3[-i]
		jack.sample.4 <- snow.4[-i]
		if(stationary) {
			return.jack <- fit.gev(c(jack.sample.1,jack.sample.2,jack.sample.3),jack.sample.4)
		} else {
			jack.year <- all.yrs[-i]
			return.jack <- fit.gev(c(jack.sample.1,jack.sample.2,jack.sample.3),jack.sample.4,c(jack.year,jack.year,jack.year),jack.year)
		}
	}
} 
if(!gev) {
	jacks <- foreach(i = 1:n.yrs,.packages="quantreg") %dopar% {
		jack.sample.1 <- snow.1[!(season.1==i+1958)]
		jack.sample.2 <- snow.2[!(season.2==i+1958)]
		jack.sample.3 <- snow.3[!(season.3==i+1958)]
		jack.sample.4 <- snow.4[!(season.4==i+1958)]
		if(!stationary) {
			jack.days.1 <- days.1[!(season.1==i+1958)]
			jack.days.2 <- days.2[!(season.2==i+1958)]
			jack.days.3 <- days.3[!(season.3==i+1958)]
			jack.days.4 <- days.4[!(season.4==i+1958)]
		}
		# obtaining thresholds
		jack.duration.1 <- duration.1[!(season.1==i+1958)]
		jack.duration.2 <- duration.2[!(season.2==i+1958)]
		jack.duration.3 <- duration.3[!(season.3==i+1958)]
		jack.duration.4 <- duration.4[!(season.4==i+1958)]
		if(stationary) {
			jack.threshold.out <- get.threshold(exceedance,jack.sample.1,jack.sample.2,jack.sample.3,jack.sample.4)
		} else {
			jack.threshold.out <- get.threshold(exceedance,jack.sample.1,jack.sample.2,jack.sample.3,jack.sample.4,jack.days.1,jack.days.2,jack.days.3,jack.days.4)
		}
		jack.threshold.1 <- jack.threshold.out$t1
		jack.threshold.2 <- jack.threshold.out$t2
		jack.threshold.3 <- jack.threshold.out$t3
		jack.threshold.4 <- jack.threshold.out$t4
		jack.parameters <- jack.threshold.out$pars
		# Finding MLE Return Levels
		jack.sig.MLE <- get.gp.mle(c(jack.sample.1,jack.sample.2,jack.sample.3,jack.sample.4),c(jack.threshold.1,jack.threshold.2,jack.threshold.3,jack.threshold.4))$sig
		jack.exd.rates <- get.exd.rate(jack.sample.1,jack.sample.2,jack.sample.3,jack.sample.4,jack.threshold.1,jack.threshold.2,jack.threshold.3,jack.threshold.4,jack.duration.1,jack.duration.2,jack.duration.3,jack.duration.4)
		return.jack <- return.gp(jack.sig.MLE,jack.exd.rates,jack.parameters[1:4],jack.parameters[5],extremal.index)
	}
}
return.jack.1 <- matrix(0,n.yrs,length(year))
return.jack.2 <- matrix(0,n.yrs,length(year))
return.jack.3 <- matrix(0,n.yrs,length(year))
return.jack.4 <- matrix(0,n.yrs,length(year))
for(i in 1:n.yrs) {
	return.jack.1[i,] <- jacks[[i]]$returns[1,]
	return.jack.2[i,] <- jacks[[i]]$returns[2,]
	return.jack.3[i,] <- jacks[[i]]$returns[3,]
	return.jack.4[i,] <- jacks[[i]]$returns[4,]
}
### Calculating acceleration constant cA
cA <- matrix(0,n.stn,length(year))
for(i in 1:length(year)) {
	summation.1 <- return.jack.1[,i] - mean(return.jack.1[,i])
	summation.2 <- return.jack.2[,i] - mean(return.jack.2[,i])
	summation.3 <- return.jack.3[,i] - mean(return.jack.3[,i])
	summation.4 <- return.jack.4[,i] - mean(return.jack.4[,i])
	cA[1,i] <- (1/6)*(sum(summation.1^3)/(sum(summation.1^2)^(3/2)))
	cA[2,i] <- (1/6)*(sum(summation.2^3)/(sum(summation.2^2)^(3/2)))
	cA[3,i] <- (1/6)*(sum(summation.3^3)/(sum(summation.3^2)^(3/2)))
	cA[4,i] <- (1/6)*(sum(summation.4^3)/(sum(summation.4^2)^(3/2)))
}

### Calculating Quantiles for BCa
Z <- qnorm(1-(1-significance)/2)

lower <- zBC + (zBC-Z)/(1-(cA*(zBC-Z)))
upper <- zBC + (zBC+Z)/(1-(cA*(zBC+Z)))

quantile.BCa.1 <- cbind(pnorm(lower[1,]),pnorm(upper[1,]))
quantile.BCa.2 <- cbind(pnorm(lower[2,]),pnorm(upper[2,]))
quantile.BCa.3 <- cbind(pnorm(lower[3,]),pnorm(upper[3,]))
quantile.BCa.4 <- cbind(pnorm(lower[4,]),pnorm(upper[4,]))

### Computing Confidence Intervals
CI <- matrix(0,20,3*2+1)
CI[,1] <- c(1,year,2,year,3,year,4,year)

# Assigning conventional percentile bootstrap CI on the left and BCa CI on the right
# For each CI, left: (alpha/2)%, middle: median, right: (1-alpha/2)%
for(i in 1:length(year)) {
	# Results for Central Park
	CI[1+i,2] <- quantile(return.boot.1[,i], (1-significance)/2)
	CI[1+i,3] <- quantile(return.boot.1[,i], 0.5)
	CI[1+i,4] <- quantile(return.boot.1[,i], 1-(1-significance)/2)
	CI[1+i,5] <- quantile(return.boot.1[,i], quantile.BCa.1[i,1])
	CI[1+i,6] <- quantile(return.boot.1[,i], 0.5)
	CI[1+i,7] <- quantile(return.boot.1[,i], quantile.BCa.1[i,2])
	# Results for Newark
	CI[6+i,2] <- quantile(return.boot.2[,i], (1-significance)/2)
	CI[6+i,3] <- quantile(return.boot.2[,i], 0.5)
	CI[6+i,4] <- quantile(return.boot.2[,i], 1-(1-significance)/2)
	CI[6+i,5] <- quantile(return.boot.2[,i], quantile.BCa.2[i,1])
	CI[6+i,6] <- quantile(return.boot.2[,i], 0.5)
	CI[6+i,7] <- quantile(return.boot.2[,i], quantile.BCa.2[i,2])
	# Results for La Guardia
	CI[11+i,2] <- quantile(return.boot.3[,i], (1-significance)/2)
	CI[11+i,3] <- quantile(return.boot.3[,i], 0.5)
	CI[11+i,4] <- quantile(return.boot.3[,i], 1-(1-significance)/2)
	CI[11+i,5] <- quantile(return.boot.3[,i], quantile.BCa.3[i,1])
	CI[11+i,6] <- quantile(return.boot.3[,i], 0.5)
	CI[11+i,7] <- quantile(return.boot.3[,i], quantile.BCa.3[i,2])
	# Results for JFK
	CI[16+i,2] <- quantile(return.boot.4[,i], (1-significance)/2)
	CI[16+i,3] <- quantile(return.boot.4[,i], 0.5)
	CI[16+i,4] <- quantile(return.boot.4[,i], 1-(1-significance)/2)
	CI[16+i,5] <- quantile(return.boot.4[,i], quantile.BCa.4[i,1])
	CI[16+i,6] <- quantile(return.boot.4[,i], 0.5)
	CI[16+i,7] <- quantile(return.boot.4[,i], quantile.BCa.4[i,2])
}
### Rounding calculated values to third decimals
CI <- round(CI,3)

### Stop the cluster
stopCluster(cl)

########################################################################################
##### Displaying results                                                           #####
########################################################################################

### Displaying fitted GEV/GP model along with standard errors for parameters
display_message <- function(T) {
	message("Following results are for the ", appendLF = F)
	if(stationary) {
		message("stationary ", appendLF = F)
	} else {
		message("non-stationary ", appendLF = F)
	}
	if(gev) {
		message("GEV ", appendLF = F)
	} else {
		message("GP ", appendLF = F)
	}
	message("model \n applied to the snowfall data.", appendLF = F)
}
display_message(T)

### Displaying MLE model paramters
message("MLE Model Paramters")
model_parameters

### Displaying their standard errors
message("Standard Errors")
std.errors

### Displaying MLE return levels
message("MLE Return Levels")
return.MLE

### Displaying return level confidence intervals
message("Return Level Confidence Intervals")
message("   Station Conventional Method  BCa \n     Years LB     Median UB     LB     Median UB")
CI

### Displaying bootstrapped linear trends
hist(trends.boot)
c(quantile(trends.boot, c((1-significance)/2,1-(1-significance)/2)))

### Total time required to run this scipt
time <- proc.time() - ptm
time