
#source("c:\\mydocs\\alosa\\alosa data\\margaree\\2021\\reference point code\\background funtions.r")

#######################
# AJFG 03/16
# modified from HDB 01/13
# modified from KGB 01/00 (switch from S to R)
# modified from NJB, Mar.19/93


	srfbh2 <- function(a,aK,S) {
	# Stock recruitment function: Beverton-Holt
	#  return(a*S/(S/K + 1))  
	   return((1/S)*((1/a) + (S/aK)))
	}
	
        srfbh <- function(a,K,S) {
	# Stock recruitment function: Beverton-Holt
	  return(a*S/(S/K + 1))  
	#   return((1/S)*((1/a) + (S/aK)))
	}
	
        srfbhpv <- function(pv,S) {
       # Stock recruitment function (parameter vector version): Beverton-Holt
        return(srfbh(pv[1],pv[2],S))
        }


	srfbhpv2 <- function(pv,S) {
	# Stock recruitment function (parameter vector version): Beverton-Holt
	  return(srfbh2(pv[1],pv[2],S))
	}
	
	bhinit <- function(s,r) {
	# Calculate initial parameters for fitting Beverton-Holt model - uses the median values of the S-R data to calculate alpha and R0.
	   middle <- length(r)/2
	  orderRec <- order(r)
	  sortedRec <- r[orderRec]
	  recsortedStock <- s[orderRec]
	  K <- recsortedStock[middle]
	  medRec <- sortedRec[middle]
	  a <- 2*medRec/K
	  param <- c(a,K)
	  return(param)
	}
	
	lnlnegloglik <- function(fitted,observed) {
	# Lognormal [lnl] negative [neg] log likelihood [lik]
	# Arguments:
	#   fitted   : fitted median recruitment (assumed to be free of missing values)
	#   observed : observed recruitment (assumed to be free of missing values)
	  len <- length(fitted)
	  sumlogr <- sum(log(observed))
	  sumsqrlogratio <- sum((log(observed/fitted))^2)  
	  result <- 0.5*len*log( (2*pi/len)*sumsqrlogratio) + sumlogr + len/2
	  return(result)  
	}
	
	ml.bhlnl2 <- function(s,r,ip, returnlog=F) {
	#
	# alternate formulation for bh model (alpha and alphaK instead
	# of alpha and K)
	
	  choice <- !is.na(r) & !is.na(s)
	  r <- r[choice]
	  s <- s[choice]
	#  assign(".Stock",s,frame=1)       # Store in expression frame
	#  assign(".Recruit",r,frame=1)     # Store in expression frame
	#  assign(".Evaluations",0,frame=1) # Store in expression frame 
	# frame argument is unneccessary in R
	assign(".Stock",s)
	assign(".Recruit",r)
	assign(".Evaluations",0)
	 
	  if (missing(ip)) {
	    ip <- bhinit(s,r)
	  }
	  logalpha <- log(ip[1])
	  logbeta  <- log(ip[1]*ip[2])
	  
	  logip <- c(logalpha,logbeta)
	
	  bh2.nll <- function(x) {
	#    assign(".Evaluations",.Evaluations+1,frame=1) # store in expression frame
	assign(".Evaluations",.Evaluations+1) # frame argument is unnecesary in R
	
	    invfitted <- srfbhpv2(exp(x),.Stock)
	    fitted <- 1/(invfitted)
	    return(lnlnegloglik(fitted,.Recruit))  
	  }
	
	# modified to use nlminb function in R instead of nlmin in S (starting values supplied first, then function to minimize)
	  nlmin.out <- nlminb(logip,bh2.nll) 
	
	
	x<-exp(nlmin.out$par) # par is the best set of parameters
	  invfitted <- srfbhpv2(x,.Stock)
	  fitted <- 1/(invfitted)
	  nll <- lnlnegloglik(fitted,.Recruit)
	  sigma.squared <- sum( (log(.Recruit/fitted))^2 )/length(.Recruit)
	
	  if(returnlog==T){
		return(log(x))
	  } else {
	#careful with the transformation bias
		x[1] <- x[1]*exp(sigma.squared/2)
                x[2] <- x[2]*exp(sigma.squared/2)
	        
	# The output from the function nlminb is different from nlmin. Here 0 indicates successful convergence, and have asked for the number of iterations as well.
		return(list(pv=x,sigma=sqrt(sigma.squared),nll=nll,
	    	  evaluations=.Evaluations, converged=nlmin.out$convergence,
		  iterations=nlmin.out$iterations))
	  }
	}

	ml.bhlnl <- function(s,r,ip, returnlog=F) {
	#
	# formulation for bh model (alpha and K - the half saturation constant)
	
	  choice <- !is.na(r) & !is.na(s)
	  r <- r[choice]
	  s <- s[choice]
	#  assign(".Stock",s,frame=1)       # Store in expression frame
	#  assign(".Recruit",r,frame=1)     # Store in expression frame
	#  assign(".Evaluations",0,frame=1) # Store in expression frame 
	# frame argument is unneccessary in R
	assign(".Stock",s)
	assign(".Recruit",r)
	assign(".Evaluations",0)
	 
	  if (missing(ip)) {
	    ip <- bhinit(s,r)
	  }
	  logalpha <- log(ip[1])
	  logbeta  <- log(ip[1]*ip[2])
	  
	  logip <- c(logalpha,logbeta)
	
	  bh.nll <- function(x) {
	
	  assign(".Evaluations",.Evaluations+1) # frame argument is unnecesary in R
	
	  fitted <- srfbhpv(exp(x),.Stock)
          return(lnlnegloglik(fitted,.Recruit))  
	  }
	
	# modified to use nlminb function in R instead of nlmin in S (starting values supplied first, then function to minimize)
	  nlmin.out <- nlminb(logip,bh.nll) 
	
	
      	  x<-exp(nlmin.out$par) # par is the best set of parameters
	

          fitted <- srfbhpv(x,.Stock)
          nll <- lnlnegloglik(fitted,.Recruit)
  
          sigma.squared <- sum( (log(.Recruit/fitted))^2 )/length(.Recruit)

          x[1] <- x[1]*exp(sigma.squared/2)
	
	     
		return(list(pv=x,sigma=sqrt(sigma.squared),nll=nll,
	    	  evaluations=.Evaluations, converged=nlmin.out$convergence,
		  iterations=nlmin.out$iterations))
	  }
	
getadmout<-function(infile="c:\\mydocu~1\\sjh99-~1\\stocka~1\\output\\walleye5.rep"){
#
# Gets the formatted output from Admodel Builder (ask SJH) and brings
# it into Splus as a list of matrices and vectors
  x <- scan(infile, what="", sep="")
  x.elements <- is.na(as.numeric(x))
  x.dims <- seq(1, sum(x.elements) - 1, 2)
  x.labs <- seq(2, sum(x.elements), 2)
  y <- vector("list", length(x.labs))
  names(y) <- x[x.elements][x.labs]
  x.omit <- rep(F, length(x))
  for (j in 2:length(x)){
        if(x.elements[j-1] == T & x.elements[j] == T){
           x.omit[j-1] <- T
        }
  }
  x1 <- x[!x.omit]
  x1.elements <- is.na(as.numeric(x1))
  x1.breaks   <- c((1:length(x1))[x1.elements], c(length(x1)+1))
  print(length(x1.breaks))
  x1.dims <-  x[x.elements][x.dims]
  print(length(x1.dims))
  for(i in 2:length(x1.breaks)){
        y[[i-1]] <- as.numeric(x1[(x1.breaks[i-1] + 1):(x1.breaks[i] - 1)])     
        if(substring(x1.dims[i-1],1,1) == "r"){
                by.row <- T
        } else {
                by.row <- F
        }
        n.row <- as.numeric(substring(x1.dims[i-1],2))
        if(n.row > 1){
                y[[i-1]] <- matrix(y[[i-1]], byrow=by.row, nrow=n.row)
        }       
  }
  return(y)
}
# DGK 06/02

getmcout<- function(infile="C:\\Users\\BILLARDMA\\Code\\2019\\Assessment\\Middle\\mcout.dat",burn=100)

{

# Gets the formatted output from the mcmc interations of Admodel Builder and brings
# it into Splus as a list of matrices and vectors
  x <- read.table(infile)
#print(x)
# remove burn-in samples
  afterburn<-(burn+1):length(x[,1])
  x<- x[afterburn,]
  x[,length(x[1,])]<-as.numeric(as.vector(x[,length(x[1,])]))
  # get locations of var names 

  names.locs<-names.list<-NULL 

  for (i in 1:length(x[1,]))
	{ 
	if(  !is.numeric(as.vector(x[1,i]))) 
		{ 
		names.locs<-c(names.locs,i) 
		names.list<-c(names.list,as.character(x[1,i]))
		} 
	} 
 print(names.list)
 print(names.locs) 
  diff<-NULL

  for (i in 1:(length(names.locs)))
        {
	if(i!=length(names.locs)) diff[i]<-names.locs[i+1] - names.locs[i] - 1	
	else diff[i] <- length(x[1,]) - names.locs[i] 
	} 
print(diff)



  data.list<-list() 
  #data.list[i]<-NULL
  for (i in 1:length(diff))
    	{ 
	if(diff[i]==1) data.list[[i]]<- as.vector(x[,(names.locs[i]+1):(names.locs[i]+diff[i])])
	else {data.list[[i]]<- as.matrix(x[(names.locs[i]+1):(names.locs[i]+diff[i])])}
	print(names.list[i])
        #names(data.list[i])<-paste(as.character(names.list[i]))
  	}  
  names(data.list)<-paste(as.character(names.list))
  return(data.list)

}



