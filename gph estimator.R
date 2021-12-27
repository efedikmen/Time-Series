#Creating the periodogram of a time series
periodogram <- function(x){
  x=unlist(x)
  n=length(x)
  pgram=1/(2*pi*n)*(abs(fft(x)))^2
  pgram=pgram[2:(n/2)+1]
  return(pgram)
}

#Geweke Porter Hudak estimate of the memory parameter d
#manually input the number of periodogram ordinates to be 
#used in the estimation
gph <-function(x,M) {
  n=length(unlist(x))
  
  #taking the first M number of periodogram ordinates
  pgram=log(periodogram(x))[1:M]
  
  #creating the regression variable
  wj=2*pi*(1:M)/n
  reg=-2*log(abs(1-exp(-1i*wj)))
  
  #regression
  fit=lm(pgram~reg)
  print(summary(fit))
  confint(fit)
  acf(resid(fit))
  plot(resid(fit))
  return(summary(fit)$coefficients[2, 1])
}