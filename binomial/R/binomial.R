library('ggplot2')
#private checker functions

#to check if the probability is between 0 and 1

check_prob<-function(prob){
  if((0<=prob)&(prob<=1)) {
    return(TRUE)
  }else {
    stop('invalid prob value')
  }
}


#to check if the trial is a non-negative integer
check_trials<-function(trials){
  if ((trials>=0)&(trials%%2==0|trials%%2==1)) {
    return(TRUE)
  }else{
    stop('invalid trials value')
  }
}


#to check if number of success is between 0 and total number of trials
check_success<-function(success,trials){
  if((success<=trials)&((success>=0)&(success%%2==0|success%%2==1))){
    return(TRUE)
  }else{
    stop('invalid success value')
  }
}


#private auxillary functions

#calculate mean of binomial distribution
aux_mean<-function(trials,prob){
  a<-trials*prob
  return(a)
}

#calculate variance of binomial distribution
aux_variance<-function(trials,prob){
  a<-trials*prob*(1-prob)
  return(a)
}

#calculate the mode of these trials
aux_mode<-function(trials,prob){
  a<-trials*prob+prob
  if(a%%2==1|a%%2==0){
    return(c(a,a-1))
  }else{
    return(floor(a))
  }
}

#calculate the skewness of these trials
aux_skewness<-function(trials,prob){
  a<-(1-2*prob)/sqrt(aux_variance(trials,prob))
  return(a)
}


#calculate the kurtosis of these trials
aux_kurtosis<-function(trials,prob){
  a<-(1-6*prob*(1-prob))/aux_variance(trials,prob)
  return(a)
}

#Function bin_choose
#' @title bin_choose function
#' @description describe the choose function
#' @param trials describe the number of trials
#' @param success describe the number of success
#' @return number of combinations possible
#' @export
#' @examples
#' bin_choose(3,2)
#' bin_choose(5,7)

bin_choose<-function(trials,success){
  if(success>trials){
    stop('success cannot be greater than trials')
  }else{
    a<-factorial(trials)/(factorial(success)*factorial(trials-success))
    return(a)
  }
}


#Function bin_probability
#' @title bin_probability function
#' @description describe the probability of having certain number of successes in certain number of trials with some probability
#' @param success describe the number of success
#' @param trials describe the number of trials
#' @param prob describe probability
#' @return the probability that such an event happens
#' @export
#' @examples
#' bin_probability(3,5,0.3)


bin_probability<-function(success,trials,prob){
  check_trials(trials)
  check_prob(prob)
  check_success(success,trials)
  a<-bin_choose(trials,success)
  b<-a*(prob^success)*((1-prob)^(trials-success))
  return(b)
}

#Function bin_distribution

#' @title bin_distribution
#' @description returns the probability for each possible success in the trials
#' @param trials describe the number of trials
#' @param prob describe probability
#' @return probability for each success
#' @export
#' @examples
#' bin_distribution(3,0.3)

bin_distribution<-function(trials,prob){
  success<-c(0:trials)
  probability<-bin_probability(success,trials,prob)
  dat<-data.frame(success,probability)
  class(dat)<-c('bindis','data.frame')
  return(dat)
}

#Function plot.bindis
#' @export
plot.bindis<-function(datframe){
  barplot(datframe$probability,xlab = 'success',ylab = 'probability',names.arg = datframe$success,col='Blue')
}


#Function bin_cumulative

#' @title cumulative binomial
#' @description gives table for cumulative binomial distribution given parameter
#' @param trials describe the number of files
#' @param prob describe probability
#' @return probability and cumulative probability for each success
#' @export
#' @examples
#' bin_cumulative(3,0.3)

bin_cumulative<-function(trials,prob){
  success<-0:trials
  probability<-bin_probability(success,trials,prob)
  cumulative<-cumsum(probability)
  dat<-data.frame('success'=success,'probability'=probability,'cumulative'=cumulative)
  class(dat)<-c('bincum','data.frame')
  return(dat)
}

#Function plot.bincum
#' @export
plot.bincum<-function(datframe){
  plot(datframe$success,datframe$cumulative,xlab='success',ylab='probability',type='o')
}

# bin_variable

#' @title binomial variable
#' @description stores parameter information
#' @param trials describe number of trials
#' @param prob describe probability
#' @return a list containing parameter
#' @export
#' @examples
#' bin_variable(3,0.3)

bin_variable<-function(trials,prob){
  check_trials(trials)
  check_prob(prob)
  a<-list('trials'=trials,'prob'=prob)
  class(a)<-'binvar'
  return(a)
}

#method print.binvar
#' @export
print.binvar<-function(x){
 cat(sprintf("Binomial variable"))
 cat(sprintf("\n"))
 cat(sprintf("\n"))
 cat('Paramaters','\n')
 cat('- numer of trials:',x$trials,"\n")
 cat('- prob of success:',x$prob,"\n")
 invisible(x)
}

#method summary.binvar
#' @export
summary.binvar<-function(x){
  a<-data.frame('trials'=x$trials,'probability'=x$prob,'mean'=aux_mean(x$trials,x$prob),'variance'=aux_variance(x$trials,x$prob),'mode'=aux_mode(x$trials,x$prob),'skewness'=aux_skewness(x$trials,x$prob),'kurtosis'=aux_kurtosis(x$trials,x$prob))
  class(summary)<-'summary.binvar'
  return(a)
}

#method print.summary.binvar

#' @export
print.summary.binvar<-function(x){
  cat(sprintf('Summary Binomial'))
  cat(sprintf("\n"))
  cat(sprintf("\n"))
  cat('Parameters',"\n")
  cat('-number of trials:',x$trials,'\n')
  cat('-prob of success:',x$prob,'\n')
  cat(sprintf('\n'))
  cat(sprintf('\n'))
  cat('Measures','\n')
  cat('-mean:',aux_mean(x$trials,x$prob),'\n')
  cat('-variance:',aux_variance(x$trials,x$prob),'\n')
  cat('-mode:',aux_mode(x$trials,x$prob),'\n')
  cat('-skewness:',aux_skewness(x$trials,x$prob),'\n')
  cat('-kurtosis:',aux_kurtosis(x$trials,x$prob),'\n')
  invisible(x)
}


# Function of measures

#' @export
bin_mean<-function(trials,prob){
  return(aux_mean(trials,prob))
}
#' @export
bin_variance<-function(trials,prob){
  return(aux_variance(trials,prob))
}
#' @export
bin_mode<-function(trials,prob){
  return(aux_mode(trials,prob))
}
#' @export
bin_skewness<-function(trials,prob){
  return(aux_skewness(trials,prob))
}


bin_kurtosis<-function(trials,prob){
  return(aux_kurtosis(trials,prob))
}








