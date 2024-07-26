swTest = function(data, histogram = F, qq_plot = F){
  if (any(is.na(data)) || any(is.infinite(data))) {
    stop("The data contains NA or inf values")
  }
  if (!is.numeric(data)) {
    stop("The format data must be numeric")
  }
  if(!is.vector(data)){
    stop("The data must be a vector")
  }
  if (!(length(data) >= 3 && length(data) <= 5000)){
    stop("The sample size must be among 3 and 5000")
  }
  
  swResuts = shapiro.test(data)
  
  if(histogram){
    hist(data)
  }
  if(qq_plot){
    qqnorm(data)
    qqline(data)
  }
  return(swResuts)
}