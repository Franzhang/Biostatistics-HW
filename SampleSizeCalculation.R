# Sample Size Calculation with two methods

Traditional <- function(Alpha, Power, alternative = 1){
 if( Alpha <= 0 || Alpha >=1 || Power<=0 || Power >=1 || !(alternative == 1 || alternative == 2)) 
 stop("invalid input")

 Critical <- qnorm(Alpha/alternative, lower.tail = T)
 # The quantiles zβ s are computed. 
 Critical1 <- qnorm(1 - Power, lower.tail = T)
 # Create a 9x9 matrix with each entry zero.  
 SampleSize <- matrix(0, nrow = 9, ncol = 9)
 # The proportions are chosen. 
 Prop <- seq(0.1, 0.9, 0.1)

 # Set up a loop command to calculate sample sizes for H0: p = p0 versus H1 (depend on alternative option) with level at Alpha and Power. 
 for (i in 1:9){
  for (j in 1:9){
   SampleSize[i, j] <- round(((Critical*sqrt(Prop[i]*(1-Prop[i])) + Critical1*sqrt(Prop[j]*(1 - Prop[j])))^2)/(Prop[i] - Prop[j])^2)
   }
  }
 rownames(SampleSize) <- c("P0=0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
 colnames(SampleSize) <- c("P1=0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")

 return(SampleSize)
}

Arcsine <- function(Alpha, Power, alternative = 1){
 if( Alpha <= 0 || Alpha >=1 || Power<=0 || Power >=1 || !(alternative == 1 || alternative == 2) ) 
 stop("invalid input")

 Critical <- qnorm(Alpha/alternative, lower.tail = T)
 # The quantiles zβ s are computed. 
 Critical1 <- qnorm(1 - Power, lower.tail = T)
 # Create a 9x9 matrix with each entry zero.  
 SampleSize <- matrix(0, nrow = 9, ncol = 9)
 # The proportions are chosen. 
 Prop <- seq(0.1, 0.9, 0.1)

 # Set up a loop command to calculate sample sizes for H0: p = p0 versus H1 (depend on alternative option) with level at Alpha and Power.
 SimpleSize <- matrix(0, nrow = 9, ncol = 9)
 for (i in 1:9){
  for (j in 1:9){
  SampleSize[i, j] <- round(((Critical + Critical1)^2)/(2*asin(sqrt(Prop[i])) - 2*asin(sqrt(Prop[j])))^2)
   }
  }
 rownames(SampleSize) <- c("P0=0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
 colnames(SampleSize) <- c("P1=0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9")
 return(SampleSize)
}

for(i in c(0.01, 0.05, 0.10)){
   for( j in c(0.80,0.90,0.95)){
       print(Traditional(i, j, 2))
   }
}

for(i in c(0.01, 0.05, 0.10)){
   for( j in c(0.80,0.90,0.95)){
       print(Arcsine(i, j, 2))
   }
}
