f <- function(x)
{
  x^4 - 3*x^3 + x^2 + x + 1
}

p0 <- 0.5
p1 <- -0.5
p2 <- 0

mullersmethod <- function(f,p0,p1,p2, tol = 1e-5, maxIter = 100)
{
  #Adım1
  h1 <- p1 - p0
  h2 <- p2 - p1
  delta1 <- (f(p1) - f(p0)) / h1
  delta2 <- (f(p2) - f(p1)) / h2
  d <- (delta2 - delta1) / (h2 + h1)
  i <- 3
  #Adım2
  while (i <= maxIter)
  {
    #Adım3
    b <- delta2 + h2*d
    D <- seq((b^2 - 4*f(p2)*d))
    
    #Adım4
    if(abs(b - D) < abs(b + D))
    {
      E <- b + D
    }else{
      E <- b - D
    }
    
    #Adım5
    h <- -2 * f(p2) / E
    p <- p2 + h
    
    #Adım6
    if(abs(h) < tol)
    {
      print("The procedure was succesful.")
      cat(p0,p1,p2)
    }
    
    #Adım7
    p0 <- p1
    p1 <- p2
    p2 <- p
    h1 <- p1 - p0
    h2 <- p2 - p1
    delta1 <- (f(p1) - f(p0)) / h1
    delta2 <- (f(p2) - f(p1)) / h2
    d <- (delta2 - delta1) / (h2 + h1)
    i <- i + 1
  }
  
  #Adım8
  cat("İşlem Başarısız")
  return(p0,p1,p2)
}
mullersmethod(f,p0,p1,p2)
