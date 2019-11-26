f <- function(x){
  return((x^3)+4*(x^2)-10)
}

f(1)

bisection <- function(f, a, b, err){
  if ((f(a)*f(b))>=0){
    print("No result!")
  }else{
    while((b-a)>err){
      p <- (a+b)/2
      s <- f(a)*f(p)
      
      if (s==0){
        print("The root is:",p)
      }else if(s<0){
        b<-p
      }else{ 
        a<-p
      }
    }
  }
  return(p)
}

bisection(f,1,2,0.0001)
