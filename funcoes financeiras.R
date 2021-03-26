j.simples <- function(m=0,c=0,i=0,n=0){
    if(m == 0){
    x <- c*(1+(i*n))
    print('montante')
      } else if(c == 0){
        x <- m / (1+i*n)
        print('capital')
          } else if(i == 0){
            x <- ((m/c)-1)/n
            print('tx de juros')
              } else{
                x <- ((m/c)-1)/ (i/100)
                print('tempo')
    }
  return(x)
}

j.composto <- function(m=0,c=0,i=0,n=0){
  if(m == 0){
    x <- c *(1+(i/100))^n
    print('montante')
    }else if(c == 0){
      x <- m/((1+(i/100))^n)
      print('capital')
        }else if(n == 0){
          x <- log(m/c)/log(1+(i/100))
          print('tempo')
            }else {
              x <- ((m/c)^(1/n) -1 )*100
              print('tx de juros (%)')
  }
  return(x)
}
 
# m =c*(1+in)     |   m = c*(1+n)^t
# c = m/(1+in)    |   c = m/(1+i)^n
# i = (m/c-1)/n   |   i = (m/c)^(1/n)-1 
# n = (m/c-1)/i   |   n = log(m/c)/log(1+i)
