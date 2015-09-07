hola_chafa <- function(rep){
  for(i in 1:rep){
    print(paste0("Esta es la ",i," llamada"))
  }
}
hola_chafa(10)



hola <- function(repeticiones){
  if(repeticiones == 0){
    return
  }
  else{
    print(paste0("esta es la ",repeticiones," llamada"))
    hola(repeticiones-1)
    
  }
  
}
hola(10)


factorial <- function(n){
  result <- 1
  for(i in 1:n){
    print(paste0("Nivel",i))
    result <- i*result
    print(paste0("Nivel",i))
  }
  return (result)
}


factorial_recursivo <- function(n){
  if(n==1){
    return(1)     #Caso Base - Salida
  }
  else{
    print(paste0("Nivel",n))
    return(n*factorial(n-1))   #Iteracion
    print(paste0("Nivel",n))  
    }
}

factorial(5)
factorial_recursivo(5)

"""Fibonacci

Ciclo tradicional
Recursivo
0,1,1,2,3,5,8,13,21,34,55,89,....
Fibo(n)= Fibo(n-1)+Fibo(n-2)
Fibo(1)=1
Fibo(0)=0
"""

fibo_aburrido <- function(n){
  i = 1
  result = 0
  ultimo = 1
  penultimo = 0 
  if(n==0){
    return(0)
  }
  if(n==1){
    return(1)
  }
  while(i!=n){
    result = penultimo + ultimo
    penultimo = ultimo
    ultimo = result
    i = i+1
  }
  return(result)
  
}
fibo_aburrido(7)

"""Fibonacci

Ciclo tradicional
Recursivo
0,1,1,2,3,5,8,13,21,34,55,89,....
Fibo(n)= Fibo(n-1)+Fibo(n-2)
Fibo(1)=1
Fibo(0)=0
"""

fibo_recursivo <- function(n){
  if(n<2) return(n)
  return(fibo_recursivo(n-1)+fibo_recursivo(n-2))
}

fibo_recursivo(6)



edad = c(28,24,15,33)
edad <- 27

if(edad <= 18) print("Que mentira") else{
  if((edad >18) && (edad <30)) print("JOvenes sin experiencia de vida")
  else print("La voz de la experiencia")
}

?ifelse













  
  








