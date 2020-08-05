#Atividade semana 3

##Parte 1: Armazenando o inverso de uma matriz, criando uma matriz especial

makeCacheMatrix <- function(x = matrix()) { ##criando uma função especial para matriz
  i <- NULL
  set <- function(y) {
    x <<- y  ##<<- aqui irei atribuir valores à matriz
    i <<- NULL
  }
  get <- function() x  ##Criando a matriz inversa
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##Parte 2: Calculo do inverso da matriz que foi inserida acima

  ##Se o inverso ja foi calculado, a função abaixo (cacheSolve) tem que devolver o inverso da matriz
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("obtendo dados")  
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

##Testando
M<-matrix(c(5,6,6,8),2,2)
m <- makeCacheMatrix(M)
cacheSolve(m)
