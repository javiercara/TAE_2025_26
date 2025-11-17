# Función de verosimilitud
logit_logL = function(beta,y,X){
  # asumimos que beta es un vector 
  # beta = [beta0 beta1 .. betak]
  # y = [y1 y2 ... yn]
  # X es la matriz de regresores
  
  n = length(y)
  suma = 0
  for (i in 1:n){
    suma = suma + y[i]*sum(X[i,]*beta) - 
      log(1 + exp( sum(t(X[i,])*beta) ))
  }
  return(suma)
}
#----------------------------------------------------------
# gradiente de la función de verosimilitud
logit_grad = function(beta,y,X){
  X = as.matrix(X)
  n = length(y)
  y = matrix(y, nrow = n, ncol = 1)
  pi = matrix(0, nrow = n, ncol = 1)
  for (i in 1:n){
    pi[i,1] = exp(sum(X[i,]*beta))/(1 + exp(sum(X[i,]*beta)))
  }
  grad = t(X) %*% (y - pi)
  return(grad)
}
#--------------------------------------------------------
# Hessiano de la matriz de verosimilitud
logit_hess = function(beta,X){
  X = as.matrix(X)
  n = nrow(X)
  W = matrix(0, nrow = n, ncol = n)
  for (i in 1:n){
    pi = exp(sum(X[i,]*beta))/(1 + exp(sum(X[i,]*beta)))
    W[i,i] = pi*(1-pi)
  }
  hess = - t(X) %*% W %*% X
  return(hess)
}
# -----------------------------------------------------------
logit_Newton = function(beta_i, y, X, max_iter = 100, tol = 10^(-6), alfa = 0.1){
  
  # punto de partida
  beta = beta_i
  
  iter = 1
  tol1 = Inf
  while ((iter <= max_iter) & (tol1 > tol)){
    f = logit_logL(beta,y,X)
    grad = logit_grad(beta,y,X)
    hess = logit_hess(beta,X)
    beta = beta - alfa*solve(hess) %*% grad
    f1 = logit_logL(beta,y,X)
    tol1 = abs((f1-f)/f)
    print(paste("Iteracion ",iter," log-verosimilitud ",f1))
    iter = iter + 1
  }
  return(beta)
}
# -----------------------------------------------------------
logit_logL_optim = function(beta,y,X){
  logL = logit_logL(beta,y,X)
  return(-logL)
}



