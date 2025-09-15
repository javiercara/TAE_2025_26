knn_dist = function(x,y){
  # funcion que calcula la distancia euclidea entre los vectores x e y
  dist = sqrt(sum((x-y)^2))
  return(dist)
}
####################################################################
knn_normaliza  = function(x, min_x = min(x), max_x = max(x)){
  # knn_normaliza(x,min_x,max_x): se normaliza x utilizando min_x y max_x
  # knn_normaliza(x): se normaliza x utilizando el min y el max de x
  x1 = (x - min_x)/(max_x - min_x)
  return(x1)
}