validacion_cruz_pos = function(num_datos,num_sub){
  # -------------------------------------------------------------------
  # esta funcion calcula las posiciones que corresponden al train
  # y las posiciones que corresponden al test en validacion cruzada
  #
  # num_datos: numero de datos
  # num_sub: numero de subconjuntos
  # 
  # javier.cara@upm.es, version 2025.09
  # ------------------------------------------------------------------
  
  # numero de datos de cada subconjuntos
  n1 = trunc(num_datos/num_sub)
  
  v = sample(1:num_datos,num_datos,replace = F)
  train = list()
  test = list()
  for (k in 1:(num_sub-1)){
    pos_test = ((k-1)*n1+1):(k*n1)
    test[[k]] = v[pos_test]
    train[[k]] = v[-pos_test]
  }
  # el ultimo puede tener un numero diferente de datos (por trunc)
  pos_test = ((num_sub-1)*n1+1):num_datos
  test[[num_sub]] = v[pos_test]
  train[[num_sub]] = v[-pos_test]
  
  return(list(num_sub = num_sub, train = train, test = test))
}

