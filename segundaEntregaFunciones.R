
# Importar paquetes

library(smooth)   # Paquete que trae el modelo sma_old
library(forecast) # Paquete que trae los modelos [holt, hw, ses, auto.arima]
library(Metrics)  # Paquete que trae las metricas de desempeño [mae]

# Seleccionar Datos

seleccionarSucursal = function(df_sucs, cap_sucs, saldos, cod_suc){
  
  df_suc = df_sucs[df_sucs$suc == cod_suc,] # Filtrar en la tabla flujo_efectivo la sucursal seleccionada
  flujo_suc = df_suc$flujo_efe/1000000      # Seleccionar la columna "flujo_efe" (flujo de efectivo)
  fechas = df_suc$fechas                    # Seleccionar la columna "fechas"
  
  cap_suc = cap_sucs[cap_sucs$suc == cod_suc,]  # Filtrar en la tabla capa_suc la sucursal seleccionada 
  cap = cap_suc$cap/1000000                    # Convertir de pesos a millones
  
  saldo_suc = saldos[saldos$suc==cod_suc,] # Filtrar en la tabla saldo la sucursal seleccionada
  saldo = saldo_suc$saldo/1000000          # Convertir de pesos a millones
  
  salida = list('saldo_2022_06_01' = saldo,   # Saldo
                'cap_suc' = cap,              # Capacidad
                'flujo_suc' = flujo_suc,      # Flujo de Efectivo
                'fechas' = fechas)            # Fechas
  
  return(salida)
  
}

# Seleccionar Frecuencia

ajusteFrecuencia = function(serie){
  
  freq_cand = findfrequency(serie)
  
  list_freq = c(7, 15, 30)
  diff = abs(list_freq - freq_cand)
  index_min_diff = which.min(diff)
  freq = list_freq[index_min_diff]
  ts_flujo = ts(serie, frequency=freq)
  
  
  return(ts_flujo)
  
}

# Comparar Modelos

mejorModelo = function(serie, 
                       list_modelos = list('sma'=sma_old, 'ses'=ses, 'holt'=holt, 'hw'=hw 
                                           ,'arima' = Arima)){
  
  best_ind = 999999999999999  # Indicador AIC
  best_mod = NA               # Mejor modelo (objeto)
  n_best_mod = NA             # Mejor modelo (nombre)
  
  
  for (n_mod in names(list_modelos)){              # Iterar entre los modelos
    
    
    fun_mod = list_modelos[[n_mod]]                # Modelo (objeto)
    
    
    if((n_mod == 'hw') & (frequency(serie) > 15))  # Si es hw y la frecuencia es mayor a 15 se establece en 15
    {
      serie_hw = ts(serie, frequency=15)
      mod_cand = fun_mod(serie_hw, lambda='auto')  # lambda = 'auto' utiliza la transformación Box Cox 
                                                   # para corregir sesgos en la distribución de errores
      
    }else
    {
      mod_cand = fun_mod(serie, lambda='auto')
    }
    
    ajustados = fitted(mod_cand)                   # Datos ajustados segun el modelo
    ind_cand = mae(as.vector(serie), as.vector(ajustados)) # Calculo de MAE
    
    
    
    if(ind_cand < best_ind) # Si el MAE es menor se selecciona el modelo actual
    {
      best_mod=mod_cand
      n_best_mod=n_mod
      best_ind=ind_cand
    }
    
  }
  
  salida = list('mod' = best_mod, 
                'n_mod' = n_best_mod,
                'ind' = best_ind)
  
  return(salida)
}


# Funcion de Solicitar Aprovisimiento/Recoleccion

solicitarServicio = function(cap_suc, saldo_suc, modelo, serie){
  
  # modelo = mejorModelo(serie = serie)$mod  # Ajustar modelo con funcion realizada

  if(inherits(modelo, "smooth")){ 
    pron = sum(modelo$forecast[1:2]) ## pronostico 2 dias de flujo 2022-06-02, 2022-06-03
  }else{
    pron = sum(modelo$mean[1:2])# pronostico 2 dias de flujo 2022-06-02, 2022-06-03
  }
  
  pron_saldo = saldo_suc + pron ## calcular saldo pronosticado 2022-06-03
  
  punto_restabl = cap_suc*0.6 ## punto de restablecimiento de inventario
  
  aprov = 0
  recol = 0
  
  if(pron_saldo >= cap_suc){
    recol = pron_saldo-punto_restabl
  }else if (pron_saldo <= 0){
    aprov = punto_restabl
  }
  
  pedido=list('a' = aprov,'r' = recol)
  return(pedido)
}

# Funcion de simulacion

simulacion = function(datos_suc, ini_sim = 519, fin_sim = 600){
  
  saldo_dia = numeric() #  Iniciarlizar un vector para acumular los saldos de simulación 
  aprov_dia = numeric() #  Iniciarlizar un vector para acumular los aprovisionamientos
  recol_dia = numeric() #  Iniciarlizar un vector para acumular las recolecciones
  
  cap = datos_suc$cap_suc
  flujo_efe = datos_suc$flujo_suc # serie con los datos hasta el 23 de agosto del 2022
  saldo_dia[ini_sim-2] = datos_suc$saldo_2022_06_01 # saldo del 1 de junio
  saldo_dia[ini_sim-1] = saldo_dia[ini_sim-2] + flujo_efe[ini_sim-1]
  
  
  # dia-2 : es el dia hasta el que hay datos disponibles para el modelo.
  # dia : es el dia que se va a predecir.
  
  serie = flujo_efe[1:(ini_sim-2)]
  
  serie = ajusteFrecuencia(serie = serie)
  
  modelo = mejorModelo(serie = serie)$mod  # Ajustar modelo con funcion realizada
  
  for (dia in ini_sim:fin_sim){
    
    serie = flujo_efe[1:(dia-2)]
    
    serie = ajusteFrecuencia(serie = serie)
    
    solicitud = solicitarServicio(cap_suc = cap, saldo_suc = saldo_dia[(dia-2)], modelo, serie = serie)
    
    aprov_dia[dia] = solicitud$a
    recol_dia[dia] = solicitud$r
    
    saldo_dia[dia] = max(saldo_dia[(dia-1)] + flujo_efe[dia] + aprov_dia[dia] -  recol_dia[dia], 0)
    
  }
  
  salida = list('saldo' = saldo_dia, 'aprov' = aprov_dia, 'recol' = recol_dia)
  
  return(salida)
  
}