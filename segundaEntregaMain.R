library(dplyr)

source('https://raw.githubusercontent.com/isabellam18/muse/main/segundaEntregaFunciones.R')

main = function(num_suc){

  ruta_sucs='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/flujo_efectivo.csv'
  df_sucs=read.csv(ruta_sucs) ### serie de tiempo flujo de sucursales 
  
  ruta_cap='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/capa_suc.csv'
  cap_sucs=read.csv(ruta_cap) ### lista con capacidad de cada sucursal
  
  ruta_saldo='https://raw.githubusercontent.com/juancamiloespana/MUSE_JCE/master/caso_estudio/saldo.csv'
  saldos=read.csv(ruta_saldo) ### lista con saldo de sucursal 1 de jun de 2022
  
  list_suc= unique(df_sucs$suc) ## lista de las sucursales con serie de flujo
  num_suc=min(num_suc, length(list_suc)) ## para cuantas suc se va a realizar el proceso
  
  df_acum = data.frame() # para guardar todos los resultados de simulaciones
  
  for(cod_suc in list_suc[1:num_suc]){ ## hacer el proceso para cada sucursal
    
    datos_suc = seleccionarSucursal(df_sucs = df_sucs, cap_sucs = cap_sucs, saldos = saldos, cod_suc = cod_suc)
    
    resultadosSimulacion = simulacion(datos_suc = datos_suc)
    
    resultadosSimulacion$cod = cod_suc
    resultadosSimulacion$sup_cap = as.integer(resultadosSimulacion$saldo>datos_suc$cap_suc)  
    resultadosSimulacion$sin_efe = as.integer(resultadosSimulacion$saldo == 0)  
    resultadosSimulacion$n_ped=as.integer(resultadosSimulacion$aprov+resultadosSimulacion$recol > 0)
    
    df_resultadosSimulacion = na.omit(data.frame(resultadosSimulacion))
    
    df_acum = rbind(df_acum, df_resultadosSimulacion)
    
  }
  
  return(df_acum)
  
}

datos = main(num_suc = 50)

c_ped = 0.1
c_sup = 0.2  
c_sin_efe = 0.4

por_suc=datos%>%group_by(cod)%>%
  summarise(n_ped=sum(n_ped)*c_ped, 
            n_sup_cap=sum(sup_cap)*c_sup, 
            n_sin_efe=sum(sin_efe)*c_sin_efe)

total=datos%>%
  summarise(n_ped=sum(n_ped)*c_ped, 
            n_sup_cap=sum(sup_cap)*c_sup, 
            n_sin_efe=sum(sin_efe)*c_sin_efe)

sum(total)
