library(readr)

fileNameLexico_EC <- "lexico_ec_custom.csv"

if(file.exists(fileNameLexico_EC)){
  lexico_ec <- tryCatch(read_csv(fileNameLexico_EC), error=function(e) 1)
  
}
  
fileNameCities_EC <- "tabla_ciudad_EC_depurada.csv"

if(file.exists(fileNameCities_EC)){
  cities_ec <- tryCatch(read_csv(fileNameCities_EC), error=function(e) 1)
  
}


