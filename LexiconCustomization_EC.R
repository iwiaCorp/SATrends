library(readr)

#lexico español-ecuador
fileNameLexico_EC <- "lexico_ec_custom.csv"

if(file.exists(fileNameLexico_EC)){
  lexico_ec <- tryCatch(read_csv(fileNameLexico_EC), error=function(e) 1)
  
}
  
#lexico cambio sentimiento español-ecuador
fileNameLexicoValenceShift_EC <- "LexicoCambioSentimientos.csv"

if(file.exists(fileNameLexicoValenceShift_EC)){
  lexicoCambioSentimiento <- tryCatch(read_csv(fileNameLexicoValenceShift_EC), error=function(e) 1)
  
}

fileNameCities_EC <- "tabla_ciudad_EC_depurada.csv"

if(file.exists(fileNameCities_EC)){
  cities_ec <- tryCatch(read_csv(fileNameCities_EC), error=function(e) 1)
  
}


