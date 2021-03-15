library(tidyverse)
library(haven)
library(survey)
library(readr)

enaho01_2019_100 <- read_dta("data/raw/enaho01-2019-100.dta") #cargar data
stata_results <- read_csv("outputs/proof-of-concept/stata-results.csv") #cargar resultados en stata

enaho01_2019_100 <- enaho01_2019_100 %>%
  filter( result == 1 | result == 2) %>% #filtra observaciones completas o incompletas
  select( aÑo:panel, p103, p110, p111a, p1121, p1136, p1137, factor07) %>%   #selccionar variables
  mutate( dpto = as_factor(str_sub( ubigeo, 1, 2)),   #Creando variable de dpto, distrito y total
          dist = as_factor(ubigeo),
          total = "Total",
          dominio_enaho = as_factor( dominio),
          #creando indicadores
          piso_tierra      = case_when( is.na( p103) ~ NA_integer_, p103 %in% c( 6, 7) ~ 1L, TRUE ~ 0L), 
          agua_red_publica = case_when( is.na( p110) ~ NA_integer_, p110 %in% c( 1, 2) ~ 1L, TRUE ~ 0L),
          desague_red_publica = case_when( is.na( p111a) ~ NA_integer_, p111a %in% c( 1, 2) ~ 1L, TRUE ~ 0L),
          electricidad = as.integer(p1121),
          cocina_lena =  case_when( is.na( p1136) | is.na( p1137) ~ NA_integer_, p1136 == 1 | p1137 == 1 ~ 1L, TRUE ~ 0L),
          cod_viv = paste0( conglome, vivienda, hogar)
          ) 

# diseño de encuesta - se asume que los conglomerados ya incluyen la primera etapa de la selección y por 
# lo tanto se considera com UPM (a pesr de que en realidad es la segunda sefun ficha técnica)
enaho_design <- svydesign( data = enaho01_2019_100, weights = ~factor07, id = ~conglome+cod_viv, strata = ~dpto)

#función muy poco elegante para calcular survey indicators para los 4 niveles
calc <- function( grupo) {
  a <- svyby( formula = ~piso_tierra + agua_red_publica + desague_red_publica + electricidad + cocina_lena,
         by = grupo,
         vartype = "cv",
         design = enaho_design,
         FUN = svymean, na.rm = TRUE,
         row.names  = FALSE)
  a1 <- a %>% select( 1, !starts_with("cv."))
  a2 <- a %>% select( 1, starts_with("cv."))
  b1 <- pivot_longer( a1, !1, names_to = "indicador", values_to = "resultado")
  b2 <- pivot_longer( a2, !1, names_to = "indicador", values_to = "resultado")
  b <- bind_rows( b1, b2)
  c <- b %>% mutate( tipo = case_when( str_extract(indicador, "cv.") == "cv." ~ "cv",
                                       TRUE ~"resultado"),
                     indicador = str_remove( indicador, "cv."))
  d <- pivot_wider( c, names_from = tipo, values_from = resultado)
}

#calculo de indicadores aplicando función
lista <- list( ~total, ~dominio_enaho, ~dpto, ~dist) #lista de niveles
lista2 <- lapply( lista, calc)
lista3 <- lapply( lista2, rename, grupo = 1 )

r_results <- bind_rows(lista3)

write_csv(r_results, "outputs/proof-of-concept/r_results.csv") #guardar .csv

#crear tabla de comparación
comparacion <- 
  left_join( stata_results, r_results, by = c("grupo", "indicador"), suffix = c( ".stata", ".r")) %>%
  mutate( dif_res = resultado.stata-resultado.r, dif_cv= cv.stata-cv.r)

#Reporte de comparación
summary( comparacion %>% select(dif_res, dif_cv))
