library(tidyverse)
library(haven)

enaho01_2019_100 <- read_dta("data/raw/enaho01-2019-100.dta")

enaho01_2019_100 <- enaho01_2019_100 %>%
  filter( result == 1 | result == 2) %>% #observaciones completas o incompletas
  select( aÑo:panel, p103, p110,p111a, p1121, p1136, p1137) %>%   #selccionar variables
  mutate( dpto = str_sub( ubigeo, 1, 2),   #Creando variable de dpto, distrito y total
          dist = ubigeo,
          total = "Total",
          dominio_enaho = as.character(dominio),
          piso_tierra      = case_when( is.na( p103) ~ NA_integer_,
                                        p103 %in% c( 6, 7) ~ 1L,
                                        TRUE ~ 0L), #creando indicadores
          agua_red_publica = case_when( is.na( p110) ~ NA_integer_,
                                        p103 %in% c( 1, 2) ~ 1L,
                                        TRUE ~ 0L),
          desague_red_publica = case_when( is.na( p111a) ~ NA_integer_,
                                        p103 %in% c( 1, 2) ~ 1L,
                                        TRUE ~ 0L),
          electricidad = p1121,
          cocina_lena =  case_when( is.na( p111a) ~ NA_integer_,
                                    p1136 == 1 | p1137 == 1 ~ 1L,
                                   TRUE ~ 0L)
          ) 
