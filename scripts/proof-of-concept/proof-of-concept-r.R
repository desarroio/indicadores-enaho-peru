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
          piso_tierra      = case_when( is.na( p103) ~ NA_character_,
                                        p103 %in% c( 6, 7) ~ "1",
                                        TRUE ~ "0"), #creando indicadores
          agua_red_publica = case_when( is.na( p110) ~ NA_character_,
                                        p103 %in% c( 1, 2) ~ "1",
                                        TRUE ~ "0"),
          desague_red_publica = case_when( is.na( p111a) ~ NA_character_,
                                        p103 %in% c( 1, 2) ~ "1",
                                        TRUE ~ "0"),
          electricidad = p1121,
          cocina_lena =  case_when( is.na( p111a) ~ NA_character_,
                                    p1136 == 1 | p1137 == 1 ~ "1",
                                   TRUE ~ "0")
          ) 
### calculo de indicadores

  
  
*************************
  ** Calculo de indicadores
*************************
  
  *Lista de niveles a calcular
local niveles total dominio_enaho dpto dist

*Lista de indicadores a calcular
local indicadores piso_tierra agua_red_publica desague_red_publica electricidad cocina_lena

*Activando svyset con pesos por hogar
svyset conglome [pweight=factor07], strata(estrato) vce(linearized) singleunit(centered)

*Configuracion para guardar resultados
tempfile resultados
tempname postfile
postfile `postfile' str20 nivel str20 grupo str20 indicador resultado cv using "`resultados'"

*Indicadores
foreach nivel of local niveles {
	
	levelsof `nivel', local(grupos)
foreach grupo of local grupos {
  
  svy:mean `indicadores' if `nivel' == "`grupo'"
		*estat cv
		foreach indicador of local indicadores {
			post `postfile' ("`nivel'") ("`grupo'") ("`indicador'") (_b[`indicador']) (_se[`indicador'] / _b[`indicador'])

		}
	}
}

*************
** Exportando
*************

postclose `postfile'

use "`resultados'", clear
compress _all
outsheet using "outputs/proof-of-concept/stata-results.csv", comma replace