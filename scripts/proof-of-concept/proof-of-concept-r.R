library(tidyverse)
library(haven)
library(survey)

enaho01_2019_100 <- read_dta("data/raw/enaho01-2019-100.dta")

enaho01_2019_100 <- enaho01_2019_100 %>%
  filter( result == 1 | result == 2) %>% #observaciones completas o incompletas
  select( aÑo:panel, p103, p110, p111a, p1121, p1136, p1137, factor07) %>%   #selccionar variables
  mutate( dpto = str_sub( ubigeo, 1, 2),   #Creando variable de dpto, distrito y total
          dist = ubigeo,
          total = "Total",
          dominio_enaho = as.character(dominio),
          piso_tierra      = case_when( is.na( p103) ~ NA_integer_,
                                        p103 %in% c( 6, 7) ~ 1L,
                                        TRUE ~ 0L), #creando indicadores
          agua_red_publica = case_when( is.na( p110) ~ NA_integer_,
                                        p110 %in% c( 1, 2) ~ 1L,
                                        TRUE ~ 0L),
          desague_red_publica = case_when( is.na( p111a) ~ NA_integer_,
                                        p111a %in% c( 1, 2) ~ 1L,
                                        TRUE ~ 0L),
          electricidad = as.integer(p1121),
          cocina_lena =  case_when( is.na( p1136) | is.na( p1137) ~ NA_integer_,
                                    p1136 == 1 | p1137 == 1 ~ 1L,
                                   TRUE ~ 0L),
          cod_viv = paste0( conglome, vivienda, hogar)
          ) 

# diseño de encuesta - se asume que los conglomerados ya incluyen la primera etapa de la selección y por 
# lo tanto se considera com UPM (a pesr de que en realidad es la segunda sefun ficha técnica)
enaho_design <- svydesign( data = enaho01_2019_100,
                           weights = ~factor07,
                           id = ~conglome+cod_viv,
                           strata = ~dpto)

### calculo de indicadores
svymean( ~piso_tierra + agua_red_publica + desague_red_publica + electricidad + cocina_lena, enaho_design, na.rm = TRUE)
cv(svymean( ~piso_tierra + agua_red_publica + desague_red_publica + electricidad + cocina_lena, enaho_design, na.rm = TRUE))


svyby( formula = ~piso_tierra,
       by = ~total,
       design = enaho_design,
       FUN = svymean, na.rm = TRUE,
       row.names  = FALSE)

enaho01_2019_100 %>% 
  count( total, piso_tierra) 

b <- enaho01_2019_100 %>%
  gather(  key = "indicador",
           value = "count",
           piso_tierra:cocina_lena)

b %>% count( total, indicador, wt = factor07)

  
#  *Lista de niveles a calcular
#local niveles total dominio_enaho dpto dist

#*Lista de indicadores a calcular
#local indicadores piso_tierra agua_red_publica desague_red_publica electricidad cocina_lena

#Activando svyset con pesos por hogar
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