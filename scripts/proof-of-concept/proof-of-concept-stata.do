clear all
set more off

cd "C:\WBG\Otros\Desarroio\Repos\indicadores-enaho-peru"

use "data/raw/enaho01-2019-100.dta"

******************************
** Limpieza y nuevas variables
******************************

*Observaciones completas o incompletas
keep if result == 1 | result == 2

*Creando variable de dpto, distrito y total
gen dpto = substr(ubigeo, 1, 2)
gen dist = ubigeo
gen total = "Total"
decode dominio, gen(dominio_enaho)

*Creando indicadores
recode p103 (6/7 = 1) (nonmissing = 0), gen(piso_tierra) // piso de tierra o peor
recode p110 (1/2 = 1) (nonmissing = 0), gen(agua_red_publica) // agua de red publica
recode p111a (1/2 = 1) (nonmissing = 0), gen(desague_red_publica) // desague de red publica
gen electricidad = p1121 // acceso a electricidad
gen cocina_lena = .
replace cocina_lena = 1 if p1136 == 1 | p1137 == 1
replace cocina_lena = 0 if p1136 == 0 & p1137 == 0 // cocina con lena o peor

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
