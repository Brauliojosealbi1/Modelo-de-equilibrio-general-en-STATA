
********************************************************************************
****************          ANÁLISIS DE POLÍTICA COMERCIAL        **************** 
**************** EN EQUILIBRIO GENERAL CON GRAVEDAD ESTRUCTURAL **************** 

********************  ELIMINACIÓN DE UNA FRONTERA ESPECÍFICA  ******************
********************************************************************************

* Elaborado por: 			Juan José Almeida Almeida

* Basado en el libro:  		" An Advanced Guide to Trade Policy Analysis: The 
* 							Structural Gravity Model" 
*							Yoto V. Yotov, Roberta Piermartini, 
*							José-Antonio Monteiro, and Mario Larch
*

* Link del libro: 					
*	https://www.wto.org/spanish/res_s/publications_s/advancedguide2016_s.htm
*
* Fuente de los datos / CEPII: 		
*	http://www.cepii.fr/cepii/en/bdd_modele/bdd_modele_item.asp?id=8

***************************** PASOS PRELIMINARES *******************************
* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the esttab command if necessary
	* findit esttab
	
global PATH "D:\Escritorio\EG GRAVITACIONAL STATA"
* Limpiar memoria y establecer parámetros
	clear all
	set more off
	clear matrix
	set matsize 800
	set type double, permanently
	
	cd "$PATH"	
		
* Cerrar y crear registro	
	*capture log close
	*log using "Exercises/1_RemovingSpecificBorder/Results/RemovingSpecificBorder.log", text replace
	
************************ PREPARACIÓN DEL DATASET *******************************
* Preparación de base TPc
	use "Datasets\data 2018\TPc_V202201.dta", clear
	keep if year == 2018
	collapse (sum) trade_sq trade_sq_yr, by(year iso3num_o iso3num_d)
	save "Datasets\data 2018\TPc_V202201_2.dta", replace

* Carga y manipulación de dataset Gravity	
	use "Datasets\data 2018\Gravity_V202211.dta", clear
	cap drop if iso3num_o == .
	cap drop if iso3num_d == .
	keep if year == 2018
	keep iso3num_o iso3num_d iso3_o iso3_d distw_harmonic distw_arithmetic year contig comlang_off
	
	* Agrupación  de países del resto del mundo (ROW) conforme working paper de CEPII
	* Primero se hace un match con una lista de países provistas en el working paper
	merge m:1 iso3num_o using "Datasets\data 2018\cty_list_ROW", keepusing(iso3num_o)
	replace iso3num_o =999 if _m ==3
	drop _m
	merge m:1 iso3num_d using "Datasets\data 2018\cty_list_ROW", keepusing(iso3num_d)
	replace iso3num_d =999 if _m == 3
	drop _m
		* agregamos el país LSO a ROW debido a problemas de heterocedasticidad en el modelo
		*	 solo cuando se usa la variable trade_sq
		* replace iso3num_o = 999 if iso3num_o == 426
		* replace iso3num_d = 999 if iso3num_d == 426
	* Luego hacemos un collapse.
	collapse (mean) distw_harmonic distw_arithmetic contig comlang_off, by(iso3num_o iso3num_d year)
	* Aproximación de valores para variables dummy (dado que, en el collapse,
	* estas variables adquirieron valores continuos. 
	replace contig = (contig > 0.5) if contig != 1 & contig != 0
	replace comlang_off = (comlang_off > 0.5) if comlang_off != 1 & comlang_off != 0

* Merge base Gravity y TPc
	merge 1:m iso3num_o iso3num_d year using "Datasets\data 2018\TPc_V202201_2.dta"
	keep if _m ==3
	drop _m
	*save "Datasets\data 2018\Gravity_V202211_2.dta", replace	
	
* Merge con base Countries
	rename iso3num_o iso3num
	merge m:m iso3num using "Datasets\data 2018\Countries_V202211.dta"
	keep if _merge == 1 | _merge == 3
	drop country_id country countrylong first_year last_year countrygroup_iso3 ///
		countrygroup_iso3num iso2 heg_iso3_2020 heg_iso3num_2020 _merge
	replace iso3 = "ROW" if iso3num == 999
	rename iso3num iso3num_o
	rename iso3 exporter
	
	rename iso3num_d iso3num
	merge m:m iso3num using "Datasets\data 2018\Countries_V202211.dta"
	keep if _merge == 1 | _merge == 3
	drop country_id country countrylong first_year last_year countrygroup_iso3 ///
		countrygroup_iso3num iso2 heg_iso3_2020 heg_iso3num_2020 _merge
	replace iso3 = "ROW" if iso3num == 999
	rename iso3num iso3num_d
	rename iso3 importer
	rename trade_sq_yr trade
	rename distw_harmonic DIST
	rename contig CNTG
		
	save "Datasets\data 2018\GE.dta", replace

********************* CARGA Y PREPARACION DE LOS DATOS *************************
	use "Datasets\data 2018\GE.dta", clear

* Definir año
		*keep if year == 2018
		
* Crear variable dummy de frontera internacional
		generate INTL_BRDR = 1
		replace INTL_BRDR = 0 if exporter == importer
	
* Crear la variable logarítmica de distancia
		generate ln_DIST = ln(DIST)
			
* Crear variable de producción agregada
		bysort exporter: egen Y = sum(trade)

* Crear variable de gasto agregado
		bysort importer: egen E = sum(trade)

* Cambiar la variable de comercio a miles de millones
		*gen trade_MML = trade/100 
		*drop trade
		*rename trade_MML trade

* Elegir un país para el grupo de referencia: GERMANY
	* El código del país de referencia se establece como "ZZZ" para que los 
	* efectos fijos del exportador y del importador del país de referencia 
	* sean siempre los últimos en crearse
		gen E_R_BLN = E if importer == "DEU"
			replace exporter = "ZZZ" if exporter == "DEU"
			replace importer = "ZZZ" if importer == "DEU"
		egen E_R = mean(E_R_BLN)
		
* Crear efectos fijos del exportador e importador
		qui tab exporter, gen(EXPORTER_FE)
		qui tab importer, gen(IMPORTER_FE)

* Establecer el número de variables de efectos fijos del exportador
		quietly ds EXPORTER_FE*
		global N = `: word count `r(varlist)'' 
		global N_1 = $N - 1
		
* Generar una nueva variable para la frontera entre los países A y B
		gen INTL_BRDR_AB = 0
			replace INTL_BRDR_AB = 1 if (exporter == "ECU" & importer == "ARE") ///
			| (importer == "ECU" & exporter == "ARE") 
			
* Save data 
	save "Datasets/1_RemovingSpecificBorder.dta", replace
	

************************* ANÁLISIS DE EQUILIBRIO GENERAL *************************
* I: Resolver el modelo de grvedad base

	* I.a. Obtener estimaciones de los costos de comercio y las elasticidades 
	* comerciales índices base
		* Estimar el modelo de gravedad en el escenario "base" con el estimador PPML:
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 ln_DIST CNTG INTL_BRDR, iter(30) noconst
			predict tradehat_BLN, mu
		* ppmlhdfe trade ln_DIST CNTG INTL_BRDR, absorb(exporter importer year) d savefe(fe_exporter fe_importer)
			* predict tradehat_BLN, mu
	
	* I.b. Construir índices base	
		* Basado en los efectos fijos estimados de exportadores e importadores, crear
		* el conjunto actual de efectos fijos
			forvalues i = 1 (1) $N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
			
		* Crear las variables apilando todos los efectos fijos de exportador e 
		* importador no cero, respectivamente
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Crear las variables apilando todos los efectos fijos de exportador e 
		* importador no cero, respectivamente		
			egen exp_pi_BLN = rowtotal(EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_BLN = rowtotal(IMPORTER_FE1-IMPORTER_FE$N ) 

		* Calcular la variable de costos comerciales bilaterales	
			generate tij_BLN = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR)

		* Calcular las resistencias multilaterales externas e internas usando la 
		* propiedad aditiva del estimador PPML que vincula los efectos fijos de los 
		* exportadores y importadores con sus respectivas resistencias multilaterales 
		* teniendo en cuenta la normalización impuesta
			generate OMR_BLN = Y * E_R / exp_pi_BLN
			generate IMR_BLN = E / (exp_chi_BLN * E_R)	
			
		* Calcular el nivel estimado de comercio internacional en la base para 
		* el nivel dado de producción y gastos			
			generate tempXi_BLN = tradehat_BLN if exporter != importer
				bysort exporter: egen Xi_BLN = sum(tempXi_BLN)
					drop tempXi_BLN
			generate Y_BLN = Y
			generate E_BLN = E
	
* Dado que el mismo procedimiento se aplica independientemente del contrafactual, 
* se crean y llaman dos funciones que implementan los pasos III y IV del 
* procedimiento iterativo para evitar duplicar los mismos comandos tres veces.		

program GEPPML_stepIII
	* El argumento de la función es el nombre adicional del archivo para guardar
    * los resultados con el fin de distinguir entre subpreguntas
	args file
	
 * III: Resolver el modelo contrafactual

	* III.a.: Obtener efectos de equilibrio general condicional
	
	* (i): Estimar el modelo gravitacional imponiendo las restricciones asociadas 
	* al escenario contrafactual. La restricción se define por separado tomando 
	* el logaritmo de los costos hipotéticos del comercio bilateral. El parámetro 
	* de esta expresión estará restringido a ser igual a 1 en el estimador de ppml.
	
		* Especificar la restricción en logaritmo
			generate ln_tij_CFL = log(tij_CFL)	
		
		* Recrear los efectos fijos de exportadores e importadores
				drop EXPORTER_FE* IMPORTER_FE*
			quietly tabulate exporter, generate(EXPORTER_FE)
			quietly tabulate importer, generate(IMPORTER_FE)

		* Estimar el modelo de gravedad restringido y generar valor de comercio predicho
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 , iter(30) noconst offset(ln_tij_CFL)
			predict tradehat_CDL, mu
		* ppmlhdfe trade, absorb(exporter importer) offset(ln_tij_CFL) iter(30)
			* predict tradehat_CDL, mu
	
	* (ii): Construir resistencias multilaterales de equilibrio general condicional
	
		* Basado en los efectos fijos de exportador e importador estimados, 
		* crear el conjunto actual de efectos fijos contrafactuales	
			forvalues i = 1(1)$N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
		
		* Crear los efectos fijos de exportador e importador para el país de 
		* referencia (Alemania)
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Crear las variables apilando todos los efectos fijos de exportador e 
		* importador no cero, respectivamente		
			egen exp_pi_CDL = rowtotal( EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_CDL = rowtotal( IMPORTER_FE1-IMPORTER_FE$N )
			
		* Calcular las resistencias multilaterales externas e internas				
			generate OMR_CDL = Y * E_R / exp_pi_CDL
			generate IMR_CDL = E / (exp_chi_CDL * E_R)
			
		* * Calcule el nivel estimado de comercio internacional de equilibrio 
		* general condicional para el nivel dado de producción y gastos.		
			generate tempXi_CDL = tradehat_CDL if exporter != importer
				bysort exporter: egen Xi_CDL = sum(tempXi_CDL)
					drop tempXi_CDL

					
	* III.b: Obtener efectos de equilibrio general de dotación completa

		* Cree el procedimiento iterativo especificando las variables iniciales, 
		* donde s = 0 representa el valor de referencia (BLN) y s = 1 representa
		* el valor de equilibrio general condicional (CD).
		
			* La elasticidad constante de sustitución se toma de la literatura
			scalar sigma = 7
		
			* El parámetro phi vincula el valor de la producción con los gastos
			generate  phi = E/Y if exporter == importer
			
			* Calcular el cambio en los costos comerciales bilaterales resultantes 
			* del contrafactual
			generate change_tij = tij_CFL / tij_BLN	

			* Reespecificar las variables en los escenarios base y condicional
				* Producción 
				generate Y_0 = Y
				generate Y_1 = Y
				
				* Gastos, incluyendo con respecto al país de referencia   
				generate E_0 = E
				generate E_R_0 = E_R
				generate E_1 = E
				generate E_R_1 = E_R			
			
				* Nivel predicho de comercio
				generate tradehat_1 = tradehat_CDL
			
		* (i) Permitir precios de fábrica endógenos
	
			* Vuelva a especificar los precios en fábrica en los escenarios base 
			* y condicional.				
			generate exp_pi_0 = exp_pi_BLN
			generate tempexp_pi_ii_0 = exp_pi_0 if exporter == importer
				bysort importer: egen exp_pi_j_0 = mean(tempexp_pi_ii_0)
			generate exp_pi_1 = exp_pi_CDL
			generate tempexp_pi_ii_1 = exp_pi_1 if exporter == importer
				bysort importer: egen exp_pi_j_1 = mean(tempexp_pi_ii_1)
				drop tempexp_pi_ii_*
			generate exp_chi_0 = exp_chi_BLN	
			generate exp_chi_1 = exp_chi_CDL	
			
			* Calcular el primer cambio de orden en los precios de fábrica en 
			* los escenarios base y condicional
			generate change_pricei_0 = 0				
			generate change_pricei_1 = ((exp_pi_1 / exp_pi_0) / (E_R_1 / E_R_0))^(1/(1-sigma))
			generate change_pricej_1 = ((exp_pi_j_1 / exp_pi_j_0) / (E_R_1 / E_R_0))^(1/(1-sigma))
		
			* Reespecificar las resistencias multilaterales externas e internas 
			* en los escenarios base y condicional
			generate OMR_FULL_0 = Y_0 * E_R_0 / exp_pi_0
			generate IMR_FULL_0 = E_0 / (exp_chi_0 * E_R_0)		
			generate IMR_FULL_1 = E_1 / (exp_chi_1 * E_R_1)
			generate OMR_FULL_1 = Y_1 * E_R_1 / exp_pi_1
			
		* Calcular el cambio inicial en las resistencias multilaterales externas 
		* e internas, que se establecen a cero		
			generate change_IMR_FULL_1 = exp(0)		
			generate change_OMR_FULL_1 = exp(0)
		

	****************************************************************************
	******************* Inicio del Procedimiento Iterativo *********************
	
	* Establecer el criterio de convergencia, es decir, que tanto los errores estándar
	* o el máximo de la diferencia entre dos iteraciones de los precios de fábrica 
	* sean menores que 0.01, donde s es el número de iteraciones
		local s = 3	
		local sd_dif_change_pi = 1
		local max_dif_change_pi = 1
	while (`sd_dif_change_pi' > 0.01) | (`max_dif_change_pi' > 0.01) {
		local s_1 = `s' - 1
		local s_2 = `s' - 2
		local s_3 = `s' - 3
		
		* (ii)	Permitir ingresos, gastos y comercio endógenos
			generate trade_`s_1' =  tradehat_`s_2' * change_pricei_`s_2' * change_pricej_`s_2' / (change_OMR_FULL_`s_2'*change_IMR_FULL_`s_2')

		* (iii) Estimación del modelo de gravedad estructural
				drop EXPORTER_FE* IMPORTER_FE*
				quietly tabulate exporter, generate (EXPORTER_FE)
				quietly tabulate importer, generate (IMPORTER_FE)
			capture ppml trade_`s_1' EXPORTER_FE* IMPORTER_FE*, offset(ln_tij_CFL) noconst iter(30) 
				predict tradehat_`s_1', mu
					
			* Actualizar producción y gastos			
				bysort exporter: egen Y_`s_1' = total(tradehat_`s_1')
				quietly generate tempE_`s_1' = phi * Y_`s_1' if exporter == importer
					bysort importer: egen E_`s_1' = mean(tempE_`s_1')
				quietly generate tempE_R_`s_1' = E_`s_1' if importer == "ZZZ"
					egen E_R_`s_1' = mean(tempE_R_`s_1')
				
			* Actualizar precios de fábrica
				forvalues i = 1(1)$N_1 {
					quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
					quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
				}
				quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
				egen exp_pi_`s_1' = rowtotal(EXPORTER_FE1-EXPORTER_FE$N ) 
				quietly generate tempvar1 = exp_pi_`s_1' if exporter == importer
					bysort importer: egen exp_pi_j_`s_1' = mean(tempvar1) 		
					
			* Actualizar resistencias multilaterales 
				generate change_pricei_`s_1' = ((exp_pi_`s_1' / exp_pi_`s_2') / (E_R_`s_1' / E_R_`s_2'))^(1/(1-sigma))
				generate change_pricej_`s_1' = ((exp_pi_j_`s_1' / exp_pi_j_`s_2') / (E_R_`s_1' / E_R_`s_2'))^(1/(1-sigma))
				generate OMR_FULL_`s_1' = (Y_`s_1' * E_R_`s_1') / exp_pi_`s_1' 
					generate change_OMR_FULL_`s_1' = OMR_FULL_`s_1' / OMR_FULL_`s_2'					
				egen exp_chi_`s_1' = rowtotal(IMPORTER_FE1-IMPORTER_FE$N )	
				generate IMR_FULL_`s_1' = E_`s_1' / (exp_chi_`s_1' * E_R_`s_1')
					generate change_IMR_FULL_`s_1' = IMR_FULL_`s_1' / IMR_FULL_`s_2'
				
			* Iteración hasta que el cambio en los precios de fábrica converja a cero
				generate dif_change_pi_`s_1' = change_pricei_`s_2' - change_pricei_`s_3'
					display "************************* iteration number " `s_2' " *************************"
						summarize dif_change_pi_`s_1', format
					display "**********************************************************************"
					display " "
						local sd_dif_change_pi = r(sd)
						local max_dif_change_pi = abs(r(max))	
						
			local s = `s' + 1
			drop temp* 
	}
	
	********************* Fin del Procedimiento Iterativo **********************
		
		* (iv)	Construcción de los índices de efectos del "equilibrio general
		* de dotación plena"
			* Utilice el resultado de la última iteración S
			local S = `s' - 2
	
		* Cálculo el equilibrio general de dotación total del precio de fábrica
			generate change_pricei_FULL = ((exp_pi_`S' / exp_pi_0) / (E_R_`S' / E_R_0))^(1/(1-sigma))		

		* Calcular el equilibrio general de dotación total de la producción de valor
			generate Y_FULL = change_pricei_FULL  * Y_BLN

		* Calcular el equilibrio general de dotación total del valor de los 
		* gastos agregados
			generate tempE_FULL = phi * Y_FULL if exporter == importer
				bysort importer: egen E_FULL = mean(tempE_FULL)
					drop tempE_FULL
			
		* Calcular el equilibrio general de dotación total de las resistencias 
		* multilaterales hacia afuera y hacia adentro 
			generate OMR_FULL = Y_FULL * E_R_`S' / exp_pi_`S'
			generate IMR_FULL = E_`S' / (exp_chi_`S' * E_R_`S')	
			
		* Calcular la dotación total de equilibrio general del valor del comercio bilateral 
			generate X_FULL = (Y_FULL * E_FULL * tij_CFL) /(IMR_FULL * OMR_FULL)			
		
		* Calcular el equilibrio general de dotación total del valor del comercio 
		* internacional total 
			generate tempXi_FULL = X_FULL if exporter != importer
				bysort exporter: egen Xi_FULL = sum(tempXi_FULL)
					drop tempXi_FULL
					
	* Guardar los resultados de los efectos de equilibrio general y condicional.		
	save "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", replace
end

program GEPPML_stepIV
	* El argumento de la función es el nombre adicional del archivo para guardar 
	* los resultados para poder distinguir entre subpreguntas.
	args file
	
* IV: Recopilar, construir y reportar índices de interés
	use "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", clear
		collapse(mean) OMR_FULL OMR_CDL OMR_BLN change_pricei_FULL Xi_* Y_BLN Y_FULL, by(exporter)
			rename exporter country
			replace country = "DEU" if country == "ZZZ"
			sort country
		
		* Cambio porcentual en el equilibrio general de dotación total de los 
		* precios de fábrica
			generate change_price_FULL = (change_pricei_FULL - 1) / 1 * 100
			
		* Cambio porcentual en el equilibrio general de dotación total de 
		* resistencias multilaterales hacia afuera
			generate change_OMR_CDL = (OMR_CDL^(1/(1-sigma)) - OMR_BLN^(1/(1-sigma))) / OMR_BLN^(1/(1-sigma)) * 100
		
		* Cambio porcentual en el equilibrio general de dotación total de las 
		* resistencias multilaterales hacia el exterior
			generate change_OMR_FULL = (OMR_FULL^(1/(1-sigma)) - OMR_BLN^(1/(1-sigma))) / OMR_BLN^(1/(1-sigma)) * 100

		* Cambio porcentual en el equilibrio general condicional del comercio bilateral
			generate change_Xi_CDL = (Xi_CDL - Xi_BLN) / Xi_BLN * 100	
			
		* Cambio porcentual en el equilibrio general de dotación total del comercio bilateral	
			generate change_Xi_FULL = (Xi_FULL - Xi_BLN) / Xi_BLN * 100
	save "Exercises\1_RemovingSpecificBorder\Results\FULL_PROD_`file'.dta", replace


	* Construcción de los cambios porcentuales en el lado de las 
	* importaciones/consumo.
	use "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", clear
		collapse(mean) IMR_FULL IMR_CDL IMR_BLN, by(importer)
			rename importer country
			replace country = "DEU" if country == "ZZZ"
			sort country		

		* Equilibrio general condicional de las resistencias multilaterales 
		* hacia el interior
			generate change_IMR_CDL = (IMR_CDL^(1/(1-sigma)) - IMR_BLN^(1/(1-sigma))) / IMR_BLN^(1/(1-sigma)) * 100
			
		* Equilibrio general de dotación total de resistencias multilaterales internas
			generate change_IMR_FULL = (IMR_FULL^(1/(1-sigma)) - IMR_BLN^(1/(1-sigma))) / IMR_BLN^(1/(1-sigma)) * 100
	save "Exercises\1_RemovingSpecificBorder\Results\FULL_CONS_`file'.dta", replace

	* Fusionar los resultados del equilibrio general de los lados de la
	* producción y el consumo.
	use "Exercises\1_RemovingSpecificBorder\Results\FULL_PROD_`file'.dta", clear
		joinby country using "Exercises\1_RemovingSpecificBorder\Results\FULL_CONS_`file'.dta"
		
		* Equilibrio general de dotación plena del PIB real
			generate rGDP_BLN = Y_BLN / (IMR_BLN ^(1 / (1 -sigma)))
			generate rGDP_FULL = Y_FULL / (IMR_FULL ^(1 / (1 -sigma)))
				generate change_rGDP_FULL = (rGDP_FULL - rGDP_BLN) / rGDP_BLN * 100
			
		* Mantener índices de interés	
			keep country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
			order country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
				
	* Exportar los resultados en Excel
		export excel using "Exercises\1_RemovingSpecificBorder\Results\FULL_`file'.xls", firstrow(variables) replace
end

* II: Definir el escenario contrafactual
	* El escenario contrafactual consiste en eliminar la frontera internacional 
	* entre los países A y B restringiendo la variable de fronteras internacionales 
	* a cero para la frontera entre los países A y B, y asumiendo los efectos de 
	* las otras fronteras internacionales y las variables geográficas (DIST y CNTG ) 
	* permanece igual.
		* Construir los costos del comercio bilateral contrafáctico imponiendo 
		* las restricciones asociadas con el escenario contrafáctico
			generate INTL_BRDR_CFL = INTL_BRDR
				replace INTL_BRDR_CFL = 0 if (exporter == "ECU" & importer == "ARE") ///
				| (importer == "ECU" & exporter == "ARE") 
			generate tij_CFL = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR_CFL)
	
* III: Resolver el modelo contrafactual
	GEPPML_stepIII "part_A"

* IV: Recopilar, construir y reportar índices de interés.
	GEPPML_stepIV "part_A"
