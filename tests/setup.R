# cheer the ministry!
# with each caipirinha, or
# fail sex life module
dictionary_tf <- tempfile()

dictionary_url <-
	"https://ftp.ibge.gov.br/PNS/2019/Microdados/Documentacao/Dicionario_e_input_20220530.zip"

download.file( dictionary_url , dictionary_tf , mode = 'wb' )

dictionary_files <- unzip( dictionary_tf , exdir = tempdir() )

sas_fn <- grep( '\\.sas$' , dictionary_files , value = TRUE )

sas_lines <- readLines( sas_fn , encoding = 'latin1' )
sas_start <- grep( '@00001' , sas_lines )

sas_end <- grep( ';' , sas_lines )

sas_end <- sas_end[ sas_end > sas_start ][ 1 ]

sas_lines <- sas_lines[ seq( sas_start , sas_end - 1 ) ]

# remove SAS comments
sas_lines <- gsub( "\\/\\*(.*)" , "" , sas_lines )

# remove tabs, multiple spaces and spaces at the end of each string
sas_lines <- gsub( "\t" , " " , sas_lines )
sas_lines <- gsub( "( +)" , " " , sas_lines )
sas_lines <- gsub( " $" , "" , sas_lines )

sas_df <- 
	read.table( 
		textConnection( sas_lines ) , 
		sep = ' ' , 
		col.names = c( 'position' , 'column_name' , 'length' ) ,
		header = FALSE 
	)

sas_df[ , 'character' ] <- grepl( '\\$' , sas_df[ , 'length' ] )

sas_df[ , 'position' ] <- as.integer( gsub( "\\@" , "" , sas_df[ , 'position' ] ) )

sas_df[ , 'length' ] <- as.integer( gsub( "\\$" , "" , sas_df[ , 'length' ] ) )

stopifnot( 
	sum( sas_df[ , 'length' ] ) == 
	( sas_df[ nrow( sas_df ) , 'position' ] + sas_df[ nrow( sas_df ) , 'length' ] - 1 ) 
)
this_tf <- tempfile()

this_url <-
	"https://ftp.ibge.gov.br/PNS/2019/Microdados/Dados/PNS_2019_20220525.zip"

download.file( this_url , this_tf , mode = 'wb' )
library(readr)

pns_tbl <-
	read_fwf(
		this_tf ,
		fwf_widths( 
			widths = sas_df[ , 'length' ] , 
			col_names = sas_df[ , 'column_name' ] 
		) ,
		col_types = 
			paste0( ifelse( sas_df[ , 'character' ] , "c" , "d" ) , collapse = '' )
	)

pns_df <- data.frame( pns_tbl )

names( pns_df ) <- tolower( names( pns_df ) )

pns_df[ , 'one' ] <- 1
# pns_fn <- file.path( path.expand( "~" ) , "PNS" , "this_file.rds" )
# saveRDS( pns_df , file = pns_fn , compress = FALSE )
# pns_df <- readRDS( pns_fn )
library(survey)

options( survey.lonely.psu = "adjust" )

pns_prestratified_design <-
	svydesign(
		id = ~ upa_pns ,
		strata = ~v0024 ,
		data = subset( pns_df , !is.na( v0028 ) ) ,
		weights = ~v0028 ,
		nest = TRUE
	)

popc.types <-
	data.frame(
		v00283 = as.character( unique( pns_df[ , 'v00283' ] ) ) ,
		Freq = as.numeric( unique( pns_df[ , 'v00282' ] ) )
	)

popc.types <- popc.types[ order( popc.types[ , 'v00283' ] ) , ]

pns_design <-
	postStratify(
		pns_prestratified_design ,
		strata = ~v00283 ,
		population = popc.types
	)

pns_design <- 
	update( 
		pns_design , 

		medical_insurance = ifelse( i00102 %in% 1:2 , as.numeric( i00102 == 1 ) , NA ) ,
		
		uf_name =
		
			factor(
			
				as.numeric( v0001 ) ,
				
				levels = 
					c(11L, 12L, 13L, 14L, 15L, 16L, 17L, 21L, 22L, 23L, 24L, 25L, 
					26L, 27L, 28L, 29L, 31L, 32L, 33L, 35L, 41L, 42L, 43L, 50L, 51L, 
					52L, 53L) ,
					
				labels =
					c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", 
					"Tocantins", "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", 
					"Paraiba", "Pernambuco", "Alagoas", "Sergipe", "Bahia", "Minas Gerais", 
					"Espirito Santo", "Rio de Janeiro", "Sao Paulo", "Parana", "Santa Catarina", 
					"Rio Grande do Sul", "Mato Grosso do Sul", "Mato Grosso", "Goias", 
					"Distrito Federal")
					
			) ,

		age_categories = factor( 1 + findInterval( c008 , seq( 5 , 90 , 5 ) ) ) ,

		male = as.numeric( v006 == 1 )
		
	)
sum( weights( pns_design , "sampling" ) != 0 )

svyby( ~ one , ~ uf_name , pns_design , unwtd.count )
svytotal( ~ one , pns_design )

svyby( ~ one , ~ uf_name , pns_design , svytotal )
svymean( ~ e01602 , pns_design , na.rm = TRUE )

svyby( ~ e01602 , ~ uf_name , pns_design , svymean , na.rm = TRUE )
svymean( ~ c006 , pns_design )

svyby( ~ c006 , ~ uf_name , pns_design , svymean )
svytotal( ~ e01602 , pns_design , na.rm = TRUE )

svyby( ~ e01602 , ~ uf_name , pns_design , svytotal , na.rm = TRUE )
svytotal( ~ c006 , pns_design )

svyby( ~ c006 , ~ uf_name , pns_design , svytotal )
svyquantile( ~ e01602 , pns_design , 0.5 , na.rm = TRUE )

svyby( 
	~ e01602 , 
	~ uf_name , 
	pns_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE , na.rm = TRUE
)
svyratio( 
	numerator = ~ p00104 , 
	denominator = ~ p00404 , 
	pns_design ,
	na.rm = TRUE
)
sub_pns_design <- subset( pns_design , p035 %in% 3:7 )
svymean( ~ e01602 , sub_pns_design , na.rm = TRUE )
this_result <- svymean( ~ e01602 , pns_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ e01602 , 
		~ uf_name , 
		pns_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pns_design )
svyvar( ~ e01602 , pns_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ e01602 , pns_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ e01602 , pns_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ medical_insurance , pns_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( e01602 ~ medical_insurance , pns_design )
svychisq( 
	~ medical_insurance + c006 , 
	pns_design 
)
glm_result <- 
	svyglm( 
		e01602 ~ medical_insurance + c006 , 
		pns_design 
	)

summary( glm_result )
total_renda <- svytotal( ~ e01602 , pns_design , na.rm = TRUE )
stopifnot( round( coef( total_renda ) , 0 ) == 213227874692 )
stopifnot( round( SE( total_renda ) , 0 ) == 3604489769 )
library(srvyr)
pns_srvyr_design <- as_survey( pns_design )
pns_srvyr_design %>%
	summarize( mean = survey_mean( e01602 , na.rm = TRUE ) )

pns_srvyr_design %>%
	group_by( uf_name ) %>%
	summarize( mean = survey_mean( e01602 , na.rm = TRUE ) )
