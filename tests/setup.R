if ( .Platform$OS.type == 'windows' ) memory.limit( 256000 )

library(lodown)
lodown( "pns" , output_dir = file.path( getwd() ) )
library(lodown)
# examine all available PNS microdata files
pns_cat <-
	get_catalog( "pns" ,
		output_dir = file.path( getwd() ) )

# download the microdata to your local computer
lodown( "pns" , pns_cat )

options( survey.lonely.psu = "adjust" )

library(survey)

pns_design <- 
	readRDS( 
		file.path( 
			getwd() , 
			"2013 long questionnaire survey design.rds" ) 
		)
pns_design <- 
	update( 
		pns_design , 

		one = 1 ,
		
		health_insurance = as.numeric( i001 == 1 )
	)
sum( weights( pns_design , "sampling" ) != 0 )

svyby( ~ one , ~ uf , pns_design , unwtd.count )
svytotal( ~ one , pns_design )

svyby( ~ one , ~ uf , pns_design , svytotal )
svymean( ~ w00101 , pns_design , na.rm = TRUE )

svyby( ~ w00101 , ~ uf , pns_design , svymean , na.rm = TRUE )
svymean( ~ c006 , pns_design )

svyby( ~ c006 , ~ uf , pns_design , svymean )
svytotal( ~ w00101 , pns_design , na.rm = TRUE )

svyby( ~ w00101 , ~ uf , pns_design , svytotal , na.rm = TRUE )
svytotal( ~ c006 , pns_design )

svyby( ~ c006 , ~ uf , pns_design , svytotal )
svyquantile( ~ w00101 , pns_design , 0.5 , na.rm = TRUE )

svyby( 
	~ w00101 , 
	~ uf , 
	pns_design , 
	svyquantile , 
	0.5 ,
	ci = TRUE ,
	keep.var = TRUE ,
	na.rm = TRUE
)
svyratio( 
	numerator = ~ w00203 , 
	denominator = ~ w00101 , 
	pns_design ,
	na.rm = TRUE
)
sub_pns_design <- subset( pns_design , atfi04 == 1 )
svymean( ~ w00101 , sub_pns_design , na.rm = TRUE )
this_result <- svymean( ~ w00101 , pns_design , na.rm = TRUE )

coef( this_result )
SE( this_result )
confint( this_result )
cv( this_result )

grouped_result <-
	svyby( 
		~ w00101 , 
		~ uf , 
		pns_design , 
		svymean ,
		na.rm = TRUE 
	)
	
coef( grouped_result )
SE( grouped_result )
confint( grouped_result )
cv( grouped_result )
degf( pns_design )
svyvar( ~ w00101 , pns_design , na.rm = TRUE )
# SRS without replacement
svymean( ~ w00101 , pns_design , na.rm = TRUE , deff = TRUE )

# SRS with replacement
svymean( ~ w00101 , pns_design , na.rm = TRUE , deff = "replace" )
svyciprop( ~ health_insurance , pns_design ,
	method = "likelihood" , na.rm = TRUE )
svyttest( w00101 ~ health_insurance , pns_design )
svychisq( 
	~ health_insurance + c006 , 
	pns_design 
)
glm_result <- 
	svyglm( 
		w00101 ~ health_insurance + c006 , 
		pns_design 
	)

summary( glm_result )
library(srvyr)
pns_srvyr_design <- as_survey( pns_design )
pns_srvyr_design %>%
	summarize( mean = survey_mean( w00101 , na.rm = TRUE ) )

pns_srvyr_design %>%
	group_by( uf ) %>%
	summarize( mean = survey_mean( w00101 , na.rm = TRUE ) )

