#### MODELLING ALCOHOL INTAKE
#### MANAGEMENT OF THE DATA
library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)
library(patchwork)
library(scales) ##to create new lines for axis texts

#### LOAD all the needed files ####

indiv.1_4 <-
	read.csv(
		"ndns data/year 1-4/ndns_rp_yr1-4a_indiv.csv",
		header = T,
		 sep = "\t"
	)
daily.1_4 <-
	read.csv(
		"ndns data/year 1-4/ndns_rp_yr1-4a_dayleveldietarydata.csv",
		header = T,
		sep = "\t"
	)
persondiet.1_4 <-
	read.csv(
		"ndns data/year 1-4/ndns_rp_yr1-4a_personleveldietarydata.csv",
		header = T,
		sep = "\t"
	)


indiv.5_6 <-
	read.csv(
		"ndns data/year 5-6/ndns_rp_yr5-6a_indiv.csv",
	  header = T,
		sep = "\t"
	)
daily.5_6 <-
	read.csv(
		"ndns data/year 5-6/ndns_rp_yr5-6a_dayleveldietarydata.csv",
		header = T,
		sep = "\t"
	)
persondiet.5_6 <-
	read.csv(
		"ndns data/year 5-6/ndns_rp_yr5-6a_personleveldietarydata.csv",
		header = T,
		sep = "\t"
	)


indiv.7_8 <-
	read.csv(
		"ndns data/year 7-8/ndns_rp_yr7-8a_indiv.csv",
		header = T,
		sep = "\t"
	)
daily.7_8 <-
	read.csv(
		"ndns data/year 7-8/ndns_rp_yr7-8a_dayleveldietarydata.csv",
		header = T,
		sep = "\t"
	)
persondiet.7_8 <-
	read.csv(
		"ndns data/year 7-8/ndns_rp_yr7-8a_personleveldietarydata.csv",
		header = T,
		sep = "\t"
	)


indiv.9 <-
	read.csv(
		"ndns data/year 9/ndns_rp_yr9a_indiv.csv",
		header = T,
		sep = "\t"
	)
daily.9 <-
	read.csv(
		"ndns data/year 9/ndns_rp_yr9a_dayleveldietarydata.csv",
		header = T,
		sep = "\t"
	)
persondiet.9 <-
	read.csv(
		"ndns data/year 9/ndns_rp_yr9a_personleveldietarydata.csv",
		header = T,
		sep = "\t"
	)
#### BUILDING A UNIQUE DATA SET ####
#' is there some unidentified subject for the various stages?
## 1-4  
sum(is.na(indiv.1_4$seriali))
sum(is.na(persondiet.1_4$seriali))
## 5-6
sum(is.na(indiv.5_6$seriali))
sum(is.na(persondiet.5_6$seriali))
## 7-8
sum(is.na(indiv.7_8$seriali))
sum(is.na(persondiet.7_8$seriali))
## 9
sum(is.na(indiv.9$seriali))
sum(is.na(persondiet.9$seriali))

#' Are all the subjects in the dietary data matched by an instance in background data?
all(persondiet.1_4$seriali %in% indiv.1_4$seriali)
all(persondiet.5_6$seriali %in% indiv.5_6$seriali)
all(persondiet.7_8$seriali %in% indiv.7_8$seriali)
all(persondiet.9$seriali %in% indiv.9$seriali)

#' Write down the name of the individual variables to select in the model
choose_var <-
	c("seriali",
		"age",
		"Sex",
		"region",
		"eqvinc",
		"htval",
		"wtval",
		"bmival",
		"PregNowB",
		"dnnow",
		"dnoft3"
		)
#' check these names are valid in all the stages
index <- c("1_4","5_6","7_8","9")

sapply(index, function(x) {
	choose_var %in% colnames(eval(parse(text = paste("indiv", x, sep = "."))))
})
#' the variable names are the same in all the stages for background data
#' 
#' Create a data set comprising all the stages with the variables of interest
#' plus a restriction is made for the adult population 
#' (the target of the investigation) 

ndns <- data.frame(matrix(ncol = length(choose_var),
													nrow = 0,
													dimnames = list(NULL, choose_var)))

for(i in index){
	ndns <- ndns %>% add_row(eval(parse(text = paste("indiv",i, sep = "."))) %>% 
													 	select(all_of(choose_var))) %>% filter(age >= 18)
}
#' The variables representing the intake data collected in the follow-up study
#' must be also regarded and merged with the rest of the data set.
#' Additionally, the day of the week the follow-up dietary diet started is 
#' important to be included
#' 
#' First, check the names of the new variables to add match across all stages

sapply(index, function(x) {
	c("Alcoholg", "AlcoholpctotE") %in% 
		colnames(eval(parse(text = paste("persondiet", x, sep = "."))))
})

sapply(index, function(x) {
	"DayofWeek" %in% colnames(eval(parse(text = paste("daily", x, sep = "."))))
})


ndns <- merge(ndns,
							do.call(
								bind_rows,
								lapply(
									list(persondiet.1_4, persondiet.5_6, persondiet.7_8, persondiet.9),
									select,
									seriali,
									Alcoholg,
									AlcoholpctotE
								)
							),
							by = "seriali") %>%
	merge(
		do.call(bind_rows,
						lapply(
							list(daily.1_4, daily.5_6, daily.7_8, daily.9),
							select,
							seriali,
							DayofWeek
						)) %>% group_by(seriali) %>%
			mutate(DayofWeek = first(DayofWeek)) %>%
			distinct(seriali, .keep_all = T)
		,
		by = "seriali"
	)

str(ndns)

#' Check for duplicates thanks to seriali variable
ndns$seriali %>% duplicated() %>% sum()

#' Some variables are coded in disguise of categorical instances;
#' they will be transformed in factors
sort(unique(ndns$dnnow))  #subjects drinking nowadays (yes or no)
sort(unique(ndns$dnoft3)) #frequency of drinking last 12 months
sort(unique(ndns$region)) 
sort(unique(ndns$Sex))
sort(unique(ndns$PregNowB)) #subjects who are pregnant or breastfeeding

#' Sex and Pregnancy can be combined in a unique factor with 3 levels:
#' Male, Female: NP, Female: Pregnant.
ndns %>% group_by(Sex, PregNowB) %>% summarise(n = n())
#' Clearly in Sex: male = 1, female = 2;
#' PregNowB: pregnant = 2, Not Pregnant = -1.
#' 
#' Therefore a new variable can be identified by Sex + PregnowB:
#' male = 0, female: NP = 1, female: pregnant = 4.  

ndns <- mutate(
	ndns,
	region = factor(
		region,
		labels = c(
			"England: North",
			"England: Central/Midlands",
			"England: South",
			"Scotland",
			"Wales",
			"Northern Ireland"
		)
	),
	sex = factor(Sex,
							 labels = c("Male", "Female")),
	pregnant = factor(PregNowB,
										labels = c("NP", "Pregnant")),
	sex.preg = factor(
		Sex + PregNowB,
		levels = c(0, 1, 4),
		labels = c("Male", "Female: NP", "Female: Pregnant")
	),
	start_day = factor(
		DayofWeek,
		levels = c(
			"Monday",
			"Tuesday",
			"Wednesday",
			"Thursday",
			"Friday",
			"Saturday",
			"Sunday"
		),
		labels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
	),
	drink.binary = factor(
		dnnow,
		levels = c(1, 2),
		labels = c("Drinker", "Non Drinker")
	),
	drink.ordinal =	forcats::fct_rev(factor(
		dnoft3,
		levels = c(1:8),
		labels = c(
			"Almost every day",
			"5-6 per wk",
			"3-4 per wk",
			"1-2 per wk",
			"1-2 per mo",
			"1 every 2 months",
			"1-2 per yr",
			"Not at all/Non-Drinker"
		),
		ordered = T
	)),
	.keep = "unused"
)

summary(ndns)
summary(ndns %>% select(where(is.numeric)), digits = 8)
#'Check missing data for continuous variables
apply(ndns[,c(2,4:9)], 2, function(x){sum(is.na(x))})

#' From the summary, something odd happens for eqvinc, htval, wtval, bmival
#' because they attain -1 values.
#' 
#### INVALID DATA ####
ndns %>% filter(wtval < 0) %>% select(wtval) %>% table()
ndns %>% filter(htval < 0) %>% select(htval) %>% table()
ndns %>% filter(bmival < 0) %>% select(bmival) %>% table()
ndns %>% filter(eqvinc < 0) %>% select(eqvinc) %>% table()

#' Transform them in missing values otherwise they would interfere with 
#' the regression models

ndns <- mutate(
	ndns,
	weight = ifelse(wtval < 0, NA, wtval) ,
	height = ifelse(htval < 0, NA, htval) ,
	BMI = ifelse(bmival < 0, NA, bmival) ,
	eq_income = ifelse(eqvinc < 0, NA, eqvinc/(1e3)),
	.keep = "unused"
)

#' Equivalised Income was scaled in order to consider it as thousands of pounds

#### TABLES AND SUMMARIES ####
#' CONTINGENCY TABLES 

##' Sex and Pregnancy status
ndns %>% group_by(sex.preg) %>% summarise (count = n())
##' Starting day of the week for the follow-up study 
ndns %>% group_by(start_day) %>% summarise(count = n()) 
##' regional distribution of the units
ndns %>%
	group_by(region) %>% summarise(count = n()) %>%
	mutate(freq = count / sum(count))
##' Drinker/Non-Dr vs frequency of drinking
ndns %>% select(drink.binary, drink.ordinal) %>% table(useNA = "ifany")


#' Summary statistics of alcohol consumption
summary(ndns$Alcoholg)
ndns %>% select(Alcoholg) %>% filter(Alcoholg == 0) %>% 
	summarise(freq_zeroes = n())/nrow(ndns)

summary(ndns$AlcoholpctotE)
ndns %>% select(AlcoholpctotE) %>% filter(AlcoholpctotE == 0) %>% 
	summarise(freq_zeroes = n())/nrow(ndns)

##' summary statistics of AI based on the starting day of the study
ndns %>% group_by(Start_Day = start_day) %>% summarise(
	mean = mean(Alcoholg),
	median = median(Alcoholg),
	min = min(Alcoholg),
	max = max(Alcoholg)
)
##' summary statistics of AI based on the frequency of drinking
ndns %>% group_by(Frequency_Drinking = drink.ordinal) %>% summarise(
	Count = n(),
	mean = mean(Alcoholg),
	median = median(Alcoholg),
	min = min(Alcoholg),
	max = max(Alcoholg)
)

##' summary statistics of AI based on drinking status
ndns %>% group_by(Drinking_Status = drink.binary) %>% summarise(
	Count = n(),
	mean = mean(Alcoholg),
	median = median(Alcoholg),
	min = min(Alcoholg),
	max = max(Alcoholg)
)

#' Contingency tables for DRINKING STATUS
##' with REGION
ndns %>% group_by(drink.binary, region) %>% 
	summarise(n = n()) %>% spread(drink.binary, n)
##' with SEX.PREG
ndns %>% group_by(drink.binary, sex.preg) %>% 
	summarise(n = n()) %>% spread(drink.binary, n)

#' Contingency tables for DRINKING FREQUENCY
##' with REGION
ndns %>% group_by(drink.ordinal, region) %>% 
	summarise(n = n()) %>% spread(drink.ordinal, n)
##' with SEX.PREG
ndns %>% group_by(drink.ordinal, sex.preg) %>% 
	summarise(n = n()) %>% spread(drink.ordinal, n)

#### PLOTS for exploratory analysis ####

#' DRINK BINARY RELATIONSHIP WITH NUMERIC PREDICTORS
ndns %>% drop_na(drink.binary) %>%
	select(age, BMI, eq_income, drink.binary) %>%
	pivot_longer(cols = !drink.binary) %>%
	ggplot(aes(y = drink.binary, x = value)) +
	geom_boxplot(colour = "blue4") +
	facet_wrap( ~ name, nrow = 3,
						 scales = "free",
						 labeller = labeller(
						 	name = c(
						 		"age" = "AGE (in years)",
						 		"BMI" = "BMI (in kg/m^2)",
						 		"eq_income" = "EQ. INCOME (in £)"
						 	)
						 )) +
	labs(x ="", y ="", 
			 title = "BOXPLOT OF THE COVARIATES BASED ON DRINKING STATUS") +
	theme_bw() +
	theme(
		strip.text = element_text(
			face = "plain",
			colour = "#000059",
			size = 11.7),
		strip.background = element_rect(
			fill = "#FFFFE9"),
		axis.title = element_text(
			face = "italic",
			colour = "dodgerblue4",
			size = 12),
		axis.text = element_text(
			colour = "dodgerblue4", 
			size = 10.5),
		plot.title = element_text(
			colour = "#000059", 
			face = "bold", 
			size = 12.5)
	)

# DRINK BINARY RELATIONSHIPS WITH CATEOGORICAL PREDICTORS
ndns %>% drop_na(drink.binary) %>% 
	ggplot(aes(x = drink.binary, y = region)) + geom_count() +
	theme_light()

ndns %>% drop_na(drink.binary) %>% 
	ggplot(aes(x = drink.binary, y = sex.preg)) + geom_count() +
	theme_light()

#' DRINK ORDINAL BARCHART
ggplot(ndns %>% filter(!is.na(drink.ordinal))) +
	geom_bar(aes(y = drink.ordinal,
							 fill = drink.ordinal),
					 show.legend = F) +
	scale_fill_brewer(
		palette = "YlOrRd",
		direction = -1) +
	theme_minimal() +
	theme(axis.text.y = element_text(size = 10,
																	 face = "italic",
																	 colour = "gray23"),
				title = element_text(colour = "black",
														 hjust = 0.5)) +
	labs(title = "DRINKING FREQUENCY",
			 subtitle = "Count Data for the answers regarding the past 12 month from the study",
			 x = "Count", y = "")

#' MEAN DIFFERENCES OF NUMERIC VARIABLES for categories of DRINK ORDINAL
mean.ord <-
	ndns %>% drop.na(drink.ordinal) %>% group_by(drink.ordinal) %>%
	summarise(
		AGE = mean(age, na.rm = T),
		BMI = mean(BMI, na.rm = T),
		INCOME = mean(eq_income, na.rm = T)
	)
mean.ord
ggplot(pivot_longer(mean.ord, cols = c(AGE, BMI, INCOME)),
			 aes(x = drink.ordinal, y = value, group = 1)) +
	geom_line(colour = "#00008c",show.legend = F) + 
	geom_point(aes(colour = drink.ordinal), 
						 size = ndns %>% drop_na(drink.ordinal) %>%
								 	group_by(drink.ordinal) %>% 
								 	summarise(n = n()/200) %>% pull(n) %>% rep(each = 3),
						 show.legend = F) +
	scale_colour_brewer(palette = "YlOrRd", direction = -1) +
	facet_grid(name ~ ., 
						 scales = "free",
						 labeller = labeller(
						 	name = c(
						 		"AGE" = "AGE (in years)",
						 		"BMI" = "BMI (in kg/m^2)",
						 		"INCOME" = "EQ. INCOME (in £)"
						 	)
						 )) +
	scale_x_discrete(labels = label_wrap(10)) +
	labs(x = "Frequency of drinking", y = "Mean values",
			 title = "PLOT OF THE VARIABLE MEANS FOR FREQUENCY CATEGORIES") +
	theme_bw() +
	theme(
		strip.text = element_text(
			face = "plain",
			colour = "#000059",
			size = 11),
		strip.background = element_rect(
			fill = "#FFFFE9"),
		axis.title = element_text(
			face = "italic",
			colour = "dodgerblue4",
			size = 12),
		axis.text = element_text(
			colour = "dodgerblue4", 
			size = 10.5),
		plot.title = element_text(
			colour = "#000059", 
			face = "bold", 
			size = 12.5)
	)


#' BOXPLOTS DRINKING ORDINAL showing differences for SEX and COUNTRY
ndns %>% drop_na(drink.ordinal) %>% 
	select(drink.ordinal, region, sex, BMI, age, eq_income) %>% 
	pivot_longer(cols = !c(drink.ordinal, sex, region)) %>% 
	ggplot(aes(drink.ordinal,value)) +
	geom_boxplot(aes(colour = region)) +
	facet_grid(name ~ sex, 
						 scales = "free",
						 labeller = labeller(
						 	name = c(
						 		"age" = "AGE (in years)",
						 		"BMI" = "BMI (in kg/m^2)",
						 		"eq_income" = "EQ. INCOME (in £)"
						 		)
						 )) +
	scale_x_discrete(labels = label_wrap(10)) +
	scale_colour_brewer(palette = "Dark2", name = "") +
	labs(title = "BOX PLOTS FOR VARIABLES ACROSS DRINKING FREQUENCY LEVELS",
			 subtitle = "Differentiated by Sex and Region",
			 x = "") +
	theme_bw() +
	theme(legend.position = "bottom") 

ggpairs(person.adult[, c(2:3, 6:9, 12)])


#' DRINK ORDINAL vs PREGNANCY
ndns %>% drop_na(drink.ordinal) %>% 
	ggplot(aes(drink.ordinal, fill = pregnant)) + 
	geom_bar(position = "dodge", colour = "deepskyblue4") + 
	scale_fill_brewer(name = "", palette = "Blues",
										labels = c("Non Pregnant","Pregnant / Breastfeeding")) +
	scale_x_discrete(labels = label_wrap(10)) +
	labs(title = "DISTRIBUTION OF DRINKING PERIODICITY", 
			 subtitle = "Based on self-declaration",
			 x = "Frequency drink alcohol in past 12 months", y = "Count") +
	theme_light() +
	theme(axis.text = element_text(colour = "dodgerblue4", size = 10),
				axis.title = element_text(colour = "dodgerblue4", face = "italic"),
				plot.title = element_text(colour = "#000059", face = "bold", size = 14),
				plot.subtitle = element_text(colour = "#000059", size = 12),
				legend.position = "bottom",
				legend.text = element_text(colour = "#00008f", size = 10))

	

#' AI vs SEX, REGION, STARTING DAY BOXPLOTS
AI.cat <- select(ndns, Alcoholg, sex.preg, region, start_day)

AI.cat %>% pivot_longer(cols = !Alcoholg) %>%
	ggplot(aes(factor(value), Alcoholg)) +
	geom_boxplot(colour = "blue4") +
	theme_bw() +
	scale_x_discrete(labels = label_wrap(10),
									 guide = guide_axis(n.dodge = 2)) +
	labs(x = "",
			 y = "Alcohol Intake (in grams)",
			 title = "AI BOXPLOTS WITH CATEGORICAL VARIABLES") +
	theme(
		strip.text.x = element_text(
			face = "plain",
			colour = "#000059",
			size = 11),
		strip.background = element_rect(
			fill = "#FFFFE9"),
		axis.title = element_text(
			face = "italic",
			colour = "dodgerblue4",
			size = 12),
		axis.text = element_text(
			colour = "dodgerblue4", 
			size = 10),
		plot.title = element_text(
			colour = "#000059", 
			face = "bold", 
			size = 12.5)
	) +
	facet_wrap(~ name,
						 nrow = 1,
						 scales = "free_x",
						 labeller = labeller(
						 	name = c(
						 		"region" = "REGION",
						 		"sex.preg" = "SEX WITH PREGNANCY STATUS",
						 		"start_day" = "STARTING DAY OF THE FOLLOW-UP"
						 	)
						 )) +
	stat_summary(fun.data = function(x){
		c(y = -20, label = length(x) )
	}, geom = "text", colour = "blue4", size = 3.2)


#' AI vs AGE, BMI, EQUIVALISED INCOME
AI.num <- select(ndns, Alcoholg, age, BMI, eq_income, sex)

AI.num %>% pivot_longer(cols = !c(Alcoholg, sex)) %>%
	ggplot(aes(x = value, y = Alcoholg, colour = sex)) +
	geom_jitter() +
	scale_colour_manual( name = NULL, values = c("#00008c","#8c8c00") ) +
	facet_wrap(~ name,
						 nrow = 1,
						 scales = "free_x",
						 labeller = labeller(
						 	name = c(
						 		"age" = "AGE (in years)",
						 		"BMI" = "BMI (in kg/m^2)",
						 		"eq_income" = "EQUIVALISED INCOME (in pounds)"
						 	)
						 )) +
	labs(x = NULL, 
			 y = "Alcohol Intake (g)", 
			  title = "AI SCATTERPLOTS WITH QUANTITATIVE VARIABLES")+
	theme_bw() +
	theme(
		strip.text.x = element_text(
			face = "plain",
			colour = "#000059",
			size = 11
		),
		strip.background = element_rect(fill = "#FFFFE9"),
		axis.title = element_text(
			face = "italic",
			colour = "dodgerblue4",
			size = 11
		),
		axis.text = element_text(colour = "dodgerblue4", size = 8.5),
		plot.title = element_text(colour = "#000059", face = "bold", size = 12.5),
		legend.position = "bottom"
	)

#' RELATIONSHIPS BETWEEN OUTCOME VARIABLES

ndns %>% drop_na(drink.ordinal, drink.binary) %>% 
	ggplot(aes(y = Alcoholg, x = drink.ordinal)) +
	geom_boxplot() +
	scale_x_discrete(labels = label_wrap(10)) +
	facet_grid(drink.binary~.) + 
	theme_bw()

##' BOXPLOTS OF AI for drink.ordinal
ggplot(ndns %>% filter(!is.na(drink.ordinal))) +
	geom_boxplot( aes(y = Alcoholg,x = drink.ordinal),
								show.legend = F, colour = "brown4")+
	theme_minimal() +
	labs(y = "AI (g)", x = "")

##' Frequency polygons of AI distinguished for drink.binary and drink.ordinal

ndns %>% drop_na(drink.ordinal) %>%
	ggplot(aes(Alcoholg, colour = drink.binary)) +
	geom_freqpoly(binwidth = 13,
								linewidth = 0.5) +
	theme_light() +
	scale_colour_discrete("") + 
	labs(
		title = "Alcohol Intake (AI)",
		subtitle = "Density distibution of AI including zeroes",
		x = "AI (g)",
		caption = "recorded in the 4-days follow-up for NDNS"
	)
	

ndns %>% drop_na(drink.ordinal) %>% ggplot(aes(Alcoholg)) +
	geom_freqpoly(aes(y = after_stat(density),colour = drink.ordinal),
								binwidth = 7,
								linewidth = 0.7,
								alpha = 0.65) +
	theme_light()



#' DISTRIBUTION OF AI (in grams)
g <- ggplot(ndns, aes(Alcoholg))
AIdens_0 <- g +
	geom_histogram(
		aes(y = after_stat(density)),
		fill = "khaki4",
		colour = 1,
		alpha = 0.4,
		binwidth = 5
	) +
	geom_density(
		colour = "red",
		fill =  "peru",
		alpha = 0.5,
		linewidth = 0.8,
		linetype = 3
	) +
	theme_light() +
	labs(
		title = "Alcohol Intake Distribution",
		subtitle = "Density including zeroes",
		x = "AI (g)",
		caption = "recorded in the 4-days follow-up for NDNS"
	) +
	theme(axis.title = 
					element_text(face = "italic", colour = "dodgerblue4", size = 11),
		axis.text = element_text(colour = "dodgerblue4", size = 8.5),
		plot.title = element_text(colour = "#000059", face = "bold", size = 12.5),
		plot.subtitle = element_text(colour = "#000059", face = "plain", size = 12.5)
		)

##' histogram without overriding density
g +
	geom_histogram(
		fill = "khaki4",
		colour = 1,
		alpha = 0.4,
		binwidth = 8
	) +
	theme_light()

##' DISTRIBUTION OF AI, filtering non-zero amounts
g_no0 <- ggplot(ndns %>% filter(Alcoholg != 0),
								aes(Alcoholg))
AIdens_no0 <- g_no0 +
	geom_histogram(
		aes(y = after_stat(density)),
		fill = "khaki",
		colour = 1,
		alpha = 0.4,
		binwidth = 4
	) +
	geom_density(
		colour = "red",
		fill =  "sandybrown",
		alpha = 0.55,
		linewidth = 0.8,
		linetype = 3
	) +
	geom_rug(aes(y = NULL)) +
	theme_light() +
	labs(
		title = "Alcohol Intake Distribution",
		subtitle = "Density for non-zero amounts",
		x = "AI (g)",
		caption = "recorded in the 4-days follow-up for NDNS"
	) +
	theme(axis.title = element_text(
		face = "italic",
		colour = "dodgerblue4",
		size = 11),
		axis.text = element_text(
			colour = "dodgerblue4", 
			size = 8.5),
		plot.title = element_text(colour = "#000059", face = "bold", size = 12.5),
		plot.subtitle = element_text(colour = "#000059", face = "plain", size = 12.5)
	)

##' histogram + rugs of AI, for non-zero amounts

AIhist_rug <- g_no0 +	
	geom_histogram(
	fill = "darkseagreen3",
	alpha = 0.7,
	colour = 1,
	binwidth = 10
	) +
	geom_rug(aes(y = NULL)) +
	theme_light() +
	labs(
		subtitle = "Distibution of AI for non-zero amounts",
		x = "AI (g)",
		y = "Count"
	)

##' dotplot OF AI non-zero amounts, to highlight outlying values
AIdotp <-
	g_no0 + geom_dotplot(
		binwidth = 4,
		colour = "darkslategray",
		fill = "darkseagreen1"
	)  +
	theme_minimal() +
	labs(
		subtitle = "Dot Plot of AI for non-zero amounts",
		x = "AI (g)"
	) +
	scale_y_continuous(NULL, breaks = NULL)


AIhist_rug + AIdotp + plot_layout(ncol = 1)

AIhist_rug + labs(title = "Alcohol Intake (AI)",
									caption = "recorded in the 4-days follow-up study for NDNS")

AIdotp + labs(title = "Alcohol Intake (AI)",
							caption = "recorded in the 4-days follow-up study for NDNS")


AIdens_0 + AIdens_no0 + plot_layout(ncol = 1)


#' HISTOGRAM of LOG-AI
ndns %>% filter(Alcoholg>0) %>% mutate(Alcoholg = log(Alcoholg + 1)) %>% 
	ggplot(aes(Alcoholg)) + geom_histogram(	fill = "darkseagreen3",
																					alpha = 0.7,
																					colour = 1,
																					bins = 50) +
	theme_light()



#### IMPUTATION MODEL ####
library(mice)  # multiple imputation
library(ggmice)  # improve visualisation of missing patterns

md.pattern(ndns, rotate.names = T)

plot_pattern(ndns %>% select(!c(sex, pregnant, AlcoholpctotE, seriali)), 
						 rotate = T) + 
	ggtitle("Missing Values Pattern in NDNS data") +
	theme(
		axis.text.x.bottom = element_text(angle = 0),
		axis.text = element_text(colour = "dodgerblue3",
														 size = 10),
		axis.title = element_text(colour = "dodgerblue4"),
		plot.title = element_text(hjust = 0.5,
															colour = "#000059",
															face = "bold"))
#' Comment: 
#' clearly, BMI is missing wherever either height or weight is missing, 
#' therefore, we impute data for those two variables and then re-compute bmi with
#' the well known formula

md.pattern(ndns %>% select(- BMI), rotate.names = T)

p <- md.pairs(ndns %>% select(-c(BMI,seriali, start_day)))
p
##' INBOUND STATISTIC
round(100 * (p$mr/(p$mr+p$mm))[8:12,], 2)
#' In each column there are the % of usable cases for that particular variable
#' to impute the variables shown in the rows.
#' It is subsetted in order to show just the relevant rows, 
#' i.e. the variables showing NA's.

##' OUTBOUND STATISTIC 
round(100 * (p$rm/(p$rm+p$rr))[,8:12], 2)
#' The entries show the % of units where the row variable is observed while
#' the column variable is not. If a particular variable has always observed data
#' while another one is NA, the displayed value should correspond to the 
#' proportion of NA's attained by the other one.

##' INFLUX / OUTFLUX
round(100 * flux(ndns %>% select(-c(BMI,seriali, start_day)))[,1:3], 2)
fluxplot(ndns %>% select(-c(BMI,seriali, start_day)))


##' START THE IMPUTATION
meth <- make.method(ndns)
meth["BMI"] <- "~I(100 * 100 * weight/(height)^2)"
pred <- quickpred(ndns, exclude = c("seriali","sex","pregnant","start_day"))
ndns.imp <- mice(ndns, meth = meth, pred = pred, print = F, seed = 20)

ndns_comp <- complete(ndns.imp, action = "long", include = T)

##' Diagnostic of imputation model
bwplot(ndns.imp, layout = c(2,2))
densityplot(ndns.imp, layout = c(2,2))


save(ndns, ndns.imp, ndns_comp, file = "model data.RData")


# sapply(X = choose_var, FUN = function(x){which(x == colnames(indiv.5_6))})
# choose_var[5] 
# # in years 5-6 variable is called   "ethgrp5"
# # check also in categorical variables the levels have the same meaning, helping 
# # yourself with the file with explanation of variables in the data tabs
# 
# i <- which(colnames(indiv.5_6)=="ethgrp5")
# colnames(indiv.5_6)[i] <- choose_var[5] 
# person.comp <- rbind(person.comp,indiv.5_6[,choose_var])
# 
# 
# lapply(X = choose_var, FUN = function(x){which(x == colnames(indiv.7_8))})
# i <- which(colnames(indiv.7_8)=="ethgrp5")
# colnames(indiv.7_8)[i] <- choose_var[5] 
# person.comp <- rbind(person.comp,indiv.7_8[,choose_var])
# 
# lapply(X = choose_var, FUN = function(x){which(x == colnames(indiv.9))})
# i <- which(colnames(indiv.9)=="ethgrp5")
# colnames(indiv.9)[i] <- choose_var[5] 
# person.comp <- rbind(person.comp,indiv.9[,choose_var])

# person.adult$ethgr5 <-
# 	factor(person.adult$ethgr5,
# 				 levels = 1:5,
# 				 labels = c(
# 				 	"White",
# 				 	"Mixed",
# 				 	"Black",
# 				 	"Asian",
# 				 	"Any other"
# 				 ))
# 
# table(person.adult$ethgr5, useNA = "ifany")

