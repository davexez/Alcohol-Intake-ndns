### MODELLING FINAL 
load("model data.Rdata")
library(mice) # deal with multiple imputations
library(dplyr)   # simplifies operation on datasets
library(car)  # vif and residual plots
library(tidyr) # manipulate dataset in useful way for multiple visualization
library(ggplot2) #visualisations
library(rms) # fit ordinal regression model with imputed data set
library(Hmisc) # diagnostics for ORM
library(twopartm)


#### LOGISTIC REGRESSION MODEL ####
	
log.mod_imp <-
	with(
		ndns.imp %>% filter(!is.na(ndns$drink.binary)),
		glm(drink.binary ~
					age *
					BMI +
					eq_income +
					sex.preg * age +
					region,
				family = "binomial")
	)

#' Diagnostics
##' start by linearity (checked for numeric predictors)
log.diagnostic <- list()
for(i in 1:5){
	mydata <-
		ndns_comp %>% 
		filter(.imp == i) %>% filter(!is.na(ndns$drink.binary)) %>% 
		select(age, BMI, eq_income)
	probs <- log.mod_imp$analyses[[i]]$fitted.values
	mydata <- mydata %>%
		mutate(logit = log(probs / (1 - probs))) %>% pivot_longer(cols = -logit)
	log.diagnostic[[i]] <- ggplot(mydata, aes(logit, value)) +
		geom_point(size = 0.5, alpha = 0.5) +
		geom_smooth(method = "loess") +
		theme_bw() +
		facet_wrap( ~ name, scales = "free_y")
}
log.diagnostic[[1]] +
	log.diagnostic[[2]] +
	log.diagnostic[[3]] +
	log.diagnostic[[4]] +
	log.diagnostic[[5]] +
	patchwork::plot_layout(ncol = 1)

##' Multicollinearity
vif(update(log.mod_imp$analyses[[1]],.~.-age:BMI-age:sex.preg))
vif(update(log.mod_imp$analyses[[2]],.~.-age:BMI-age:sex.preg))
vif(update(log.mod_imp$analyses[[3]],.~.-age:BMI-age:sex.preg))
vif(update(log.mod_imp$analyses[[4]],.~.-age:BMI-age:sex.preg))
vif(update(log.mod_imp$analyses[[5]],.~.-age:BMI-age:sex.preg))

##' Outliers
par(mfrow = c(3,2))
plot(log.mod_imp$analyses[[1]], which = 4)
plot(log.mod_imp$analyses[[2]], which = 4)
plot(log.mod_imp$analyses[[3]], which = 4)
plot(log.mod_imp$analyses[[4]], which = 4)
plot(log.mod_imp$analyses[[5]], which = 4)


#' ESTIMATES of the model
pool_log <- pool(log.mod_imp)
summary(pool_log)

#' TEST SIGNIFICANCE OF INTERACTION
##' just main effects
D3(log.mod_imp,
	 with(
	 	ndns.imp %>% filter(!is.na(ndns$drink.binary)),
	 	glm(
	 		drink.binary ~
	 			age +
	 			BMI +
	 			eq_income +
	 			sex.preg +
	 			region,
	 		family = "binomial"
	 	)
	 ))


##' significance of interaction between age and sex.preg
D3(log.mod_imp,
	 with(
	 	ndns.imp %>% filter(!is.na(ndns$drink.binary)),
	 	glm(drink.binary ~
	 				age *
	 				BMI +
	 				eq_income +
	 				sex.preg +
	 				region,
	 			family = "binomial")
	 ))

##' significance of REGION variable
D3(log.mod_imp,
	 with(
	 	ndns.imp %>% filter(!is.na(ndns$drink.binary)),
	 	glm(drink.binary ~
	 				age *
	 				BMI +
	 				eq_income +
	 				sex.preg*age,
	 			family = "binomial")))

##' significance of SEX.PREG variable
D3(log.mod_imp,
	 with(
	 	ndns.imp %>% filter(!is.na(ndns$drink.binary)),
	 	glm(drink.binary ~
	 				age *
	 				BMI +
	 				eq_income +
	 				region,
	 			family = "binomial")))

#' ODDS RATIOS
pool_log$pooled %>% select(term, estimate) %>% mutate(OR = exp(estimate))

#### PROPORTIONAL ODDS ORDINAL MODEL ####

or.model <- fit.mult.impute(formula = drink.ordinal ~
														 	age *
														 	BMI +
														 	eq_income +
														 	sex.preg * age +
															region,
														 fitter = orm, 
														 data = ndns %>% filter(!is.na(drink.ordinal)), 
														 xtrans = ndns.imp %>% filter(!is.na(drink.ordinal)),
														 y = T, x = T)
or.model

par(mfrow = c(4,4))
#enlarge the plotting area before running next command
residuals(or.model, "partial", pl = T, label.curves = F)
par(mfrow = c(1,1))
anova(or.model)

#' NO INTERACTION
lrtest(or.model,
			 update(or.model, .~.-age:BMI -age:eq_income -age:sex.preg))
#' NO AGE:BMI INTERACTION
lrtest(or.model,
			 update(or.model, .~.-age:BMI))
#' NO AGE:EQ_INCOME INTERACTION
lrtest(or.model,
			 update(or.model, .~.-age:eq_income))
#' NO AGE:sex.preg INTERACTION
lrtest(or.model,
			 update(or.model, .~.-age:sex.preg))

AIC(or.model)
AIC(update(or.model, .~.-age:eq_income))
AIC(update(or.model, .~.-age:BMI))

#### TWO-PART MIXTURE MODEL ####
ndns.agg <- split(ndns_comp,ndns_comp$.imp)
mix.mods <- lapply(ndns.agg,
									 function(x) {
									 	tpm(
									 		formula_part1 = log(Alcoholg + 1) ~ age * eq_income + BMI * age + region + 
									 			sex.preg * age + start_day,
									 		formula_part2 = log(Alcoholg + 1) ~ age * eq_income + BMI * age + region + 
									 			sex.preg * age + start_day,
									 		data = x,
									 		link_part1 = "logit",
									 		family_part2 = gaussian(link = "identity")
									 	)
									 })
summary(mix.mods$'1')
plot(mix.mods$'1')
summary(mix.mods$'2')
plot(mix.mods$'2')
summary(mix.mods$'3')
plot(mix.mods$'3')
summary(mix.mods$'4')
plot(mix.mods$'4')
summary(mix.mods$'5')
plot(mix.mods$'5')


mod.red <-									 	tpm(
	formula_part1 = log(Alcoholg + 1) ~ age + BMI * age + eq_income + sex.preg * age +
		region + start_day,
	formula_part2 = log(Alcoholg + 1) ~ age + BMI * age + eq_income + sex.preg * age +
		region + start_day,
	data = ndns %>% drop_na(eq_income, BMI),
	link_part1 = "logit",
	family_part2 = gaussian(link = "identity")
)
summary(mod.red)
par(mfrow = c(3,3))
plot(mod.red)

