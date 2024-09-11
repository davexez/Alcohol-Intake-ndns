# MODELLING ALCOHOL INTAKE WITH THE NATIONAL DIET AND NUTRITIONAL SURVEY FOR THE UK ADULT POPULATION

Excessive Alcohol Intake (AI) is an obvious driver of liver disease and is also seen as a risk factor for other
illnesses like cardiovascular disease, certain cancers and dementia. In this project, we want to investigate
which factors are associated with AI and thus may help us to predict it in the UK population. We will
use data from the National Diet and Nutrition Survey (NDNS), a rolling programme that collects data
from around 1000 people per year on their food consumption and nutrient intake. Within the NDNS each
individual’s food and drink intake is recorded on four consecutive days and there is also detailed information
available about participants (age, gender, BMI, physical activity, household income, region etc. . . ). The
aim of the project is to model personal AI in the adult population by these subject specific measurements.
From a statistical view point AI is an interesting variable to model, as it is known that about 20% of the
UK population do not consume alcohol at all, i.e. there will be exact zeros in intake for this part of the
population, whereas we can expect a continuous spread of intake values across the rest of the population. An
easy way to address this issue would be to dichotomize AI (zero vs non-zero) or categorize it (eg. no alcohol,
low intake, medium intake, high intake) which would allow us to use a generalized linear model framework
(logistic regression, ordinal regression), but would also mean a loss of information. The alternative is to model
AI as a mixture of a discrete (exact zeros) and a continuous distribution. We want to explore and compare
all three options in this project and identify factors that are associated with AI.


## Usage
The datasets can be found in "ndns data" folder in .csv format, in the same structure as they are made available from the NDNS repositories for different years. The script is in two parts: "adjusting_data.R" containing the pre-processing to get the polished dataset, and "modelling_final.R" where the models are present, all done in R. "model_data.RData" contain the polished dataset (after all the pre-processing steps) ready to be used at the modelling stage.