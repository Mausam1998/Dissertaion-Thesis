# Load necessary libraries
library(readxl)
library(mlogit)
library(cregg)

# Read the dataset
cbc.df <- read_excel("cleaned_data.xlsx")

# Prepare the data for analysis
cbc.sh <- data.frame(
  choice = as.logical(cbc.df$choice), 
  id = as.factor(cbc.df$`ResponseId`), 
  price = as.factor(cbc.df$price), 
  peer_recommendation = as.factor(cbc.df$`peer.recommendation`), 
  food_quality = as.factor(cbc.df$`food.quality.menu.variety`), 
  ambience = as.factor(cbc.df$ambience)
)

# Create an alternative-specific variable for the model (replicating 1, 2 across the rows)
cbc.sh$altm <- rep(c(1, 2), times = nrow(cbc.sh) / 2)

# Convert the data for mlogit analysis using dfidx
cbc.mnl <- mlogit.data(cbc.sh, choice = 'choice', shape = 'long', id.var = "id")

# Sanity check using a simple GLM model
# Ensure that variables used in the model are the correct ones from your dataset
glm_model <- glm(choice ~ price + peer_recommendation + food_quality + ambience, data = cbc.sh, family = "binomial")
# Fit a simplified multinomial logistic regression model to avoid singularity issues
mlogit_model <- mlogit(choice ~ 0 + price + peer_recommendation, data = cbc.mnl, method = "nr")

summary(glm_model)

# Multinomial logistic regression using mlogit
mlogit_model <- mlogit(choice ~ 0 + price + peer_recommendation + food_quality + ambience, data = cbc.mnl)
summary(mlogit_model)

# Plot results across factors using cregg
f1 <- choice ~ price + peer_recommendation + food_quality + ambience
plot(mm(cbc.mnl, f1, id = ~id), vline = 0.5)

# Estimate average marginal component effects using cregg
amces <- cj(cbc.sh, choice ~ price + peer_recommendation + food_quality + ambience, id = ~id)
head(amces[c("feature", "level", "estimate", "std.error", "p")], 20L)

# Estimate marginal means by group using cregg
# Note: Replace peer_recommendation with the actual grouping variable in your dataset, if applicable
mm_by <- cj(cbc.mnl, choice ~ price + peer_recommendation + food_quality + ambience, id = ~id, estimate = "mm", by = ~peer_recommendation)
plot(mm_by, group = "peer_recommendation", vline = 0.5)

