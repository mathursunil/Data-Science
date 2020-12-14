# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Cross Fit gym problem

# Chapter 24 - cross fit Gym 
## data from causact package that accompanies this book
library(causact)
library(tidyverse)


data("gymDF")  ## load into environment
gymDF  ## view the data

dag_create() %>%
  dag_node("Yoga Stretching?","yoga",
           dec = TRUE) %>%
  dag_node("Signup Probability","prob") %>%
  dag_edge("yoga","prob") %>%
  dag_plate("Stretch Type","",
            nodeLabels = c("yoga","prob")) %>%
  dag_render(shortLabel = TRUE, wrapWidth = 12)

graph2 = dag_create() %>%
  dag_node("Number of Signups","k",
           rhs = binomial(nTrials,theta),
           data = gymDF$nSigned) %>%
  dag_node("Signup Probability","theta",
           child = "k",
           rhs = 1 / (1+exp(-y))) %>%
  dag_node("Number of Trials","nTrials",
           child = "k",
           data = gymDF$nTrialCustomers) %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta * x,
           child = "theta") %>%
  dag_node("Yoga Stretch Flag","x",
           data = gymDF$yogaStretch,
           child = "y") %>%
  dag_node("Gym Intercept","alpha",
           rhs = normal(mu_alpha,sd_alpha),
           child = "y") %>%
  dag_node("Gym Yoga Slope Coeff","beta",
           rhs = normal(mu_beta,sd_beta),
           child = "y") %>%
  dag_node("Avg Crossfit Intercept","mu_alpha",
           rhs = normal(-1,1.5),
           child = "alpha") %>%
  dag_node("Avg Crossfit Yoga Slope","mu_beta",
           rhs = normal(0,0.75),
           child = "beta") %>%
  dag_node("SD Crossfit Intercept","sd_alpha",
           rhs = uniform(0,3),
           child = "alpha") %>%
  dag_node("SD Crossfit Yoga Slope","sd_beta",
           rhs = uniform(0,1.5),
           child = "beta") %>%
  dag_plate("Gym","j",
            nodeLabels = c("alpha","beta"),
            data = gymDF$gymID,
            addDataNode = TRUE) %>%
  dag_plate("Observation","i",
            nodeLabels = c("k","x","j",
                           "nTrials","theta","y"))
graph2 %>% dag_render()


drawsDF = graph2 %>% dag_greta()

draw = drawsDF %>% 
  slice_sample(n=1)
  #select(alpha_12, beta_12)
  select(mu_alpha, mu_beta, sd_alpha, sd_beta)

  draw = draw %>%
    #mutate(y_yoga = alpha_12 + beta_12 * 1) %>%
    mutate(y_yoga = mu_alpha + mu_beta *1) %>% 
    #mutate(y_trad = alpha_12)
    mutate(y_trad = mu_alpha)
  
  draw = draw %>%
    mutate(theta_yoga = 1 / (1+exp(-y_yoga))) %>%
    mutate(theta_trad = 1 / (1+exp(-y_trad))) 

draw


postDF = drawsDF %>%
  #select(alpha_12,beta_12) %>%
  select(mu_alpha, mu_beta) %>% 
  #mutate(y_yoga = alpha_12 + beta_12 * 1) %>%
  mutate(y_yoga = mu_alpha + mu_beta *1) %>% 
  #mutate(y_trad = alpha_12) %>%
  mutate(y_trad = mu_alpha) %>% 
  mutate(theta_yoga = 1 / (1+exp(-y_yoga))) %>%
  mutate(theta_trad = 1 / (1+exp(-y_trad))) %>%
  #mutate(z_12 = theta_yoga - theta_trad)
  mutate(z_new = theta_yoga - theta_trad)

postDF %>% 
  #ggplot(aes(x = z_12)) + 
  ggplot(aes(x = z_new)) +
  geom_density(fill = "cadetblue", alpha = 0.5)

#summary(postDF$z_12)
summary(postDF$z_new)

moneyDF = postDF %>%
  #mutate(ValueCreated = 500 * z_12)
  mutate(ValueCreated = 500 * z_new)

moneyDF %>%
  ggplot(aes(x = ValueCreated)) +
  geom_density(fill = "cadetblue", alpha = 0.5) +
  scale_x_continuous(labels = scales::dollar)
summary(moneyDF$ValueCreated)

#breaks = c(-1000,-20,0,20,40,60,80,100,1000)
breaks = c(-1000,-20,-10,0,10,20,30,40,50,60,70,80,90,100,1000)
#labels = c("<-$20","-$20 - $0","$0 - $20",
#           "$20 - 40$","$40 - $60","$60 - $80",
#          "$80-$100","$100+")
labels = c("<-$20","-$20 - $10","-$10 - $0",
"$0 - 10$","$10 - $20","$20 - $30",
"$30 - 40$","$40 - $50","$50 - $60",
"$60 - 70$","$70 - $80","$80 - $90",
"$90-$100","$100+")

bins = cut(moneyDF$ValueCreated, 
           breaks, 
           include.lowest = T, 
           right=FALSE, 
           labels=labels)
moneyDF$bins = bins  ## add new column

## add label for percentage in each bin
plotDF = moneyDF %>%
  group_by(bins) %>%
  summarize(countInBin = n()) %>%
  mutate(pctInBin = countInBin / sum(countInBin)) %>%
  mutate(label = paste0(round(100*pctInBin,0),"%")) %>%
  mutate(makeMoney = ifelse(bins %in% levels(bins)[1:2],
                            "Not Profitable",
                            "Profitable"))

## Create more interpretable plot
plotDF %>%
  ggplot(aes(x = bins, y = pctInBin, 
             fill = makeMoney)) +
  geom_col(color = "black") +
  geom_text(aes(label=label), nudge_y = 0.015) +
  xlab("Value Added Per Trial Customer") +
  ylab("Probability of Outcome") +
  scale_fill_manual(values = c("lightcoral",
                               "cadetblue")) +
  theme(legend.position = "none") +
  coord_flip() +
  ggtitle("Making Yoga Stretching Mandatory for New Gym")


data("gymDF")  ## load into environment
gymDF  ## view the data
gymDF %>% group_by(gymID) %>%
  summarize(numberOfObservations = n())

plotDF = gymDF %>%
  mutate(stretchType = 
           ifelse(yogaStretch == 1, 
                  "Yoga Stretch", 
                  "Traditional"))

ggplot(plotDF, 
       aes(x = timePeriod, #map time to x-axis
           y = nTrialCustomers, #map numTrials to y-axis
           fill = stretchType #map fill to stretch type
       )) + 
  geom_col(width = 0.75, 
           alpha = 0.55) +  # use transparency
  geom_col(width = 0.75, 
           aes(y = nSigned)) + # different y-mapping
  facet_wrap(vars(gymID), labeller = label_both) + 
  theme_minimal(11) + 
  theme(plot.caption = 
          element_text(size = 9, face = "italic"),
        legend.position = "bottom") +
  ylab("# of Trial Customers") +
  labs(caption = 
         "Darker fill for # of sign-ups.")

# Complete Pooling
gymDF %>% group_by(yogaStretch) %>%
  summarize(nTrials = sum(nTrialCustomers),
            nSuccess = sum(nSigned)) %>%
  mutate(pctSuccess = nSuccess / nTrials)

library(causact)
library(greta)
graph = dag_create() %>%
  dag_node("Number of Signups","k",
           rhs = binomial(nTrials,theta),
           data = gymDF$nSigned) %>%
  dag_node("Signup Probability","theta",
           child = "k",
           rhs = beta(2,2)) %>%
  dag_node("Number of Trials","nTrials",
           child = "k",
           data = gymDF$nTrialCustomers) %>%
  dag_plate("Yoga Stretch Flag","x",
            data = gymDF$yogaStretch,
            nodeLabels = "theta",
            addDataNode = TRUE) %>%
  dag_plate("Observation","i",
            nodeLabels = c("k","x","nTrials"))

graph %>% dag_render()

drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot() 

## get DF of observed data and remove target measure
simDF = gymDF %>% select(-nSigned)

## get 20 random draws of posterior
paramsDF = drawsDF %>%
  slice_sample(n=20) %>%
  mutate(sampDrawID = row_number()) ## add id to track

## get all combinations of observed explanatory
## variables and the parameter draws
simDF = simDF %>%
  crossing(paramsDF)
## 44 observations * 20 sample draws = 880 rows

## if no yoga, then theta_0.  If yoga, then theta_1
simPredDF = simDF %>%
  mutate(signupProb = 
           ifelse(yogaStretch == 0, 
                  theta_0, theta_1)) %>%
  mutate(nSigned = rbinom(n = n(),
                          size = nTrialCustomers,
                          prob = signupProb))  

obsDF = tibble(obs = gymDF$nSigned)

colors = c("simulated" = "cadetblue", 
           "observed" = "navyblue")

## make plot
ggplot(simPredDF) +
  stat_density(aes(x=nSigned, 
                   group = sampDrawID,
                   color = "simulated"),
               geom = "line",  
               position = "identity") +  
  stat_density(data = obsDF, aes(x=obs, 
                                 color = "observed"),
               geom = "line",  
               position = "identity",
               lwd = 2) +
  scale_color_manual(values = colors) +
  labs(x = "# of Monthly Signups",
       y = "Density Estimated from Data",
       color = "Data Type")
## using simulation to help choose Priors
library(tidyverse)
numSims = 10000

## guess at distribution and params for alpha
## let's say alpha ~ normal(0,10) was my first guess
## then simulate effects on theta.  Do they mimic
## your uncertainty in theat without data?  If no,
## iterate with a different guess. After 
## many iterations I settled on normal(-1,1.5)
## below code gets repsample of theta for this prior
simDF = tibble(alpha = rnorm(n = numSims, 
                             mean = -1., 
                             sd = 1.5))

simDF = simDF %>%
  mutate(y = alpha) %>% ##follows DAG
  mutate(theta = 1 / (1 + exp(-y)))  ## inv logit

## plot implied theta distribution
ggplot(simDF) +
  geom_density(aes(x=theta),fill = "cadetblue") 

## Similar exercise to get prior Beta
library(tidyverse)
numSims = 10000

## guess at distribution and params for beta
## since beta is deviation from base rate alpha
## assume a base rate and then, see effects
## mean should absolutely be zero to indicate no prior
## preference for whether yoga is helpful
## let the data tip the scales
## sd should be smaller than that used for alpha
baseRate = 0.20  ## assumed for now

## logit function 
## input: probability, 
## output: linear Predictor
## inverse of the inverse logit function
## linearPredictor = log(probability / (1-probability))
alpha = log(baseRate / (1 - baseRate))

simDF = tibble(alpha = alpha,
               beta = rnorm(n = numSims,
                            mean = 0,  
                            sd = 0.75))

simDF = simDF %>%
  mutate(yNoYoga = alpha) %>%
  mutate(yWithYoga = alpha + beta) %>%
  mutate(thetaNoYoga = 1 / (1 + exp(-yNoYoga))) %>%
  mutate(thetaWithYoga = 1 / (1 + exp(-yWithYoga))) %>%
  mutate(probDiff = thetaWithYoga - thetaNoYoga)

## plot implied theta distribution
ggplot(simDF) +
  geom_density(aes(x=probDiff),fill = "cadetblue") 
# Posterior distribution
drawsDF = graph %>% dag_greta()
drawsDF %>% dagp_plot()

# Posterior Predictive test
## get DF of observed data and remove target measure
simDF = gymDF %>% select(-nSigned)

## get 20 random draws of posterior
paramsDF = drawsDF %>%
  slice_sample(n=20) %>%
  mutate(sampDrawID = row_number()) 

## when data like gym_id is in the name of column 
## headings, e.g. alpha_4,
## we use the tidyr package pivot_longer function 
## to turn headings into data
## and extract the gymID information from the heading
tidyParamDF = paramsDF %>% 
  pivot_longer(cols = -sampDrawID) %>%
  mutate(gymID = str_extract(name, '[0-9]+')) %>%
  mutate(gymID = as.integer(gymID)) %>%
  mutate(paramName = str_extract(name, '[:alpha:]+'))

## now we get explanatory variable info 
## combined with relevant params
simDF = simDF %>% left_join(tidyParamDF, by = "gymID") 
## 44 observations * 2 params per row * 20 sims 
## = 1,760 rows

## get relevant alpha and beta on same row
simPredDF = simDF %>%
  pivot_wider(id_cols = gymID:sampDrawID,
              names_from = paramName,
              values_from = value)

## if no yoga, then y=alpha.  If yoga, then y=alpha+beta
simPredDF = simPredDF %>%
  mutate(y = ifelse(yogaStretch == 0, 
                    alpha, alpha+beta)) %>%
  mutate(theta = 1 / (1 + exp(-y))) %>%
  mutate(nSigned = rbinom(n = n(),
                          size = nTrialCustomers,
                          prob = theta))

obsDF = tibble(obs = gymDF$nSigned)

colors = c("simulated" = "cadetblue", 
           "observed" = "navyblue")

## make plot
ggplot(simPredDF) +
  stat_density(aes(x=nSigned, 
                   group = sampDrawID,
                   color = "simulated"),
               geom = "line",
               position = "identity") +
  stat_density(data = obsDF, aes(x=obs, 
                                 color = "observed"),
               geom = "line",  
               position = "identity",
               lwd = 2) +
  scale_color_manual(values = colors) +
  labs(x = "# of Monthly Signups",
       y = "Density Estimated from Data",
       color = "Data Type")


## Partial Pooling
library(causact)
library(greta)

graph2 = dag_create() %>%
  dag_node("Number of Signups","k",
           rhs = binomial(nTrials,theta),
           data = gymDF$nSigned) %>%
  dag_node("Signup Probability","theta",
           child = "k",
           rhs = 1 / (1+exp(-y))) %>%
  dag_node("Number of Trials","nTrials",
           child = "k",
           data = gymDF$nTrialCustomers) %>%
  dag_node("Linear Predictor","y",
           rhs = alpha + beta * x,
           child = "theta") %>%
  dag_node("Yoga Stretch Flag","x",
           data = gymDF$yogaStretch,
           child = "y") %>%
  dag_node("Gym Intercept","alpha",
           rhs = normal(mu_alpha,sd_alpha),
           child = "y") %>%
  dag_node("Gym Yoga Slope Coeff","beta",
           rhs = normal(mu_beta,sd_beta),
           child = "y") %>%
  dag_node("Avg Crossfit Intercept","mu_alpha",
           rhs = normal(-1,1.5),
           child = "alpha") %>%
  dag_node("Avg Crossfit Yoga Slope","mu_beta",
           rhs = normal(0,0.75),
           child = "beta") %>%
  dag_node("SD Crossfit Intercept","sd_alpha",
           rhs = uniform(0,3),
           child = "alpha") %>%
  dag_node("SD Crossfit Yoga Slope","sd_beta",
           rhs = uniform(0,1.5),
           child = "beta") %>%
  dag_plate("Gym","j",
            nodeLabels = c("alpha","beta"),
            data = gymDF$gymID,
            addDataNode = TRUE) %>%
  dag_plate("Observation","i",
            nodeLabels = c("k","x","j",
                           "nTrials","theta","y"))
graph2 %>% dag_render()

drawsDF = graph2 %>% dag_greta()
drawsDF %>% dagp_plot()

## Final section
simDF = gymDF %>% select(-nSigned)

## get 20 random draws of posterior
paramsDF = drawsDF %>%
  slice_sample(n=20) %>%
  mutate(sampDrawID = row_number())

tidyParamDF = paramsDF %>% 
  pivot_longer(cols = -sampDrawID) %>%
  mutate(gymID = str_extract(name, '[0-9]+')) %>%
  mutate(gymID = as.integer(gymID)) %>%
  mutate(paramName = str_extract(name, '[:alpha:]+'))

## combine needed info
simDF = simDF %>% left_join(tidyParamDF, by = "gymID") 

## get relevant alpha and beta on same row
simPredDF = simDF %>%
  pivot_wider(id_cols = gymID:sampDrawID,
              names_from = paramName,
              values_from = value)

## if no yoga, then y=alpha. If yoga,then y=alpha+beta
simPredDF = simPredDF %>%
  mutate(y = ifelse(yogaStretch == 0, 
                    alpha, alpha+beta)) %>%
  mutate(theta = 1 / (1 + exp(-y))) %>%
  mutate(nSigned = rbinom(n = n(),
                          size = nTrialCustomers,
                          prob = theta))

obsDF = tibble(obs = gymDF$nSigned)

colors = c("simulated" = "cadetblue", 
           "observed" = "navyblue")

## make plot
ggplot(simPredDF) +
  stat_density(aes(x=nSigned, 
                   group = sampDrawID,
                   color = "simulated"),
               geom = "line", 
               position = "identity") +
  stat_density(data = obsDF, aes(x=obs, 
                                 color = "observed"),
               geom = "line",  
               position = "identity",
               lwd = 2) +
  scale_color_manual(values = colors) +
  labs(x = "# of Monthly Signups",
       y = "Density Estimated from Data",
       color = "Data Type")
#Figure 24.14: Posterior predictive check comparing the observed data to data generated using 20 different draws from the posterior distribution.
