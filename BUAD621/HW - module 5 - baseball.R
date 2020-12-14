# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Baseball game problem

#The following code will extract and plot the number of runs scored per game at the Colorado Rockies baseball field in the 2010-2014 baseball seasons.

library(dplyr)
library(causact)
# extract relevant data
baseDF = causact::baseballData %>% as_tibble() %>%
  filter(Home == "COL") %>%
  mutate(runsInGame = HomeScore + VisitorScore) %>%
  select(Home, runsInGame) %>%
  gather(key = Home, value = runsInGame)

# plot data
baseDF %>% ggplot() +
  geom_histogram(aes(x=runsInGame), breaks = 0:30)

library(greta)

graph = dag_create() %>% 
  dag_node("count Data","k",
  rhs = poisson("lambda"),
  data = baseDF$runsInGame) %>% 
  dag_node("Rate Parameter", "lambda",
           rhs = uniform(0,50),
           child = "k") %>% 
  dag_plate("Observations", "i",
            nodeLabels = "k")

graph %>%  dag_render()

drawsDF = graph %>% dag_greta()
drawsDF %>%  dagp_plot()

## new code

randomDrawDF = drawsDF %>% 
  sample_n(size = 1)

# simulate 405 observations using this lambda

simobsDF = tibble(
  simlambda = rpois( n = 405,
                    lambda  = randomDrawDF$lambda[1]),
  simNum = 1,
  dataType = "simulated")

colors = c("simulated" = "cadetblue" ,
           "observed" = "navyblue") 

## original data
baseDF %>% 
  filter(runsInGame ==11)
#observed data
simobsDF %>% 
  filter(simlambda==11)
