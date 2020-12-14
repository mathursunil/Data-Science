# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : DAG modeling

### DAG Modelling Walkthrough

library(tidyverse)
library(greta)
library(causact)

# DAG shortcuts from CAUSACT PACKAGE

# ---------------------------------------
# Starting the DAG creation process
dag_create()   ## returns a list of data frames to store DAG info

dag_create() %>% 
  dag_node("BernoulliRV")   ## node description - returns list

dag_create() %>% 
  dag_node("BernoulliRV") %>% 
  dag_render()   ## makes picture

# ---------------------------------------
# Replicate Homework Question With DAG Shortcuts
# Assume a generative model with Bernoulli data (X) 
# and a uniform prior:
#         X ~ Bernoulli(theta)
#     theta ~ uniform(0,1)
# assume you observe two successes and one failure:
#       x_1 = 1, x_2 = 1, x_3 = 0.
# What is posterior probability that 
# theta >= 65%?  (i.e. P(theta>0.65))

dag_create() %>%  # first pass - just get one node showing
  dag_node(descr = "Store Data", label = "x") %>%
  dag_render()

dag_create() %>%  # second pass - add distribution + data
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_render()

## note:  rhs distributions can be any greta distribution
?greta::distributions  # see list here

dag_create() %>%  # third pass - parent nodes for rhs args 
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), ## rhs not quoted
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", 
           label = "theta",  # label needs quotes
           rhs = uniform(0,1)) %>%
  dag_render()

dag_create() %>%  # fourth pass - connect parent to child 
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", label = "theta",  
           rhs = uniform(0,1)) %>%  
  dag_edge(from = "theta",
           to = "x") %>% ## ADDED LINE TO CREATE EDGE
  dag_render()

## above is the observational model we want:
# X ~ Bernoulli(theta)
# theta ~ uniform(0,1)
# DATA: c(1,1,0)

# running Bayesian inference - remove dag_render() and save graph object
graph = dag_create() %>%  # 4th pass - parent child connect
  dag_node(descr = "Store Data", label = "x", 
           rhs = bernoulli(theta), 
           data = c(1,1,0)) %>%
  dag_node(descr = "Success Probability", label = "theta",  
           rhs = uniform(0,1)) %>%  
  dag_edge(from = "theta",
           to = "x")

graph %>% dag_render()  ## final viz of generative DAG

graph %>% dag_greta(mcmc=FALSE) ## creates greta code

drawsDF = graph %>% dag_greta() ## runs greta code
drawsDF  ### see the rep sample

# plot posterior the ggplot() way
drawsDF %>% 
  ggplot(aes(x=theta)) + 
  geom_density(fill = "cadetblue")

# the super quick causact way
drawsDF %>% dagp_plot() ## eyeball P(theta>0.65)

# computation second - get P(theta>0.65) using dplyr recipe
drawsDF %>% 
  mutate(indicatorFlag = 
           ifelse(theta > 0.65,
                  "theta > 0.65",
                  "theta <= 0.65")) %>%
  group_by(indicatorFlag) %>%
  summarize(countInstances = n()) %>%
  mutate(percentageOfInstances = countInstances / sum(countInstances))

# fancier and more terse computation - mean of indicator function
# represents the percentage of occurrences
drawsDF %>% 
  mutate(flag = ifelse(theta > 0.65,1,0)) %>%
  pull(flag) %>%
  mean() # this is P(theta>0.55)

#### CLASS EXERCISE ####
# Instead of 2 successes and 1 failure,
# assume 20 successes and 10 failures.
# Modify the model and sample the posterior.
# 1) Eyeball the posterior using tidyDrawsDF %>% dagp_plot()
# 2) Use the new drawsDF to find P(theta > 0.65)

