# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : News vendoe decision problem

# quick coding example
library(tidyverse)
library(greta)
library(causact)
theme_set(theme_minimal(16))

# some fake demand data
demandData = c(70,55,80,60,61)

# create generative DAG
graph = dag_create() %>%
  dag_node("Demand","x",
           rhs = normal(mu,sigma),
           data = demandData) %>%
  dag_node("Exp Demand","mu",
           rhs = uniform(50,100)) %>%
  dag_node("Std Dev of Demand","sigma",
           rhs = gamma(3,0.2)) %>%
  dag_edge(from = c("mu","sigma"),
           to = "x") %>%
  dag_node("Future Demand Simul","x_sim",
           rhs = normal(mu,sigma)) %>%
  dag_edge(from = c("mu","sigma"),
           to = "x_sim") 

graph %>% dag_render()

drawsDF = graph %>% dag_greta()

drawsDF %>% dagp_plot()

# BEGIN:: posterior predictive check
postDF = drawsDF %>% 
  slice_sample(n = 20) %>%
  select(mu,sigma)

# get observed demand
plotDF = tibble(demand = demandData,
                xtype = "Observed",
                demandBatchNumber = 0)

# simulate demand based on mu/sigma draws
for (i in 1:nrow(postDF)){
  tempDF = tibble(demand = rnorm(n = length(demandData),
                            mean = postDF$mu[i],
                            sd = postDF$sigma[i]),
                  xtype = "Simulated",
                  demandBatchNumber = i)
  plotDF = bind_rows(plotDF,tempDF)
}

# plot posterior pred check
colors = c("Observed" = "navyblue", 
           "Simulated" = "cadetblue")

ggplot(plotDF, aes(x = demand)) + 
  geom_density(aes(color = xtype,
                   group = demandBatchNumber),
               alpha = 0.5) +
  scale_color_manual(values = colors)


# END:: posterior predictive check


# revenue function - takes vectors of demand and orders
# returns vector of revenues
revFunction = function(demand, order) {
  rev = 2 * pmin(demand,order)
  return(rev)
}

revFunction(c(2,4,3),c(4,4,4)) #est passes

# expense function - inputs q outputs expense $
expenseFunction = function(order) {
  expense = 1 * order
  return(expense)
}

expenseFunction(c(2,4,8))

## create posterior draws for outcomes of interest

## add to previous
outcomeDF = expand_grid(x_sim = drawsDF$x_sim,
                        orderQty = seq(50,100,by=5)) %>%
  mutate(revenue = revFunction(x_sim,orderQty),
         expenses = expenseFunction(orderQty),
         profit = revenue - expenses) %>%
  group_by(orderQty) %>%
  summarize(q05 = stats::quantile(profit,0.05),
            q50 = stats::quantile(profit,0.50),
            q95 = stats::quantile(profit,0.95),
            expense = first(expenses))

### lets plot the three outcomes of interest for each decision
outcomeDF %>%
  ggplot(aes(x = orderQty)) +
  geom_linerange(aes(ymin = q05, ymax = q95),
                 size = 2, color = "cadetblue") +
  geom_point(aes(y = q50),
             size = 4, color = "navyblue") +
  labs(y = "profit")

### now make a golem and plot the posterior predictive check
### Change for Golem:  mu ~ uniform(80,100)



