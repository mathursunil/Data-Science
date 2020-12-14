# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Newspaper demand and supply homework

# quick coding example
library(tidyverse)
library(greta)
library(causact)
theme_set(theme_minimal(16))

# some fake demand data
demandData = c(70,55,80,60,61)

# create geenrative DAG
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

# revenue function - takes vectors of demand and orders
# returns vector of revenues
revFunction = function(demand, order) {
  soldrev = 2 * pmin(demand,order)
  unsoldrev = ifelse(order > demand, 0.25 * (order - demand),0)
  rev = soldrev + unsoldrev
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

potentialOrderQty = seq(50,100,by=10)
## add to previous
outcomeDF = expand_grid(x_sim = drawsDF$x_sim,
                        orderQty = potentialOrderQty) %>%
  mutate(revenue = revFunction(x_sim,orderQty),
         expenses = expenseFunction(orderQty),
         profit = revenue - expenses) %>%
  group_by(orderQty) %>%
  summarize(q05 = stats::quantile(profit,0.05),
            q50 = stats::quantile(profit,0.50),
            q95 = stats::quantile(profit,0.95),
            revenue = median(revenue),
            profit = median(profit),
            expense = median(expenses))

#For the order quantity that maximizes median earnings, what is the probability the newsvendor will have papers to return to the supplier at the end of the day, i.e. P(x_sim <  orderQty)? (enter as decimal rounded to hundreths place. e.g. 15.3% would be entered as 0.15 and do not round x_sim for this calculation) 

drawsDF %>% 
  mutate(IndFlag = ifelse(x_sim < 70, TRUE, FALSE)) %>% 
  summarise(pct = mean(IndFlag))


### lets plot the three outcomes of interest for each decision
outcomeDF %>%
  ggplot(aes(x = orderQty)) +
  geom_linerange(aes(ymin = q05, ymax = q95),
                 size = 2, color = "cadetblue") +
  geom_point(aes(y = q50),
             size = 4, color = "navyblue") +
  labs(y = "profit")


