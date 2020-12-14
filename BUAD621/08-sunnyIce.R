# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Model for Sunny  / Cloudy day


library(causact)
library(tidyverse)

## show graphical model
dag_create() %>%
  dag_node("Ice Cream Sales","x",
           obs = TRUE) %>%
  dag_node("Sunny Day","y") %>%
  dag_edge("y","x") %>%
  dag_render()

## represent two models of the world, sunny and cloudy
## prior belief for each listed in that order
priorProb = c("sunny" = 0.9,
              "cloudy" = 0.1)

## here is a likelihood given sales and conditional on
## whether it was sunny or not
likeFun = function(sales) {
  likelihood = c(  ## below functions are assumed given
    "sunny" = prod(0.01 / 200 * sales),
    "cloudy" =  prod(0.01 - 0.01 / 200 * sales))
  return(likelihood)
}

# show same as video minute 7:14
likeFun(50)
priorProb * likeFun(50) ## numerators in video 7:43

# calculate p(data) as sum of prior*likelihoods for
# all of the models of the world (i.e. sunny or cloudy)
pdata = function(sales) {
  pEvidence = sum(priorProb * likeFun(sales))
  return(pEvidence)
}

#confirm match number in video
pdata(50)

# now do Bayes Rule for given data of 50
# Compute the posterior.
pModelGivendata = function(sales) {
  posterior = priorProb * likeFun(sales) / pdata(sales)
  return(posterior)
}

#comfirm match number in video
pModelGivendata(sales = 50)

#make function for plotting as function of sales
getPlot = function(obsSales) {
plotDF = tibble(twoModels = c("Sunny","Cloudy"),
                prior = priorProb,
                likelihood = likeFun(obsSales),
                posterior = pModelGivendata(obsSales))

#make tidy data to use facet grid by ProbType for plot
tidyPlotDF = plotDF %>%
  gather("ProbType","Probability",-twoModels) %>%
  mutate(ProbType = fct_relevel(ProbType, c("prior","likelihood","posterior")))# order levels for plot


#create named vector for facet labels
labels = c(prior = "PRIOR\np(model)",likelihood = "LIKELIHOOD\np(data|model)",posterior="POSTERIOR\np(model|data)")

#create plot
plot = tidyPlotDF %>%
  ggplot(aes(x = twoModels,
             y = Probability,
             fill = ProbType)) +
  geom_col(width = 0.02,
           show.legend = FALSE) +
  facet_grid(rows = vars(ProbType),
             labeller = labeller(ProbType = labels),
             scales = "free_y") +
  geom_text(aes(y = Probability * 1.2,
                label = signif(Probability,2))) +
  labs(title = paste0("Belief Evolution")) +
  theme_minimal(16)

return(plot)
}  #end plot function definition

getPlot(obsSales = 50)

### class question: what would be the posterior probability for cloudy if the prior was based on Buffalo weather where 90% of days are cloudy?


### class question: for the Buffalo example, what would happen if you observed sales from more than one store (assume all stores have identical likelihoods, but their sales figures are independent) and the sales of five stores are given as the following?
buffaloSales = c(50,150,170,140,160)

# show graphical model for last class question
#####
dag_create() %>%
  dag_node("Ice Cream Sales","x",
           obs = TRUE) %>%
  dag_node("Sunny Day","y") %>%
  dag_plate("Buffalo Store","i",
            nodeLabels = "x",
            data = buffaloSales) %>%
  dag_edge("y","x") %>%
  dag_render()





