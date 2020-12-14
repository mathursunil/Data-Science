# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Car repair shop problem


# Final Homework assignment
#Patrick owns five Midas automotive repair shops (one of them is shown in Figure 1). To keep track of each
#shop, he visits one shop every day. He suspects that each shop is more productive when he is there as opposed
#to when he is not. He hires you to consult for him as he wants to make a data-driven decision about his visit
#schedule to each of the five shops.
#Patrick gives you a file which a previously hired data analyst (note: he fired that data analyst for not making the insights easy enough to understand.) has cleaned up and put into csv format. It contains 10 weeks worth of data:
library(tidyverse)
library(causact)
library(greta)
carsDF = readr::read_csv("carsFixed.csv")
qValuesDF = readr::read_csv("FinalHWqValues.csv")
summaryDF = carsDF %>%
  group_by(shopID)  %>% 
  filter(shopID == 5)
    
  
summaryDF1 = carsDF %>%
  group_by(shopID) %>%
  mutate(cars_Fixed_Boss_Present = ifelse(boss == 1, carsFixed, 0)) %>% 
  mutate(cars_Fixed_Boss_Absent = ifelse(boss == 0, carsFixed,0)) %>% 
  mutate("Boss_Presence" = ifelse(boss==1, "Boss Present", "Boss Absent")) %>% 
  summarize(numberOfBossVisits = sum(boss),
            carsFixed = sum(carsFixed),
            FixedBossPresent = sum(cars_Fixed_Boss_Present),
            AvgFixedBossPresent = FixedBossPresent / numberOfBossVisits,
            FixedBossAbsent = sum(cars_Fixed_Boss_Absent),
            AvgFixedBossAbsent = FixedBossAbsent / (50 - numberOfBossVisits))

DF1 = carsDF %>%
  group_by(shopID) %>%
  mutate(cars_Fixed_Boss_Present = ifelse(boss == 1, carsFixed, 0)) %>% 
  mutate(cars_Fixed_Boss_Absent = ifelse(boss == 0, carsFixed,0)) 

pdf("Sunil Final HW - Car Shop.pdf")

# Plot the graph
summaryDF1 %>% 
  ggplot() +
    geom_col(aes(x = observation, y = carsFixed, fill = Boss_Presence)) +
    facet_wrap(~shopID) +
    labs(title = "Productivity is higher when Boss is Present", 
         subtitle = "Shop 3 & 5 have max visits, Shop 2 and 4 will benefit from more visits",
         caption = "Weekly Data for 5 stores - last 10 weeks") +
    xlab("Observations per store") +
    ylab( "Number of Cars Fixed")  
    
     
  
  
# Simple mode of probability  
simpleGraph = dag_create() %>%
  dag_node("Cars Fixed","K",
           data = carsDF$carsFixed,
           rhs = poisson(lambda)) %>%
  dag_node("Exp Cars Fixed - Shop Level","lambda",
           rhs = exp(alpha_shop + beta_shop * x),
           child = "K") %>%
  dag_node("Intercept - Shop Level","alpha_shop",
           rhs = normal(alpha,sigma_alpha),
           child = "lambda") %>%
  dag_node("Boss Effect - Shop Level","beta_shop",
           rhs = normal(beta,sigma_beta),
           child = "lambda") %>%
  dag_node("Intercept - Midas Level","alpha",
           rhs = normal(3,2),
           child = "alpha_shop") %>%
  dag_node("Std Dev - Midas Level","sigma_alpha",
           rhs = uniform(0,2),
           child = "alpha_shop") %>%
  dag_node("Exp Boss Effect - Midas Level","beta",
           rhs = normal(0,1),
           child = "beta_shop") %>%
  dag_node("Std Dev Boss Effect","sigma_beta",
           rhs = uniform(0,2),
           child = "beta_shop") %>%
  dag_node("Boss Present","x",
           data = carsDF$boss,
           child = "lambda") %>%
  dag_plate("Observation","i",
            nodeLabels = c("K","lambda","x")) %>%
  dag_plate("Shop","j",
            nodeLabels = c("beta_shop","alpha_shop"),
            data = carsDF$shopID,
            addDataNode = TRUE)

simpleGraph %>% dag_render()
simpDrawsDF = simpleGraph %>% dag_greta()
simpDrawsDF %>% dagp_plot()

draw = simpDrawsDF %>% 
  slice_sample(n=4000) %>% 
  select ( alpha_shop_1, alpha_shop_2, alpha_shop_3, alpha_shop_4, alpha_shop_5,
           beta_shop_1, beta_shop_2, beta_shop_3, beta_shop_4, beta_shop_5)

draw = draw %>% 
  mutate(lambda_without_Boss_1 = exp(alpha_shop_1 + beta_shop_1 * 0)) %>% 
  mutate(lambda_with_Boss_1 = exp(alpha_shop_1 + beta_shop_1 * 1))%>% 
  mutate(lambda_without_Boss_2 = exp(alpha_shop_2 + beta_shop_2 * 0)) %>% 
  mutate(lambda_with_Boss_2 = exp(alpha_shop_2 + beta_shop_2 * 1)) %>% 
  mutate(lambda_without_Boss_3 = exp(alpha_shop_3 + beta_shop_3 * 0)) %>% 
  mutate(lambda_with_Boss_3 = exp(alpha_shop_3 + beta_shop_3 * 1)) %>% 
  mutate(lambda_without_Boss_4 = exp(alpha_shop_4 + beta_shop_4 * 0)) %>% 
  mutate(lambda_with_Boss_4 = exp(alpha_shop_4 + beta_shop_4 * 1)) %>% 
  mutate(lambda_without_Boss_5= exp(alpha_shop_5 + beta_shop_5 * 0)) %>% 
  mutate(lambda_with_Boss_5= exp(alpha_shop_5 + beta_shop_5 * 1)) %>% 
  # calculate extra cars ehen boss is present
  mutate(extracars_shop1 = lambda_with_Boss_1 - lambda_without_Boss_1) %>% 
  mutate(extracars_shop2 = lambda_with_Boss_2 - lambda_without_Boss_2) %>% 
  mutate(extracars_shop3 = lambda_with_Boss_3 - lambda_without_Boss_3) %>% 
  mutate(extracars_shop4 = lambda_with_Boss_4 - lambda_without_Boss_4) %>% 
  mutate(extracars_shop5 = lambda_with_Boss_5 - lambda_without_Boss_5)
  
  
# create outcomeDF for lambda withBoss & lambdawithoutBoss
# summarize q05, q50 and q95
draw1 = draw %>% 
  select(extracars_shop1, extracars_shop2,extracars_shop3, extracars_shop4, extracars_shop5) %>% 
summarize(q05_1 = stats::quantile(extracars_shop1,0.05),
            q50_1 = stats::quantile(extracars_shop1,0.50),
            q95_1 = stats::quantile(extracars_shop1,0.95),
            q05_2 = stats::quantile(extracars_shop2,0.05),
            q50_2 = stats::quantile(extracars_shop2,0.50),
            q95_2 = stats::quantile(extracars_shop2,0.95),
            q05_3 = stats::quantile(extracars_shop3,0.05),
            q50_3 = stats::quantile(extracars_shop3,0.50),
            q95_3 = stats::quantile(extracars_shop3,0.95),
            q05_4 = stats::quantile(extracars_shop4,0.05),
            q50_4 = stats::quantile(extracars_shop4,0.50),
            q95_4 = stats::quantile(extracars_shop4,0.95),
            q05_5 = stats::quantile(extracars_shop5,0.05),
            q50_5 = stats::quantile(extracars_shop5,0.50),
            q95_5 = stats::quantile(extracars_shop5,0.95)
            ) %>% 
 
  print(list[1])
 
shop1 = c("shop1", draw1$q05_1,draw1$q50_1, draw1$q95_1) 
shop2 =  c("shop2", draw1$q05_2,draw1$q50_2, draw1$q95_2) 
shop3 = c("shop3", draw1$q05_3,draw1$q50_3, draw1$q95_3)
shop4 = c("shop4", draw1$q05_4,draw1$q50_4, draw1$q95_4)
shop5 = c("shop5",draw1$q05_5,draw1$q50_5, draw1$q95_5)

qValuesDF %>%
  ggplot(aes(x = ShopID)) +
  geom_linerange(aes(ymin = q05, ymax = q95),
                 color = "cadetblue",
                 size = 4) +
  geom_point(aes(y = q50), color = "navyblue",
             size = 3) +
  labs(title = "Cars Fixed per day would increase with Boss Visit", 
       subtitle = "Potential daily increments range for each shop",
       caption = "Probability Data for 5 Shops - 5%, 50%, 95% range depicted") +
 
  labs(y = "Increase in Cars Fixed per Day") 
dev.off()
 
#plot the outcome of interest
## ggplot + geom_linerange(ymin = q05, ymax = q95) + geom_point(q50)
  
  

## code from Adam
##function to get alpha_shop_X
getAlpha = function(shopID) {
  ## get string of column name
  varName = paste0("alpha_",shopID)
  varName2 = paste0("beta_",shopID)
  ## convert string to an R-object symbol or name
  colObject = sym(varName) 
  colObject2 = sym(varName2)
  ## use !! (bang-bang operator) to unquote/use symbol
  df = drawsDF %>% select(!!colObject,!!colObject2)
  return(df)
}


getAlpha(shopID = 2)
