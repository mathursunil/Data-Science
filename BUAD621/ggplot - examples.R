# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : ggplot2 examples

library("tidyverse")
mpg
DF = mpg
DF %>% 
  ggplot() +
  #geom_point(aes (x = displ, y = hwy,  color = class)) +
  #geom_point(aes (x = displ, y = hwy,  size = class))
  #geom_point(aes (x = displ, y = hwy,  alpha = class))
  #geom_point(aes (x = displ, y = hwy,  shape = class))
  geom_point(aes (x = displ, y = hwy),  color = "blue")
  geom_point(aes(x= cyl, y = hwy))
?mpg
#facets
  DF %>% 
    ggplot() +
    geom_point(aes (x = displ, y = hwy)) +
    facet_wrap( ~ class, nrow = 2)
  
  DF %>% 
    ggplot() +
    geom_point(aes (x = displ, y = hwy)) +
    facet_grid( . ~ cyl)

  DF %>% 
    ggplot() +
    geom_smooth (aes (x = displ, y = hwy)) 
  
  DF %>% 
    ggplot() +
    geom_smooth (aes (x = displ, y = hwy, linetype = drv)) 
  
  DF %>% 
    ggplot() +
    geom_smooth (aes (x = displ, y = hwy, color = drv)) 

  DF %>% 
    ggplot() +
    geom_point(aes (x = displ, y = hwy)) +
    geom_smooth (aes (x = displ, y = hwy, linetype = drv))      

  DF %>% 
    ggplot(aes (x = displ, y = hwy)) +
    geom_point(aes(color = class)) +
    geom_smooth (
      data = filter(mpg, class == "subcompact"),
      se = FALSE
    )  
 
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point() +
    geom_smooth(se = FALSE)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_smooth(mapping = aes(group = drv), se = FALSE) +
    geom_point()
  
  ggplot(mpg, aes(x = displ, y = hwy, colour = drv)) +
    geom_point() +
    geom_smooth(se = FALSE)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(colour = drv)) +
    geom_smooth(se = FALSE)
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(aes(colour = drv)) +
    geom_smooth(aes(linetype = drv), se = FALSE)
  #> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
  
  ggplot(mpg, aes(x = displ, y = hwy)) +
    geom_point(size = 4, color = "white") +
    geom_point(aes(colour = drv))

  #Diamonds dataset
  DFdiamonds = diamonds  
  
  DFdiamonds %>% 
    ggplot() + 
    geom_bar(aes(x = cut))
  
  DFdiamonds %>% 
    ggplot() + 
    stat_count(aes(x = cut))
  
  ggplot(data = diamonds) +
    geom_pointrange(
      mapping = aes(x = cut, y = depth),
      stat = "summary"
    )
  
  ggplot(data = diamonds) +
    geom_bar(aes(x=cut, color = cut))
    
  ggplot(data = diamonds) +
    geom_bar(aes(x=cut, fill = cut))

  ggplot(data = diamonds) +
    geom_bar(aes(x=cut, fill = clarity))  
  
  ?geom_j