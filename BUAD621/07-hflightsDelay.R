# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Flight delay problem

library("dplyr")
library("ggplot2")
library("hflights")

?hflights

### Purpose - advocate for which airline to fly out of Houston to avoid delays

### get the data we need to assess delay by day of the week and carrier
delayByCarrier = hflights %>%
  select(day = DayOfWeek, ###give columns better names
         airline = UniqueCarrier, 
         delay = ArrDelay)

str(delayByCarrier)

### Mappings:
#x-axis: Airline
#y-axis: Delay
#color: day
# dplyr::sample_frac used because 200,000 data pts is too many to plot
ggplot(data=delayByCarrier %>% sample_frac(size = 0.1),
       mapping=aes(x=airline,y=delay,color = day)) +
  geom_point()

ggplot(data=delayByCarrier %>% sample_frac(size = 0.1),
       mapping=aes(x=airline,y=delay,color = day)) +
  facet_grid(day ~ airline) + 
  geom_point()

##some times when there is so much information, all insight gets lost
## CLASS EXERCISE:: CREATE A DATAFRAME CALLED delayFlagDF WITH A COLUMN CALLED delayFlag that takes the value 1 if a flight is late (based on arrival delay) and 0, otherwise.  A skeleton of the dplyr workflow you will need is below - replace the ...

delayFlagDF = 
  delayByCarrier %>%
    mutate(delayFlag = ...)



## CLASS EXERCISE:: Get the number of flights each airline flew on each day of the week and give the percentage of those flights that were delayed.  TO DO THIS:  CREATE A DATAFRAME CALLED delayByCarrier2 that for each day/airline combination calculates these two additional columns:
#     3) count    -  the number of flights that 
#     4) PctDelayed - the percentage of delayFlag values that are equal to 1
# hint: these two columns are created in one summarize function that comes after a group_by ... use the skelton code below and replace the ...

delayByCarrier2 = delayFlagDF %>%
  group_by(...) %>%
  summarise(count = ...,
            PctDelayed = ...) %>%
  filter(count > 20) %>%
  arrange(desc(PctDelayed))


###one way to cut down the information in each chart is by using facets
ggplot(delayByCarrier2,
       aes(x=airline, y= PctDelayed)) + 
  geom_col() + 
  facet_grid(day~.)

###this still is not great... what else can we do to help us understand if the percentage delay by airline fluctuates during the course of a week
ggplot(delayByCarrier2,
       aes(x=PctDelayed, 
           y=airline, 
           color = as.factor(day))) + 
  geom_point()

###let's go with larger points and change the y-axis to 
###day of Week and use color for airline
ggplot(delayByCarrier2,
       aes(x= PctDelayed, 
           y=as.factor(day), 
           color = airline)) + 
  geom_point(size = 4)

ggplot(delayByCarrier2,
       aes(x= PctDelayed, 
           y=as.factor(day), 
           color = airline)) + 
  geom_point(size = 4) + 
  geom_line()

ggplot(delayByCarrier2,
       aes(x= PctDelayed, 
           y=as.factor(day), 
           color = airline)) + 
  geom_point(size = 4) + 
  geom_line(aes(group = airline))

###one last thing... maybe filter the data to airlines of interest
delayByCarrier3 = delayByCarrier2 %>% 
  filter(count > 500)

ggplot(delayByCarrier3,
       aes(x=as.factor(day), y= PctDelayed, color = airline)) + 
  geom_point(size = 4) + 
  geom_line(aes(group = airline))

###then take advantage of one last attribute (i.e. size) to show number of flights each day
ggplot(delayByCarrier3,
       aes(x=as.factor(day), y= PctDelayed, color = airline)) + 
  geom_point(aes(size = count)) + 
  geom_line(aes(group = airline)) + 
  scale_size(range = c(3, 10))


