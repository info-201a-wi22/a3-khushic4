## Assignment 3 Analysis.R  File 
## Choosing to focus on the variable "prison population."

data <- read.csv('source/incarceration_trends.csv')
library("dplyr")
library("ggplot2")
library("tidyr")
library("lintr")
library("plotly")
library("leaflet")
library("knitr")

## Summary Script Information


incarceration_trends <- data %>%
  filter(total_jail_pop == total_jail_pop, na.rm = TRUE) %>%
  filter(female_jail_pop == female_jail_pop, na.rm = TRUE) %>%
  filter(male_jail_pop == male_jail_pop, na.rm = TRUE) %>%
  filter(black_jail_pop == black_jail_pop, na.rm = TRUE) %>%
  filter(white_jail_pop == white_jail_pop, na.rm = TRUE) %>%
  filter(latinx_jail_pop == latinx_jail_pop, na.rm = TRUE)

summary_info <- list()
  summary_info$max_county_in_2018 <- incarceration_trends %>%
    filter(year == max(year)) %>%
    filter(total_jail_pop == max(total_jail_pop)) %>% 
    pull(county_name)
  summary_info$year_with_most_women <- incarceration_trends %>%
    filter(female_jail_pop == max(female_jail_pop)) %>%
    pull(year)
  summary_info$county_with_most_men <- incarceration_trends %>%
    filter(male_jail_pop == max(male_jail_pop)) %>%
    pull(county_name)
  summary_info$highest_county_wa <- incarceration_trends %>%
    filter(state == "WA") %>%
    filter(total_jail_pop == max(total_jail_pop)) %>%
    pull(county_name)
  summary_info$year_with_most_wa <- incarceration_trends %>%
    filter(state == "WA") %>%
    filter(total_jail_pop == max(total_jail_pop)) %>%
    pull(year)
  
## Chart 1: Trends Over Time 
## Choosing to analyze 10 of the "blue" states in America and their jail populations over time; 
## WA, CA, OR, NV, CO, NM, MN, IL, NY, VA
  
 
  incarceration_trends_blue <- incarceration_trends %>%
    filter(state == "CA" | state == "WA" | state == "OR" | state == "NV" | state == "CO" | state == "NM" | state == "MN" | state == "IL" | state == "NY" | state == "VA")

  
  blue_states_by_race <- ggplot(data = incarceration_trends_blue) +
    geom_smooth(mapping = aes(x= year, y= black_jail_pop, color = "Black")) +
    geom_smooth(mapping = aes(x= year, y= white_jail_pop, color = "White")) + 
    geom_smooth(mapping = aes(x= year, y= latinx_jail_pop, color = "LatinX")) + 
    labs(
      
      title = "Population in Prison by Race in Blue States" ,
      x = "Year" ,
      y = "Prison Population Rate", 
      color = "Race Color Code"
    )
  
  plot(blue_states_by_race)
  
  
## Chart 2: Variable Comparison Chart 
## Comparing white jail populations with black jail populations in WA

  incarceration_WA <- incarceration_trends_blue %>%
    filter(state == "WA")
    
    
  washington_white_jail <- ggplot(data = incarceration_WA) +
    geom_point(mapping = aes(x= white_jail_pop, y= black_jail_pop)) +
    geom_smooth(mapping = aes(x= white_jail_pop, y= black_jail_pop)) +
    labs(
      title = "White Jail Population vs Black Jail Population in Washington" , 
      x = "White Jail Population" , 
      y = "Black Jail Population"
    )
  
 plot(washington_white_jail) 

  
## Map for geographical distribution of Incarcerations
  
 states_data1 <- incarceration_trends %>%
   filter(year == max(year)) %>%
   mutate(State = tolower(state.name[match(state, state.abb)])) %>%
   group_by(State) %>%
   summarize(sum_rate = sum(black_jail_pop, na.rm = TRUE)/sum(total_jail_pop, na.rm = TRUE))
 
  state_shape <- map_data("state") %>%
   rename(State = region) %>%
    left_join(states_data1, by= "State") 
    
    
   state_map_plot <- ggplot(state_shape, aes(x = long, y = lat, group = group)) +
   geom_polygon(aes(fill = sum_rate),
                color = "white",
                size = .1
   ) +
   coord_map() +
     labs(
       title = "Black Jail Population Rate", 
       ) 
plot(state_map_plot)

  
  
  
  
  
  
  
  
  
  
  
  
  