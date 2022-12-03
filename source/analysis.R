library(tidyverse)
library(dplyr)
library(maps)

# The functions might be useful for A4
source("../source/a4-helpers.R")
## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
incarceration_df <- get_data()

state_f_adult_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(female_adult_jail_pop_1 = sum(female_adult_jail_pop)) %>%
  filter(female_adult_jail_pop_1 == max(female_adult_jail_pop_1)) %>%
  arrange(-female_adult_jail_pop_1) %>%
  head(1) %>%
  pull(state)

max_f_adult_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(female_adult_jail_pop_1 = sum(female_adult_jail_pop)) %>%
  filter(female_adult_jail_pop_1 == max(female_adult_jail_pop_1)) %>%
  arrange(-female_adult_jail_pop_1) %>%
  head(1) %>%
  pull(female_adult_jail_pop_1)

state_m_adult_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(male_adult_jail_pop_1 = sum(male_adult_jail_pop)) %>%
  filter(male_adult_jail_pop_1 == max(male_adult_jail_pop_1)) %>%
  arrange(-male_adult_jail_pop_1) %>%
  head(1) %>%
  pull(state)

max_m_adult_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(male_adult_jail_pop_1 = sum(male_adult_jail_pop)) %>%
  filter(male_adult_jail_pop_1 == max(male_adult_jail_pop_1)) %>%
  arrange(-male_adult_jail_pop_1) %>%
  head(1) %>%
  pull(male_adult_jail_pop_1)


highest_black_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(black_jail_pop_1 = sum(black_jail_pop), female_jail_pop_1 = sum(female_adult_jail_pop)) %>%
  filter(black_jail_pop_1 == max(black_jail_pop_1)) %>%
  pull(black_jail_pop_1)

state_highest_black_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(black_jail_pop_1 = sum(black_jail_pop), female_jail_pop_1 = sum(female_adult_jail_pop)) %>%
  filter(black_jail_pop_1 == max(black_jail_pop_1)) %>%
  pull(state)

cali_highest_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(total_jail_pop_1 = sum(total_jail_pop)) %>%
  filter(total_jail_pop_1 == max(total_jail_pop_1)) %>%
  pull(total_jail_pop_1)

state_highest_jail_pop <- incarceration_df %>%
  drop_na() %>%
  group_by(state) %>%
  summarize(total_jail_pop_1 = sum(total_jail_pop)) %>%
  filter(total_jail_pop_1 == max(total_jail_pop_1)) %>%
  pull(state)
## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
library(ggplot2)

#calculates total prison population for each year from 1970-2018
get_year_prison_pop <- function() {
  df <- incarceration_df %>%
    group_by(year) %>%
    summarise(total_prison_population = sum(total_prison_pop, na.rm = TRUE))
  return(df)
}

# creates the plot fro the total prison population of each year
plot_prison_pop_for_us <- function() {
  df_for_plot <- get_year_prison_pop()
  plot <- ggplot(data = df_for_plot) +
    geom_col(aes(x = year, y = total_prison_population)) +
    labs(
      title = "Prison population over time"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_prison_pop_for_us()
## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#gets the prison population for each state input into the function
get_prison_pop_by_states <- function(states) {
  df <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(year, state) %>%
    summarise(total_prison_population = sum(total_prison_pop, na.rm = TRUE))
  return(df)
}
get_prison_pop_by_states(c("CA", "CO", "DC", "NY", "AL", "TX"))

#plots the state priosn population on a line graph 
plot_jail_pop_by_states <- function(states) {
  df_for_plot <- get_prison_pop_by_states(states)
  plot <- ggplot(data = df_for_plot) +
    geom_line(aes(x = year, y = total_prison_population, color = state)) +
    labs(
      title = "State prison population over time"
    ) +
    scale_y_continuous(labels = scales::comma)
  return(plot)
}

plot_jail_pop_by_states(c("CA", "CO", "WA", "NY", "AL", "TX"))
## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#calculate the black jail population for each region
total_black_jail_pop_region <- function() {
  df <- incarceration_df %>%
    group_by(region) %>%
    drop_na() %>%
    summarize(black_jail_pop_1 = sum(black_jail_pop)) %>%
    select(region, black_jail_pop_1)
  return(df)
}

total_black_jail_pop_region()

#calculate the latinx jail population for each region
total_latinx_jail_pop_region <- function() {
  df <- incarceration_df %>%
    group_by(region) %>%
    drop_na() %>%
    summarize(latinx_jail_pop_1 = sum(latinx_jail_pop)) %>%
    select(region, latinx_jail_pop_1)
  return(df)
}

total_latinx_jail_pop_region()

combined_data <- left_join(total_black_jail_pop_region(), total_latinx_jail_pop_region(), by = "region")

black_latinx_data <- combined_data %>%
  select(region, latinx_jail_pop_1, black_jail_pop_1) %>%
  gather(key = race, value = population, -region)

#creates a barchart comparing black and latinx jail popualtions by region
plot_black_latinx_jail_pop <- function() {
  ggplot(black_latinx_data) +
    geom_col(
      mapping = aes(x = region, y = population, fill = race), position = "dodge"
    ) +
    labs(
      title = "Black vs Latinx jail populations by region"
    ) +
    scale_y_continuous(labels = scales::comma)
}

plot_black_latinx_jail_pop()

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
## Load data frame ----
# show the native_prison_pop in each state
total_native_prison_pop_state <- function() {
  df2 <- incarceration_df %>%
    group_by(state) %>%
    drop_na() %>%
    summarize(native_prison_pop_1 = sum(native_prison_pop)) %>%
    select(state, native_prison_pop_1)%>%
  mutate(Code = state)%>%
  return(df2)
}

total_native_prison_pop_state()

state_name_with_codes <- read.csv("state_names_and_codes.csv")

native_pop_state <-left_merge <- left_join(total_native_prison_pop_state(), state_name_with_codes, by = "Code")

native_pop_state_2 <- native_pop_state %>%
  mutate(region = tolower(State))
  
state_map <- map_data("state") 

native_prison_pop_map <- left_join(state_map, native_pop_state_2, by = "region")

#creates a map shaded with the different colors that correspond with the native prison population for each state
native_prison_pop_national <- function(){
  plot <- ggplot(native_prison_pop_map) +
    geom_polygon(
      mapping= aes(x = long, y = lat, group = group, fill = native_prison_pop_1),
      color= "white",
      size = .1
    ) +
    coord_map() +
    labs(
      title = "Native prison population for each state"
    ) +
    scale_fill_continuous(low = "#132B43", high = "Red") +
    labs(fill = "Native Pop")
  return(plot)
}

native_prison_pop_national()





