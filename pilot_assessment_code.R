#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Code to support the pilot assessment
# written by clare betts january 2021
#
# Improvements needed:
# 1 - correct instances where the indicator is moving away from target
# 2 - adapt rate of change to be calculated from most recent 5 years of data
# 3 - check result for A1 NH3
# 4 - if target is 0, adjust calculations accordingly
# 5 - check behavior where no target exists
# 6 - long term = 10 or more
# 7 - short term = 5
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(directlabels)
library(viridis)

#setwd("C:\\Users\\m994810\\Desktop\\1. EAU")

# read in helper functions
source("pilot_assessment_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User set some inputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

long.term <- 10
short.term <- 5



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading & processing data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

read.csv("metadata.csv") %>%
  load_process_metadata() %>%
  list2env(envir = .GlobalEnv)# Can use assign() from base to do this inside function

read.csv("25.year.data.csv") %>%
  load_process_data() %>%
  list2env(envir = .GlobalEnv)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculating smoothed trend
# come back this warnings produced
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use loess smoother to model value ~ time, and extract predicted values
smoothed_trend <- map(dat_list, ~predict(loess(value ~ year, data = .x))) %>%
  set_names(variables)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do assessment and save output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

do_assessment(
  variables = variables,
  targets = targets,
  thresholds = thresholds,
  smoothed_trend = smoothed_trend
) %>%
  map2(.y=names(.),~write_csv(.x,path = paste0(.y,".csv")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assessment.long <- read.csv("assessment.long.csv")
assessment.short <- read.csv("assessment.short.csv")
assessment.target <- read.csv("assessment.target.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualisation helper functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# colours:
# https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/

# helper functions
# increase = good, decrease = bad
colour.lookup <- data.frame(trend = c("strong improvement",
                                      "moderate improvement",
                                      "little change",
                                      "moderate decline", 
                                      "strong decline",
                                      "unknown"),
                            cols = c("#006164", "#57C4AD", "#E6E1BC", "#EDA247", "#DB4325", "grey")) %>%
  #specify factors with a set order
  mutate(trend = factor(trend, levels = trend),
         cols = factor(cols, levels = cols))

# visualise colours
ggplot(colour.lookup, aes(trend, fill = trend)) +
  geom_bar() +
  scale_fill_manual(values = levels(factor(colour.lookup$cols))) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) 


# Function to build teh assessment table which the visualisations are built from

assessment.table.builder <- function(x, cols = colour.lookup){
  
  # count the number of variables in each category
  ass.tab <- reshape::melt(table(x$category, x$term)/colSums(table(x$category, x$term))) %>%
    dplyr::select(
      trend = Var.1,
      time_period = Var.2,
      value
    ) %>%
    # only over zero
    dplyr::filter(
      value > 0
    ) %>%
    left_join(., cols) 
  
  # get trend factor levels in he correct order
  ass.tab$trend <- factor(ass.tab$trend,
                          levels = levels(cols$trend))
  
  return(droplevels(ass.tab))
}

# function to convert the assessment table to a plot
assessment.plot <- function(x){
  ggplot(x, aes(x = time_period, y = value, fill = trend)) +
    geom_bar(position="stack", stat="identity") + 
    scale_y_continuous(labels = scales::percent) + 
    scale_fill_manual(values = levels(x$cols)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 25)) +
    labs(x = "",
         y = "") + 
    geom_text(aes(label = trend), 
              size = 4, 
              position = position_stack(vjust = 0.5)) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot results - long term trend assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# summarise by primary goal
primary_goals <- unique(goal.indicator.lookup$Primary.goal)

i <- "Clean air" 

# summarise by goal
for (i in primary_goals){
  temp <- data.frame("Primary.goal" = c(assessment.long$Primary.goal, assessment.short$Primary.goal),
                     "category" = c(assessment.long$category, assessment.short$category),
                     "term" = c(rep("Long term", nrow(assessment.long)),
                                rep("Short term", nrow(assessment.short)))) %>%
    dplyr::filter(Primary.goal == i)
  
  assessment.table <- assessment.table.builder(temp)
  
  figure <- assessment.plot(assessment.table) +
    labs(title = i)
  
  print(figure)
} 

# summarise by indicator
i <- "A1"

for (i in sort(unique(assessment$Indicator))){
  temp <- data.frame("Indicator" = c(assessment.long$Indicator, assessment.short$Indicator),
                     "category" = c(assessment.long$category, assessment.short$category),
                     "term" = c(rep("Long term", nrow(assessment.long)),
                                rep("Short term", nrow(assessment.short)))) %>%
    dplyr::filter(Indicator == i)
  
  # if there is data for the indicator
  if(nrow(temp) > 0){
    assessment.table <- assessment.table.builder(temp)
    
    figure <- assessment.plot(assessment.table) +
      labs(title = i)
    
    print(figure)
  }
} 

# summarise by natural capital framework
i <- "Pressure"
for (i in sort(unique(assessment$natural.capital.framework))){
  temp <- data.frame("natural.capital.framework" = c(assessment.long$natural.capital.framework, assessment.short$natural.capital.framework),
                     "category" = c(assessment.long$category, assessment.short$category),
                     "term" = c(rep("Long term", nrow(assessment.long)),
                                rep("Short term", nrow(assessment.short)))) %>%
    dplyr::filter(natural.capital.framework == i)
  
  if(nrow(temp) > 0){
    assessment.table <- assessment.table.builder(temp)
    
    figure <- assessment.plot(assessment.table) +
      labs(title = i)
    
    print(figure)
  }
} 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot results - target assessment
# an example
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

colour.lookup <- data.frame(score = c("On track to exceed target",
                                      "On track to achieve target",
                                      "Progress made but insufficient to meet target", 
                                      "Little progress made",
                                      "unknown"),
                            cols = c("#006164", "#57C4AD", "#EDA247", "#DB4325", "grey"))

temp <- data.frame(variable = c("A1", "A2", "A3", "A4", "A5", "A6", "A7"),
                   score = c(3.8, 4, 4, 3, 3, 2, 2)) %>%
  arrange(score)

# assign score - no weighting involved yet
# haven't averaged across composite indicators
assessment.targets <- assessment.targets %>%
  dplyr::mutate(score = case_when(category == "On track to exceed target" ~ 5,
                                  category == "On track to achieve target" ~ 4,
                                  category == "Progress made but insufficient to meet target" ~ 3, 
                                  category == "Little progress made" ~ 2,
                                  category == "unknown" ~ 1))

temp <- assessment.targets[assessment.targets$Primary.goal == "Thriving plants and wildlife", ] %>%
  dplyr::arrange(score) %>%
  # make variable a factor to preserve the ordering
  dplyr::mutate(variable = factor(variable, levels = variable))

ggplot(temp, aes(x = reorder(variable, desc(variable)), y = score, colour = score)) +
  geom_point(size = 8) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    #axis.text = element_text( size=48 ),
    legend.position="none"
  ) +
  scale_y_continuous(labels = rev(c("On track to \nexceed target",
                                    "On track to \nachieve target",
                                    "Progress made \nbut insufficient \nto meet target", 
                                    "Little progress \nmade",
                                    "Unknown")),
                     breaks = c(1, 2, 3, 4, 5),
                     limits = c(1, 5)) +
  scale_color_gradient(low= "#DB4325", high="#006164") +
  ylab("") +
  xlab("")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# haven't updated code from here on, the below probably won't work at all anymore
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  








#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# try another way of visualizing assessment

colour.lookup <- data.frame(trend = c("strong increase",
                                      "moderate increase",
                                      "little change",
                                      "moderate decrease", 
                                      "strong decrease",
                                      "unknown"),
                            cols = c("#006164", "#57C4AD", "#E6E1BC", "#EDA247", "#DB4325", "grey"))

temp <- assessment[assessment$Primary.goal == "Thriving plants and wildlife", ]

temp <- mutate(temp,
               score = case_when(category == "unknown" ~ 0,
                                 category == "strong decrease" ~ 1,
                                 category == "moderate decrease" ~ 2,
                                 category == "little change" ~ 3,
                                 category == "moderate increase" ~ 4,
                                 category == "strong increase" ~ 5)
) %>%
  arrange(score)

temp$variable <- factor(temp$variable, levels = temp$variable[order(temp$score)])

temp <- left_join(temp, colour.lookup, by= c("category" = "trend"))

ggplot(temp, aes(x = variable, y = score, colour = variable)) +
  geom_point(size = 4) +
  coord_flip() +
  theme_bw() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    #axis.text = element_text( size=48 ),
    legend.position="none"
  ) +
  scale_colour_manual(values = temp$cols) +
  scale_y_continuous(labels = c("unknown",
                                "strong \ndecrease",
                                "moderate \ndecrease",
                                "little \nchange",
                                "moderate \nincrease",
                                "strong \nincrease")) +
  #ylim(0, 5) +
  ylab("") +
  xlab("")

# radar plot of above

library(fmsb)
library(stringr)

to_plot <- as.data.frame(matrix(temp$score, ncol = length(temp$score)))
colnames(to_plot) <- temp$variable
# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
to_plot <- rbind(rep(5, length(temp$score)), rep(0, length(temp$score)), to_plot)

# The default radar chart 
radarchart(to_plot,
           vlcex = 0.5,
           axistype = 0,
           seg = 5,      #Number of segments
           vlabels = stringr::str_wrap(temp$variable, width = 20)
)

## Thrivig plants and wildlife
temp <- assessment[assessment$Primary.goal == "Thriving plants and wildlife", ] %>%
  mutate(score = case_when(category == "unknown" ~ 0,
                           category == "strong decrease" ~ 1,
                           category == "moderate decrease" ~ 2,
                           category == "little change" ~ 3,
                           category == "moderate increase" ~ 4,
                           category == "strong increase" ~ 5)) %>%
  left_join(.,
            colour.lookup, 
            by= c("category" = "trend")) %>%
  arrange(Indicator)


ggplot(temp, aes(x = variable, y = score + 1)) +
  geom_bar(stat="identity", fill = temp$cols) +
  coord_polar() +
  scale_x_discrete(labels = temp$Indicator) +
  labs(x = "Thriving plants and wildlife") +
  ylim(0,6) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## air
temp <- assessment[assessment$Primary.goal == "Clean air", ] %>%
  mutate(score = case_when(category == "unknown" ~ 0,
                           category == "strong decrease" ~ 1,
                           category == "moderate decrease" ~ 2,
                           category == "little change" ~ 3,
                           category == "moderate increase" ~ 4,
                           category == "strong increase" ~ 5)) %>%
  left_join(.,
            colour.lookup, 
            by= c("category" = "trend")) %>%
  arrange(Indicator)


ggplot(temp, aes(x = variable, y = score + 1)) +
  geom_bar(stat="identity", fill = temp$cols) +
  coord_polar() +
  scale_x_discrete(labels = temp$Indicator) +
  labs(x = "Clean air") +
  ylim(0,6) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
