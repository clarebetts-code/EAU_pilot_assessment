#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Code to support the pilot assessment
# written by clare betts january 2021
#
# Improvements needed:
# 1 - if target is 0, adjust calculations accordingly
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(directlabels)
library(viridis)
library(scales)


# read in helper functions
source("pilot_assessment_functions.R")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# User set some inputs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

long_term <- 10
short_term <- 5
target_term <- 5 # not currently implemented

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading & processing data
# outputs are assigned to the global environment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

load_process_metadata("metadata.csv") 

load_process_data("25.year.data.csv") 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculating smoothed trend
# warnings produced because some indicators have too few data
# need to make an informed choice about how appropriate it is to do trend assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use loess smoother to model value ~ time, and extract predicted values
save_smoothed_trend(dat_list)

smoothed_trend <- get_smoothed_trend(dat_list)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do assessment and save output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

do_assessment(
  variables = variables,
  targets = targets,
  thresholds = thresholds,
  smoothed_trend = smoothed_trend
) %>%
  map2(.y = names(.), ~write_csv(.x, path = paste0(.y, ".csv")))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assessment_long <- read.csv("assessment_long.csv") %>%
  left_join(goal_indicator_lookup)

assessment_short <- read.csv("assessment_short.csv")%>%
  left_join(goal_indicator_lookup)

assessment_target <- read.csv("assessment_target.csv")%>%
  left_join(goal_indicator_lookup)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# visualisation helper functions
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# colours:
# https://www.visualisingdata.com/2019/08/five-ways-to-design-for-red-green-colour-blindness/
# increase = good, decrease = bad
colour_lookup <- data.frame(trend = c("Strong improvement",
                                      "Moderate improvement",
                                      "Little change",
                                      "Moderate decline", 
                                      "Strong decline",
                                      "Unknown",
                                      "Target met",
                                      "Substantial progress",
                                      "Some progress towards target",
                                      "Insufficient  progress",
                                      "No target"),
                            cols = c("#006164", "#57C4AD", "#E6E1BC", "#EDA247", "#DB4325", "grey","#006164", "#006164", "#57C4AD",  "#DB4325", "grey")) %>%
  #specify factors with a set order
  mutate(trend = factor(trend, levels = trend),
         cols = factor(cols, levels = unique(cols)))

# visualise colours
ggplot(colour_lookup, aes(trend, fill = trend)) +
  geom_bar() +
  scale_fill_manual(values = as.character(colour_lookup$cols)) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Plot results - assessment of change
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# summarise by primary goal of Clean Air
visualise_assessment(classification = "Primary.goal", 
                     group = "Clean air",
                     x = assessment_long,
                     myLab = "Long term")


# summarise indicator A1
visualise_assessment(classification = "Indicator", 
                     group = "A1",
                     x = assessment_target,
                     myLab = "Target assessment")


# summarise by natural capital framework of Pressure
visualise_assessment(classification = "natural.capital.framework", 
                     group = "Pressure",
                     x = assessment_target,
                     myLab = "Target assessment")


# bespoke grouping
assessment_short <- assessment_short %>%
  mutate(Group = case_when(Indicator %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "B3", "D2", "D3", "E3", "J1") ~ "Atmosphere"))

assessment_long <- assessment_long %>%
  mutate(Group = case_when(Indicator %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "B3", "D2", "D3", "E3", "J1") ~ "Atmosphere"))

assessment_target <- assessment_target %>%
  mutate(Group = case_when(Indicator %in% c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "B3", "D2", "D3", "E3", "J1") ~ "Atmosphere"))

p1 <- visualise_assessment(classification = "Group", 
                           group = "Atmosphere",
                           x = assessment_short,
                           myLab = "Short term") +
  labs(title = "")

p2 <- visualise_assessment(classification = "Group", 
                           group = "Atmosphere",
                           x = assessment_long,
                           myLab = "Long term")

p3 <- visualise_assessment(classification = "Group", 
                     group = "Atmosphere",
                     x = assessment_target,
                     myLab = "Target assessment")+
  labs(title = "")

cowplot::plot_grid(p1, p2, p3,
                   nrow = 1)

# 
# 
# 
# # summarise by primary goal
# primary_goals <- unique(goal_indicator_lookup$Primary.goal)
# 
# i <- "Clean air" 
# 
# # summarise by goal
# for (i in primary_goals){
#   temp <- data.frame("Primary.goal" = c(assessment_long$Primary.goal, assessment_short$Primary.goal),
#                      "category" = c(assessment_long$category, assessment_short$category),
#                      "term" = c(rep("Long term", nrow(assessment_long)),
#                                 rep("Short term", nrow(assessment_short)))) %>%
#     dplyr::filter(Primary.goal == i)
#   
#   assessment_table <- assessment_table.builder(temp)
#   
#   figure <- assessment_plot(assessment_table) +
#     labs(title = i)
#   
#   print(figure)
# } 
# 
# # summarise by indicator
# i <- "A1"
# 
# for (i in sort(unique(assessment$Indicator))){
#   temp <- data.frame("Indicator" = c(assessment_long$Indicator, assessment_short$Indicator),
#                      "category" = c(assessment_long$category, assessment_short$category),
#                      "term" = c(rep("Long term", nrow(assessment_long)),
#                                 rep("Short term", nrow(assessment_short)))) %>%
#     dplyr::filter(Indicator == i)
#   
#   # if there is data for the indicator
#   if(nrow(temp) > 0){
#     assessment_table <- assessment_table.builder(temp)
#     
#     figure <- assessment_plot(assessment_table) +
#       labs(title = i)
#     
#     print(figure)
#   }
# } 
# 
# # summarise by natural capital framework
# i <- "Pressure"
# for (i in sort(unique(assessment$natural.capital.framework))){
#   temp <- data.frame("natural.capital.framework" = c(assessment_long$natural.capital.framework, assessment_short$natural.capital.framework),
#                      "category" = c(assessment_long$category, assessment_short$category),
#                      "term" = c(rep("Long term", nrow(assessment_long)),
#                                 rep("Short term", nrow(assessment_short)))) %>%
#     dplyr::filter(natural.capital.framework == i)
#   
#   if(nrow(temp) > 0){
#     assessment_table <- assessment_table.builder(temp)
#     
#     figure <- assessment_plot(assessment_table) +
#       labs(title = i)
#     
#     print(figure)
#   }
# } 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Plot results - target assessment
# # an example
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# colour_lookup <- data.frame(score = c("On track to exceed target",
#                                       "On track to achieve target",
#                                       "Progress made but insufficient to meet target", 
#                                       "Little progress made",
#                                       "unknown"),
#                             cols = c("#006164", "#57C4AD", "#EDA247", "#DB4325", "grey"))
# 
# temp <- data.frame(variable = c("A1", "A2", "A3", "A4", "A5", "A6", "A7"),
#                    score = c(3.8, 4, 4, 3, 3, 2, 2)) %>%
#   arrange(score)
# 
# # assign score - no weighting involved yet
# # haven't averaged across composite indicators
# assessment_targets <- assessment_targets %>%
#   dplyr::mutate(score = case_when(category == "On track to exceed target" ~ 5,
#                                   category == "On track to achieve target" ~ 4,
#                                   category == "Progress made but insufficient to meet target" ~ 3, 
#                                   category == "Little progress made" ~ 2,
#                                   category == "unknown" ~ 1))

# temp <- assessment_targets[assessment_targets$Primary.goal == "Thriving plants and wildlife", ] %>%
#   dplyr::arrange(score) %>%
#   # make variable a factor to preserve the ordering
#   dplyr::mutate(variable = factor(variable, levels = variable))
# 
# ggplot(temp, aes(x = reorder(variable, desc(variable)), y = score, colour = score)) +
#   geom_point(size = 8) +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     #axis.text = element_text( size=48 ),
#     legend.position="none"
#   ) +
#   scale_y_continuous(labels = rev(c("On track to \nexceed target",
#                                     "On track to \nachieve target",
#                                     "Progress made \nbut insufficient \nto meet target", 
#                                     "Little progress \nmade",
#                                     "Unknown")),
#                      breaks = c(1, 2, 3, 4, 5),
#                      limits = c(1, 5)) +
#   scale_color_gradient(low= "#DB4325", high="#006164") +
#   ylab("") +
#   xlab("")
# 
# 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# # haven't updated code from here on, the below probably won't work at all anymore
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# 
# 
# 
# 
# 
# 
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
# # try another way of visualizing assessment
# 
# colour_lookup <- data.frame(trend = c("strong increase",
#                                       "moderate increase",
#                                       "little change",
#                                       "moderate decrease", 
#                                       "strong decrease",
#                                       "unknown"),
#                             cols = c("#006164", "#57C4AD", "#E6E1BC", "#EDA247", "#DB4325", "grey"))
# 
# temp <- assessment[assessment$Primary.goal == "Thriving plants and wildlife", ]
# 
# temp <- mutate(temp,
#                score = case_when(category == "unknown" ~ 0,
#                                  category == "strong decrease" ~ 1,
#                                  category == "moderate decrease" ~ 2,
#                                  category == "little change" ~ 3,
#                                  category == "moderate increase" ~ 4,
#                                  category == "strong increase" ~ 5)
# ) %>%
#   arrange(score)
# 
# temp$variable <- factor(temp$variable, levels = temp$variable[order(temp$score)])
# 
# temp <- left_join(temp, colour_lookup, by= c("category" = "trend"))
# 
# ggplot(temp, aes(x = variable, y = score, colour = variable)) +
#   geom_point(size = 4) +
#   coord_flip() +
#   theme_bw() +
#   theme(
#     panel.grid.minor.x = element_blank(),
#     panel.grid.minor.y = element_blank(),
#     panel.grid.major.y = element_blank(),
#     #axis.text = element_text( size=48 ),
#     legend.position="none"
#   ) +
#   scale_colour_manual(values = temp$cols) +
#   scale_y_continuous(labels = c("unknown",
#                                 "strong \ndecrease",
#                                 "moderate \ndecrease",
#                                 "little \nchange",
#                                 "moderate \nincrease",
#                                 "strong \nincrease")) +
#   #ylim(0, 5) +
#   ylab("") +
#   xlab("")
# 
# # radar plot of above
# 
# library(fmsb)
# library(stringr)
# 
# to_plot <- as.data.frame(matrix(temp$score, ncol = length(temp$score)))
# colnames(to_plot) <- temp$variable
# # To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
# to_plot <- rbind(rep(5, length(temp$score)), rep(0, length(temp$score)), to_plot)
# 
# # The default radar chart 
# radarchart(to_plot,
#            vlcex = 0.5,
#            axistype = 0,
#            seg = 5,      #Number of segments
#            vlabels = stringr::str_wrap(temp$variable, width = 20)
# )
# 
# ## Thrivig plants and wildlife
# temp <- assessment[assessment$Primary.goal == "Thriving plants and wildlife", ] %>%
#   mutate(score = case_when(category == "unknown" ~ 0,
#                            category == "strong decrease" ~ 1,
#                            category == "moderate decrease" ~ 2,
#                            category == "little change" ~ 3,
#                            category == "moderate increase" ~ 4,
#                            category == "strong increase" ~ 5)) %>%
#   left_join(.,
#             colour_lookup, 
#             by= c("category" = "trend")) %>%
#   arrange(Indicator)
# 
# 
# ggplot(temp, aes(x = variable, y = score + 1)) +
#   geom_bar(stat="identity", fill = temp$cols) +
#   coord_polar() +
#   scale_x_discrete(labels = temp$Indicator) +
#   labs(x = "Thriving plants and wildlife") +
#   ylim(0,6) +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
# 
# ## air
# temp <- assessment[assessment$Primary.goal == "Clean air", ] %>%
#   mutate(score = case_when(category == "unknown" ~ 0,
#                            category == "strong decrease" ~ 1,
#                            category == "moderate decrease" ~ 2,
#                            category == "little change" ~ 3,
#                            category == "moderate increase" ~ 4,
#                            category == "strong increase" ~ 5)) %>%
#   left_join(.,
#             colour_lookup, 
#             by= c("category" = "trend")) %>%
#   arrange(Indicator)
# 
# 
# ggplot(temp, aes(x = variable, y = score + 1)) +
#   geom_bar(stat="identity", fill = temp$cols) +
#   coord_polar() +
#   scale_x_discrete(labels = temp$Indicator) +
#   labs(x = "Clean air") +
#   ylim(0,6) +
#   theme_bw() +
#   theme(axis.title.y = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
