#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Code to support the pilot assessment
# written by clare betts january 2021
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(directlabels)
library(viridis)

setwd("C:\\Users\\m994810\\Desktop\\1. EAU")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01
# this function doesn't yet distinguish between those indicators which are traveling away 
# from their target because the target is already met, and those where the indicators are s
# imply moving in teh wrong direction
years_until_target_reached <- function(rate.of.change, temp_target, final.value){
  
  # if the trend is decreasing
  if (rate.of.change < 0){
    years <- case_when(
      # if target is greater than the final value
      # i.e. target met already
      temp_target > final.value ~ Inf,
      # everything else calculate number of years
      TRUE ~ abs(log(final.value/temp_target)/log(1 + rate.of.change)))
  }
  
  # if the trend is increasing
  if (rate.of.change > 0) {
    years <- case_when(
      # if the target is smaller than the final value 
      # i.e. target met already
      temp_target < final.value ~ Inf,
      # everything else calculate number of years
      TRUE ~ abs(log(final.value/temp_target)/log(1 + rate.of.change)))
  }
  # no condition for if trend == 0
  
  return(years)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do trend assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# x <- variables[1]
trend_assess_this <- function(x, term = c("short", "long"),
                              targets, 
                              thresholds, 
                              smoothed_trend){
  
  if (term == "long"){
    # alter number of years here
    temp_dat <- smoothed_trend[[x]] # %>%
    #   tail(10) # last 10 values
  } 
  if(term == "short"){
    temp_dat <- smoothed_trend[[variables[i]]] %>%
      tail(5) # last 5 values
  }
  
  final.value <- rev(temp_dat)[1]
  first.value <- temp_dat[1]
  
  rate.of.change <- ((final.value-first.value)/first.value)/length(temp_dat)
  
  # get threshold
  temp_threshold <- thresholds %>%
    dplyr::filter(variable == x) 
  
  target_trend <- temp_threshold$target_trend
  
  # select just thresholds
  temp_threshold <- temp_threshold %>%
    select(Threshold1, Threshold2, Threshold3, Threshold4) 
  
  # if target trend is to decrease then reverse the labels
  trend_labels <- c("strong decline", "moderate decline", "little change", "moderate improvement", "strong improvement") 
  
  if (target_trend == "increase") {
    trend_labels <- trend_labels
  } 
  if (target_trend == "decrease") {
    trend_labels <- rev(trend_labels)
  }
  
  # assign rate of change assessment
  # this seems inelegant, return to this when i have more time
  category <- case_when(rate.of.change < temp_threshold[1] ~ trend_labels[1],
                        rate.of.change >= temp_threshold$Threshold1 & rate.of.change < temp_threshold$Threshold2 ~ trend_labels[2],
                        rate.of.change >= temp_threshold$Threshold2 & rate.of.change < temp_threshold$Threshold3 ~ trend_labels[3],
                        rate.of.change >= temp_threshold$Threshold3 & rate.of.change < temp_threshold$Threshold3 ~ trend_labels[4],
                        rate.of.change > temp_threshold$Threshold4 ~ trend_labels[5])
  
  temp_target <- targets$target[targets$variable == x]
  
  # calculate years until target met
  years <- years_until_target_reached(rate.of.change, temp_target, final.value)
  
  # return result
  return(list(first.value = first.value,
              final.value = final.value,
              rate.of.change = rate.of.change,
              category = category,
              years = years))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do target assessment
# This will need to be returned to later on, at present it is a bit crude
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# x <- assessment.long
target_assess_this <- function(x){
  x <- x %>%
    dplyr::select(variable, rate.of.change,  years,  target, category) %>%
    mutate(
      category = case_when(
        # all those with a long term assessment of "little change" should be "Little progress made"
        category == "little change" ~ "Little progress made",
        # all those with infinite years to target shoudl be "Little progress made"
        is.infinite(years) ~ "Little progress made",
        # all which will reach their target in more than 15 years should be "Progress made but insufficient to meet target"
        years > 15 ~ "Progress made but insufficient to meet target",
        # all which will reach their target in less than 15 years should be "On track to achieve target"
        years <= 15 ~ "On track to achieve target",
        # all which will reach their target in less than 5 years should be "On track to exceed target"
        years < 5 ~ "On track to exceed target"
      )
    )
  return(x)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# loading data
# processing data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

metadata <- read.csv("metadata.csv")

# store targets
targets <- unique(metadata[, c("variable", "target")])

# store thresholds
thresholds <- metadata[, c("variable", "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend")]

goal.indicator.lookup <- unique(metadata[, c("variable", "Indicator", "Primary.goal", "natural.capital.framework")]) 

dat <- read.csv("25.year.data.csv") %>% 
  mutate(
    # value as numeric
    value = as.numeric(value),
    # variable as factor
    variable = as.factor(variable)
    ) %>%
  # get rid of NAs
  drop_na() %>%
  # get rid of unused factor levels
  droplevels() %>%
  # group by variable
  group_by(variable)

variables <- levels(dat$variable)

dat_list <- dat  %>%
  # split in to a list of data frames
  group_split() %>%
  # assign names
  purrr::set_names(variables)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# calculating smoothed trend
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# use loess smoother to model value ~ time, and extract predicted values
smoothed_trend <- map(dat_list, ~predict(loess(value ~ year, data = .x))) %>%
  set_names(variables)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# a data frame to store assessment results in
assessment.short <- data.frame("variable" = variables,
                         "first.value" = NA,
                         "final.value" = NA,
                         "rate.of.change" = NA, 
                         "category" = NA, 
                         "years" = NA)

assessment.short <- left_join(assessment.short, targets)

assessment.long <- assessment.short

# Do a long term and short term assessment on all variables
for (i in 1:length(variables)){
  
 long.term.assessment <- trend_assess_this(variables[i], term = "long",
                                           targets = targets, 
                                           thresholds = thresholds, 
                                           smoothed_trend = smoothed_trend)
 
 assessment.long$first.value[i] <- long.term.assessment$first.value
 assessment.long$final.value[i] <- long.term.assessment$final.value
 assessment.long$rate.of.change[i] <- long.term.assessment$rate.of.change
 assessment.long$category[i] <- long.term.assessment$category
 assessment.long$years[i] <- long.term.assessment$years
 
 short.term.assessment <- trend_assess_this(variables[i], term = "short",
                                           targets = targets, 
                                           thresholds = thresholds, 
                                           smoothed_trend = smoothed_trend)
 
 assessment.short$first.value[i] <- short.term.assessment$first.value
 assessment.short$final.value[i] <- short.term.assessment$final.value
 assessment.short$rate.of.change[i] <- short.term.assessment$rate.of.change
 assessment.short$category[i] <- short.term.assessment$category
 assessment.short$years[i] <- short.term.assessment$years
 
}        

# convert long term assessment into an assessment against targets
assessment.targets <- target_assess_this(assessment.long)


# add indicator & goal onto assessments
assessment.long <- full_join(assessment.long, goal.indicator.lookup)
assessment.short <- full_join(assessment.short, goal.indicator.lookup)
assessment.targets <- full_join(assessment.targets, goal.indicator.lookup)


# fill in "unknown" for all those where no trend assessment could be done
assessment.long$category[is.na(assessment.long$category)] <- "unknown"
assessment.short$category[is.na(assessment.short$category)] <- "unknown"
assessment.targets$category[is.na(assessment.targets$category)] <- "unknown"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
write.csv(assessment.long, 
          "assessment.long.csv", 
          row.names = FALSE)
write.csv(assessment.short, 
          "assessment.short.csv", 
          row.names = FALSE)
write.csv(assessment.targets, 
          "assessment.targets.csv", 
          row.names = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# load assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

assessment.long <- read.csv("assessment.long.csv")
assessment.short <- read.csv("assessment.short.csv")
assessment.targets <- read.csv("assessment.targets.csv")

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

i <- "Thriving plants and wildlife" 

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
