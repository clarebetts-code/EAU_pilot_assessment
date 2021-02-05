#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Helper functions to support the pilot assessment
# written by clare betts january 2021
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process metadata file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "variable", "target", "Indicator", "Primary.goal", "natural.capital.framework", 
# "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend"
# foo <- "metadata.csv"


#' @title load and process metadata
#' load_process_metadata takes an input csv and parses it to return three
#' dataframes, targets, thresholds and goal.indicator.lookup. These outputs are
#' assigned directly to the global environment.
#'
#' @param filename the name of the file which the data are to be read from.
#' Each row of the table appears as one line of the file. If it does not
#' contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#' 
#' @return Three dataframes.
#' @export
#'
load_process_metadata <- function(filename){
  
  # Checks that the supplied file path is a string
  if(!is.character(filename)){
    stop("Filename must be a string")
  }
  
  # Checks that the supplied file actually exists. 
  if(!file.exists(filename)){
    stop(paste0("Cannot find a file called '", filename, "', check filename and try again."))
  }
  
  # read in data
  dat <- read.csv(filename)
  
  # store targets
  targets <- unique(dat[, c("variable", "target", "target_year", "target_trend")])
  
  # store thresholds
  thresholds <- dat[, c("variable", "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend")]
  
  goal_indicator_lookup <- unique(dat[, c("variable", "Indicator", "Primary.goal", "natural.capital.framework")]) 
  
  # assign objects to global environment
  assign("targets", targets, envir = .GlobalEnv)
  assign("thresholds", thresholds, envir = .GlobalEnv)
  assign("goal_indicator_lookup", goal_indicator_lookup, envir = .GlobalEnv)
  
  #return(NULL)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process raw data file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "value", "variable"
# foo <- "25.year.data.csv"
load_process_data <- function(foo){
  
  # read in data
  dat <- read.csv(foo)
  
  dat <- dat %>% 
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
  
  # assign objects to global environment
  assign("dat_list", dat_list, envir = .GlobalEnv)
  assign("variables", variables, envir = .GlobalEnv)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# functions calculate the smoothed trend and return warnings where appropriate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- dat_list
get_smoothed_trend <- function(x){
  evie <- map(x, ~quiet_smoother(x=.x)) %>%
    set_names(variables)
  
  # pull out result
  evie_result <- map(evie, "result")
  
  # deal with warnings
  evie_warnings <- map_lgl(evie, ~length(.x$warnings) > 1)
  
  if (sum(evie_warnings) > 0){
    print("Warnings generated for:")
    print(names(evie)[evie_warnings]) 
  }
  
  # return the result
  return(evie_result)
}

# function to do the smoothing
# x <- dat_list[[1]]
quiet_smoother <- quietly(function(x){
  predict(loess(value ~ year, data = x))
})



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01

years_until_target_reached <- function(rate_of_change, final_value, temp_target, temp_target_trend){
  
    years <- case_when(
      # if the desired trend is decreasing, and target is greater than the final value
      # i.e. target met already
      temp_target_trend == "decrease" & temp_target > final_value ~ 0,
      # if the trend is increasing, and the target is smaller than the final value 
      # i.e. target met already
      temp_target_trend == "increase" & temp_target < final_value ~ 0,
      
      # if target is less than final value, but rate of change is positive
      # i.e. moving away from target
      temp_target < final_value & rate_of_change > 0 ~ Inf,
      # if target is greater than final value, but rate of change is negative
      # i.e. moving away from target
      temp_target > final_value & rate_of_change < 0 ~ Inf,
      
      # everything else calculate number of years
      TRUE ~ abs(log(final_value/temp_target)/log(1 + rate_of_change))
    )

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
    # test if there are enough years of data
    if(length(smoothed_trend[[x]]) >= long_term){
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(long_term) 
    } else {
      temp_dat <- {}
    }
  } 
  
  if(term == "short"){
    # test if there are enough years of data
    if(length(smoothed_trend[[x]]) >= short_term){
      temp_dat <- smoothed_trend[[x]] %>%
        tail(short_term)
      } else {
          temp_dat <- {}
        } 
    }
  
  final_value <- rev(temp_dat)[1]
  first_value <- temp_dat[1]
  
  rate_of_change <- ((final_value-first_value)/first_value)/length(temp_dat)
  
  # get threshold
  temp_threshold <- thresholds %>%
    dplyr::filter(variable == x) 
  
  target_trend <- temp_threshold$target_trend
  
  # select just thresholds
  temp_threshold <- temp_threshold %>%
    select(Threshold1, Threshold2, Threshold3, Threshold4) 
  
  # if target trend is to decrease then reverse the labels
  trend_labels <- c("Strong decline", "Moderate decline", "Little change", "Moderate improvement", "Strong improvement") 
  
  if (target_trend == "increase") {
    trend_labels <- trend_labels
  } 
  if (target_trend == "decrease") {
    trend_labels <- rev(trend_labels)
  }
  
  # assign rate of change assessment
  # this seems inelegant, return to this when i have more time
  if (is_empty(rate_of_change)){
    first_value <- NA
    final_value <- NA
    rate_of_change <- NA
    category <- "Unknown"
    
    } else {
      category <- case_when(rate_of_change < temp_threshold[1] ~ trend_labels[1],
                        rate_of_change >= temp_threshold$Threshold1 & rate_of_change < temp_threshold$Threshold2 ~ trend_labels[2],
                        rate_of_change >= temp_threshold$Threshold2 & rate_of_change < temp_threshold$Threshold3 ~ trend_labels[3],
                        rate_of_change >= temp_threshold$Threshold3 & rate_of_change < temp_threshold$Threshold4 ~ trend_labels[4],
                        rate_of_change > temp_threshold$Threshold4 ~ trend_labels[5]) 

  }
  # return result
  return(list(first_value = first_value,
              final_value = final_value,
              rate_of_change = rate_of_change,
              category = category))
}




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do target assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# x <- variables[1]
target_assess_this <- function(x, 
                               targets, 
                               smoothed_trend){
  

  # test if there are enough years of data
  # currently uses 5 years of data
    if(length(smoothed_trend[[x]]) < 5){
      stop("Too few years of data for a target assessment")
    } else {
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(5) 
    }
  
  first_value <- temp_dat[1]
  final_value <- rev(temp_dat)[1]
  
  rate_of_change <- ((final_value-first_value)/first_value)/length(temp_dat)
  
  temp_target <- targets$target[targets$variable == x]
  
  temp_target_trend <- targets$target_trend[targets$variable == x]
  
  temp_target_year <- targets$target_year[targets$variable == x]
    
  years <- years_until_target_reached(rate_of_change, final_value, temp_target, temp_target_trend)
 
  category <- case_when(
    # if no target exists then years == NA
    is.na(years) ~ "No target",
    
    # already reached target value & year & target met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final_value <= temp_target & temp_target_trend == "decrease" ~ "Target met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final_value >= temp_target & temp_target_trend == "increase" ~ "Target met",
    
    # already reached target value & year & target not met
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final_value <= temp_target & temp_target_trend == "increase" ~ "Target not met",
    temp_target_year < as.numeric(format(Sys.Date(), "%Y")) & final_value >= temp_target & temp_target_trend == "decrease" ~ "Target not met",
    
    # already passed target
    temp_target_trend == "decrease" & final_value <= temp_target ~ "Target met",
    temp_target_trend == "increase" & final_value >= temp_target ~ "Target met",
    
    # moving away from target
    rate_of_change < 0 & final_value < temp_target ~ "Insufficient  progress",
    rate_of_change > 0 & final_value > temp_target ~ "Insufficient  progress",
    
    # moving in right direction
    # if target will be missed by more than 5 years
    years > (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) + 5 ~ "Insufficient  progress",
    # if target will be missed but will reach it within 5 years of target year
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) + 5 ~ "Some progress towards target",
    # if target will be reached before the target year
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) ~ "Substantial progress"
      )

  return(list(first_value = first_value,
              final_value = final_value,
              rate_of_change = rate_of_change, 
              years = years, 
              category = category))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to do the three assessments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

do_assessment <- function(variables, 
                          targets,
                          thresholds,
                          smoothed_trend){
  # a data frame to store assessment results in
  assessment_short <- data.frame("variable" = variables,
                                 "first_value" = NA,
                                 "final_value" = NA,
                                 "rate_of_change" = NA, 
                                 "category" = NA)
  
  assessment_short <- left_join(assessment_short, targets)
  
  assessment_long <- assessment_target <- assessment_short
  
  # i <- 1
  for (i in 1:length(variables)){
    # Do a long term assessment on all variables
    long_term_assessment <- trend_assess_this(variables[i], 
                                              term = "long",
                                              targets = targets, 
                                              thresholds = thresholds, 
                                              smoothed_trend = smoothed_trend)
    
    assessment_long$first_value[i] <- long_term_assessment$first_value
    assessment_long$final_value[i] <- long_term_assessment$final_value
    assessment_long$rate_of_change[i] <- long_term_assessment$rate_of_change
    assessment_long$category[i] <- long_term_assessment$category

    # Do a short term assessment on all variables
    short_term_assessment <- trend_assess_this(variables[i], 
                                               term = "short",
                                               targets = targets, 
                                               thresholds = thresholds, 
                                               smoothed_trend = smoothed_trend)
    
    assessment_short$first_value[i] <- short_term_assessment$first_value
    assessment_short$final_value[i] <- short_term_assessment$final_value
    assessment_short$rate_of_change[i] <- short_term_assessment$rate_of_change
    assessment_short$category[i] <- short_term_assessment$category

    # Do an assessment against targets
    trend_assessment <- target_assess_this(variables[i],
                                           targets = targets, 
                                           smoothed_trend = smoothed_trend)
    
    assessment_target$first_value[i] <- trend_assessment$first_value
    assessment_target$final_value[i] <- trend_assessment$final_value
    assessment_target$rate_of_change[i] <- trend_assessment$rate_of_change
    assessment_target$category[i] <- trend_assessment$category
    assessment_target$years[i] <- trend_assessment$years
  }     
  
  return(list(assessment_long = assessment_long,
              assessment_target = assessment_target,
              assessment_short = assessment_short))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to build the assessment table which the visualisations are built from
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- temp
# cols <- colour_lookup
assessment_table_builder <- function(x, cols = colour_lookup){
  
  # count the number of variables in each category
  ass_tab <- reshape::melt(table(x$category)/sum(table(x$category))) %>%
    dplyr::select(
      trend = Var.1,
      value
    ) %>%
    # only over zero
    dplyr::filter(
      value > 0
    ) %>%
    left_join(., cols) 
  
  # get trend factor levels in the correct order
  ass_tab$trend <- factor(ass_tab$trend,
                          levels = levels(cols$trend))
  ass_tab$cols <- factor(ass_tab$cols,
                          levels = levels(cols$cols))
  
  return(droplevels(ass_tab))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to convert the assessment table to a plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assessment_plot <- function(x){
  ggplot(x, aes(x = 1, y = value, fill = trend)) +
    geom_bar(position="stack", stat="identity") + 
    scale_y_continuous(labels = scales::percent) + 
    scale_fill_manual(values = levels(x$cols)) +
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position="none",
          plot.title = element_text(hjust = 0.5, size = 25),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "") + 
    geom_text(aes(label = trend), 
              #label = function(x) stringr::str_wrap(x, width = 10),
              size = 4, 
              position = position_stack(vjust = 0.5)) 
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment_target
# myLab <- "Long term"
# classification <- "Primary.goal"
# group <- "Clean air"
visualise_assessment <- function(classification = "Primary.goal", 
                                 group = "Clean air",
                                 x,
                                 myLab = "assessment"){
  
  temp <- data.frame(classification = x[classification],
                     "category" = x$category) %>%
    dplyr::filter(.data[[eval(classification)]] == group)
  
  assessment_table <- assessment_table_builder(temp) 
  
  figure <- assessment_plot(assessment_table) +
    labs(title = group,
         x = myLab)
  
  print(figure)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to plot a smoothed trend
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dat <- dat_list[[1]]
smoothed_plot <- function(dat){
  xstart1 <- max(dat$year) - short_term
  xstart2 <- max(dat$year) - long_term
  xend <- max(dat$year)
  
  p1 <- ggplot(dat, aes(x = year, y = value)) + 
    geom_point() +
    labs(y = unique(dat$variable)) + 
    stat_smooth(method = "loess", formula = y ~ x, span = 0.75) +
    theme(axis.title.x=element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  
  
  
  p2 <- ggplot(dat, aes(x = year, y = value)) + 
    geom_blank() +
    # short term assessment period
    geom_segment(aes(x = xstart1,
                     y = 1,
                     xend = xend,
                     yend = 1),
                 colour = "#006164",
                 size = 3) +
    # long term assessment period
    geom_segment(aes(x = xstart2,
                     y = 2,
                     xend = xend,
                     yend = 2),
                 colour = "#57C4AD",
                 size = 3) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title.y = element_text(color = "white"),
          axis.text.y = element_text(color = "white")) + 
    scale_y_continuous(breaks=c(0.5, 2.5),
                       limits = c(0.5, 2.5)) + 
    scale_x_continuous(breaks= scales::pretty_breaks())
    
    
    cowplot::plot_grid(p1, p2,
                       nrow = 2,
                       rel_heights = c(6, 1))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to save the smoothed trends
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#x <- dat_list
save_smoothed_trend <- function(x){
  
  filenames <- paste0("smoothed_trends\\", names(x), ".png")
  
  plots <- map(x, smoothed_plot) 
  
  for (i in 1:length(plots)){
    ggsave(filename = filenames[i],
           plot = plots[[i]],
           device ="png")
  }
}