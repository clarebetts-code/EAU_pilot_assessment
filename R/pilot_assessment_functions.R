#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Helper functions to support the pilot assessment
# written by clare betts january 2021
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process metadata file
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# file must contain columns "variable", "target", "Indicator", "Primary_goal", "natural_capital_framework",
# "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend"
# filename <- "metadata.csv"

#' @title load and process metadata
#' load_process_metadata takes an input string, reads a file in table format and creates a data frame from it,
#' and parses it to return three dataframes: targets, thresholds and goal_indicator_lookup. These outputs are
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
#' @import utils
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

  goal_indicator_lookup <- unique(dat[, c("variable", "Indicator", "Primary_goal", "natural_capital_framework")])

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
# filename <- "25.year.data.csv"

#' @title Load and process data
#' @description load_process_data takes an input string, reads a file in table format, creates a data frame
#' from it and parses it to return two dataframes: dat_list and variables. These outputs are
#' assigned directly to the global environment.
#'
#' @param filename the name of the file which the data are to be read from.
#' Each row of the table appears as one line of the file. If it does not
#' contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#'
#' @return Two dataframes.
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import utils
load_process_data <- function(filename){

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

  dat <- dat %>%
    dplyr::mutate(
      # value as numeric
      value = as.numeric(value),
      # variable as factor
      variable = as.factor(variable)
    ) %>%
    # get rid of NAs
    tidyr::drop_na() %>%
    # get rid of unused factor levels
    droplevels() %>%
    # group by variable
    dplyr::group_by(variable)

  variables <- levels(dat$variable)

  dat_list <- dat  %>%
    # split in to a list of data frames
    dplyr::group_split() %>%
    # assign names
    purrr::set_names(variables)

  # assign objects to global environment
  assign("dat_list", dat_list, envir = .GlobalEnv)
  assign("variables", variables, envir = .GlobalEnv)

}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate the smoothed trend and return warnings where appropriate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- dat_list

#' @title get_smoothed_trend
#' @description Apply a loess smoother trend to your data, predicting value from year.
#' Uses default setting span = 0.75 which is not appropriate for small data sets, warnings are produced
#' and returned to the console.
#'
#' @param x A list if data frames (dat_list) output by load_process_data
#'
#' @return A list of smoothed trends
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stats
get_smoothed_trend <- function(x){

  # function to do the smoothing
  # x <- dat_list[[1]]
  quiet_smoother <- purrr::quietly(function(x){
    predict(loess(value ~ year, data = x))
  })


  evie <- purrr::map(x, ~quiet_smoother(x=.x)) %>%
    purrr::set_names(variables)

  # pull out result
  evie_result <- purrr::map(evie, "result")

  # deal with warnings
  evie_warnings <- purrr::map_lgl(evie, ~length(.x$warnings) > 1)

  if (sum(evie_warnings) > 0){
    print("Warnings generated for:")
    print(names(evie)[evie_warnings])
  }

  # return the result
  return(evie_result)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to plot a smoothed trend
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#dat <- dat_list[[1]]

#' @title smoothed_plot
#' @description called from within save_smoothed_trend.
#' Visualises the smoothed trend for a variable
#'
#' @param dat a data frame, an element from dat_list
#' @param long_term Number of years of data to do the long term assessment from
#' @param short_term Number of years of data to do the short term assessment from
#'
#' @return a graph of the smoothed trend
#'
#' @import cowplot
#' @import ggplot2
#' @import scales
#' @import stats
smoothed_plot <- function(dat,
                          long_term,
                          short_term){
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

#' @title save_smoothed_trend
#' @description Visualises the smoothed trend for a variable
#'
#' @param x a list of data frames containing teh time series of each variable
#' @param filepath file path to specify where to save the plots
#'
#' @return saves a .png to file. The file name is relative to the
#' current working directory, getwd().
#' @export
#'
#' @import purrr
#' @import ggplot2
save_smoothed_trend <- function(x, filepath){

  filenames <- paste0(filepath, names(x), ".png")

  plots <- purrr::map(x, smoothed_plot, 
                      long_term,
                      short_term)

  for (i in 1:length(plots)){
    ggplot2::ggsave(filename = filenames[i],
                    plot = plots[[i]],
                    device ="png")
  }
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01

#' @title years_until_target_reached
#' @description Called from within target_assess_this.
#' Calculates the number of years until the target is reached for a single indicator,
#' using the formula:
#' $$ years = abs(log(final_value/temp_target)/log(1 + rate_of_change))
#'years = abs(log(final_value/temp_target)/log(1 + rate_of_change))
#'see https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01 for more information
#'
#' @param rate_of_change output from trend_assess_this or target_assess_this
#' @param final_value output from trend_assess_this or target_assess_this
#' @param temp_target output from target_assess_this
#' @param temp_target_trend output from target_assess_this
#'
#' @return number of years (integer)
#'
#' @import dplyr
years_until_target_reached <- function(rate_of_change,
                                       final_value,
                                       temp_target,
                                       temp_target_trend){

    years <- dplyr::case_when(
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

#' @title trend_assess_this
#' @description Called from within do_assessment.
#' Performs an assessment of change on an indicator or variable.
#'
#' @param x The name of the variable as a string.
#' @param term Number of years to perform the assessment on.
#' @param thresholds output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
#'
#' @return a list containing the first and last values in the series, the rare of change and the
#' result of the assessment.
#'
#' @import dplyr
#' @import rlang
trend_assess_this <- function(x, 
                              term = 5,
                              thresholds,
                              smoothed_trend){


    if(length(smoothed_trend[[x]]) >= term){
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(term)
    } else {
      temp_dat <- {}
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
    dplyr::select(Threshold1, Threshold2, Threshold3, Threshold4)

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
  if (rlang::is_empty(rate_of_change)){
    first_value <- NA
    final_value <- NA
    rate_of_change <- NA
    category <- "Unknown"

    } else {
      category <- dplyr::case_when(
        rate_of_change < temp_threshold[1] ~ trend_labels[1],
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

#' @title target_assess_this
#' @description Called from within do_assessment.
#' Performs an assessment of change against a target on an indicator or variable.
#'
#' @param x The name of the variable as a string.
#' @param targets output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
#' @param term set by user, number of years of data to use
#'
#' @return a list containing the first and last values in the series, the rare of change, the
#' number of years until target is reached and the result of the assessment.
#'
#' @import dplyr
target_assess_this <- function(x,
                               targets,
                               smoothed_trend,
                               term = 5){


  # test if there are enough years of data
  # currently uses 5 years of data
    if(length(smoothed_trend[[x]]) < term){
      stop("Too few years of data for a target assessment")
    } else {
      # use number of years set at beginning here.
      temp_dat <- smoothed_trend[[x]] %>%
        tail(term)
    }

  first_value <- temp_dat[1]
  final_value <- rev(temp_dat)[1]

  rate_of_change <- ((final_value-first_value)/first_value)/length(temp_dat)

  temp_target <- targets$target[targets$variable == x]

  temp_target_trend <- targets$target_trend[targets$variable == x]

  temp_target_year <- targets$target_year[targets$variable == x]

  years <- years_until_target_reached(rate_of_change, final_value, temp_target, temp_target_trend)

  category <- dplyr::case_when(
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
    years > (temp_target_year - as.numeric(format(Sys.Date(), "%Y")) + 5) ~ "Insufficient  progress",
    # if target will be reached before the target year - this needs to come before "some progress made", case_when ends at first TRUE
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y"))) ~ "Substantial progress",
    # if target will be missed but will reach it within 5 years of target year
    years <= (temp_target_year - as.numeric(format(Sys.Date(), "%Y")) + 5) ~ "Some progress towards target"
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

#' @title do_assessment
#' @description Performs a short term, long term and target assessment on a list of variables.
#'
#' @param variables String containing the names of the variables
#' @param targets output from load_process_metadata
#' @param thresholds output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
#' @param short_term set by user, number of years of data to use for short term assessment
#' @param long_term set by user, number of years of data to use for long term assessment
#' @param target_term set by user, number of years of data to use for target assessment
#'
#' @return a list of three dataframes, one for each assessment
#' 
#' @export
#'
#' @import dplyr
do_assessment <- function(variables,
                          targets,
                          thresholds,
                          smoothed_trend,
                          short_term,
                          long_term,
                          target_term){
  # a data frame to store assessment results in
  assessment_short <- data.frame("variable" = variables,
                                 "first_value" = NA,
                                 "final_value" = NA,
                                 "rate_of_change" = NA,
                                 "category" = NA)

  assessment_short <- dplyr::left_join(assessment_short, targets)

  assessment_long <- assessment_target <- assessment_short

  # i <- 1
  for (i in 1:length(variables)){
    # Do a long term assessment on all variables
    long_term_assessment <- trend_assess_this(variables[i],
                                              term = long_term,
                                              thresholds,
                                              smoothed_trend)

    assessment_long$first_value[i] <- long_term_assessment$first_value
    assessment_long$final_value[i] <- long_term_assessment$final_value
    assessment_long$rate_of_change[i] <- long_term_assessment$rate_of_change
    assessment_long$category[i] <- long_term_assessment$category

    # Do a short term assessment on all variables
    short_term_assessment <- trend_assess_this(variables[i],
                                               term = short_term,
                                               thresholds,
                                               smoothed_trend)

    assessment_short$first_value[i] <- short_term_assessment$first_value
    assessment_short$final_value[i] <- short_term_assessment$final_value
    assessment_short$rate_of_change[i] <- short_term_assessment$rate_of_change
    assessment_short$category[i] <- short_term_assessment$category

    # Do an assessment against targets
    trend_assessment <- target_assess_this(variables[i],
                                           targets,
                                           smoothed_trend,
                                           term = target_term)

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

#' @title assessment_table_builder
#' @description Called from wthin visualise_assessment.
#' Builds a summary table from as assessment data frame
#'
#' @param x a data frame created by do_assessment
#' @param cols colour lookup table
#'
#' @return a summary data frame
#'
#' @import dplyr
#' @import reshape
assessment_table_builder <- function(x,
                                     cols = colour_lookup){

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
    dplyr::left_join(
      .,
      cols)

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
# x <- assessment_table

#' @title assessment_plot
#' @description Called from wthin visualise_assessment.
#' Plots the results of an assessment in a stacked bar chart.
#'
#' @param x output from assessment_table_builder
#'
#' @return prints a ggplot object
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
assessment_plot <- function(x){

  # Checks that the supplied data frame contains value and trend
  if(sum(c("value", "trend", "cols") %in% colnames(x)) < 3){
    stop("Data frame must contain the columns 'value', 'trend' and 'cols")
  }

  ggplot2::ggplot(x, aes(x = 1, y = .data$value, fill = .data$trend)) +
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
    geom_text(aes(label = .data$trend),
              #label = function(x) stringr::str_wrap(x, width = 10),
              size = 4,
              position = position_stack(vjust = 0.5))
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise assessment
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment_target
# myLab <- "Long term"
# classification <- "Primary_goal"
# group <- "Clean air"

#' @title visualise_assessment
#' @description ties assessment_table_builder and assessment_plot together to visualise the
#' results of as assessment.
#'
#' @param classification the column name from which to subset the assessment by
#' @param group the factor level from within classification by which to subset the assessment
#' @param x output from do_assessment
#' @param myLab string specifying the x axis label
#'
#' @return prints a ggplot object
#' @export
#'
#' @import dplyr
#' @import ggplot2
visualise_assessment <- function(classification = "Primary_goal",
                                 group = "Clean air",
                                 x,
                                 myLab = "assessment",
                                 cols = colour_lookup){
  
  # if the selected column is natural_capital_framework then we need to do a bit more work
  if (classification == "natural_capital_framework"){
    selection2 <- gsub(pattern = "Asset - ", replacement = "", x = group)
    
    temp <- data.frame(classification = x[classification],
                       "category" = x$category)
    
    temp <- temp[str_detect(string = temp[[classification]],
                       pattern = selection2), ]
  } else {
    temp <- data.frame(classification = x[classification],
                     "category" = x$category) %>%
    dplyr::filter(.data[[eval(classification)]] == group)
  }

  assessment_table <- assessment_table_builder(temp,
                                               cols = cols)

  figure <- assessment_plot(assessment_table) +
    ggplot2::labs(title = group,
         x = myLab)

  print(figure)

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise target assessment (dots)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#x <- assessment_target

#' @title visualise_assessment_dot
#' @description visualises the result of a target assessment as a dot plot.
#'
#' @param classification the column name from which to subset the assessment by
#' @param group the factor level from within classification by which to subset the assessment
#' @param x output from do_assessment
#' @param myLab string specifying the x axis label
#'
#' @return prints a ggplot object
#' @export
#'
#' @import dplyr
#' @import ggplot2
visualise_assessment_dot <- function(x,
                                     classification = "Group",
                                     group = "Atmosphere",
                                     myLab = "Target assessment"){
  # convert category into a score
  x <- x %>%
    dplyr::mutate(score = dplyr::case_when(
      category == "Unknown" ~ 0,
      category == "No target" ~ 0,
      category == "Insufficient  progress" ~ 1,
      category == "Some progress towards target" ~ 2,
      category == "Substantial progress" ~ 3,
      category == "Target met" ~ 4)) %>%
    dplyr::filter(.data[[eval(classification)]] == group,
                  score > 0) %>%
    dplyr::arrange(score) %>%
    # make variable a factor to preserve the ordering
    dplyr::mutate(variable = factor(variable, levels = variable))

  figure <- ggplot(x, aes(x = reorder(variable, desc(variable)), y = score, colour = score)) +
    geom_point(size = 8) +
    coord_flip() +
    theme_bw() +
    theme(
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none") +
    scale_y_continuous(labels = rev(c("Target \nmet",
                                      "Substantial \nprogress",
                                      "Some progress \ntowards target",
                                      "Insufficient  \nprogress")),
                       breaks = c(1, 2, 3, 4),
                       limits = c(1, 4)) +
    scale_color_gradient(low= "#DB4325", high="#006164") +
    labs(x = "",
         y = "",
         title = myLab)

  print(figure)

}
