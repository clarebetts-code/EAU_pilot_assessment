# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Helper functions to support the pilot assessment
# written by clare betts january 2021
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to check file names
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @param filename the name of the file which the data are to be read from.
#' Each row of the table appears as one line of the file. If it does not
#' contain an absolute path, the file name is relative to the
#' current working directory, getwd().
#' @return
#' @export
#'
#' @examples
filename_checker <- function(filename){
  
  # Checks that the supplied file path is a string
  if (!is.character(filename)) {
    stop("Filename must be a string")
  }
  
  # Checks that the supplied file actually exists.
  if (!file.exists(filename)) {
    stop(paste0("Cannot find a file called '", filename, "', check filename and try again."))
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process metadata file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# filename <- "G:\\1. EAU\\EAU_pilot_assessment\\EAU_pilot_assessment\\data\\metadata.csv"
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
#' The metadata file must contain teh following:
#' Indicator - string, the ID of the indicator (e.g. A1)
#' variable - string, the name of the indicator or component of the indicator including the indicator ID (e.g. A1 NH3)
#' Primary_goal - string, primary 25YEP goal, one of "Clean air", "Mitigating and adapting to climate change", "Clean and plentiful water", 
#'               "Thriving plants and wildlife", "Minimising waste", "Using resources from nature more sustainably and efficiently", 
#'               "Reducing the risk of harm from natural hazards", "Enhancing beauty, heritage and engagement", "Enhancing biosecurity", 
#'               "Managing exposure to chemicals"
#' Secondary_goal - string, secondary 25YEP goal, one of "Clean air", "Mitigating and adapting to climate change", "Clean and plentiful water", 
#'               "Thriving plants and wildlife", "Minimising waste", "Using resources from nature more sustainably and efficiently", 
#'               "Reducing the risk of harm from natural hazards", "Enhancing beauty, heritage and engagement", "Enhancing biosecurity", 
#'               "Managing exposure to chemicals"
#' Third_goal - string, third 25YEP goal, one of "Clean air", "Mitigating and adapting to climate change", "Clean and plentiful water", 
#'               "Thriving plants and wildlife", "Minimising waste", "Using resources from nature more sustainably and efficiently", 
#'               "Reducing the risk of harm from natural hazards", "Enhancing beauty, heritage and engagement", "Enhancing biosecurity", 
#'               "Managing exposure to chemicals"
#' natural_capital_framework - string, natural capital framework categorisation, one of "Pressure", "Asset condition", "Service/benefit"
#' target - numeric, target value
#' target_year - numeric, target year
#' target_trend - string, either "increase" or "decrease" (defaults to "increase")
#' years_short - number of years to include in short term assessment (defaults to 6 years)
#' years_long - number of years to include in short term assessment (defaults to whole time series), this variable identifies the number 
#'              of years of data which will be smoothed
#' Threshold1 - threshold value
#' Threshold2 - threshold value
#' Threshold3 - threshold value
#' Threshold4 - threshold value
#'
#' @return Three dataframes.
#' @export
#'
#' @import utils
load_process_metadata <- function(filename) {

  # check file exists
  filename_checker(filename)
  
  # read in data
  dat <- read.csv(filename)

  # store targets
  targets <- unique(dat[, c("variable", "target", "target_year", "target_trend")])

  # store thresholds
  thresholds <- dat[, c("variable", "years_short", "years_long", "transformation", "Threshold1", "Threshold2", "Threshold3", "Threshold4", "target_trend")]

  goal_indicator_lookup <- unique(dat[, c("variable", "Indicator", "Primary_goal", "Secondary_goal",	"Third_goal", "natural_capital_framework")])

  # assign objects to global environment
  # I think this is easier for the user (rather than returning a list of dataframes) but i may be wrong.
  assign("targets", targets, envir = .GlobalEnv)
  assign("thresholds", thresholds, envir = .GlobalEnv)
  assign("goal_indicator_lookup", goal_indicator_lookup, envir = .GlobalEnv)

  # return(NULL)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to load and process raw data file
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
load_process_data <- function(filename) {

  # check file exists
  filename_checker(filename)

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

  dat_list <- dat %>%
    # split in to a list of data frames
    dplyr::group_split() %>%
    # assign names
    purrr::set_names(variables)

  # assign objects to global environment
  assign("dat_list", dat_list, envir = .GlobalEnv)
  assign("variables", variables, envir = .GlobalEnv)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to correctly back transform log10 transformed means & standard error
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @title error_transform
#' @description Acorrectly back transform log10 transformed means & standard error
#'
#' @param logmean A vecor of means
#' @param logse A vector of standard errors
#'
#' @return A dataframe of back transformed means & standard errors
#' @export
#'
#' @import stats
error_transform <- function(logmean, logse){
  
  lowlog <- logmean - 2*logse
  highlog <- logmean + 2*logse
  
  low <- 10^lowlog
  high <- 10^highlog
  
  fitted <- 10^(logmean)
  se <- (high-low)/4
  
  result <- cbind(fitted, se)
  
  return(result)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate the smoothed trend and return warnings where appropriate
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- dat_list

#' @title get_smoothed_trend
#' @description Apply a loess smoother trend to your data, predicting value from year.
#' Uses default setting span = 0.75 which is not appropriate for small data sets, warnings are produced
#' and returned to the console.
#'
#' @param x A list if data frames (dat_list) output by load_process_data
#' @param thresholds output from load_process_metadata
#'
#' @return A list of smoothed trends
#' @export
#'
#' @import dplyr
#' @import tidyr
#' @import purrr
#' @import stats
get_smoothed_trend <- function(x, thresholds) {

  # function to do the smoothing
  # x <- dat_list[[1]]
  quiet_smoother <- purrr::quietly(function(y, x) {
    predict(loess(y ~ x, 
                  span = 0.75), 
            se = TRUE)
  })

  # check if the user has specified a shorter number of years to use
  for (i in 1:length(x)){
    specified_years <- thresholds$years_long[thresholds$variable == names(x[i])]
    if(!is.na(specified_years)){
      x[[i]] <- tail(x[[i]], specified_years)
    }
  }
  
  smoothed_data <- purrr::map(x, 
                              ~ quiet_smoother(x = .x$year,
                                               y = .x$value)) %>%
    purrr::set_names(variables)

  # check if the user has specified a transformation for any of the data
  # if flagged with a 1 apply a log(x+1) transformation
  transformed_data <- thresholds[!is.na(thresholds$transformation), "variable"]
  
  if(length(transformed_data) > 0){
    for (i in transformed_data){
      
        # transform the data
        temp_dat <- x[[i]] %>%
          mutate(transformed_value = log(value + 1))
        
        # smooth the data & back transform
        smoothed_data[[i]] <- quiet_smoother(x = temp_dat$year,
                                             y = temp_dat$transformed_value) 
        
        # CB 18/08/21 allowing the user to specify the transformation and correctly applying the appropriate back transformation, 
        # especially to correctly back transform the se is hard. i have tried but don't have teh time to see this through now
          
        # This won't correctly back transforming se, over write for now
        smoothed_data[[i]]$result$fit <- exp(smoothed_data[[i]]$result$fit) - 1
        smoothed_data[[i]]$result$se.fit <- NA

    }
  }
  
  # pull out result
  smoothed_data_result <- purrr::map(smoothed_data, "result") %>%
    purrr::map( ~dplyr::bind_cols(.x$fit, .x$se.fit) %>%
                  rlang::set_names(c("fit", "se.fit"))) 


  # deal with warnings
  smoothed_data_warnings <- purrr::map_lgl(smoothed_data, ~ length(.x$warnings) > 1)

  if (sum(smoothed_data_warnings) > 0) {
    print("Warnings generated for:")
    print(names(smoothed_data)[smoothed_data_warnings])
  }

  # Note from EF-T:
  # * Really like what you're doing here with the quiet_smoother function and
  # * returning which dataframes produced warnings but it would be even better
  # * if you returned the warnings themselves. I had a go and didn't get anywhere
  # * but some googling returned https://collateral.jamesgoldie.dev/, which
  # * might be worth a look!

  # return the result
  return(smoothed_data_result)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to plot a smoothed trend
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dat <- dat_list[[33]]
# smoothed <- smoothed_trend[[33]]

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
#' @import ggplot2
#' @import scales
#' @import stats
smoothed_plot <- function(dat,
                          smoothed) {
  dat <- dat %>%
    tail(x = ., 
         n = nrow(smoothed)) %>%
    mutate(se = smoothed$se.fit,
           smoothed = smoothed$fit
           )

  p1 <- ggplot(data = dat, aes(x = year, y = value)) +
    geom_line(aes(x = year, y = smoothed, colour = "smoothed"), size = 1) +
    geom_point(aes(x = year, y = value, colour = "raw data"), size = 1.5) +
    geom_ribbon(aes(ymax = smoothed + se, ymin = smoothed - se), alpha = 0.2) +
    labs(y = unique(dat$variable),
         colour = "Legend") +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  
  return(p1)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to save the smoothed trends
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- dat_list
# y <- smoothed_trend
# filepath <- "smoothed_trends\\"

#' @title save_smoothed_trend
#' @description Visualises the smoothed trend for a variable
#'
#' @param x a list of data frames containing the time series of each variable
#' @param filepath file path to specify where to save the plots
#'
#' @return saves a .png to file. The file name is relative to the
#' current working directory, getwd().
#' @export
#'
#' @import purrr
#' @import ggplot2
save_smoothed_trend <- function(x, y, filepath) {

  filenames <- paste0(filepath, names(x), ".png")

  # create plots
  plots <- purrr::map2(
    x, 
    y, 
    smoothed_plot
  )

  # save plots
  for (i in 1:length(plots)) {
    ggplot2::ggsave(
      filename = filenames[i],
      plot = plots[[i]],
      device = "png"
    )
  }
  
  # add the smothed data into the original list of dataframes
  res <- {}
  for (i in names(x)){
    
    dat <- x[[i]]
    smoothed <- y[[i]]
    
    dat <- dat %>%
      tail(x = ., 
           n = nrow(smoothed)) %>%
      mutate(se = smoothed$se.fit,
             smoothed = smoothed$fit
      )
    
    res[[i]] <- dat
  }
  
  # convert to one dataframe
  res2 <- do.call(rbind, res)
    
  # save data
  write.csv(res2, 
            row.names = FALSE,
            file = paste0(filepath, "smoothed_data.csv"))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to do the three assessments
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' @title do_assessment
#' @description Performs a short term, long term and target assessment on a list of variables.
#'
#' @param variables String containing the names of the variables
#' @param targets output from load_process_metadata
#' @param thresholds output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
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
                          trend_method = "total proportional change",
                          target_method = "annual change") {
  # a data frame to store assessment results in
  assessment_short <- data.frame(
    "variable" = variables,
    "first_value" = NA,
    "final_value" = NA,
    "rate_of_change" = NA,
    "category" = NA
  )
  
  assessment_short <- dplyr::left_join(assessment_short, targets)
  
  assessment_long <- assessment_target <- assessment_short
  
  # ? Note from EF-T: why not just map over the variables directly, like this:
  # ?
  # ?  variables %>%
  # ?  purrr::map(
  # ?    ~  <- (
  # ?      .x,
  # ?      term = long_term,
  # ?      thresholds = thresholds,
  # ?      smoothed_trend = smoothed_trend
  # ?    )
  # ?  ) %>% rlang::set_names(
  # ?    variables
  # ?  ) %>% dplyr::bind_rows(
  # ?    .id = "variable"
  # ?  )
  # ?
  # ? If you were mapping over a list of named dataframes instead of a list of
  # ? names this could be even more concise!
  
  for (i in 1:length(variables)) {
    print(variables[i])
    
    # Do a long term assessment on all variables
    # we have already smoothed the correct length of time series for the long term assessemnt
    long_term <- if(is.na(thresholds$years_long[i])){nrow(smoothed_trend[[i]])} else {thresholds$years_long[i]}
    
    long_term_assessment <- trend_assess_this(variables[i],
                                              term = long_term,
                                              thresholds,
                                              smoothed_trend,
                                              trend_method = "total proportional change"
    )
    
    assessment_long$first_value[i] <- long_term_assessment$first_value
    assessment_long$final_value[i] <- long_term_assessment$final_value
    assessment_long$rate_of_change[i] <- long_term_assessment$rate_of_change
    assessment_long$category[i] <- long_term_assessment$category
    
    # Do a short term assessment on all variables
    # need to look at the metadata to determine if we use the default or an override
    short_term <- if(is.na(thresholds$years_short[i])){6} else {thresholds$years_short[i]}
    
    short_term_assessment <- trend_assess_this(variables[i],
                                               term = short_term,
                                               thresholds,
                                               smoothed_trend,
                                               trend_method = "total proportional change"
    )
    
    assessment_short$first_value[i] <- short_term_assessment$first_value
    assessment_short$final_value[i] <- short_term_assessment$final_value
    assessment_short$rate_of_change[i] <- short_term_assessment$rate_of_change
    assessment_short$category[i] <- short_term_assessment$category
    
    # Do an assessment against targets
    # use the same as the short term assessment
    target_term <- short_term
    
    target_assessment <- target_assess_this(variables[i],
                                           targets,
                                           smoothed_trend,
                                           term = target_term,
                                           target_method = "annual change"
    )
    
    assessment_target$first_value[i] <- target_assessment$first_value
    assessment_target$final_value[i] <- target_assessment$final_value
    assessment_target$rate_of_change[i] <- target_assessment$rate_of_change
    assessment_target$category[i] <- target_assessment$category
    assessment_target$years[i] <- target_assessment$years
  }
  
  return(list(
    assessment_short = assessment_short,
    assessment_long = assessment_long,
    assessment_target = assessment_target
    
  ))
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to calculate years until target reached
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# year until target reached (n)
# n = log(current value/target value) / log(1 + rate of change)
# https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01

#' @title years_until_target_reached
#' @description Called from within target_assess_this.
#' Calculates the number of years until the target is reached for a single indicator,
#' using the formula:
#' $$ years = abs(log(final_value/temp_target)/log(1 + rate_of_change))
#' years = abs(log(final_value/temp_target)/log(1 + rate_of_change))
#' see https://intl.siyavula.com/read/maths/grade-12/finance/03-finance-01 for more information
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
                                       temp_target_trend) {

  # ? So, final_value is desired outcome and temp_target is expected outcome?
  # * I'm not entirely sure what the objects represent but the case logic is sound

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
    TRUE ~ abs(log(final_value / temp_target) / log(1 + rate_of_change))
  )

  return(years)
}

# function to calculate the rate of change
# x <- temp_dat
#' @title calc_rate_of_change
#' @description Called from within trend_assess_this and target_assess_this.
#'
#' @param method one of "total proportional change", "annual change", or "average annual change"
#' @param x the data frame containing the smoothed data
#'
#' @return
#' @export
#'
#' @examples
calc_rate_of_change <- function(method = "total proportional change",
                                x){
  
  # use the second and penultimate values for assessment
  final_value <- rev(x$fit)[2]
  first_value <- x$fit[2]
  
  #check method is appropriately specified
  if (!(method %in% c("total proportional change", "annual change", "average annual change"))){
    stop("trend assessment method needs to be one of 'total proportional change', 'annual change', or 'average annual change'")
  }
  
  if(method == "total proportional change"){
    rate_of_change <- ((final_value - first_value) / first_value)
  }
  
  if(method == "annual change"){
    rate_of_change <- ((final_value - first_value) / first_value) / nrow(x)
  }
  
  if(method == "average annual change"){
    rate_of_change <- x %>%
      mutate(change = (fit-lag(fit))/lag(fit)) %>%
      summarise(mean_change = mean(change, na.rm = TRUE)) %>%
      pull()
  }
  
  return(list(rate_of_change = rate_of_change,
              first_value = first_value,
              final_value = final_value))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do trend assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- variables[1]

#' @title trend_assess_this
#' @description Called from within do_assessment.
#' Performs an assessment of change on an indicator or variable.
#'
#' @param x The name of the variable as a string.
#' @param term Number of years to perform the assessment on. Defaults to 5.
#' @param thresholds output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
#' @param method one of "total proportional change", "annual change", or "average annual change"
#'
#' @return a list containing the first and last values in the series, the rate of change and the
#' result of the assessment.
#'
#' @import dplyr
#' @import rlang
trend_assess_this <- function(x,
                              term = 6,
                              trend_method = "total proportional change",
                              thresholds,
                              smoothed_trend) {
  
  if (nrow(smoothed_trend[[x]]) >= term) {
    # use number of years set at beginning here.
    temp_dat <- smoothed_trend[[x]] %>%
      tail(term)
  } else {
    temp_dat <- {}
  }

  # calculate rate of change
  res <- calc_rate_of_change(method = trend_method,
                             x = temp_dat)
  rate_of_change <- res$rate_of_change
  first_value <- res$first_value
  final_value <- res$final_value
  
  # get threshold
  temp_threshold <- thresholds %>%
    dplyr::filter(variable == x) %>%
    dplyr::select(Threshold1, Threshold2, Threshold3, Threshold4)

  target_trend <- thresholds %>%
    dplyr::filter(variable == x) %>%
    dplyr::select(target_trend)
  
  # if target trend is to decrease then reverse the labels
  trend_labels <- c("Strong decline", "Moderate decline", "Little change", "Moderate improvement", "Strong improvement")

  # check of target_trend exists
  if (!(target_trend %in% c("increase", "decrease"))){
    stop("target_trend has not been specified correctly in the metadata")
  }
  
  if (target_trend == "increase") {
    trend_labels <- trend_labels
  }
  if (target_trend == "decrease") {
    trend_labels <- rev(trend_labels)
  }

  # assign rate of change assessment
  # this seems inelegant, return to this when i have more time

  # ? Note from EF-T: Though I don't think there's anything necessarily wrong
  # ? with how you're doing this, it does feel overly convoluted.
  # ? Have you tried breaking it down into smaller functions?
  # ? I'd suggest seperating out the calculating rate_of_change,
  # ? generating trend_labels, and categorising rate_of_change
  # ? elements. Also, the trend_labels are essentially bins, so
  # ? maybe treating them as such explicitly, using, say cut()
  # ? might help...

  if (rlang::is_empty(rate_of_change)) {
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
      rate_of_change > temp_threshold$Threshold4 ~ trend_labels[5]
    )
  }

  # return result
  return(list(
    first_value = first_value,
    final_value = final_value,
    rate_of_change = rate_of_change,
    category = category
  ))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to do target assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- variables[1]

#' @title target_assess_this
#' @description Called from within do_assessment.
#' Performs an assessment of change against a target on an indicator or variable.
#'
#' @param x The name of the variable as a string.
#' @param targets output from load_process_metadata
#' @param smoothed_trend output from get_smoothed_trend
#' @param term set by user, number of years of data to use
#' @param method one of annual change", or "average annual change", 
#' N.B. "total proportional change" cannot be used to calculate progress towards a target
#'
#' @return a list containing the first and last values in the series, the rare of change, the
#' number of years until target is reached and the result of the assessment.
#'
#' @import dplyr
target_assess_this <- function(x,
                               targets,
                               smoothed_trend,
                               term = 6,
                               target_method = "annual change") {

  # test if there are enough years of data
  # currently uses 6 years of data
  if (nrow(smoothed_trend[[x]]) < term) {
    temp_dat <- {}
    
    # # if there aren't set some dummy values
    # first_value <- NA
    # final_value <- NA
    # rate_of_change <- NA
    # years <- NA
    # category <- NA
  } else {
    # if there are do assessment
    # use number of years set at beginning here.
    temp_dat <- smoothed_trend[[x]] %>%
      tail(term)
  }

  # calculate rate of change
  res <- calc_rate_of_change(method = target_method,
                             x = temp_dat)
  rate_of_change <- res$rate_of_change
  first_value <- res$first_value
  final_value <- res$final_value

  temp_target <- targets$target[targets$variable == x]

  temp_target_trend <- targets$target_trend[targets$variable == x]

  temp_target_year <- targets$target_year[targets$variable == x]

  # calculate years until target reached
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

  if (rlang::is_empty(rate_of_change)) {
    first_value <- NA
    final_value <- NA
    rate_of_change <- NA
    years <- NA
    category <- "Unknown"
  } else {
    first_value <- first_value
    final_value <- final_value
    rate_of_change <- rate_of_change
    years <- years
    category <- category
  }
  
  return(list(
    first_value = first_value,
    final_value = final_value,
    rate_of_change = rate_of_change,
    years = years,
    category = category
  ))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to build the assessment table which the visualisations are built from
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                                     cols = colour_lookup) {

  # count the number of variables in each category
  ass_tab <- reshape::melt(table(x$category) / sum(table(x$category))) %>%
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
      cols
    )

  # get trend factor levels in the correct order
  ass_tab$trend <- factor(ass_tab$trend,
    levels = levels(cols$trend)
  )
  ass_tab$cols <- factor(ass_tab$cols,
    levels = levels(cols$cols)
  )

  return(droplevels(ass_tab))
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to convert the assessment table to a plot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
assessment_plot <- function(x) {

  # Checks that the supplied data frame contains value and trend
  if (sum(c("value", "trend", "cols") %in% colnames(x)) < 3) {
    stop("Data frame must contain the columns 'value', 'trend' and 'cols")
  }

  ggplot2::ggplot(x, aes(x = 1, y = .data$value, fill = .data$trend)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = levels(x$cols)) +
    theme(
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 25),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    ) +
    labs(y = "") +
    geom_text(aes(label = .data$trend),
      # label = function(x) stringr::str_wrap(x, width = 10),
      size = 4,
      position = position_stack(vjust = 0.5)
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise assessment
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment_target
# myLab <- "Long term"
# classification <- "Primary_goal"
# group <- "Clean air"

#' @title visualise_assessment
#' @description ties assessment_table_builder and assessment_plot together to visualise the
#' results of as assessment.
#'
#' @param x output from do_assessment
#' @param myLab string specifying the x axis label
#'
#' @return prints a ggplot object
#' @export
#'
#' @import dplyr
#' @import ggplot2
visualise_assessment <- function(
  # classification = "Primary_goal",
  # group = "Clean air",
  x,
  myLab = "assessment",
  cols = colour_lookup){
  
  # # if the selected column is natural_capital_framework then we need to do a bit more work
  # if (classification == "natural_capital_framework"){
  #   selection2 <- gsub(pattern = "Asset - ", replacement = "", x = group)
  #   
  #   temp <- data.frame(classification = x[classification],
  #                      "category" = x$category)
  #   
  #   temp <- temp[str_detect(string = temp[[classification]],
  #                      pattern = selection2), ]
  # } else {
  #   temp <- data.frame(classification = x[classification],
  #                    "category" = x$category) %>%
  # 
  #   dplyr::filter(.data[[eval(classification)]] == group)
  # }

  assessment_table <- assessment_table_builder(x,
                                               cols = cols)

  figure <- assessment_plot(assessment_table) +
    ggplot2::labs(
      #title = group,
      x = myLab
    )

  print(figure)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise all three assessments in one plot
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment_target
# myLab <- "Long term"
# classification <- "Primary_goal"
# group <- "Clean air"

#' @title visualise_assessment
#' @description makes multiple calls to visualise_assessment to group three visualisations 
#' in one plot. The user needs to filter the assessment results to include only the results they 
#' want to visualise. i.e. only indicators of "clean air"
#'
#' @param assessment_short output from do_assessment
#' @param assessment_long output from do_assessment
#' @param assessment_target output from do_assessment
#' @param title The title of the plot
#' #'
#' @return prints a ggplot object
#' @export
#'
#' @import dplyr
#' @import ggplot2
#' @import cowplot
visualise_all_assessments <- function(assessment_short,
                                      assessment_long,
                                      assessment_target,
                                      title = "title"){
  p1 <- assessment_short %>%
    visualise_assessment(x = .,
                         myLab = "Short term trend") +
    labs(y = "Percentage of Indicators") 
  
  p2 <- assessment_long %>%
    visualise_assessment(x = .,
                         myLab = "Long term trend") +
    labs(y = "")
  
  p3 <- assessment_target %>%
    visualise_assessment(x = .,
                         myLab = "Target assessment") +
    labs(y = "")
  
  plot_row <- cowplot::plot_grid(p1, p2, p3,
                                 nrow = 1)
  
  # now add the title
  title <- cowplot::ggdraw() + 
    cowplot::draw_label(
      title,
      fontface = "plain",
      size = 18,
      #x = 0.2,
      hjust = 0.5,
      vjust = 0.5
    ) 
  
  cowplot::plot_grid(
    title, 
    plot_row,
    ncol = 1,
    # rel_heights values control vertical title margins
    rel_heights = c(0.05, 1)
  )
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# function to visualise target assessment (dots)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x <- assessment_target

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
                                     myLab = "Target assessment") {
  # convert category into a score
  x <- x %>%
    dplyr::mutate(score = dplyr::case_when(
      category == "Unknown" ~ 0,
      category == "No target" ~ 0,
      category == "Insufficient  progress" ~ 1,
      category == "Some progress towards target" ~ 2,
      category == "Substantial progress" ~ 3,
      category == "Target met" ~ 4
    )) %>%
    dplyr::filter(
      .data[[eval(classification)]] == group,
      score > 0
    ) %>%
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
      legend.position = "none"
    ) +
    scale_y_continuous(
      labels = rev(c(
        "Target \nmet",
        "Substantial \nprogress",
        "Some progress \ntowards target",
        "Insufficient  \nprogress"
      )),
      breaks = c(1, 2, 3, 4),
      limits = c(1, 4)
    ) +
    scale_color_gradient(low = "#DB4325", high = "#006164") +
    labs(
      x = "",
      y = "",
      title = myLab
    )

  print(figure)
}
