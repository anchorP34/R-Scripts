# Import libraries to be used
library(lubridate)
library(stringr)
library(dplyr)

# Date Conversion:
#     This function looks to see what the current date value is and 
#     changes the parameter value to a string that represents that new date.
# 
# Inputs:
#     current_date (date): What the current date of the for loop is
#     variable_string (string): String of what the vairable is
# 
# Outputs:
#     string of the date for what that parameter represents
# 

date_conversion <- function(current_date, variable_string){
  if (variable_string == 'current_date'){
    return(current_date)
  }
  else {
    # Split the string based on _, get the last 2 values
    split_vals <- str_split(variable_string, pattern = '_')[[1]][3:4]
    
    operation <- split_vals[1]
    day_offset <- as.integer(split_vals[2])
    
    new_conversion <- switch(operation
                             , minus = current_date - days(day_offset)
                             , plus = current_date + days(day_offset))
    
    return(new_conversion)
    
  }
}

# Daily Data Pull:
#   This is designed to to create a SQL script for each individual day
#   from the start_date to the end_date. This has the idea of doing 
#   daily extracts and aggregations to prevent query timeouts
# 
# Inputs:
#   start_date_string (string): First date that would like to be included in the daily pull (yyyy-mm-dd)
#   end_date_string (string): Last date that would like to be included in the daily pull (yyyy-mm-dd)
#   sql_text (string): SQL statement that would like to be run 
#   variable_mapping (list): strings that need to be replaced if they are commonly referenced
#   print_sql (string): If you would like to print the SQL, put 'all' or if you would just like the last
#   day's SQL to be printed, put 'last'
# 
# 
# Outputs: Prints string to the console that can be either run in the function (if you add a connection)
#           or if you just wanted to copy it over to a database program.
# 
# Notes:
#     There is 1 different moving variables in the SQL at all times
#         current_date -> The current date in the for loop
#     These dates can be used in the variable mapping as well for more flexibility 
#     in moving different dates around.
# 
#     If you want the date to be changed based off the current_date, make sure you
#     wrap it in $'s.
# 
#     Included in the vairable mappings can be the number reassignments. For example,
#     if you put $current_date_plus_3$ and the current_date happens to be '2020-03-10', 
#     that will transform the SQL to change that into '2020-03-13'. This gives the ability
#     to have different lag dates around your SQL code. Just make sure that there is a $
#       in before and after the variable.
#     
#     If you do not put a start_date, it will choose 5 days prior to the current date
#     If you do not put an end_date, it will choose yesterday


daily_data_pull <- function(start_date_string
                    , end_date_string
                    , sql_text
                    , variable_mapping
                    , print_sql = 'last'
){
  
  # Check to see if the start and end date arguments were passed in
  if (missing(start_date_string)){
    start_date = today() - days(5)
  }
  else {start_date = ymd(start_date_string)}
  
  if (missing(end_date_string)){
    end_date = today() - days(1)
  }
  else {end_date = ymd(end_date_string)}
  
  # Copying the text over for debugging purposes
  replaced_sql <- sql_text
  
  # Replace the sql text with values in the sql_mapping
  for (param in names(variable_mapping)){
    replaced_sql <- gsub( param, variable_mapping[param], replaced_sql)
  }
  
  # This will be the current date parameter for each day
  current_date <- start_date
  
  
  while ( current_date <= end_date){
    
    # Starts the final_sql over every time for refreshing the parameters
    final_sql <- replaced_sql
    # Find all of the parameter values that need to be replaced
    parameter_replacements <- as_tibble(unique(str_match_all(final_sql,'\\$(.*?)\\$')[[1]]))
    colnames(parameter_replacements) <- c('Parameter','CleanValue')
    
    # For loop to replace all of the 
    for (i in 1:nrow(parameter_replacements)){
      
      clean_val <- parameter_replacements[i,2]
      date_val <- date_conversion(current_date, clean_val)
      
      # Have to do this to factor in special characters
      parameter <- str_c('\\$',clean_val, '\\$')
      
      final_sql <- gsub(parameter, date_val, final_sql)
    }
    
    
    ##########################################################
    # Here is where you would execute your SQL command
    ##########################################################
    
    ################## RUN SQL HERE ##########################
    
    ##########################################################
    
    ##########################################################

    # Print to the console when the date has been run
    cat(str_c("--",current_date, " is finished\n\n"))
  
    # Add one day to the current_day to eventually end the while loop
    current_date <- current_date + days(1)
    
    # Print the final_sql depending on the print_sql input parameter
    if (print_sql == 'all'){cat(final_sql)}
    if (print_sql == 'last' & current_date > end_date) {cat(final_sql)}
    
  }
  
}

# Example
test_sql = "
    SELECT '$current_date$' as utc_date
    , *
    FROM dbo.whatever_table
    #where_clause#
"

sql_mapping = list("#where_clause#" = "WHERE date_index BETWEEN '$current_date_minus_4$' AND '$current_date_plus_2$'")


daily_data_pull(start_date_string = '2020-03-01'
                , end_date_string = '2020-04-01'
                , sql_text = test_sql
                , variable_mapping = sql_mapping)




