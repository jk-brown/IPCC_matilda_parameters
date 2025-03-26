lognorm <- function(m, sd) {
  # re-parameterization of supplied mean value
  mn <- log(m^2 / sqrt(sd^2 + m^2))
  
  # re-parameterization of supplied sd value
  stdev <- sqrt(log(1 + (sd^2 / m^2)))
  
  # stores new value in list - when pushed to rlnorm(), will provide normal distribution
  # of arithmetic mean (m) and standard deviation (sd)
  c(mn, stdev)
}

# Normalize a single series

normalize_single_series <- function(df, reference_start, reference_end) {
  
  # Filter the reference period
  reference_data <- df %>% filter(year >= reference_start & year <= reference_end)
  
  # Calculate the mean value for the reference period
  reference_mean <- mean(reference_data$value, na.rm = TRUE)
  
  # Subtract the reference mean from the original values
  df <- df %>%
    mutate(normalized_value = value - reference_mean)
  
  return(df)
}

# Normalize ensemble function
normalize_to_reference <- function(df, reference_start, reference_end) {
  
  # Filter data for the reference period
  reference_data <- df %>% filter(year >= reference_start & year <= reference_end)
  
  # Calculate mean for reference period for each run
  reference_mean <- reference_data %>%
    group_by(run_number, variable) %>%
    summarise(reference_mean = mean(value), .groups = "drop")
  
  # Merge the reference statistics back to the original data
  df <- df %>%
    left_join(reference_mean, by = c("run_number", "variable")) %>%
    mutate(value = value - reference_mean) %>%  # Overwrite the value column
    select(-reference_mean)                     # drop the reference_mean column
  
  return(df)
}

## Save params
save_params <- function(params, file_path) {
  if (file.exists(file_path)) {
    # Use menu() to ask the user interactively
    choice <- menu(c("Yes", "No"), title = paste("File already exists:", file_path, 
                                                 "\nDo you want to overwrite it?"))
    
    if (choice == 1) {  # User selected "Yes"
      write.csv(params, file_path, row.names = FALSE)
      message("File overwritten: ", file_path)
    } else {
      message("File not overwritten. Choose a unique file name if needed.")
    }
    
  } else {
    write.csv(params, file_path, row.names = FALSE)
    message("File successfully written: ", file_path)
  }
}