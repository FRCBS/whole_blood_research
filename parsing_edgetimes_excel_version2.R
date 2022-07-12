library("readxl")
library("dplyr")

parse_file.function <- function(file_location, sheet_name) {
  
  # Reading the file and deleting empty rows
  df <- na.omit(read_excel(file_location, sheet_name))
  
  # Making empty list
  list_data <- list()
  
  # Counting rows
  rows = nrow(df) 
  row <- 1
  
  # Reading the excel row by row
  for (row in 1:rows){
    
    # Reading data from a row
    row_data <- df[row,]
    
    # Defining the edge between nodes in flowchart and/or excel
    from <- as.character(select(row_data, from))
    to <- as.character(select(row_data, to))
    edge_name <- paste(from, to, sep = " - ")
    
    # Limits for the times between events
    lower <- as.numeric(select(row_data, start))
    upper <- as.numeric(select(row_data, end))
    
    # Probability for the edge
    probability <- as.numeric(select(row_data, probability))
    
    # Listing parsed information
    list_data[row] <- list(c(from, to, edge_name, lower, upper, probability))
  }
  return(list_data)
}

check_vehicle.function <- function(vehicle, to, possible_rows, lower, upper){
  
  vehicles <- c("Ambulance", "Ambulance and doctor", "Helicopter", "Fire truck")
  
  # Going through list_data to make a correct match between vehicle and edge
  # If vehicle is NULL, should check the to
  if(is.null(vehicle)){
    
    for (i in 1:length(vehicles)){
      vehicle_to_test <- as.character(vehicles[i])
      
      # comparing data_to and vehicle if it is a match
      match_found <- grepl(vehicle_to_test, to, fixed = TRUE)
      
      # Must check that the correct vehicle is in the "to"
      if(match_found){
        comp_check <- grepl("Ambulance and doctor", to, fixed = TRUE)
        if(vehicle_to_test == "Ambulance" && comp_check == FALSE){
          vehicle <- vehicle_to_test
        } else {vehicle <- vehicle_to_test}
      }
    }
  } else {
    # If vehicle exists, should get a match from possible to (to_to_test) instead
    for (i in 1:length(possible_rows)){
      row <- possible_rows[i]
      to_to_test <- as.character(lapply(row,'[[',2))
      
      # Comparing to_to_test and vehicle if it is a match
      match_found <- grepl(vehicle, to_to_test, fixed = TRUE)
      
      # Must check that the correct vehicle is in the "to", returning correct limits at the same time... ?
      if(match_found){
        comp_check <- grepl("Ambulance and doctor", to_to_test, fixed = TRUE)
        
        if(vehicle == "Ambulance" && comp_check == FALSE){
          to <- to_to_test
          lower <- as.numeric(lapply(row,'[[',4))
          upper <- as.numeric(lapply(row,'[[',5))
        } else if (vehicle != "Ambulance") { 
          to <- to_to_test
          lower <- as.numeric(lapply(row,'[[',4))
          upper <- as.numeric(lapply(row,'[[',5))
        }
      }
    }
  }
  # Returning matched vehicle and 'to'
  ret <- c(to, vehicle, lower, upper)
  return(ret)
}

draw_next_node.function <- function(from, list_data, vehicle){
  
  # Getting the next node, this is not the best way to do it
  # Must find the other "to", done below. Checking if multiple next nodes exists and drawing one with given probabilities.
  
  match_count <- 0
  possible_to <- list()
  probs <- list()
  possible_rows <- list()
  
  for (j in 1:length(list_data)){
    row <- list_data[j]
    data_from <- as.character(lapply(row,'[[',1))
    
    if(data_from == from){
      
      # Match found, so increasing the match_count and saving the row in a list
      match_count <- match_count + 1
      to <- as.character(lapply(row,'[[',2))
      lower <- as.numeric(lapply(row,'[[',4))
      upper <- as.numeric(lapply(row,'[[',5))
      possible_rows <- append(possible_rows, row)
      prob <- as.numeric(lapply(row,'[[',6))
      probs <- append(probs, prob)
    }
  }
  # Drawing a row from the 'rows' to get to and the distribution limits
  if(match_count > 1){
    row <- sample(possible_rows, 1, replace = FALSE, probs)
    to <- as.character(lapply(row,'[[',2))
    lower <- as.numeric(lapply(row,'[[',4))
    upper <- as.numeric(lapply(row,'[[',5))
  }
  # Checking if check_vehicle is needed
  # Times to check are when 'to' includes "leaves" or "transport" or from includes "Packing"
  check_1 <- grepl("Transport", to, fixed = TRUE)
  check_2 <- grepl("leav", to, fixed = TRUE)
  # If leaving the scene, new vehicle will be drawn, thus setting it NULL
  if(check_3 <- grepl("Packing", from, fixed = TRUE)){vehicle <- NULL}
  if(check_1 || check_2 || check_3){
    node <- check_vehicle.function(vehicle, to, possible_rows, lower, upper)
    to <- node[1]
    vehicle <- node[2]
    lower <- node[3]
    upper <- node[4]
  }
  return(c(from, to, vehicle, lower, upper))
}

time_calculator.function <- function(list_data){
  
  # Calculating the time between events (nodes)
  # stop_sign <- "Shock Room"
  
  # Drawing first node, assuming that first from is 'Risk analysis' and vehicle not known
  first_node <- draw_next_node.function(from <- "Risk analysis", list_data, vehicle <- NULL)
  from <- as.character(first_node[1])
  to <- as.character(first_node[2])
  vehicle <- as.character(first_node[3])
  lower <- as.numeric(first_node[4])
  upper <- as.numeric(first_node[5])
  
  # Going through the chart, drawing nodes when needed
  while(from != "Shock Room"){
    
    # Checking if it is time to stop and exit the loop
    #if(from == stop_sign) {break}
    # Updating the total_time_count
    random_time <- runif(1, lower, upper)
    total_time_count <- total_time_count + random_time
    
    # Checking if the infusion has started
    if(from == "Infusion starts"){infusion_starts <- TRUE}
    
    #If infusion has started, updating also infusion_time_count
    if(infusion_starts == TRUE) {
      infusion_time_count <- infusion_time_count + random_time
    }
    
    # Some printing to see if this works at all
    #cat("\n")
    #cat(from, "-", to, "\n")
    #cat(lower, " is the lower limit for the time distribution\n")
    #cat(upper, " is the upper limit for the time distribution\n")
    #cat(random_time, " is the randomized time between limits\n")
    #cat(total_time_count, " is the total time that has passed\n")
    #cat(infusion_time_count, " is the time that has passed since the infusion started\n\n")
    
    # "to" is the next "from", getting forward in the chart
    from <- to
    if (from != "Shock Room"){
      next_node <- draw_next_node.function(from, list_data, vehicle)
      to <- as.character(next_node[2])
      vehicle <- as.character(next_node[3])
      lower <- as.numeric(next_node[4])
      upper <- as.numeric(next_node[5])
    }
  }
  ret <- c(from, to, total_time_count, infusion_time_count, infusion_starts)
}

make_timepoints.function <- function(number_of_wanted_points){
  
  # Calculating times for transportation as many times as requested.
  # Saving times in a list and returning it
  
  ret_times <- c()
  set.seed(2)
  infusion_starts <- FALSE
  total_time_count <- 0
  infusion_time_count <- 0
  
  file_location <- "C:\\Projektit\\whole_blood_research\\excel\\EmergencyProcess_EdgeTimes.xlsx"
  sheet_name_a <- "test_times_a"
  sheet_name_b <- "test_times_b"
  from <- "Risk analysis"
  
  for(i in 1:number_of_wanted_points){
    
    total_time_count <- 0
    infusion_time_count <- 0
    # Reading the file, listing information from the sheets
    list_data_a <- parse_file.function(file_location, sheet_name_a)
    list_data_b <- parse_file.function(file_location, sheet_name_b)
    
    # Drawing whether the infusion starts on the scene (chart_a) or during transportation (chart_b)
    charts <- c("Chart A", "Chart B")
    chart_to_follow <- charts[sample(1:length(charts),1)]
    if(chart_to_follow == "Chart A"){
      list_data <- list_data_a
    } else {list_data <- list_data_b}
    cat(chart_to_follow, "is the chart to follow\n")
    
    # Starting the time-calculator
    current_stage <- time_calculator.function(list_data)
    
    # Information from the calculators:
    total_time_count <- as.numeric(current_stage[3])
    infusion_time_count <- as.numeric(current_stage[4])
    ret_times <- c(ret_times, infusion_time_count)
    
    cat("Patient transported to Shock Room!\n")
    cat(total_time_count, "is the total time from the 'Risk Analysis'\n")
    cat(infusion_time_count, "is the total time from the 'Infusion Starts'\n\n")
  }
  return(ret_times)
}


