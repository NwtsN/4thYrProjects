#################################### Distribution for package arrivals ####################################


standev=round(10/1.96,digits = 2)
numbers=10:40
mu=25
probs=dnorm(numbers,mean = mu,sd=standev)
normalized_probabilities <- probs / sum(probs)
plot(y=normalized_probabilities,x=numbers)



#################################### Calculating Values ####################################


library(dplyr)
# Calculate the number of combinations for each factor based on their levels
num_combinations <- 8*2*2*2*3*3*3*31

# Generate sequences for each factor with the specified repetition patterns
vehicle_issues <- rep(c("battery", "engine", "petrol", "battery_petrol", "battery_engine", "engine_petrol", "all_issues", "no_issues"), length.out = num_combinations, times = num_combinations/8)
v1_breakdown <- rep(c("breaks down", "doesnt break down"), length.out = num_combinations, each = 8)
v2_breakdown <- rep(c("breaks down", "doesnt break down"), length.out = num_combinations, each = 8*2)
fix_battery = rep(c("fix","no fix"), length.out = num_combinations, each = 8*2*2)
complaint <- rep(c("no complaint", "one complaint", "two complaints"), length.out = num_combinations, each = 8*2*2*2)
competitor_delivery <- rep(c("use twice", "use once", "dont use"), length.out = num_combinations, each = 8*2*2*2*3)
package_type <- rep(c("special", "normal", "cheap"), length.out = num_combinations, each = 8*2*2*2*3*3)
package_arrivals <- rep(as.character(10:40), length.out=num_combinations,each = 8*2*2*2*3*3*3)

# Combine the sequences into a data frame
ordered_factors <- data.frame(
  vehicle_issues = vehicle_issues,
  Vehicle_1_breakdown = v1_breakdown,
  Vehicle_2_breakdown = v2_breakdown,
  fix_battery = fix_battery,
  complaint = complaint,
  competitor_delivery = competitor_delivery,
  package_type = package_type,
  package_arrivals = package_arrivals
)

# Function to calculate cost based on vehicle issues
calculate_vehicle_cost <- function(issue, fix_battery) {
  issue_cost <- sum(
    ifelse(grepl("battery", issue) && fix_battery == "no fix", 50, 0),
    ifelse(grepl("petrol", issue), 40, 0),
    ifelse(grepl("engine", issue), 60, 0)
  )
  return(issue_cost)
}


# Function to calculate profit per combination, adjusted for use with apply()
calculate_profit <- function(row) {
  # Convert the row to a list for easier element access
  row_list <- as.list(row)
  
  # Calculate vehicle costs based on issues
  v1_cost <- ifelse(row_list$Vehicle_1_breakdown == "breaks down", calculate_vehicle_cost(row_list$vehicle_issues, row_list$fix_battery), 0)
  v2_cost <- ifelse(row_list$Vehicle_2_breakdown == "breaks down", calculate_vehicle_cost(row_list$vehicle_issues, row_list$fix_battery), 0)
  
  # Additional cost if a vehicle breaks down
  additional_cost <- ifelse(row_list$Vehicle_1_breakdown == "breaks down", 100, 0)
  additional_cost <- additional_cost+ifelse(row_list$Vehicle_2_breakdown == "breaks down", 100, 0)
  
  # Total vehicle cost
  total_vehicle_cost <- v1_cost + v2_cost + additional_cost
  
  # Complaint cost
  complaint_cost <- ifelse(row_list$complaint == "one complaint", 50, ifelse(row_list$complaint == "two complaints", 100, 0))
  
  # Competitor delivery cost
  competitor_cost <- ifelse(row_list$competitor_delivery == "use twice", 400, ifelse(row_list$competitor_delivery == "use once", 200, 0))
  
  # Package profit
  package_profit <- ifelse(row_list$package_type == "special", 200, ifelse(row_list$package_type == "normal", 100, 50))
  
  # Total profit before multiplying by package arrivals
  total_profit <- (package_profit* as.numeric(row_list$package_arrivals)) - total_vehicle_cost - complaint_cost - competitor_cost
  
  return(total_profit)
}


# Calculate profits as before
profits <- apply(ordered_factors, 1, calculate_profit)

# Convert profits to a dataframe for easy writing to CSV
profits_df <- data.frame(profit_per_day = profits)

# Write the dataframe to a CSV file
write.csv(t(profits_df), file = "profits_per_day_fix_battery.csv", row.names = FALSE)

