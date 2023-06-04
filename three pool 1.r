CO2 <- read_excel('D:/AAAA-资料E盘/data/培养实验/csoct.xlsx')
day <- CO2$day
flux <- CO2$co2...3
# Load data
data <- read.csv("soil_respiration_data.csv")
day <- data$Day
flux <- data$CO2_flux


# Define model parameters
microbial_growth_rate <- 0.02
decomposition_rate_fast <- 0.3
decomposition_rate_medium <- 0.1
decomposition_rate_slow <- 0.05
pool_fast <- 0.5
pool_medium <- 0.3
pool_slow <- 0.2

# Define initial values for the three pools
C_fast_0 <- 0.1
C_medium_0 <- 0.2
C_slow_0 <- 0.3

# Define function for the three-pool C decomposition model
decomp_model <- function(C, t) {
  C_fast <- C[1]
  C_medium <- C[2]
  C_slow <- C[3]
  
  # Calculate flux from each pool
  F_fast <- decomposition_rate_fast * C_fast
  F_medium <- decomposition_rate_medium * C_medium
  F_slow <- decomposition_rate_slow * C_slow
  
  # Calculate change in each pool over time
  dC_fast_dt <- pool_fast * (microbial_growth_rate * C_fast_0 - F_fast)
  dC_medium_dt <- pool_medium * (microbial_growth_rate * C_medium_0 - F_medium)
  dC_slow_dt <- pool_slow * (microbial_growth_rate * C_slow_0 - F_slow)
  
  # Update values for each pool
  C_fast_new <- C_fast + dC_fast_dt
  C_medium_new <- C_medium + dC_medium_dt
  C_slow_new <- C_slow + dC_slow_dt
  
  # Return new values for each pool
  return(c(C_fast_new, C_medium_new, C_slow_new))
}

# Define initial values for the model
C_0 <- c(C_fast_0, C_medium_0, C_slow_0)

# Integrate the model over time
library(deSolve)
times <- seq(0, 938, by = 1)
results <- ode(y = C_0, times = times, func = decomp_model)

# Plot the results
plot(day, flux, type = "l", xlab = "Day", ylab = "CO2 Flux")
lines(day, results[, 1] + results[, 2] + results[, 3], col = "red")
legend("topleft", legend = c("Observed Flux", "Modelled Flux"), col = c("black", "red"), lty = 1)
