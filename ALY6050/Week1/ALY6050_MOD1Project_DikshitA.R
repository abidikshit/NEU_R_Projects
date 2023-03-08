prob_rs_win_home = 0.6
prob_nyy_win_home = 0.57

prob_rs_win_series = prob_rs_win_home^2 * (1 - prob_nyy_win_home) + prob_rs_win_home * (1 - prob_rs_win_home) * prob_nyy_win_home * 2
cat("Probability of Red Sox winning the series:", prob_rs_win_series, "\n")

outcomes =c(-520, 0, 500)
probabilities =c((1 - prob_rs_win_series)^2, 2 * prob_rs_win_series * (1 - prob_rs_win_series), prob_rs_win_series^2)
cat("Probabilities:", probabilities)

mean_X =sum(outcomes * probabilities)
variance_X =sum((outcomes - mean_X)^2 * probabilities)
sd_X =sqrt(variance_X)
cat("Expected net win:", mean_X, "\n")
cat("Standard deviation of net win:", sd_X, "\n")

set.seed(1)
Y =sample(outcomes, 10000, replace = TRUE, prob = probabilities)

# Calculate the 95% confidence interval for the expected net win
lower_ci =mean(Y) - qt(0.975, df = length(Y) - 1) * sd(Y) / sqrt(length(Y))
upper_ci =mean(Y) + qt(0.975, df = length(Y) - 1) * sd(Y) / sqrt(length(Y))

cat("95% Confidence interval for expected net win:", lower_ci, "-", upper_ci, "\n")
if (lower_ci <= mean_X & mean_X <= upper_ci) {
  cat("The confidence interval contains E(X).\n")
} else {
  cat("The confidence interval does not contain E(X).\n")
}

freq_table =table(Y)
expected_counts =length(Y) * probabilities
chisq_test =chisq.test(freq_table, p = expected_counts, rescale.p = TRUE)

# Print the Chi-squared test results
cat("Chi-squared test results:\n")
print(chisq_test)

cat("The predicted net win indicates that the betting strategy is not in your favour because it is negative, according to the expected net win and the standard deviation calculated in part (ii). Your actual net win might, however, be positive based on the 95% confidence interval derived in part (iii), however this cannot be said with absolute certainty. 

The distribution of Y does not closely approximate the distribution of X, according to the Chi-squared goodness of fit test carried out in part (iv), which raises the possibility that the simulation may not accurately reflect the real probabilities of the outcomes. In general, it's vital to exercise caution and be aware of the hazards while placing bets.")

prob_rs_win_series =prob_nyy_win_home^2 * (1 - prob_rs_win_home) + prob_nyy_win_home * (1 - prob_nyy_win_home) * prob_rs_win_home * 2
cat("Probability of Red Sox winning series:", prob_rs_win_series, "\n")

# Calculate the possible outcomes and their probabilities
outcomes =c(-520, 0, 500)
probabilities =c((1 - prob_rs_win_series)^2, 2 * prob_rs_win_series * (1 - prob_rs_win_series), prob_rs_win_series^2)

# Calculate the expected net win and the standard deviation
mean_X =sum(outcomes * probabilities)
variance_X =sum((outcomes - mean_X)^2 * probabilities)
sd_X =sqrt(variance_X)
cat("Expected net win:", mean_X, "\n")
cat("Standard deviation of net win:", sd_X, "\n")

# Generate 10,000 random values of X
set.seed(1)
Y =sample(outcomes, 10000, replace = TRUE, prob = probabilities)

lower_ci =mean(Y) - qt(0.975, df = length(Y) - 1) * sd(Y) / sqrt(length(Y))
upper_ci =mean(Y) + qt(0.975, df = length(Y) - 1) * sd(Y) / sqrt(length(Y))
cat("95% Confidence interval for expected net win:", lower_ci, "-", upper_ci, "\n")

check_confidence_interval <- function(lower_ci, upper_ci, mean_X) {
  if (lower_ci <= mean_X & mean_X <= upper_ci) {
    return("The confidence interval contains E(X).")
  } else {
    return("The confidence interval does not contain E(X).")
  }
}

freq_table =table(Y)
expected_counts =length(Y) * probabilities
chisq_test =chisq.test(freq_table, p = expected_counts, rescale.p = TRUE)
# Print the Chi-squared test results
cat("Chi-squared test results:\n")
print(chisq_test)

cat("The predicted net win indicates that the betting strategy is not in your favour because it is negative, according to the expected net win and the standard deviation calculated in part (ii). Your actual net win might, however, be positive based on the 95% confidence interval derived in part (iii), however this cannot be said with absolute certainty. 

The distribution of Y does not closely approximate the distribution of X, according to the Chi-squared goodness of fit test carried out in part (iv), which raises the possibility that the simulation may not accurately reflect the real probabilities of the outcomes. In general, it's vital to exercise caution and be aware of the hazards while placing bets.")

# rows represent Red Sox and columns represent Yankees
prob_matrix =matrix(nrow = 5, ncol = 2)
prob_matrix[1, ] =c(prob_rs_win_home, 1 - prob_rs_win_home)
prob_matrix[2, ] =c(1 - prob_nyy_win_home, prob_nyy_win_home)
prob_matrix[3, ] =c(prob_rs_win_home, 1 - prob_rs_win_home)
prob_matrix[4, ] =c(1 - prob_nyy_win_home, prob_nyy_win_home)
prob_matrix[5, ] =c(prob_rs_win_home, 1 - prob_rs_win_home)

simulate_series <- function() {
  red_sox_wins <- 0
  yankees_wins <- 0
  net_win <- 0
  
  for (i in 1:5) {
    # Sample the winner of the game based on the probabilities
    winner <- sample(c("Red Sox", "Yankees"), size = 1, prob = prob_matrix[i, ])
    
    # Add a win to the winner's count
    if (winner == "Red Sox") {
      red_sox_wins <- red_sox_wins + 1
    } else {
      yankees_wins <- yankees_wins + 1
    }
    
    # If one team has won 3 games, end the series and calculate the net win
    if (red_sox_wins == 3) {
      net_win <- 500
      break
    } else if (yankees_wins == 3) {
      net_win <- -520
      break
    }
  }
  
  return(net_win)
}

set.seed(1)
net_wins =replicate(10000, simulate_series())

mean_net_win =mean(net_wins)
sd_net_win =sd(net_wins)
cat("Expected net win:", mean_net_win, "\n")
cat("Standard deviation of net win:", sd_net_win, "\n")

conf_interval = t.test(net_wins)$conf.int
cat("95% confidence interval for expected net win:", conf_interval, "\n")

check_confidence_interval <- function(mean_net_win, conf_interval) {
  if (mean_net_win >= conf_interval[1] && mean_net_win <= conf_interval[2]) {
    return("The confidence interval contains the expected net win.")
  } else {
    return("The confidence interval does not contain the expected net win.")
  }
}

freq_table =table(net_wins)
freq_dist =data.frame(net_win = as.numeric(names(freq_table)), count = as.numeric(freq_table))

options(scipen = 100)
chisq.test(freq_dist$count, p = dnorm(freq_dist$net_win, mean = mean_net_win, sd = mean_net_win), rescale.p = TRUE)

## NA
