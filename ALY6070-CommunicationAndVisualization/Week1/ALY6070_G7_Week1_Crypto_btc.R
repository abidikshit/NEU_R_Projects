my_packages = c("plyr", "plotly", "ggplot2", "psych", "tidyr", "tidyverse","dplyr","lubridate","readr","caret")
#install.packages(my_packages)
lapply(my_packages, require, character.only = T)

#Dataset used from https://coinmetrics.io/community-network-data/
bitcoin <- read.csv("/Users/abidikshit/R_Projects/Data/coin_Bitcoin.csv", header = T)
cat("Number of Rows before cleanup:", nrow(bitcoin), "\n") # Printing string and variable row count on the same line
cat("Number of Columns before cleanup:", ncol(bitcoin), "\n") 
cat("Blank cells count before cleanup:", sum(!complete.cases(bitcoin))) # Displaying Blank Cells Count for uncleaned data

bitcoin$Date <- as.Date(bitcoin$Date)

headTail(bitcoin, top = 4, bottom = 4, ellipsis = F)

summary(bitcoin)

ggplot(bitcoin, aes(x = Date, y = Close)) + geom_line() + xlab("Date") + ylab("Close Price (USD)") + ggtitle("Bitcoin Price Trend Over Time")

ggplot(bitcoin, aes(x = Close)) + geom_histogram(binwidth = 50) + xlab("Close Price (USD)") + ylab("Frequency") + ggtitle("Histogram of Bitcoin Closing Prices")

bitcoin$Year <- year(bitcoin$Date)
ggplot(bitcoin, aes(x = Year, y = Close)) + geom_boxplot() + xlab("Year") + ylab("Close Price (USD)") + ggtitle("Box Plot of Bitcoin Closing Prices by Year")

ggplot(bitcoin, aes(x = Close, y = Volume, colour = Date)) + 
  geom_point() + 
  xlab("Close Price (USD)") + 
  ylab("Trading Volume") + 
  ggtitle("Scatter Plot of Bitcoin Closing Prices and Trading Volume") + 
  scale_color_viridis_c() +
  guides(colour = guide_legend(title = "Date")) +
  geom_smooth(method="lm", se=FALSE, color="blue")

total_volume <- aggregate(Volume ~ Year, data = bitcoin, sum)
mean_volume <- mean(total_volume$Volume)

ggplot(total_volume, aes(x = reorder(Year, -Volume), y = Volume)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  xlab("Year") + 
  ylab("Total Trading Volume") + 
  ggtitle("Total Trading Volume of Bitcoin by Year") +
  geom_text(aes(label = format(Volume, big.mark = ",")), vjust = -0.5) +
  geom_hline(yintercept = mean_volume, linetype = "dashed", color = "coral") +
  annotate("text", x = Inf, y = mean_volume, vjust = -1, hjust = 1, label = paste0("Mean: $", format(mean_volume, big.mark = ","))) +
  geom_text(aes(label = "Mean Volume"), hjust = 1.5, color = "red", size = 3) +
  theme_classic()


bitcoin_pca <- read_csv("/Users/abidikshit/R_Projects/Data/coin_Bitcoin.csv", col_types = cols_only(Date = col_date(), Open = col_double(), High = col_double(), Low = col_double(), Close = col_double(), Volume = col_double(), Marketcap = col_double()))
bitcoin_pca <- bitcoin_pca[,c(2:7)]

scaled_bitcoin <- scale(bitcoin_pca)

pca_bitcoin <- prcomp(scaled_bitcoin, scale = TRUE)

summary(pca_bitcoin)

scree_plot <- ggplot(data.frame(PC = 1:6, Variance = pca_bitcoin$sdev^2 / sum(pca_bitcoin$sdev^2)), aes(x = PC, y = Variance)) + geom_bar(stat = "identity", fill = "steelblue") + geom_line(aes(x = PC, y = cumsum(Variance)), color = "red") + xlab("Principal Component") + ylab("Proportion of Variance") + ggtitle("Scree Plot of Bitcoin Price History Dataset")
scree_plot

pcs_bitcoin <- predict(pca_bitcoin, scaled_bitcoin)

# Create the cumulative variance plot
cumulative_variance <- cumsum(pca_bitcoin$sdev^2 / sum(pca_bitcoin$sdev^2))
plot(cumulative_variance, xlab = "Number of components", ylab = "Cumulative variance", 
     type = "b", pch = 19, col = "blue", ylim = c(0,1))

# Add a dashed line at the 95% variance threshold
abline(h = 0.95, lty = 2, col = "red")

# Add a vertical line at the optimal number of components
optimal_components <- which.max(cumulative_variance >= 0.95)
abline(v = optimal_components, lty = 2, col = "green")

# Add text labels for the variance threshold and optimal number of components
text(optimal_components, 0.97, paste0("Optimal components: ", optimal_components), pos = 3)
text(length(cumulative_variance)*0.75, 0.97, "95% variance threshold", pos = 3, col = "red")


# Choose the number of components
n_components <- length(which(cumsum(pca_bitcoin$sdev^2 / sum(pca_bitcoin$sdev^2)) < 0.8)) + 1
cat("Number of components:", n_components, "\n")

## NA
