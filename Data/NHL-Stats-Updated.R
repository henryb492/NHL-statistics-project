library(tidyverse)
home_away_penalties <- read_csv("Data/home_away_penalties.csv")
nhl_game_details <- read_csv("Data/nhl_game_details.csv")
nhl_wins_losses <- read_csv("Data/nhl_wins_losses.csv")
#F-test Results and graphs to pass tests
regres8<-lm(Win..~PT:GH+Shots) #Best model found out of many tested

# Get the predicted values from the model
Y_pred <- predict(regres8)

# Calculate the mean of Y
Y_mean <- mean(Win..)

# Calculate SS_Regression
SS_Regression <- sum((Y_pred - Y_mean)^2)

# Calculate SS_Residual
SS_Residual <- sum((Win.. - Y_pred)^2)

# Calculate SS_Total (for verification)
SS_Total <- sum((Win..- Y_mean)^2)

# Print the results
cat("SS_Regression:", SS_Regression, "\n")
cat("SS_Residual:", SS_Residual, "\n")
cat("SS_Total:", SS_Total, "\n")

#Plots in Poster that satifies F-test conditions
residuals<-residuals(regres8)
standardized_residuals<-residuals/ sd(residuals)
residual_data<-data.frame(residuals = residuals, z_scores= standardized_residuals)
#Normal Probability Plot
ggplot(residual_data, aes(x=residuals, y=z_scores)) +
  geom_point(shape = 21, fill = "red") +
  geom_abline(slope = 10.2, intercept = 0, color = "blue") +  # Reference line
  ggtitle("Normal Probability Plot") +
  theme_minimal() +
  xlab("Residuals") +
  ylab("Standardized Z-Scores") +
  theme(axis.title.x = element_text(color = "darkgreen", size=13),
        axis.title.y = element_text(color = "darkgreen", size = 13),
        plot.title = element_text(color = "darkcyan", size = 18, hjust = 0.5))
#Residual Plot
predicted_values<-predict(regres8)
plot_data<-data.frame(predicted = predicted_values, resisduals = residuals)
ggplot(plot_data, aes(x = predicted, y = residuals)) +
  geom_point(shape = 15) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residual Plot") +
  xlab("Predicted Win %") +
  ylab("Residuals") +
  theme(axis.title.x = element_text(color = "blue", size=13),
        axis.title.y = element_text(color = "blue", size = 13),
        plot.title = element_text(color = "darkred", size = 18, hjust = 0.5))
#Four Plots Included In Poster
df_long <- `NHL.Game.Details.Data...Sheet1.(6)` %>%
  pivot_longer(cols = c(GH, GA),  
               names_to = "Type",  
               values_to = "Goals")  

# Boxplot included in poster comparing goals scored in home and away games
goals_scored <- ggplot(df_long, aes(x = Season, y = Goals, fill = Type)) +
  geom_boxplot() +
  facet_wrap(~ Type, scales = "free_y") +  # Create separate plots for GH and GA
  labs(y = "Goals", title = "Goals Scored in Home vs Away Games") +
  theme_minimal() +
  scale_fill_manual(values = c("GH" = "skyblue", "GA" = "salmon"), labels=c("Goals at Home", "Goals Away")) +  # Custom colors for GH and GA
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  strip.text = element_blank()) + # Rotate x-axis labels
  guides(fill = guide_legend(title = NULL)) 
goals_scored


combined_data <- bind_rows(
  subset(`NHL.Game.Details.Data...Sheet1.(6)`, H.or.A == "H") %>%
    filter(Team != "VGK") %>%
    mutate(H.or.A = "Home"),
  
  subset(`NHL.Game.Details.Data...Sheet1.(6)`, H.or.A == "A") %>%
    filter(Team != "VGK") %>%
    mutate(H.or.A = "Away")
)

ggplot(combined_data, aes(x = Team, y = PT, fill = Team)) +
  geom_col() +
  facet_wrap(~ H.or.A, scales = "free_y") +  # Facet by Home or Away
  theme_minimal() +
  labs(title = "Penalties Taken by Home and Away Teams", x = "Team", y = "Penalties Taken") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels for readability


ggplot(`NHL.Game.Details.Data...Sheet1.(6)`, aes(x=H.or.A, y=PT, fill=H.or.A)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Penalties Taken at Home Vs. Away", x="") +
  scale_fill_manual(values = c("A" = "blue", "H" = "red"), labels = c("Home", "Away")) +
  guides(fill = guide_legend(title = "Penatlies Taken")) +
  theme(axis.text.x = element_blank())
ggplot(`NHL.Game.Details.Data...Sheet1.(6)`, aes(x=H.or.A, y=Shots, fill=H.or.A)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Shots Taken at Home Vs. Away", x="") +
  scale_fill_manual(values = c("A" = "blue", "H" = "red"), labels = c("Home", "Away")) +
  theme(axis.text.x = element_blank()) +
  guides(fill = guide_legend(title = NULL))
