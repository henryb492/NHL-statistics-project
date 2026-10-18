# Home-ice advantage in the NHL Pacific Division
#
# HOW TO RUN: clone/download the repo, open this script in R or RStudio, and run
# it top to bottom (or run `Rscript NHL-Stats-Updated.R` from a terminal).
# The script finds the CSV files itself and installs tidyverse if it is missing.

# ---- Setup: packages ---------------------------------------------------------
required_pkgs <- c("tidyverse")
missing_pkgs  <- required_pkgs[!required_pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
library(tidyverse)

# ---- Setup: locate the data folder -------------------------------------------
# Works no matter where the repo is saved, and whether the script is run with
# Rscript, source(), or the Run button in RStudio. No hard-coded paths.
get_script_dir <- function() {
  args     <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", args[grep("^--file=", args)])
  if (length(file_arg) > 0) return(dirname(normalizePath(file_arg[1])))   # Rscript
  src <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)           # source()
  if (!is.null(src)) return(dirname(normalizePath(src)))
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) return(dirname(normalizePath(p)))                      # RStudio
  }
  getwd()
}

data_files <- c("home_away_penalties.csv", "nhl_game_details.csv", "nhl_wins_losses.csv")
base_dirs  <- unique(c(get_script_dir(), getwd()))
candidates <- unlist(lapply(base_dirs, function(b) c(file.path(b, "Data"), file.path(b, "data"), b)))
data_dir   <- Find(function(d) all(file.exists(file.path(d, data_files))), candidates)

if (is.null(data_dir)) {
  stop("Could not find the CSV files (", paste(data_files, collapse = ", "), ").\n",
       "Keep them in a 'Data' folder next to this script, or set your working directory ",
       "to the repository folder (RStudio: Session > Set Working Directory > To Source File Location).")
}
message("Reading data from: ", data_dir)

# ---- Load data ---------------------------------------------------------------
home_away_penalties <- read_csv(file.path(data_dir, "home_away_penalties.csv"), show_col_types = FALSE)
nhl_game_details    <- read_csv(file.path(data_dir, "nhl_game_details.csv"),    show_col_types = FALSE)
nhl_wins_losses     <- read_csv(file.path(data_dir, "nhl_wins_losses.csv"),     show_col_types = FALSE)

#F-test Results and graphs to pass tests
regres8<-lm(Win.. ~ PT:GH + Shots, data = nhl_game_details) #Best model found out of many tested

# lm() drops rows with missing values (GH is blank on away rows), so use the
# same rows the model used. Mixing in all 336 rows gives the wrong sums of squares.
y      <- model.response(model.frame(regres8))
Y_pred <- fitted(regres8)
Y_mean <- mean(y)

# Calculate SS_Regression
SS_Regression <- sum((Y_pred - Y_mean)^2)

# Calculate SS_Residual
SS_Residual <- sum((y - Y_pred)^2)

# Calculate SS_Total (for verification)
SS_Total <- sum((y - Y_mean)^2)

# Print the results
cat("SS_Regression:", SS_Regression, "\n")
cat("SS_Residual:", SS_Residual, "\n")
cat("SS_Total:", SS_Total, "\n")
print(summary(regres8))   # F-statistic and p-value are at the bottom

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
df_long <- nhl_game_details %>%
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

#Combining data for the home and away penalties, and Shots
combined_data <- bind_rows(
  subset(nhl_game_details, H.or.A == "H") %>%
    filter(Team != "VGK") %>%
    mutate(H.or.A = "Home"),
  
  subset(nhl_game_details, H.or.A == "A") %>%
    filter(Team != "VGK") %>%
    mutate(H.or.A = "Away")
)

#Split into two parts, compares penalties taken from home, and away teams
ggplot(combined_data, aes(x = Team, y = PT, fill = Team)) +
  geom_col() +
  facet_wrap(~ H.or.A, scales = "free_y") +  # Facet by Home or Away
  theme_minimal() +
  labs(title = "Penalties Taken by Home and Away Teams", x = "Team", y = "Penalties Taken") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels for readability

#Boxplot comparing penalties taken from home and away teams

ggplot(nhl_game_details, aes(x=H.or.A, y=PT, fill=H.or.A)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Penalties Taken at Home Vs. Away", x="") +
  scale_fill_manual(values = c("A" = "blue", "H" = "red"), labels = c("Home", "Away")) +
  guides(fill = guide_legend(title = "Penatlies Taken")) +
  theme(axis.text.x = element_blank())

#Boxplot comparing shots taken from home and away teams
ggplot(nhl_game_details, aes(x=H.or.A, y=Shots, fill=H.or.A)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Shots Taken at Home Vs. Away", x="") +
  scale_fill_manual(values = c("A" = "blue", "H" = "red"), labels = c("Home", "Away")) +
  theme(axis.text.x = element_blank()) +
  guides(fill = guide_legend(title = NULL))
