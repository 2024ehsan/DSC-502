############################################################
# QUESTION 1
############################################################
cat("\n====================\n")
cat("QUESTION 1\n")
cat("====================\n")

# Create df1
df1 <- data.frame(
  Name  = c('James','Paul','Richards','Marico','Samantha','Ravi','Raghu',
            'Richards','George','Ema','Samantha','Catherine'),
  State = c('Alaska','California','Texas','North Carolina','California','Texas',
            'Alaska','Texas','North Carolina','Alaska','California','Texas'),
  Sales = c(14,24,31,12,13,7,9,31,18,16,18,14)
)

cat("\n-- df1 (head) --\n")
print(head(df1))

cat("\n1A) What does aggregate() do?\n")
cat("It groups rows by State, then sums Sales for each State.\n")
cat("Code: aggregate(Sales ~ State, data=df1, FUN=sum)\n")

q1_agg <- aggregate(Sales ~ State, data = df1, FUN = sum)

cat("\n-- Output: aggregate --\n")
print(q1_agg)

cat("\n1B) dplyr version (same result).\n")
cat("It groups by State and returns one row per State with total Sales.\n")

if (!requireNamespace("dplyr", quietly = TRUE)) {
  cat("NOTE: dplyr is not installed. Skipping dplyr part.\n")
} else {
  library(dplyr)
  q1_dplyr <- df1 %>%
    group_by(State) %>%
    summarise(sum_sales = sum(Sales))

  cat("\n-- Output: dplyr group_by/summarise --\n")
  print(q1_dplyr)
}

############################################################
# QUESTION 2
############################################################
cat("\n====================\n")
cat("QUESTION 2\n")
cat("====================\n")

# Read WorldCupMatches.csv
worldcup_file <- "WorldCupMatches.csv"

if (!file.exists(worldcup_file)) {
  cat("\nERROR: Can't find file:", worldcup_file, "\n")
  cat("Fix: Put WorldCupMatches.csv in the same folder as this .R script,\n")
  cat("or change worldcup_file to the correct full path.\n")
} else {

  worldcup <- read.csv(worldcup_file)

  cat("\n-- worldcup (head) --\n")
  print(head(worldcup))

  # (a) Size of the data frame
  cat("\n2(a) Size of the data frame\n")
  cat("dim(worldcup) gives (rows, columns):\n")
  print(dim(worldcup))
  cat("Rows:", nrow(worldcup), "\n")
  cat("Columns:", ncol(worldcup), "\n")

  # (b) Summary
  cat("\n2(b) summary(worldcup)\n")
  print(summary(worldcup))

  # (c) Unique locations (City)
  cat("\n2(c) How many unique locations (cities) were matches held at?\n")
  unique_locations <- length(unique(worldcup$City))
  cat("Unique cities:", unique_locations, "\n")

  # (d) Average attendance
  cat("\n2(d) Average attendance (ignoring missing values)\n")
  avg_attendance <- mean(worldcup$Attendance, na.rm = TRUE)
  cat("Average attendance:", avg_attendance, "\n")

  # (e) Total goals scored by each Home Team
  # IMPORTANT: Column names in your file include dots:
  #   Home.Team.Name, Home.Team.Goals
  cat("\n2(e) Total Home Team Goals per Home Team\n")
  home_goals <- aggregate(Home.Team.Goals ~ Home.Team.Name, data = worldcup, FUN = sum)
  colnames(home_goals) <- c("HomeTeam", "TotalGoals")

  cat("-- home_goals (head) --\n")
  print(head(home_goals))

  # (f) Average attendance for each year
  cat("\n2(f) Average attendance per year\n")
  attendance_per_year <- aggregate(Attendance ~ Year, data = worldcup, FUN = mean)
  colnames(attendance_per_year) <- c("Year", "AverageAttendance")
  print(attendance_per_year)

  cat("\nTrend note:\n")
  cat("Look at the AverageAttendance values by Year.\n")
  cat("If they generally rise over time, attendance is increasing; if they fall, decreasing.\n")

  # Optional plot (simple visualization)
  cat("\nOptional: Plot Year vs Average Attendance\n")
  plot(attendance_per_year$Year, attendance_per_year$AverageAttendance,
       main = "Average World Cup Match Attendance by Year",
       xlab = "Year", ylab = "Average Attendance")
}

############################################################
# QUESTION 3
############################################################
cat("\n====================\n")
cat("QUESTION 3\n")
cat("====================\n")

# The assignment says metabolites.csv, but your uploaded file name might be metabolite.csv.
# This code tries metabolites.csv first, then metabolite.csv.
metabolites_file <- "metabolites.csv"
if (!file.exists(metabolites_file)) {
  metabolites_file <- "metabolite.csv"
}

if (!file.exists(metabolites_file)) {
  cat("\nERROR: Can't find metabolites file.\n")
  cat("Tried: metabolites.csv and metabolite.csv\n")
  cat("Fix: Put the metabolites file in the same folder as this .R script.\n")
} else {

  metabolites <- read.csv(metabolites_file)

  cat("\nReading file:", metabolites_file, "\n")
  cat("\n-- metabolites (head) --\n")
  print(head(metabolites))

  # (a) Count Alzheimers patients
  cat("\n3(a) How many Alzheimer patients are in the data set?\n")

  if (!("Diagnosis" %in% colnames(metabolites))) {
    cat("ERROR: Column 'Diagnosis' not found. Here are the column names:\n")
    print(colnames(metabolites))
  } else {
    alz_count <- sum(metabolites$Diagnosis == "Alzheimer", na.rm = TRUE)
    cat("Alzheimer patient count:", alz_count, "\n")
  }

  # (b) Missing values per column
  cat("\n3(b) Number of missing values for each column\n")
  missing_by_col <- colSums(is.na(metabolites))
  print(missing_by_col)

  # (c) Remove rows with missing Dopamine
  cat("\n3(c) Remove rows with missing Dopamine (new data frame)\n")

  if (!("Dopamine" %in% colnames(metabolites))) {
    cat("ERROR: Column 'Dopamine' not found. Here are the column names:\n")
    print(colnames(metabolites))
  } else {
    metabolites_clean <- metabolites[!is.na(metabolites$Dopamine), ]
    cat("Original rows:", nrow(metabolites), "\n")
    cat("After removing NA Dopamine rows:", nrow(metabolites_clean), "\n")
  }

  # (d) Replace missing values in c4-OH-Pro with the median (in the cleaned df)
  cat("\n3(d) Replace missing c4-OH-Pro values with the median\n")

  if (exists("metabolites_clean")) {
    if (!("c4-OH-Pro" %in% colnames(metabolites_clean))) {
      cat("ERROR: Column 'c4-OH-Pro' not found. Here are the column names:\n")
      print(colnames(metabolites_clean))
    } else {
      med_c4 <- median(metabolites_clean$`c4-OH-Pro`, na.rm = TRUE)
      missing_before <- sum(is.na(metabolites_clean$`c4-OH-Pro`))

      metabolites_clean$`c4-OH-Pro`[is.na(metabolites_clean$`c4-OH-Pro`)] <- med_c4

      missing_after <- sum(is.na(metabolites_clean$`c4-OH-Pro`))

      cat("Median used:", med_c4, "\n")
      cat("Missing before:", missing_before, "\n")
      cat("Missing after:", missing_after, "\n")
    }
  } else {
    cat("Skipping part (d) because metabolites_clean was not created (check Dopamine column).\n")
  }

  # (e) Drop columns with >25% missing values (from ORIGINAL metabolites df)
  cat("\n3(e) Drop columns with more than 25% missing values\n")
  threshold <- 0.25 * nrow(metabolites)
  keep_cols <- colSums(is.na(metabolites)) <= threshold
  metabolites_reduced <- metabolites[, keep_cols]

  cat("Original columns:", ncol(metabolites), "\n")
  cat("Columns after dropping >25% missing:", ncol(metabolites_reduced), "\n")
}