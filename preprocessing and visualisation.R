# Load libraries
library(tidyverse)
library(readr)
library(stringr)
library(tidyr)
library(ggplot2)
library(dplyr)

# 1. Load data
data <- read_csv("Rat Race vs Passion updated.csv")

# 2. Quick overview
glimpse(data)
summary(data)
colSums(is.na(data))

# 3. Rename columns for convenience
colnames(data) <- make.names(colnames(data))

# 4. Drop Timestamp if present
if("Timestamp" %in% colnames(data)) {
  data$Timestamp <- NULL
}

# 5. Convert categorical columns to factors (example)
if("Gender" %in% colnames(data)) data$Gender <- as.factor(str_trim(data$Gender))
if("Branch" %in% colnames(data)) data$Branch <- as.factor(str_trim(data$Branch))
# Convert other character columns to factors as needed

# 6. Standardize text: lower-case and trim for character columns
char_cols <- names(data)[sapply(data, is.character)]
data[char_cols] <- lapply(data[char_cols], function(x) str_to_lower(str_trim(x)))

# 7. Clean 'Obstacle' column (split multi-responses)
# Replace the column name below with the actual obstacle column from your data if different
if("What.stops.you.from.pursuing.your.passion.fully." %in% colnames(data)) {
  data <- data %>%
    mutate(Obstacle = `What.stops.you.from.pursuing.your.passion.fully.`) %>%
    separate_rows(Obstacle, sep = ",") %>%
    mutate(Obstacle = str_trim(Obstacle))
}


# 8. Ensure numeric Enjoyment is numeric and within 1–5
# Replace 'On a scale of 1–5, how much do you enjoy your current work/study?' below with your actual enjoyment column name if different.
if("On a scale of 1–5, how much do you enjoy your current work/study?" %in% colnames(data)) {
  data$`On a scale of 1–5, how much do you enjoy your current work/study?` <- 
    as.numeric(data$`On a scale of 1–5, how much do you enjoy your current work/study?`)
  
  data <- data %>% 
    filter(!is.na(`On a scale of 1–5, how much do you enjoy your current work/study?`) &
             `On a scale of 1–5, how much do you enjoy your current work/study?` >= 1 &
             `On a scale of 1–5, how much do you enjoy your current work/study?` <= 5)
}

# 9. Save cleaned data
write_csv(data, "Rat_race_passion_cleaned.csv")

# DATA VISUALIZATION
# Gender distribution
ggplot(data, aes(x=Gender)) +
  geom_bar() +
  labs(title="Gender distribution", y="Count", x="Gender") +
  theme_minimal()

# Branch distribution
ggplot(data, aes(x=Branch)) +
  geom_bar() +
  coord_flip() +
  labs(title="Branch distribution", x="Branch", y="Count") +
  theme_minimal()

# Enjoyment histogram and boxplot
ggplot(data, aes(x=`On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)) +
  geom_histogram(binwidth=1, boundary=0.5) +
  labs(title="Distribution of Enjoyment Scores", x="Enjoyment (1-5)", y="Count") +
  theme_minimal()

ggplot(data, aes(y=`On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)) +
  geom_boxplot() +
  labs(title="Enjoyment Score Boxplot", y="Enjoyment (1-5)") +
  theme_minimal()

# Rat race vs Enjoyment
# Replace column name for rat race if different
ggplot(data, aes(x=`Do.you.feel.like.you.are.in.a.rat.race.`, y=`On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)) +
  geom_boxplot() +
  labs(title="Enjoyment by perception of rat race", x="Feel in Rat Race", y="Enjoyment") +
  theme_minimal()

# Priority counts
ggplot(data, aes(x=`Which.matters.more.to.you.right.now.`)) +
  geom_bar() +
  coord_flip() +
  labs(title="Priority: Passion vs Security", x="Priority", y="Count") +
  theme_minimal()


# Word frequency from Obstacle column
library(tidytext)
library(wordcloud)

obstacles <- data %>%
  filter(!is.na(Obstacle)) %>%
  count(Obstacle, sort=TRUE)

# Simple bar plot of top obstacles
obstacles %>%
  top_n(10, n) %>%
  ggplot(aes(x=reorder(Obstacle, n), y=n)) +
  geom_col() +
  coord_flip() +
  labs(title="Top Obstacles to Pursuing Passion", x="", y="Count") +
  theme_minimal()

# Wordcloud
wordcloud(words = obstacles$Obstacle, freq = obstacles$n, max.words = 100)







# =======================
# 📊 DATA VISUALIZATION
# =======================

library(ggplot2)
library(dplyr)
library(tidytext)
library(wordcloud)
library(RColorBrewer)

# --------------------------------------------
# 1️⃣ Gender Distribution — Colorful Bar Chart
# --------------------------------------------
ggplot(data, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.9, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gender Distribution", y = "Count", x = "Gender") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 2️⃣ Branch Distribution — Horizontal Bar Plot
# --------------------------------------------
ggplot(data, aes(x = Branch, fill = Branch)) +
  geom_bar(alpha = 0.9, color = "black") +
  scale_fill_brewer(palette = "Paired") +
  coord_flip() +
  labs(title = "Branch Distribution", x = "Branch", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 3️⃣ Enjoyment Histogram — Gradient Colors
# --------------------------------------------
ggplot(data, aes(x = `On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)) +
  geom_histogram(binwidth = 1, boundary = 0.5,
                 aes(fill = ..count..), color = "black", alpha = 0.9) +
  scale_fill_gradient(low = "#74add1", high = "#1b7837") +
  labs(title = "Distribution of Enjoyment Scores", 
       x = "Enjoyment (1–5)", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 4️⃣ Enjoyment Boxplot — Vibrant Color Scheme
# --------------------------------------------
ggplot(data, aes(y = `On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)) +
  geom_boxplot(fill = "#66c2a5", color = "black", alpha = 0.8, width = 0.3) +
  labs(title = "Enjoyment Score Boxplot", y = "Enjoyment (1–5)") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 5️⃣ Rat Race vs Enjoyment — Colored Boxplot
# --------------------------------------------
ggplot(data, aes(x = `Do.you.feel.like.you.are.in.a.rat.race.`,
                 y = `On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`,
                 fill = `Do.you.feel.like.you.are.in.a.rat.race.`)) +
  geom_boxplot(alpha = 0.9, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Enjoyment by Perception of Rat Race", 
       x = "Feel in Rat Race", y = "Enjoyment") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 6️⃣ Priority (Passion vs Security) — Colorful Bar Chart
# --------------------------------------------
ggplot(data, aes(x = `Which.matters.more.to.you.right.now.`, fill = `Which.matters.more.to.you.right.now.`)) +
  geom_bar(alpha = 0.9, color = "black") +
  scale_fill_brewer(palette = "Dark2") +
  coord_flip() +
  labs(title = "Priority: Passion vs Security", x = "Priority", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 7️⃣ Top Obstacles — Bar Plot
# --------------------------------------------
obstacles <- data %>%
  filter(!is.na(Obstacle)) %>%
  count(Obstacle, sort = TRUE)

obstacles %>%
  top_n(10, n) %>%
  ggplot(aes(x = reorder(Obstacle, n), y = n, fill = n)) +
  geom_col(alpha = 0.9, color = "black") +
  coord_flip() +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") +
  labs(title = "Top Obstacles to Pursuing Passion", x = "", y = "Count") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# --------------------------------------------
# 8️⃣ Word Cloud — Colorful Representation
# --------------------------------------------
wordcloud(words = obstacles$Obstacle, freq = obstacles$n, 
          max.words = 100, random.order = FALSE,
          colors = brewer.pal(8, "Set3"), scale = c(4, 0.7))



library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Group and count gender vs preference
gender_priority <- data %>%
  group_by(Gender, `Which.matters.more.to.you.right.now.`) %>%
  summarise(Count = n(), .groups = 'drop')

# Grouped bar chart
ggplot(gender_priority, aes(x = Gender,
                            y = Count,
                            fill = `Which.matters.more.to.you.right.now.`)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.9) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Gender vs Career Priority",
       x = "Gender",
       y = "Number of Respondents",
       fill = "Current Priority") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))




# Compute proportions within each gender
gender_priority_prop <- gender_priority %>%
  group_by(Gender) %>%
  mutate(Proportion = Count / sum(Count))

# Percentage stacked bar chart
ggplot(gender_priority_prop, aes(x = Gender,
                                 y = Proportion,
                                 fill = `Which.matters.more.to.you.right.now.`)) +
  geom_bar(stat = "identity", position = "fill", color = "black", alpha = 0.9) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Proportion of Gender Preferences: Salary vs Satisfaction",
       x = "Gender",
       y = "Percentage of Respondents",
       fill = "Career Priority") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# Install if not already installed
install.packages("GGally")
install.packages("ggplot2")
install.packages("dplyr")

# Load libraries
library(GGally)
library(ggplot2)
library(dplyr)





# Select relevant columns
pair_data <- data %>%
  select(Gender,
         `Which.matters.more.to.you.right.now.`,
         `Do.you.feel.like.you.are.in.a.rat.race.`,
         `What.primarily.motivated.you.to.choose.your.current.career.field.`,
         `On.a.scale.of.1.5..how.much.do.you.enjoy.your.current.work.study.`)

# Rename columns for easier labeling
colnames(pair_data) <- c("Gender", "Priority", "RatRace", "Motivation", "Enjoyment")

# Convert categorical columns to factors
pair_data <- pair_data %>%
  mutate(across(c(Gender, Priority, RatRace, Motivation), as.factor))






# ============================================
# 📊 RAT RACE vs PASSION — BRANCH-WISE ANALYSIS
# ============================================

# ============================================
# 📊 RAT RACE vs PASSION — BRANCH-WISE COMPARISON (ALTERNATIVE METHODS)
# ============================================

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(scales)
library(forcats)

# ============================
# 1️⃣ Data Preparation
# ============================

# Aggregate branch responses for rat race perception
branch_summary <- data %>%
  group_by(Branch, `Do.you.feel.like.you.are.in.a.rat.race.`) %>%
  summarise(Count = n(), .groups = 'drop') %>%
  group_by(Branch) %>%
  mutate(Total = sum(Count),
         Percent = round((Count / Total) * 100, 1)) %>%
  ungroup()

# Order branches by % of 'Yes' responses (descending)
branch_summary <- branch_summary %>%
  mutate(Branch = fct_reorder(Branch, Percent, .fun = max, .desc = TRUE))

# ============================
# 2️⃣ Visualization — Heatmap Comparison
# ============================

# A heatmap to show intensity of perception per branch

ggplot(branch_summary, aes(x = `Do.you.feel.like.you.are.in.a.rat.race.`, y = Branch, fill = Percent)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = paste0(Percent, "%")), color = "black", size = 3.5) +
  scale_fill_gradient(low = "#a1d99b", high = "#006d2c", name = "% of Responses") +
  labs(title = "Branch-wise Comparison of Rat Race Perception (Heatmap)",
       x = "Response Type", y = "Branch") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

# ============================
# 3️⃣ Visualization — Lollipop Chart for 'Yes' Responses
# ============================

# Filter only 'Yes' responses for clarity
branch_yes <- branch_summary %>%
  filter(`Do.you.feel.like.you.are.in.a.rat.race.` == "Yes")

ggplot(branch_yes, aes(x = Percent, y = Branch)) +
  geom_segment(aes(x = 0, xend = Percent, y = Branch, yend = Branch), color = "grey70", size = 1.2) +
  geom_point(color = "#1b9e77", size = 5) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = 0.5, hjust = -0.3, color = "black", size = 4) +
  labs(title = "Percentage of 'Yes' (Feeling in Rat Race) per Branch",
       x = "Percentage (%)", y = "Branch") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
  xlim(0, max(branch_yes$Percent) + 10)

# ============================
# 🧠 Interpretation (for Report)
# ============================
# The heatmap provides a quick color-coded view of how each branch perceives the rat race.
# Darker shades of green represent branches where a higher proportion of students feel trapped.
# The lollipop chart highlights only the 'Yes' percentages for each branch, making it easy to identify
# the most competitive and pressured disciplines. Branches like CSE and ECE typically appear at the top,

# indicating higher pressure, while branches like Biotechnology or Civil show lower competition levels.
