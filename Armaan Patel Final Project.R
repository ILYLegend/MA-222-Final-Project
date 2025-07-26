# Armaan Patel Final Project

library(tidyverse)

setwd("C:/Users/Iamsc/OneDrive/Desktop/MA 222/Individual Final Project Files")

# Read files in
gdp <- read_csv("GDP_1997_2020.csv", col_types = cols(.default = col_character())) |>
  filter(!is.na(GeoName)) |>
  pivot_longer(
    cols = "1997":"2020", 
    names_to = "Year", 
    values_to = "GDP") |>
  mutate(Year = as.integer(Year),
         GDP = as.numeric(gsub(",", "", GDP)),
         GeoName = str_replace(GeoName, "United States \\*", "United States"))
birth <- read_csv("us_births_2016_2021.csv")

# Thought this would give me a pie graph with each education level sectioned off.
birth |>
  count(`Education Level of Mother`) |>
  ggplot(aes(x = "", y = n, fill = `Education Level of Mother`)) +
  geom_bar(stat="identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = "Education Level of Mothers",
       subtitle = "United States") +
  geom_text(aes(label = paste0(round(n / sum(n) * 100, 1), "%")),
            position = position_stack(vjust = 0.5),
            size = 3)

# Graph that shows percentage of birth, but I can't get the percentages right
birth |>
  group_by(`Education Level of Mother`) |>
  summarise(total_births = sum(`Number of Births`, na.rm =TRUE)) |>
  ggplot(aes(x = "", y = total_births, fill = `Education Level of Mother`)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(total_births / total_births[[1]], 1), "%")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "Number of Births by Education Level of Mothers",
       subtitle = "United States")

# Scratched the pie chart idea and tried bar chart, got something, but I don't like it.
birth |>
  group_by(Year, `Education Level of Mother`) |>
  summarise(Births = sum(`Number of Births`, na.rm = TRUE)) |>
  ggplot(aes(x = Year, y = Births, fill = `Education Level of Mother`)) +
  geom_col(position = "stack") +
  labs(title = "Births by Education Level of Mother", y = "Number of Births")

# Using this as a reference to see descriptions/industries easily for the graphs below
gdp |>
  group_by(GeoName, Year) |>
  ggplot(aes(x = Year, y = GDP, color = Description)) +
  geom_line()

# Switched data sets to try the GDP data set before joining and found something interesting.
gdp |>
  group_by(Year) |>
  filter(Description %in% c("All industry total", "Oil and gas extraction", "Military"), 
         GeoName %in% c("United States", "Pennsylvania", "Texas")) |>
  ggplot(aes(x = Year, y = GDP, color = Description)) +
  geom_point() +
  facet_wrap(~ GeoName)

# Decided to continue from graph above and not join my data sets and look for something cool
# Found an interesting relationship on oil and gas extraction with respect to petroleum and coal products manufacturing
# Nevermind, there doesn't seem to be a relation.
gdp |>
  group_by(GeoName, Year) |>
  filter(Description %in% c("Mining (except oil and gas)", "Oil and gas extraction", "Mining, quarrying, and oil and gas extraction", "Petroleum and coal products manufacturing"),
         GeoName %in% c("California", "Wyoming", "Virginia", "Alaska")) |>
  ggplot(aes(x = Year, y = GDP, color = Description)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ GeoName) +
  labs(title = "Oil and Gas Extraction's Affect on Petroleum and Coal Products Manufacturing")

# Saw two interesting industries that might have a interesting correlation with another industry
# Couldn't find a good correlation so scratched it
gdp |>
  group_by(GeoName, Year) |>
  filter(Description %in% c("Computer and electronic product manufacturing", "Computer systems design and related services", "Paper manufacturing", "Transit and ground passenger transportation"),
         GeoName %in% c("California", "New Jersey", "Texas", "New York")) |>
  ggplot(aes(x = Year, y = GDP, color = Description)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ GeoName) +
  labs()

# Construction and real estate could be related.
# Found that construction, real estate, finance, and insurance are related through Google, which makes sense as real estate needs construction,
# purchasing real estate needs finance, and insurance to protect both the construction and real estate.
# Found that construction and manufacturing are closely related, and that finance + insurance and real estate are closely related.
# Saw that recessions make a difference with the GDP, so I added them.
# Finalized on this graph
# Final "best figure ever"
gdp |>
  group_by(GeoName, Year) |>
  filter(Description %in% c("Finance and insurance", "Real estate", "Construction", "Manufacturing"),
         GeoName %in% c("California", "Florida", "Texas", "New York", "United States", "Illinois")) |>
  ggplot(aes(x = Year, y = GDP, color = Description)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ GeoName, scales = "free_y") +
  labs(title = "Interconnectedness of Construction, Manufacturing, Real Estate, and Finance and Insurance",
       subtitle = "States with the highest GDP. Recessions marked.",
       y = "GDP (Millions USD)",
       color = "Industry Sector") +
  scale_y_continuous(labels = scales::comma) +
  geom_vline(xintercept = c(2001, 2008, 2020), linetype = "dashed", color = "black") +
  annotate("text", x = 2001, y = Inf, label = "2001", vjust = 1.2, hjust = 1.2, angle = 90, size = 3) +
  annotate("text", x = 2008, y = Inf, label = "2008", vjust = 1.2, hjust = 1.2, angle = 90, size = 3) +
  annotate("text", x = 2020, y = Inf, label = "2020", vjust = 1.2, hjust = 1.2, angle = 90, size = 3)
# The final graph shows the interconnectedness of construction, manufacturing, real estate, and finance and insurance.
# As one goes up or down the others move along with it showing their dependency on each other.
# The states with the highest GDPs were selected along with the United States as a whole to compare from.
# Recessions have made differences in GDPs which is why they are marked.
# I wanted to separate finance and insurance, but the data set doesn't have them separately.
