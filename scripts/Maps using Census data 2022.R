library(dplyr)
library(sf)
library(ggplot2)
library(ggspatial)
library(terra)

bd_map_adm1 <- st_read("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/shapefiles/Bangladesh/adm_levels/ADM_1/geoBoundaries-BGD-ADM1_simplified.shp")
bd_map_adm2 <- st_read("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/R Data for Gendev/AfA GenDev Buy-In/data/shapefiles/Bangladesh/adm_levels/ADM_2/geoBoundaries-BGD-ADM2.shp")

#Divisions map
ggplot(data = bd_map_adm1) +
  geom_sf(aes(fill = shapeName)) +  # Replace with actual column name for ADM1
  geom_sf_text(aes(label = shapeName), size = 3, color = "black") +
  theme_void() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +  # Removes the legend +
  labs(title = "ADM1 Level Map of Bangladesh",
       fill = "ADM1") +
  coord_sf()

#Districts map

ggplot(data = bd_map_adm2) +
  geom_sf(aes(fill = shapeName)) +  # Replace with actual column name for ADM1
  geom_sf_text(aes(label = shapeName), size = 2, color = "black") +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none") +  # Removes the legend +
  labs(title = "ADM2 Level Map of Bangladesh") +
  coord_sf()

#Population map
library(readxl)
library(dplyr)
library(scales)

bd_census <- read_excel("C:/Users/Piyush/OneDrive - SRCPL/Documents/Open Projects/UCSD Gendev/Bangladesh Census Data/revised_census_merged.xlsx")

bd_census <- bd_census %>%
  rename(shapeName = district)

merged_data <- bd_map_adm2 %>%
  left_join(bd_census, by = c("shapeName" = "shapeName"))

ggplot(data = merged_data) +
  geom_sf(aes(fill = total_pop)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Population by District",
    fill = "Population"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#Male and female population

merged_data <- merged_data %>%
  mutate(
    male_percent = (total_male_pop / total_pop) * 100,
    female_percent = (total_female_pop / total_pop) * 100
  )

ggplot(data = merged_data) +
  geom_sf(aes(fill = male_percent)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Male Population % by District",
    fill = "Male Population % "
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

ggplot(data = merged_data) +
  geom_sf(aes(fill = female_percent)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Female Population % by District",
    fill = "Female Population % "
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


#Average population growth

ggplot(data = merged_data) +
  geom_sf(aes(fill = avg_pop_growth)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Average Population Growth Rate in % (per year)",
    fill = "Average Population Growth Rate per year (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

#Female Literacy Rate

ggplot(data = merged_data) +
  geom_sf(aes(fill = overall_female_lit_rate_7yr)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Female Literacy Rate in % (ages 7 years and above)",
    fill = "Female Literacy Rate (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

# Percentage of women in the salaried workforce
merged_data <- merged_data %>%
  mutate(
    female_percent_salaried = (total_female_salary_wage / total_salary_wage) * 100)

ggplot(data = merged_data) +
  geom_sf(aes(fill = female_percent_salaried)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Percentage of women in the salaried workforce",
    fill = "Female salaried (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Percentage of women 15 years and above who have access to a cellphone

ggplot(data = merged_data) +
  geom_sf(aes(fill = mobile_phone_female)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Percentage of women (age 15 and above) who have access to a cellphone",
    fill = "Female cellphone access (age 15 and above) (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Percentage of women who have a bank account

ggplot(data = merged_data) +
  geom_sf(aes(fill = have_financial_account_female)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Percentage of women who have a bank account",
    fill = "Female bank account (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Percentage of women 15 years and above who have access to bank account

ggplot(data = merged_data) +
  geom_sf(aes(fill = mobile_phone_female)) +
  scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
  theme_minimal() +
  labs(
    title = "Percentage of women (age 15 and above) who have access to a cellphone",
    fill = "Female cellphone access (age 15 and above) (%)"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Persons with disability


ggplot(data = merged_data) +
  geom_sf(aes(fill = total_persons_disability)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Total Persons with Disability by District",
    fill = "Disabled persons"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


#Sex Ratio
ggplot(data = merged_data) +
  geom_sf(aes(fill = overall_sex_ratio)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Overall sex ratio",
    fill = "Number of males per 100 females"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


#Child-Woman Ratio
ggplot(data = merged_data) +
  geom_sf(aes(fill = overall_child_woman_ratio)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey50", labels = scales::comma) +
  theme_minimal() +
  labs(
    title = "Overall child-woman ratio",
    fill = "Number of children under age of 5 per every 1000 women aged 15-49 years"
  ) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

