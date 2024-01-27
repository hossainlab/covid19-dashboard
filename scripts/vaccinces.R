# Load packages 
library(tidyverse)
library(data.table)

# Color and maps 
library(plotly)
library(leaflet)
library(leafpop)
library(RColorBrewer)

# Dates
library(lubridate)


# data 
library(rvest)
library(coronavirus)

# Vaccination Data
df_vax <- read.csv("data/vaccinations.csv")


# daily vaccinations
df_vax_daily <- df_vax |> 
  select(date, location, daily_vaccinations) |> 
  group_by(location, date) |> 
  summarise(total = sum(daily_vaccinations), .groups="drop") |> 
  filter(location != "World") |> 
  arrange(-total)|> 
  pivot_wider(names_from = date, values_from = total) |> 
  slice(1:20) |> 
  pivot_longer(cols = -location, 
               names_to = "date", 
               values_to = "total") |> 
  mutate(date = ymd(date)) 



# world statistics
df_world_vax <- df_vax |> 
  select(location,
         total_vaccinations,
         people_vaccinated,
         people_fully_vaccinated,
  ) 
# world statistics
df_world_vax <- df_vax |> 
  select(location,
         total_vaccinations,
         people_vaccinated,
         people_fully_vaccinated,
  ) |> 
  filter(location == "World") |> 
  summarise(total_vax = max(total_vaccinations, na.rm = T),
            ppl_once = max(people_vaccinated, na.rm = T),
            fully_vax = max(people_fully_vaccinated, na.rm = T)) |> 
  mutate(across(where(is.numeric), scales::comma))


# total vaccines administered
df_vax_total <- df_vax |> 
  select(location, iso_code, total_vaccinations) |> 
  group_by(location) |> 
  filter(total_vaccinations == max(total_vaccinations, na.rm = T), location != "World") 

# figure out how to use iso code to create leaflet maps
# https://www.r-graph-gallery.com/183-choropleth-map-with-leaflet.html

# Plotly choroplet map
# https://plotly.com/r/choropleth-maps/
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'Mercator')
)

# add trace
vax_map <- plot_geo(df_vax_total)
vax_map <- vax_map  |> 
  add_trace(
  z = ~total_vaccinations,
  color = ~total_vaccinations,
  colors = 'Greens',
  text = ~location,
  locations = ~iso_code,
  marker = list(line = l)
)

# add title to colorbar
vax_map <- vax_map  |> 
  colorbar(title = 'Total vaccinations (M)')

# add title and subtitle
vax_map <- vax_map  |> 
  layout(title = 'Total COVID-19 Vaccination Doses Administered<br>Source:<a href="https://github.com/owid/covid-19-data/tree/master/public/data/vaccinations">Our World in Data</a>', geo = g)

vax_map


# Top 20
# define 20 colors with colorRampPalette
ncountry_vaccination <- 20 
country_color_vaccination <- colorRampPalette(brewer.pal(10, "RdYlGn"))(ncountry_vaccination)


# for limits of graph
max = arrange(df_vax_total, -total_vaccinations)$total_vaccinations[1]

vax_total <- df_vax_total  |> 
  arrange(-total_vaccinations) |> 
  ungroup() |> 
  slice(1:20) |> 
  mutate(location = fct_reorder(location, total_vaccinations))


plot_ly(data = vax_total, 
        x = ~location, 
        y = ~total_vaccinations, 
        type = "bar", 
        marker = list(color = country_color_vaccination)) |> 
  layout(title = "", 
         xaxis = list(title = "Country", categoryorder = "total descending"), 
         yaxis = list(title = "Total Vaccinations"))


