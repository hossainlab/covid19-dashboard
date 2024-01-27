# Install Packages
install.packages(c("lubridate", "plotly", "RColorBrewer", "tidyverse","DT", "glue", "forcats", "ggpubr", "leaflet", "leafpop"))
install.packages("coronavirus")
remotes::install_github("covid19r/covid19r")


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



# Data 
data("coronavirus")
head(coronavirus)

# data prep for daily cases 
df_daily <- coronavirus |> 
  group_by(date, type) |> 
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = type, 
              values_from = total) |> 
  arrange(date) |> 
  ungroup() |> 
  mutate(active = confirmed - death - recovery) |> 
  mutate(confirmed_cum = cumsum(confirmed), 
         death_cum = cumsum(death),
         recovered_cum = cumsum(recovery),
         active_cum = cumsum(active))


# covid19 cases by country 
df_country <- coronavirus |> 
  group_by(country, type) |> 
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |> 
  pivot_wider(names_from = type, 
              values_from = total) |> 
  mutate(active = confirmed - death - recovery) |> 
  pivot_longer(cols = -country, 
               names_to = "type", 
               values_to = "total")



# for global statistics
df_world <- coronavirus |> 
  group_by(type) |> 
  summarise(total = sum(cases, na.rm = T), .groups = "drop") |>
  pivot_wider(names_from = type, 
              values_from = total) |> 
  mutate(active = confirmed - death - recovery)


# for leaflet map
df_map <- coronavirus  |> 
  filter(cases > 0)  |> 
  group_by(country, province, lat, long, type)  |> 
  summarise(cases = sum(cases), .groups = "drop")  |> 
  mutate(log_cases = 2 * log(cases))  |> 
  ungroup()

df_map.split <- df_map  |> 
  split(df_map$type)


# Color pallete
pal <- colorFactor(c("grey", "red", "green"),
                   domain = c("confirmed", "death", "recovered"))



map_object <- leaflet()  |> 
  addProviderTiles(providers$CartoDB.DarkMatter) |> 
  setView(41.405559, 0.247590, zoom = 2)

names(df_map.split) |> 
  walk(function(coronavirus) {
    map_object <<- map_object |> 
      addCircleMarkers(data = df_map.split[[coronavirus]],
                       lng = ~long, lat = ~lat,
                       color = ~pal(type),
                       stroke = FALSE,
                       fillOpacity = 0.5,
                       radius = ~log_cases,
                       popup =  popupTable(df_map.split[[coronavirus]],
                                           feature.id = FALSE,
                                           row.numbers = FALSE,
                                           zcol=c("type","cases","country","province")),
                       group = coronavirus,
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto'))
  })

map_object |> 
  addLayersControl(
    overlayGroups = names(df_map.split),
    options = layersControlOptions(collapsed = FALSE)) |> 
  hideGroup(c("confirmed", "recovered"))


# Daily Cumulative Cases
plot_ly(
  data = df_daily, 
  x = ~date, 
  y = ~active_cum, 
  name = "Active", 
  fillcolor = "blue", 
  type = "scatter", 
  mode = "none", 
  stackgroup = "one") |> 
  add_trace(y = ~confirmed_cum, 
            name = "Recovered", 
            fillcolor = "green") |> 
  add_trace(y = ~death_cum, 
            name = "Death", 
            fillcolor = "red") |> 
  layout(title = "", 
         xaxis = list(title = "Date", type = "date"), 
         yaxis = list(title = "Cumulative Number of Cases"), 
         legend = list(x = 0.1, y = 0.9), 
         hovermode = "compare")


plot_ly(data = df_daily, 
        x = ~date, 
        y = ~confirmed, 
        type = "histogram")

names(df_daily)
