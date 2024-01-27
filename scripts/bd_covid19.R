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

bd_coronavirus <- coronavirus |> 
  filter(country == "Bangladesh")


# data prep for daily cases 
bd_coronavirus_daily <- bd_coronavirus |> 
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





# for leaflet map
df_mapbd_coronavirus <- bd_coronavirus  |> 
  filter(cases > 0)  |> 
  group_by(lat, long, type)  |> 
  summarise(cases = sum(cases), .groups = "drop")  |> 
  mutate(log_cases = 2 * log(cases))  |> 
  ungroup()

df_mapbd_coronavirus.split <- df_mapbd_coronavirus  |> 
  split(df_mapbd_coronavirus$type)


# Color pallete
pal <- colorFactor(c("grey", "red", "green"),
                   domain = c("confirmed", "death", "recovered"))



map_object <- leaflet()  |> 
  addProviderTiles(providers$CartoDB.DarkMatter) |> 
  setView(41.405559, 0.247590, zoom = 2)

names(df_mapbd_coronavirus.split) |> 
  walk(function(bd_coronavirus) {
    map_object <<- map_object |> 
      addCircleMarkers(data = df_mapbd_coronavirus.split[[bd_coronavirus]],
                       lng = ~long, lat = ~lat,
                       color = ~pal(type),
                       stroke = FALSE,
                       fillOpacity = 0.5,
                       radius = ~log_cases,
                       popup =  popupTable(df_mapbd_coronavirus.split[[bd_coronavirus]],
                                           feature.id = FALSE,
                                           row.numbers = FALSE,
                                           zcol=c("type","cases")),
                       group = bd_coronavirus,
                       labelOptions = labelOptions(noHide = F,
                                                   direction = 'auto'))
  })

map_object |> 
  addLayersControl(
    overlayGroups = names(df_mapbd_coronavirus.split),
    options = layersControlOptions(collapsed = FALSE)) |> 
  hideGroup(c("confirmed", "recovered"))


# Daily Cumulative Cases
plot_ly(
  data = bd_coronavirus_daily, 
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
         hovermode = "compare"