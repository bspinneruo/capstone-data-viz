library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(sf)
library(rio)
library(here)
library(tidyverse)

# Load Data
df1 <- import(here::here("data/corp_76to20_bystate_MGyearrecoded.xlsx"))
df2 <- import(here::here("data/suspensions_72to21_bystate_MGyearrecoded.xlsx"))
df3 <- import(here::here("data/treatment dataset_2year.xlsx"))

# ✅ Register Seaford Font
seaford_font_path <- here::here("fonts/Seaford.ttf")
if (file.exists(seaford_font_path)) {
  font_add("Seaford", seaford_font_path)
  showtext_auto()
}

# Filter for states adopting bans in 2020+
df_corp <- df3 %>%
  filter(YEAR > 2020) %>%
  group_by(STATE_CODE)

# Convert US states to an sf object
us_states_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE)) %>%
  rename(region = ID) %>%
  mutate(region = tolower(region)) %>%
  st_transform(crs = 4326)  # Convert CRS to WGS84

state_lookup <- data.frame(STATE_CODE = state.abb, region = tolower(state.name))

# Merge df_corp with full state names
df_corp_join <- left_join(df_corp, state_lookup, by = "STATE_CODE")

# Fix DC missing in state mapping
df_corp_join$region <- if_else(is.na(df_corp_join$region), "district of columbia", df_corp_join$region)

# Merge with map data
df_join <- left_join(us_states_sf, df_corp_join, by = "region")

# Define color palette
color_palette <- list("0" = "#e5de54", "1" = "#808c73")

brand_colors <- list(
  dark_gray = "#303333",
  sage = "#808c73",
  light_gray = "#bdbfbf"
)

# #===============================
# # Brand Theme Definition
# #===============================
theme_brand <- function(base_size = 12) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      # Text elements
      text = element_text(family = "Seaford"),  # Seaford
      plot.title = element_text(
        size = rel(1.2),
        face = "bold"
      ),
      plot.subtitle = element_text(
        size = rel(1)
      ),
      axis.title = element_text(
        face = "bold"
      ),
      
      # Grid elements
      panel.grid.major = element_line(color = brand_colors$light_gray),
      panel.grid.minor = element_blank(),
      
      # Legend
      legend.position = "bottom"
    )
}



# ✅ Custom Styling for UI
custom_css <- "
.navbar-default {
  background-color: #e5de54 !important;
  border-color: #e5de54;
}
.navbar-default .navbar-brand, .navbar-default .navbar-nav > li > a {
  color: #303333 !important;
  font-family: 'Seaford', sans-serif;
}
h1, h2, h3, h4, h5, h6, p, label {
  font-family: 'Seaford', sans-serif;
  color: #303333;
}
"

# UI
ui <- fluidPage(
  titlePanel("Racial Disparities in School Discipline: Explorer Dashboard"),
  
  tabsetPanel(
    
    # 📌 Main Page (Static Map)
    tabPanel("Dashboard", 
             leafletOutput("static_map", height = 600)
    ),
    
    # 📌 Suspension Rates by State
    tabPanel("Suspension Rates by State", 
             leafletOutput("susp_map", height = 600),
             plotlyOutput("susp_plot", height = 400)
    ),
    
    # 📌 Corporal Punishment Rates by State
    tabPanel("Corporal Punishment Rates by State", 
             leafletOutput("corp_map", height = 600),
             plotlyOutput("corp_plot", height = 400)
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # 📌 Static Map
  output$static_map <- renderPlot({
    ggplot() +
      geom_sf(data = df_join, aes(fill = as.factor(DD)), color = "white", size = 0.3) +
      scale_fill_manual(values = color_palette, labels = c("Not Adopted", "Adopted")) +
      theme_brand() +
      labs(
        title = "Corporal Punishment Ban Adoption by State",
        subtitle = "States that adopted bans starting in 2020",
        fill = "Ban Status"
      ) +
      theme_minimal()
  })

  
  # 📌 Interactive Suspension Rates Map
  
  
  # Fix DC missing in state mapping
  df_corp2 <- left_join(df2, state_lookup, by = "STATE_CODE")
  df_corp2$region <- if_else(is.na(df_corp2$region), "district of columbia", df_corp2$region)
  
  # Merge with map data
  int_data <- left_join(us_states_sf, df_corp2, by = "region")
  ## Combined all -- us_states_sf2
  
  ### Pivot race wider 
  
  int_data <- int_data %>%
    mutate(across(c(pct_, OSS_), ~ replace_na(.x, 0))) %>%  # Replace NA with 0 in selected columns
    pivot_wider(
      names_from = race,   # Convert race categories into column names
      values_from = pct_,  # Values to fill in new columns
      values_fill = list(pct_ = 0)) # replace na's with 0s
  
  long_data <- int_data %>%
    pivot_longer(cols = c(AI, AS, BL, HI, HP, MR, WH, total), names_to = "race", values_to = "pct_") %>% 
    group_by(YEAR, race, STATE_CODE)
  
  output$susp_map <- renderLeaflet({
    leaflet(int_data, options = leafletOptions(zoomControl = FALSE, dragging = FALSE)) %>%
      
      addTiles() %>%
      
      # ✅ Set initial view to focus on the U.S.
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%  # Center at U.S. geographic center
      
      # ✅ Restrict users from moving outside the U.S.
      fitBounds(lng1 = -125, lat1 = 25, lng2 = -66, lat2 = 50) %>%
      
      addPolygons(
        fillColor = ~colorNumeric(
          palette = colorRampPalette(c( "#303333", "#e0dfda", "#f0efed"))(100),
          domain = int_data$total
        )(total),
        color = "white",
        weight = 1,
        fillOpacity = .8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
        label = ~paste0(region, ": ", round(total, 2), "% Avg. Overall Suspended"),
        layerId = ~STATE_CODE  # Use STATE_CODE instead of region
      )
  })
  
  
  # 📌 Interactive Corporal Punishment Map
  cp <- df1 %>%
    mutate(
      ENR_ = as.numeric(ENR_),  
      CORP_ = as.numeric(CORP_)
    )
  
  # Merge df_corp with full state names
  cp <- left_join(cp, state_lookup, by = "STATE_CODE")
  
  # Fix DC missing in state mapping
  cp$region <- if_else(is.na(cp$region), "district of columbia", cp$region)
  
  # Merge with map data
  cp <- left_join(us_states_sf, cp, by = "region")
  ## Combined all -- us_states_sf2
  
  # Calculate corporal punishment rate as a percentage of enrollment
  df_trend <- cp %>%
    mutate(Corporal_Rate = pmax(CORP_ / ENR_) * 100) %>%
    replace_na(list(Corporal_Rate = 0))  # Replace NaN values with 0
  
 wide_trend <- df_trend %>%
    pivot_wider(
      names_from = race,   # Convert race categories into column names
      values_from = Corporal_Rate,  # Values to fill in new columns
      values_fill = list(Corporal_Rate = 0)) # replace na's with 0s
  
wide_trend2 <- wide_trend %>%
    pivot_longer(cols = c(AI, AS, BL, HI, HP, MR, WH, sumrace), names_to = "race", values_to = "Corporal_Rate") %>% 
    group_by(YEAR, race, STATE_CODE)
  
  output$corp_map <- renderLeaflet({
    leaflet(wide_trend) %>%
      addTiles() %>%
      setView(lng = -98.5795, lat = 39.8283, zoom = 4) %>%
      addPolygons(
        fillColor = ~colorNumeric(
          palette = colorRampPalette(c( "#303333", "#e0dfda", "#f0efed"))(100),
          domain = wide_trend$sumrace
        )(sumrace),
        color = "white",
        weight = 1,
        fillOpacity = .8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
        label = ~paste0(region, ": ", round(sumrace, 2), "% Avg. Overall Suspended"),,
        layerId = ~STATE_CODE
      )
  })
  
  # 📌 Observe Clicks on Suspension Map
  selected_state_susp <- reactiveVal(NULL)
  observeEvent(input$susp_map_shape_click, {
    selected_state_susp(input$susp_map_shape_click$id)
  })
  
  # 📌 Observe Clicks on Corporal Punishment Map
  selected_state_corp <- reactiveVal(NULL)
  observeEvent(input$corp_map_shape_click, {
    selected_state_corp(input$corp_map_shape_click$id)
  })
  
  # 📌 Suspension Rate Line Plot
  output$state_plot <- renderPlotly({
    req(selected_state_susp())
    
    # Filter data for selected state
    state_data <- long_data %>% filter(STATE_CODE == selected_state_susp())  # Match with STATE_CODE
    
    # Create smoothed data
    smoothed_data <- state_data %>%
      group_by(race) %>%
      mutate(Smoothed = pmax(0, predict(loess(pct_ ~ YEAR, span = 0.3))))

    
    # Define custom colors
    race_colors <- c(
      "AI" = "#bdbfbf",  # American Indian
      "AS" = "#808c73",  # Asian
      "BL" = "#677812",  # Black
      "HI" = "#e5de54",  # Hispanic
      "HP" = "#a1a1a1",  # Pacific Islander
      "MR" = "#303333",  # Multiracial
      "WH" = "#0c4d1d"  # White
    )
    
    
    # Create interactive Plotly line chart
    plot_ly(
      data = state_data, 
      x = ~YEAR, 
      y = ~Smoothed, 
      color = ~race, 
      colors = race_colors
      # type = 'scatter', 
      # mode = 'lines'
    ) %>%
      add_trace(
        data = smoothed_data, 
        x = ~YEAR, 
        y = ~Smoothed, 
        color = ~race, 
        colors = race_colors,
        type = 'scatter', 
        mode = 'lines',
        showlegend = TRUE  # Hide duplicate legend entries
      ) %>%
      layout(
        title = paste("Suspension Rates in", selected_state()),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Suspension Rate (%)"),
        legend = list(title = list(text = "Race"),
                      hovermode = "x unified")
      )
  })
  
  
  # 📌 Corporal Punishment Line Plot
  
  output$corp_plot <- renderPlotly({
    
    race_colors <- c(
      "AI" = "#bdbfbf",  # American Indian
      "AS" = "#808c73",  # Asian
      "BL" = "#677812",  # Black
      "HI" = "#e5de54",  # Hispanic
      "HP" = "#a1a1a1",  # Pacific Islander
      "MR" = "#303333",  # Multiracial
      "WH" = "#0c4d1d"  # White
    )
    
    
    req(selected_state_corp())
  plot_ly(
    data = wide_trend, 
    x = ~YEAR, 
    y = ~Corporal_Rate, 
    color = ~race, 
    colors = race_colors
    # type = 'scatter', 
    # mode = 'lines'
  ) %>%
    add_trace(
      data = df_trend, 
      x = ~YEAR, 
      y = ~Corporal_Rate, 
      color = ~race, 
      colors = race_colors,
      type = 'scatter', 
      mode = 'lines',
      showlegend = TRUE  # Hide duplicate legend entries
    ) %>%
    layout(
      title = paste("Corporal Punishment Rates Over Time in", selected_state()),
      xaxis = list(title = "Year"),
      yaxis = list(title = "Corporal Punishment Rate (%)"),
      legend = list(title = list(text = "Race"),
                    hovermode = "x unified")
    )
})
}

# Run the application 
shinyApp(ui = ui, server = server)

