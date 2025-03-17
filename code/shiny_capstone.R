library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(sf)
library(rio)
library(here)
# Load Data
df1 <- import(here("data/corp_76to20_bystate_MGyearrecoded.xlsx"))
df2 <- import(here("data/suspensions_72to21_bystate_MGyearrecoded.xlsx"))
df3 <- import(here("data/treatment dataset_2year.xlsx"))

# Filter for states adopting bans in 2020+
df_corp <- df3 %>%
  filter(YEAR > 2020) %>%
  group_by(STATE_CODE)

# Convert US states to an sf object
us_states_sf <- st_as_sf(maps::map("state", fill = TRUE, plot = FALSE))

# Fix column names for merging
us_states_sf <- us_states_sf %>%
  rename(region = ID) %>%
  mutate(region = tolower(region))

# Create state lookup for abbreviations
state_lookup <- data.frame(STATE_CODE = state.abb, region = tolower(state.name))

# Merge df_corp with full state names
df_corp <- left_join(df_corp, state_lookup, by = "STATE_CODE")

# Fix DC missing in state mapping
df_corp$region <- if_else(is.na(df_corp$region), "district of columbia", df_corp$region)

# Merge with map data
us_states_sf <- left_join(us_states_sf, df_corp, by = "region")

# Define color palette
library(jsonlite)

# Old code (causes warning)
color_palette <- c("0" = "#e5de54", "1" = "#808c73")
toJSON(color_palette)  # ⚠ Warning occurs here

# ✅ Fixed: Convert named vector to named list
color_palette_list <- as.list(color_palette)

# Now safely convert to JSON
toJSON(color_palette_list)
# Define UI
ui <- fluidPage(
  titlePanel("Racial Disparities in School Discipline"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_state", "Select a State:", 
                  choices = unique(us_states_sf$region),
                  selected = "alabama"),
      hr(),
      helpText("Select a state to view its suspension trends over time.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Static Map", plotOutput("static_map")),
        tabPanel("Interactive Map", leafletOutput("map", height = 600)),
        tabPanel("State Timeline", plotOutput("state_plot", height = 500))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Render Static ggplot2 Map
  output$static_map <- renderPlot({
    ggplot() +
      geom_sf(data = us_states_sf, aes(fill = as.factor(DD)), color = "white", size = 0.3) +
      scale_fill_manual(values = color_palette, labels = c("Not Adopted", "Adopted")) +
      theme_minimal() +
      labs(
        title = "Corporal Punishment Ban Adoption by State",
        subtitle = "States that adopted bans starting in 2020",
        fill = "Ban Status"
      ) +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()
      )
  })
  
  # Render Interactive Leaflet Map
  output$map <- renderLeaflet({
    leaflet(us_states_sf) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~color_palette[as.character(DD)],
        color = "white",
        weight = 1,
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
        label = ~paste0(region, ": ", ifelse(DD == 1, "Adopted", "Not Adopted")),
        layerId = ~region
      )
  })
  
  # Observe Click on Leaflet Map & Update State Selection
  observeEvent(input$map_shape_click, {
    selected_region <- input$map_shape_click$id
    updateSelectInput(session, "selected_state", selected = selected_region)
  })
  
  # Render State-Specific Timeline
  output$state_plot <- renderPlot({
    
    # Filter data for selected state
    state_data <- df2 %>% filter(region == input$selected_state)
    
    ggplot(state_data, aes(x = YEAR, y = SuspensionRate, group = Race, color = Race)) +
      geom_line(alpha = 0.6) + 
      geom_smooth(method = "loess", se = FALSE, span = 0.7) +
      scale_color_manual(values = c(
        "AI" = "#bdbfbf",  # light_gray
        "BL" = "#808c73",  # sage
        "WH" = "#303333"   # dark_gray
      )) +
      theme_minimal() +
      labs(
        title = paste("Suspension Rates in", input$selected_state),
        x = "Year",
        y = "Suspension Rates (%)"
      )
  })
}

# Run App
shinyApp(ui, server)


