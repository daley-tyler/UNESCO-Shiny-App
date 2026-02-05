#App File
#App Link: https://statylerdaley.shinyapps.io/UNESCO-Shiny-App/
library(tidyverse)
library(sf)
library(shiny)
library(leaflet)
library(shinythemes)

load("unesco_heritage_data.RData")

#Code for layout, tabs, and inputs
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("UNESCO vs Non-UNESCO Heritage Sites"),
  tabsetPanel(
    tabPanel("Read Me",
      h2("Project Overview"),
      p("This is my Shiny application that allows users to explore and compare heritage and historic sites that are recognized by UNESCO as World Heritage Sites versus other significant sites that are not designated by UNESCO. The goal is to reveal geographic patterns and potential biases in UNESCO's selection process for these sites and to highlight important historic/heritage sites around the world, especially in regions under represented on the UNESCO list."),
      h3("Data Sources"),
      p("The UNESCO World Heritage Sites dataset comes from whc001.shp and includes site name, category, year of inscription, country, region, and area in hectares. The non-UNESCO heritage sites come from MapCruzin points-of-interest shapefiles for Africa, Asia/Middle East, Oceania, and South America, filtered to museums, archaeological sites, monuments, memorials, ruins, and attractions."),
      p("Due to the non-UNESCO data not including country or year information, comparisons by country and year focus on the UNESCO data. The app primarily works to compare counts and distributions at the regional level."),
      h3("How to Use This App"),
      tags$ul(
        tags$li(strong("Map tab:"), " Explore using an interactive world map of heritage sites. Use the sidebar to choose regions and site types, and to toggle UNESCO and non-UNESCO layers (Note: Due to the amount of points in Asia, you may have to deselect Asia when turning on the non-UNESCO layer."),
        tags$li(strong("Bar Plot tab:"), " Compare counts of sites by region or country. Use the controls to change the summary."),
        tags$li(strong("Distribution tab:"), " Explore the distributions of UNESCO site attributes such as year of or area in hectares across different regions.")),
      h3("Key Insights"),
      p("UNESCO World Heritage Sites are concentrated in Europe and North America, while regions such as Africa, Asia/Middle East, Oceania, and South America have many heritage sites that are not represented on the World Heritage list. This app allows you to explore that imbalance for yourself in an interactive manner."),
      h3("Sources"),
      tags$ul(
        tags$li("https://data.unesco.org/explore/dataset/whc001/export/?sort=date_inscribed"),
        tags$li("https://mapcruzin.com/download-shapefile/")
      )
    ),
    tabPanel("Map",
      sidebarLayout(
        sidebarPanel(
          checkboxGroupInput("regionSelect", "Regions/Continents:",
            choices = c("Europe/North America", "Asia/Middle East", "Africa", "Oceania", "South America"),
            selected = c("Africa", "Asia/Middle East", "Oceania", "South America", "Europe/North America")
          ),
          checkboxGroupInput("unescoType", "UNESCO Site Category:",
            choices = c("Cultural", "Natural", "Mixed"),
            selected = c("Cultural", "Natural", "Mixed")
          ),
          checkboxGroupInput("nonType", "Non-UNESCO Site Type:",
            choices = c("Museum" = "museum", "Archaeological Site" = "archaeological_site", "Monument" = "monument", "Memorial" = "memorial", "Ruins" = "ruins", "Attraction" = "attraction"),
            selected = c("museum", "archaeological_site", "monument", "memorial", "ruins", "attraction")
          ),
          checkboxInput("toggleUNESCO", "Show UNESCO Sites", value = TRUE),
          checkboxInput("toggleNon", "Show Non-UNESCO Sites", value = FALSE)
        ),
        mainPanel(leafletOutput("map", height = "600px"))
      )
    ),
    tabPanel("Bar Plot",
      sidebarLayout(
        sidebarPanel(
          radioButtons("aggLevel", "Aggregate by:",
            choices = c("Region", "Country"),
            selected = "Region"
          ),
          checkboxGroupInput("regionFilter", "Select Regions:",
            choices = c("Europe/North America", "Asia/Middle East", "Africa", "Oceania", "South America"),
            selected = c("Europe/North America", "Asia/Middle East", "Africa", "Oceania", "South America")
          )
        ),
        mainPanel(plotOutput("barPlot", height = "500px"))
      )
    ),
    tabPanel("Distribution",
      sidebarLayout(
        sidebarPanel(
          selectInput("numericVar", "Numeric Variable:",
            choices = c("Year Inscribed", "Area (hectares)"),
            selected = "Year Inscribed"
          ),
          checkboxGroupInput("distRegionFilter", "Regions to Include:",
            choices = c("Europe/North America", "Asia/Middle East", "Africa", "Oceania", "South America"),
            selected = c("Europe/North America", "Asia/Middle East", "Africa", "Oceania", "South America")
          )
        ),
        mainPanel(plotOutput("distPlot", height = "500px"))
      )
    )
  )
)

# Server code with plots
server <- function(input, output) {
  UNESCO_filtered <- reactive({
    all_sites_df %>%
      filter(
        is_unesco == "UNESCO",
        region_group %in% input$regionSelect,
        category %in% input$unescoType,
        !is.na(lon),
        !is.na(lat)
      )
 })
  
 nonUNESCO_filtered <- reactive({
    all_sites_df %>%
      filter(
        is_unesco == "Non-UNESCO",
        region_group %in% input$regionSelect,
        type %in% input$nonType,
        !is.na(lon),
        !is.na(lat)
      )
 })
# Leaflet map
 output$map <- renderLeaflet({
  m <- leaflet() %>%
    addTiles()
    
    if (input$toggleUNESCO) {
      uf <- UNESCO_filtered()
      if (nrow(uf) > 0) {
        m <- m %>%
          addCircleMarkers(
            lng = uf$lon,
            lat = uf$lat,
            color = "#2c3e50",
            radius = 6,
            stroke = FALSE,
            fillOpacity = 0.85,
            popup = paste0(
              "<b>", uf$name, "</b><br>",
              "Category: ", uf$category, "<br>",
              "Country: ", uf$country, "<br>",
              "(UNESCO Site)"
            )
          )
      }
    }
    
    if (input$toggleNon) {
      nf <- nonUNESCO_filtered()
      if (nrow(nf) > 0) {
        m <- m %>%
          addCircleMarkers(
            lng = nf$lon,
            lat = nf$lat,
            color = "#e67e22",
            radius = 4,
            stroke = FALSE,
            fillOpacity = 0.7,
            popup = paste0(
              "<b>", nf$name, "</b><br>",
              "Type: ", str_to_title(str_replace_all(nf$type, "_", " ")), "<br>",
              "Region: ", nf$continent, "<br>",
              "(Non-UNESCO Site)"
            )
          )
      }
    }
    m
  })
# Bar plot by region/country  
 output$barPlot <- renderPlot({
  data_filt <- all_sites_df %>%
    filter(region_group %in% input$regionFilter)
    
    if (input$aggLevel == "Region") {
      summary_df <- data_filt %>%
        group_by(region_group, is_unesco) %>%
        summarise(count = n(), .groups = "drop")
      
      ggplot(summary_df, aes(x = region_group, y = count, fill = is_unesco)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(x = "Region", y = "Number of Sites", title = "Heritage Sites by Region (UNESCO vs Non-UNESCO)", fill = "Site Type") +
        scale_fill_manual(values = c("UNESCO" = "#18bc9c", "Non-UNESCO" = "#e67e22"), drop = TRUE) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), text = element_text(size = 12))
    } else {
      summary_df <- data_filt %>%
        filter(is_unesco == "UNESCO") %>%
        group_by(country) %>%
        summarise(count = n(), .groups = "drop") %>%
        arrange(desc(count)) %>%
        head(15)
      
      ggplot(summary_df, aes(x = reorder(country, count), y = count)) +
        geom_bar(stat = "identity", fill = "#18bc9c") +
        coord_flip() +
        labs(x = "Country", y = "Number of UNESCO Sites", title = "Top 15 Countries by UNESCO World Heritage Sites") +
        theme_minimal() +
        theme(text = element_text(size = 12))
    }
 })

# Distribution plot for showing year/area
output$distPlot <- renderPlot({
  data_filt <- all_sites_df %>%
    filter(region_group %in% input$distRegionFilter, is_unesco == "UNESCO")
    
    if (input$numericVar == "Year Inscribed") {
      data_year <- data_filt %>%
        filter(!is.na(year_inscribed))
      
      ggplot(data_year, aes(x = year_inscribed)) +
        geom_histogram(binwidth = 5, fill = "#3498db", color = "white") +
        labs(x = "Year Inscribed", y = "Number of UNESCO Sites", title = "Distribution of UNESCO Site Inscription Years") +
        theme_minimal() +
        theme(text = element_text(size = 12))
    } else {
      data_area <- data_filt %>%
        filter(!is.na(area_hectares), area_hectares > 0)
      
      ggplot(data_area, aes(x = area_hectares)) +
        geom_histogram(bins = 30, fill = "#18bc9c", color = "white") +
        scale_x_log10() +
        labs(x = "Area (hectares, log10 scale)", y = "Number of UNESCO Sites", title = "Distribution of UNESCO Site Areas (hectares)") +
        theme_minimal() +
        theme(text = element_text(size = 12))
    }
  })
}

shinyApp(ui = ui, server = server)