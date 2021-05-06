library(shiny)
library(shinythemes)      # Bootswatch color themes for shiny
library(data.table)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(choroplethr)      # Creating Choropleth Maps in R
library(choroplethrMaps)  # Maps used by the choroplethr package
library(choroplethrZip)
library(purrr)


# Initialization

# Raw Data
educ <- read.csv("ACSST5Y2018.S1501_data_with_overlays_2021-04-22T200644.csv")
soi_data <- fread("https://www.irs.gov/pub/irs-soi/18zpallagi.csv")
race_raw <- read.csv("https://data.diversitydatakids.org/files/Demographics/01001_12_C/01001_12_C_860_5/01001_12_C_860_5.csv")

zip_cca <- usa::zipcodes %>%
    filter(city == "Chicago") %>%
    dplyr::select(zip)

# Modifications

# Racial Data:
tempt4 <- race_raw %>%
    mutate(geoid = stringr::str_split(geoid, "US") %>% 
               map_chr(., 2)) %>%
    rename(zipcode = geoid) %>%
    dplyr::select(-contains("se")) %>%
    dplyr::select(zipcode, 
                  year, 
                  total_est, 
                  aian_est, 
                  api_est, 
                  asian_est, 
                  black_est, 
                  hisp_est, 
                  nhopi_est, 
                  nhwhite_est, 
                  other_est) %>%
    filter(zipcode %in% zip_cca$zip,
           year == "2013-2017")

# SOI:
soi_chicago1 <- soi_data %>%
    filter(zipcode %in% zip_cca$zip,
           A00100 > 0,
           A18500 > 0,
           A00200 > 0) %>%
    mutate(zipcode = as.character(zipcode)) %>%
    group_by(zipcode) %>% 
    summarise(total_income = sum(A00100),
              total_real_estate = sum(A18500),
              total_wage = sum(A00200)) %>%
    left_join(tempt4) %>%
    mutate(average_income = total_income/total_est,
           average_real_estate = total_real_estate/total_est,
           average_wage = total_wage/total_est,
           prop_nhwhite = (nhwhite_est/ total_est),
           prop_black = (black_est / total_est),
           prop_aian = aian_est / total_est,
           prop_asian = asian_est / total_est,
           prop_hist = hisp_est / total_est)

# Chicago:
chicago_educ1 <- educ %>%
    dplyr::select(-Geographic.Area.Name,
                  -starts_with("Margin"),
                  -contains(c("percent", "male"))) %>%
    mutate(zipcode = stringr::str_split(id, "US") %>% map_chr(., 2)) %>%
    filter(zipcode %in% zip_cca$zip) %>%
    rename_with(~ gsub(".", " ", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("  ", " ", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("Estimate Total", "", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("Population", "", .x, fixed = TRUE)) %>%
    rename_with(~ gsub("years", "", .x, fixed = TRUE)) %>%
    mutate(hs = `  25  and over High school graduate or higher`/ `  25  and over`,
           co = `  25  and over Bachelor s degree or higher` / `  25  and over`) %>%
    dplyr::select(zipcode, `  25  and over`, hs, co)

# Combined:
combined_chicago <- chicago_educ1 %>%
    left_join(soi_chicago1)


# UI Definition
ui <- fluidPage(
    
    title = "Chicago Map by Zipcode",
    theme = shinythemes::shinytheme("flatly"),
    
    sidebarLayout(
        
        sidebarPanel(
            width = 3,
            selectInput("select",
                        label = "choose variable of interest",
                        choices = c("Percent White", 
                                    "Percent Black", 
                                    "Percent Hispanic", 
                                    "Percent Asian",
                                    "Average Income", 
                                    "Average Wage",
                                    "Average Property Tax", 
                                    "Percentage Highschool or more",
                                    "Percentage College or more"))
        ),
        
        mainPanel(width = 9,
                  tabsetPanel(
                      tabPanel(title = "Output Map 1",
                               plotOutput(outputId = "map1"))
                  )
                  
                  
        )
    )
)

# Define server logic required to draw interactive map
server <- function(input, output, session) {
    
    output$map1 <- renderPlot({
        data <- switch(input$select, 
                       "Percent White" = combined_chicago$prop_nhwhite,
                       "Percent Black" = combined_chicago$prop_black,
                       "Percent Hispanic" = combined_chicago$prop_hist,
                       "Percent Asian" = combined_chicago$prop_asian,
                       "Average Income" = combined_chicago$average_income,
                       "Average Wage" = combined_chicago$average_wage,
                       "Average Property Tax" = combined_chicago$average_real_estate,
                       "Percentage Highschool or more" = combined_chicago$hs,
                       "Percentage College or more" = combined_chicago$hs)
        
        region <- list(combined_chicago$zipcode)
        value <- list(data)
        
        temp_map <- data.frame(region, value)
        colnames(temp_map) <- c("region", "value")
            
        
        choro = ZipChoropleth$new(temp_map)
        choro$title = "Map of Chicago"
        choro$set_zoom_zip(zip_zoom = temp_map$region, 
                           county_zoom = NULL, 
                           msa_zoom = NULL, 
                           state_zoom = NULL)
        
        choro$render()
        
        
    })
}



# Run the application 
shinyApp(ui = ui, server = server)

