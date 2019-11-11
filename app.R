#Loading necessary packages
library(readxl)
library(plotly)
library(dplyr)
library(DT)
library(gdata)
library(data.table)
library(ggplot2)
library(dplyr)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(leaflet)
library(shiny)
library(forcats)

#Preprocessing Data
crime_vic <- read_excel("crimeVic.xlsx")

setnames(crime_vic, "Year ending September", "Year")
setnames(crime_vic, 'Local Government Area', "Local_Government_Area")
setnames(crime_vic, "Offence Division", "Offence_Division")
setnames(crime_vic, "Offence Subdivision", "Offence_Subdivision")
setnames(crime_vic, "Offence Subgroup", "Offence_Subgroup")
setnames(crime_vic, "Incidents Recorded", "Incidents_Recorded")
setnames(crime_vic, "Rate per 100,000 population", "Rate_per_100k_population")

crime_vic$Offence_Division <- as.factor(crime_vic$Offence_Division)

crime_vic$Offence_Subdivision <- as.factor(crime_vic$Offence_Subdivision)

crime_vic$Offence_Subgroup <- as.factor(crime_vic$Offence_Subgroup)

geoInfo <- unique(crime_vic[,2])

geoInfo$lga <- unique(crime_vic$Local_Government_Area)

loc<- (unique(crime_vic$Local_Government_Area))

loc <- paste(loc,"Victoria,Australia", sep=",")

register_google(key ="AIzaSyBoZtf99Xk0Pp-z_rrx5o-BgU__WlYIW6A")

geocode <- geocode(loc)

final_geo <- cbind(geoInfo,geocode)

finalVic <- inner_join(crime_vic,final_geo,by='Local_Government_Area',copy=FALSE)

res2 <- finalVic %>% group_by(Local_Government_Area, Offence_Division) %>% summarise(Incidents = sum(Incidents_Recorded))

res3 <- finalVic %>% group_by(Local_Government_Area, Offence_Division) %>% summarise(Rate = mean(Rate_per_100k_population))

res3$Rate <- round(res3$Rate)

final_res <- inner_join(res2,res3,by = c('Local_Government_Area', 'Offence_Division'),copy=FALSE)

finalPlot_division <- inner_join(final_geo,final_res,by='Local_Government_Area',copy=FALSE)


# Define UI for the application
ui <- fluidPage(

    titlePanel(h2("Crime Stats Victoria 2008 - 2017", align = "center")),
    
    sidebarLayout(
        
        sidebarPanel(
            
            sliderInput("Year",
                        label = "Year Range",
                        min =  2008,
                        max = 2017,
                        step = 1,
                        value = c(2008,2017),
                        sep = ""),
            uiOutput("selectRegion"),
            includeMarkdown("ABOUT.md")
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Map", leafletOutput("map")),
                tabPanel("Line Plot", plotlyOutput("linePlot"),
                         DT::dataTableOutput("SelectedData"))
                
            )
        )
        
    )
    

)

# Define server logic required to draw the plots
server <- function(input, output) {

    output$selectRegion <- renderUI({
        
        selectInput("regionInput","Select Local Government Area:",
                    sort(unique(crime_vic$Local_Government_Area)),
                    multiple = TRUE,
                    selected = c("MONASH","BALLARAT","GREATER GEELONG"),
                    selectize = TRUE)
        
    })
    
    selected_years_region <- reactive({
        
        selected <- crime_vic %>% filter((Year >= input$Year[1]) &
                                             (Year <= input$Year[2])) %>% filter(Local_Government_Area %in% input$regionInput)
        
        selected$Year <- as.factor(selected$Year)
        
        selected_grouped <- selected %>% group_by(Year, Local_Government_Area) %>% summarise(Incidents = sum(Incidents_Recorded),
                                                                                             Rate = mean(Rate_per_100k_population))
        
        selected_grouped <- na.omit(selected_grouped)
        
        if(nrow(selected_grouped) == 0){
            return(NULL)
        }
        selected_grouped
        
    })
    
    selected_years <- reactive({
        
        selected <- crime_vic %>% filter((Year >= input$Year[1]) &
                                             (Year <= input$Year[2]))
        
        selected$Year <- as.factor(selected$Year)
        
        selected_grouped <- selected %>% group_by(Year, Local_Government_Area) %>% summarise(Incidents = sum(Incidents_Recorded),
                                                                                             Rate = mean(Rate_per_100k_population))
        
        selected_grouped <- na.omit(selected_grouped)
        
        if(nrow(selected_grouped) == 0){
            return(NULL)
        }
        selected_grouped
        
    })
    
    output$linePlot <- renderPlotly({
        
        plot <- ggplot(data = selected_years_region(), aes(x = Year, y = Rate, group = Local_Government_Area, 
                                                           col = Local_Government_Area)) +
            theme_bw() + labs(title = "Crime Statistics in Victoria from 2008 - 17",
                              x = "Year",
                              y = "Rate of incident per 100k") +
            geom_line() + geom_point() + scale_y_continuous(expand = c(0,0), limits = c(0,350))  
        plot <- plotly_build(plot)    
        plot <- ggplotly(plot, tooltip = c("x", "y")) %>% layout(xaxis = list(showspikes = T), yaxis = list(showspikes = T),
                                          hovermode = "x", margin = list(r = 15)) 
            
        
    })
    
    output$SelectedData <- DT::renderDataTable(
        selected_years_region(),
        options = list(scrollX = TRUE)
    )
    
    #plots the map
    
    finalPlot_division$popup <- paste("<b>location #: </b>", finalPlot_division$Local_Government_Area,
                                      "<br>", "<b>Offence: </b>", finalPlot_division$Offence_Division,
                                      "<br>", "<b>Recorded incident: </b>", finalPlot_division$Incidents,
                                      "<br>", "<b>Rate per 100K: </b>", finalPlot_division$Rate,
                                      "<br>", "<b>lon: </b>", finalPlot_division$lon,
                                      "<br>", "<b>lat: </b>", finalPlot_division$lat
    )
    title <- tags$div(
        HTML('<b style=width="300">Victorian LGA Crime rate (2008 - 2017)</b>')
    )
    output$map <- renderLeaflet({
        leaflet(finalPlot_division, width = "100%") %>% setView(lng = 144.7852, lat = -37.4713, zoom = 4) %>% addTiles() %>%
            addTiles(group = "OSM (selected)") %>%
            addProviderTiles(provider = "Esri.WorldStreetMap",group = "World StreetMap") %>%
            addProviderTiles(provider = "Esri.WorldImagery",group = "World Imagery") %>%
            addMarkers(lng = finalPlot_division$lon, lat = finalPlot_division$lat, popup = finalPlot_division$popup, clusterOptions = markerClusterOptions()) %>%
            addControl(title, position = "bottomleft")   %>% addLayersControl(
                baseGroups = c("OSM (selected)","World StreetMap", "World Imagery"),
                options = layersControlOptions(collapsed = FALSE)
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
