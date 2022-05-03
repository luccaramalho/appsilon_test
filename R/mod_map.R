mod_map_UI <- function(id) {
  
  ns <- NS(id)

  tagList(
    
    dashboardSidebar(
      
      selectizeInput(
        inputId = ns("species_selector"),
        label = "Select a species:",
        selected = NULL,
        choices = c("Click here" = " ", sort(occurencePL$vernacularName))
      ),
      
      awesomeRadio(
        inputId = ns("species_selector_type"),
        label = NULL,
        choices = c("Vernacular name", "Scientific name"),
        selected = "Vernacular name",
        inline = FALSE
      ),

                     
      dateRangeInput(ns('dateRange'),
                     'Date range:',
                     start = min(occurencePL$eventDate), end = max(occurencePL$eventDate)),
      
      tags$br(),
      
      actionButton(ns("resetView"),"Reset view"),
      
      actionButton(ns("resetData"),"Reset Data"),
      
      actionButton(ns("help"),"Help/About"),
      
      column(12,h5(paste(comma(nrow(occurencePL)), HTML("observations and "),length(unique(occurencePL$family)), "families.")))
      
      ),
    
    dashboardBody(
      
      fluidRow(
             box(
               title = "Map of observed bird species", status = "warning", solidHeader = FALSE,
               leafletOutput(ns("map_output"),height='500px'),
               width=12,align='center'),
             
             valueBoxOutput(ns("totalCountOutput")),
             
             box(
               title = "Timeline of observed species count by year", status = "warning", solidHeader = FALSE,
               plotlyOutput(ns("timeline_output"),height = '180px'),height='250px',width = 8, align='center'
             ))

  ))
}



mod_map_server <- function(id) {
  
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      
      # { UPDATE SELECTINPUTS FOR SELECTED TYPE SPECIES } ---------------------------------------
      
      observeEvent(input$species_selector_type, {
        if (input$species_selector_type == "Vernacular name") {
          updateSelectizeInput(session,"species_selector",
                               choices = c("Click here" = " ", sort(occurencePL$vernacularName)))
        } else if (input$species_selector_type == "Scientific name") {
          updateSelectizeInput(session,"species_selector",
                               choices = c("Click here" = " ", sort(occurencePL$scientificName)))
        }
      })
      
      # ------------------------------------------------------------------------------------

      
      
      
      # { REACTIVE VALUE FOR USER'S SELECTED SPECIES } ---------------------------------------
      
      speciesReactiveValue <- reactive({

        x <- subset(occurencePL, vernacularName %in% input$species_selector | scientificName %in% input$species_selector & eventDate>= min(input$dateRange) & eventDate<=(max(input$dateRange)))
        return(x)

      })
      
      # ------------------------------------------------------------------------------------

      
      
      
      
      # { VALUE BOX TOTAL COUNT OUTPUT } ---------------------------------------------------
      
      # Output for valuebox totalCount
      output$totalCountOutput <- renderValueBox({
        valueBox(
          HTML(paste('<img src="bird_pin_md.png" style="float:right" height="100" width=auto>',
                     h1(speciesReactiveValue()$totalCount[1])
                     )),
          HTML(paste(
                     h3(speciesReactiveValue()$vernacularName[1]),
                     
                     h4(speciesReactiveValue()$scientificName[1])
                     )),
          icon = NULL,width=4
        )
      })
      
      #--------------------------------------------------------------------------------------
      
      
      
      
      # { BUTTON ACTIONS } ------------------------------------------------------------------
      
      # Reset view button
      observeEvent(input$resetView, {
        
        leafletProxy("map_output") %>%
          setView(lng = 19.1451, lat = 51.9194, zoom = 6)
        
      })
      
      # Help/About button
      observeEvent(input$help, {showModal(welcome_modal)})
      
      # Reset data buttons
      observeEvent(input$resetData, {
        
        updateSelectInput(session,
                          "species_selector", selected=" ")
        
        updateDateRangeInput(session,
                             "dateRange",'Date range input:',
                             start = min(occurencePL$eventDate),
                             end = max(occurencePL$eventDate))
      })
      
      #--------------------------------------------------------------------------------------
      
      
      
      
      # { RANDOM VALUE SELECTOR } -----------------------------------------------------------
      
      # Select only valid URI files for photos
      valid_vernacular_URI <- occurencePL %>% 
        select(accessURI, vernacularName) %>% na.omit() %>%
        select(vernacularName) %>% 
        unique() %>% pull(vernacularName)
      
      # Update random value
      observe({
        updateSliderInput(session, "species_selector", value = sample(valid_vernacular_URI,1))
      })
      
      #--------------------------------------------------------------------------------------

      
      
      
      # { MAP OUTPUT } ----------------------------------------------------------------------
      
      # Custom icon for marker - bird pin
      bird_pin <- makeIcon(iconUrl = "bird_pin.png", iconWidth = 40, iconHeight = "auto")
      
      output$map_output <- renderLeaflet({
        
        leaflet(speciesReactiveValue()) %>% addTiles() %>% addGeoJSON(polandGeojson) %>%
          
          addMarkers(lng = ~longitudeDecimal, lat = ~latitudeDecimal,
                            
                            popup = ~paste(
                              "Vernacular/Scientific name : ","<span class='badge'>",vernacularName,"</span>"," / ",
                              "<span class='badge'>",scientificName,"</span><br>",
                              "<hr>",
                              "Observation/Total count: ","<span class='badge'>",individualCount,"</span> / ",
                              "<span class='badge'>",scales::comma(totalCount),"</span>",
                              "<hr>",
                              "Locality: ","<span class='badge'>",locality,"</span>",
                              "<hr>",
                              "Date of observation: ","<span class='badge'>",as.Date(eventDate),"</span>",
                              "<hr>",
                              "<b><a href=",occurrenceID," target='_blank'>Occurence ID</a><b>",
                              "<hr>",
                              "<img src='",ifelse(is.na(accessURI)==TRUE,"noimage.png",accessURI),"' style='width:400px;height:250px;>'",
                              "<div id='map-graphPopup' class='shiny-html-output'></div>",sep=""), 
                          
                            clusterOptions = markerClusterOptions(), icon = bird_pin)  %>%  
          addMiniMap() %>%
          addEasyButton(
            easyButton(icon="fa-crosshairs", title="Locate Me",
            onClick=JS("function(btn, map){ map.locate({setView: true}); }"))
            ) %>%
          
          setView(
            lng = 19.1451,
            lat = 51.9194,
            zoom = 6)
      })
      
      #--------------------------------------------------------------------------------------
      
      
      
      
      # { TIMELINE OUTPUT } -----------------------------------------------------------------
      
      output$timeline_output <- renderPlotly({
        
        
        birdTimelineTotal <- data.frame(year=c(min(occurencePL$year):c(max(occurencePL$year))),yearSum=0)
        
        # Create column 'yearSum' with sum of values by year
        birdTimelineUser <- speciesReactiveValue() %>% select("year","individualCount") %>% group_by(year) %>%
          mutate(yearSum = sum(individualCount)) %>% select(-"individualCount") %>% unique() %>% arrange(year)
        
        timeline <- left_join(birdTimelineTotal, birdTimelineUser, by="year")
        
        ### FUNCTION - REPLACE LATER
        for (i in 1:nrow(birdTimelineTotal)) {
          
          if (is.na(timeline$yearSum.y[i])==TRUE) {
            
            timeline$yearSum[i] <- timeline$yearSum.x[i]
            
          } else {
            
            timeline$yearSum[i] <- timeline$yearSum.y[i]
            
          }
          
        }
        
        timeline <- select(timeline, year, yearSum)
        
        t <- list(family = "Lato")
        
        ggplotly(ggplot(timeline) + aes(x = year, y = yearSum) +
                   geom_line(size = 0.6, colour = "#5F9EA0") +
                   labs(title = NULL, x = "Year", y = "Count") +
                   theme(plot.title = element_text(size = 50),
                         axis.title.x = element_text(size = 12),
                         axis.title.y = element_text(size = 12))+
                   theme_minimal()
                 
                 
        ) %>% 
          layout(font=t) %>% config(displayModeBar = F) 
        
      })
      
      #--------------------------------------------------------------------------------------
    }
  )
  
}