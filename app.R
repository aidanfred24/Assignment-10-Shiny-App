#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library for shiny and theme
library(shiny)
library(shinythemes)

#libraries for simple interactive plots
library(ggplot2)
library(ggiraph)

#Data pipelining
library(tidyverse)

# map data for ggiraph map plots
library(mapdata)

#Text conversion
library(stringr)

#Fancy Colors
library(RColorBrewer)
library(paletteer)

# More complex geographic plots
library(tmap)
library(sf)

# Load data
accident3 <- read_sf("accident3.shp")
accident5 <- read.csv("accident5.csv")

# State and county data for geom_polygon ggplots
state <- map_data("state") %>%
  rename(STATENAME = region)

county <- map_data("county") %>%
  rename(COUNTYNAME = subregion, STATENAME = region)

# Name lists for better user input
#-----------------------------------
state_choices <- setNames(tolower(unique(accident5$STATENAME)),
                          str_to_title(unique(accident5$STATENAME)))

fill_choice <- setNames(c("fatalcase", "prop", "count"),
                        c("Fatalities Per Case", "% of Fatalities", "Total Cases"))

fill_choice2 <- setNames(c("fatalcase", "prop", "count"),
                        c("Fatalities", "Fatalities (%)", "Cases"))

subject_choice <- setNames(c("DAY_WEEKNAME", "LGT_CONDNAME", 
                           "ROUTENAME", "WEATHERNAME"),
                           c("Day of Week", "Light Condition",
                             "Road Type", "Weather Condition"))

road_choices <- unique(accident3$ROUTENA)

fill_choice3 <- setNames(c("MONTHNA", "DAY_WEE", "WEATHER"),
                         c("Month", "Day", "Weather Conditions"))

#------------------------------------

# App UI
ui <- fluidPage(
  
  # Theme choice
  theme = shinytheme("sandstone"),

  # Application title
  titlePanel("Traffic Accident Data (2021)"),
  
  # Add name below title
  tags$p(" Created by Aidan Frederick", 
         style = "color: gray; font-size: 12px; text-align: left; margin-top: -5px;"),
  
  # Navigation bar format
    navbarPage(
      "Navigation",
      
      #First map tab
        tabPanel("Map", 
          sidebarLayout(
            
            # Display user input for location
            sidebarPanel(selectInput(inputId = "Location1",
                                     label = "Location",
                                     choices = c("United States", 
                                                 state_choices)),
                         # Conditional panel for US map
                         conditionalPanel(
                           condition = "input.Location1 == 'United States'",
                         
                         selectInput(inputId = "Subject",
                                     label = "Subject",
                                     choices = subject_choice),
                         
                         uiOutput("sub_subjects")),
                     
                         # Buttons for user selection of stats
                         radioButtons(inputId = "Fill",
                                      label = "Statistics",
                                      choices = fill_choice),
                     ),
  
          # plot map on main panel
          mainPanel(
            
            #wrap grey box around the map
            tags$div(
            style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
            
            # plot output
            girafeOutput("location", width = "90%")
          ))
          )
        ),
        
        # tab for weather histograms
        tabPanel("Weather",
                 
                 sidebarLayout(
                   sidebarPanel(selectInput(inputId = "Location2",label = "Location",
                                            choices = state_choices),
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     
                     # wrap plot in grey box
                     tags$div(
                     style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
                     
                     #plot output
                     girafeOutput("Weather", width = "90%")
                   ))
               
               )),
      
      # Tab for timeline of accidents by month and state
      tabPanel("Month",
        
        sidebarLayout(
          sidebarPanel(selectInput(inputId = "Location3",label = "Location",
                                   choices = state_choices),
          ),
          
          mainPanel(
            # wrap plot in grey box
            tags$div(
              style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
              
              # Display plot
              girafeOutput("Month", width = "90%")
        
      )
      )
    )),
    
    # Tab for geographic map of cases
    tabPanel("Cases",
             
             sidebarLayout(
               sidebarPanel(
                 
                 # Format sidebar into rows
                 fluidRow(
                   # Title above selection
                   h4("View", style = "margin-top: -5px; margin-bottom: 10px;"),
                   
                   # Input for road type and fill parameters
                   selectInput(inputId = "Road",
                               label = "Road Type",
                               choices = road_choices),
                   selectInput(inputId = "Fill2",
                               label = "Color by:",
                               choices = fill_choice3,
                               selected = "Weather")),
                 
                 # 2nd row in sidebar
                 fluidRow(
                   # Title for selection
                   h4("Fatalities"),
                   
                   # Split selections into columns next to each other
                   column(6,
                     numericInput(inputId = "MinVal", 
                                  label = "Minimum ",
                                  value = 1,
                                  min = 0,
                                  max = 20,
                                  width = "80px")),
                   column(6,
                     numericInput(inputId = "MaxVal", 
                                  label = "Maximum ",
                                  value = 20,
                                  min = 0,
                                  max = 20,
                                  width = "80px")))
                 ),
               
               # wrap output and display plot 
               mainPanel(
                  tags$div(
                    style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
                    textOutput("none"),
                    tmapOutput("case_map",
                               height = "85vh"),  # Fit plot to 85% height of window
    ))
  ))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # 3rd dropdown on first tab
  # dynamic selection for user depending on subject
  output$sub_subjects <- renderUI({
    
    selected <- names(subject_choice[which(subject_choice == input$Subject)])
    
    choices <- unique(accident5[[input$Subject]])
    
    selectInput(inputId = "Sub_Subjects",
                label = selected,
                choices = choices)
    
  })
  
  output$location <- renderGirafe({
    
    # Generate different plots depending on input
    if (input$Location1 == "United States"){
      
      # require certain inputs prior to running
      req(input$Location1, input$Subject, input$Sub_Subjects, input$Fill)
      
      # data manipulation, filter to user input, calculate stats
      accident6 <- accident5 %>%
        group_by(STATENAME, !!sym(input$Subject)) %>%
        summarise(count = n(), total = sum(FATALS)) %>%
        mutate(STATENAME = tolower(STATENAME),
               fatalcase = total / count,
               prop = (total / sum(total)) * 100) %>%
        filter(!!sym(input$Subject) == input$Sub_Subjects)
      
      # join data to map data after filtering
      accident6 <- accident6 %>%
        left_join(x = state, y = accident6, by = "STATENAME")
      
      # US map
      us_map <- ggplot(data=accident6, 
                       mapping=aes(x=long,
                                   y=lat,
                                   group=group,
                                   
                                   #interactivity level for ggiraph
                                   data_id = STATENAME,
                                   
                                   #Info displayed on hover
                                   tooltip = paste("State:", 
                                                   str_to_title(STATENAME),
                                                   "\n", 
                                                   names(fill_choice[which(fill_choice == input$Fill)]),
                                                   ":",
                                                   round(.data[[input$Fill]], 2)))) + 
        coord_fixed(1.3) + 
        # draw shape of states and US
        geom_polygon_interactive(color="black", aes(fill = .data[[input$Fill]])) + 
        scale_fill_distiller_interactive(palette = "RdYlBu")+
        # Remove plot elements, chang coloring
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.title.y=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_rect(fill = "#FFFFFF"),
              panel.grid = element_blank())+
        # change labels
        labs(title = paste("Traffic Accident", 
                           names(fill_choice[which(fill_choice == input$Fill)]),
                           "by State"),
             fill = names(fill_choice2[which(fill_choice2 == input$Fill)]))
        
      # generate ggiraph plot
      girafe(ggobj = us_map, 
             options = list(opts_hover(css = "fill:green;stroke:black")))
  
    } else {
        
      # more manipulation, tailor to user input, calculate stats
        accident7 <- accident5 %>%
          filter(!STATENAME %in% c("Alaska", 
                                   "Hawaii")) %>%
          mutate(COUNTYNAME = tolower(COUNTYNAME),
                 STATENAME = tolower(STATENAME)) %>%
          group_by(STATENAME, 
                   COUNTYNAME) %>%
          summarize(count = n(), 
                    total = sum(FATALS)) %>%
          mutate(fatalcase = total / count,
                 prop = (total / sum(total)) * 100)
        
        # Join to map data
        accident7 <- accident7 %>% 
          left_join(x = county, y = accident7,
                    by = c("STATENAME", "COUNTYNAME"))
        
        # replace NAs with 0
        accident7 <- accident7 %>% 
          replace(is.na(accident7), 0)
        
        # filter to state
        accident7 <- accident7 %>%
          filter(STATENAME == input$Location1)
        
        # State map by county
        county_map <- ggplot(data=accident7, 
                         mapping=aes(x=long,
                                     y=lat,
                                     group=group,
                                     #interactivity level for ggiraph
                                     data_id = COUNTYNAME,
                                     tooltip = paste("County:", 
                                                     str_to_title(COUNTYNAME),
                                                     "\n", 
                                                     names(fill_choice[which(fill_choice == input$Fill)]),
                                                     ":",
                                                     round(.data[[input$Fill]], 2)))) + 
          coord_fixed(1.3) + 
          #map out state and counties
          geom_polygon_interactive(color="black", aes(fill = .data[[input$Fill]])) +
          scale_fill_distiller_interactive(palette = "RdYlBu")+
          geom_polygon_interactive(data=accident7, fill = NA, color="white") + 
          geom_polygon_interactive(color="black", fill=NA) + 
          #change plot elements
          theme(axis.title.x=element_blank(),
                axis.text.x=element_blank(),
                axis.ticks.x=element_blank(),
                axis.title.y=element_blank(),
                axis.text.y=element_blank(),
                axis.ticks.y=element_blank(),
                panel.background = element_rect(fill = "#FFFFFF"),
                panel.grid = element_blank())+
          labs(title = paste(str_to_title(input$Location1),
                             "Traffic Accident", 
                             names(fill_choice[which(fill_choice == input$Fill)])),
               fill = names(fill_choice2[which(fill_choice2 == input$Fill)]))
      
      # generate ggiraph plot
      girafe(ggobj = county_map,
             options = list(opts_hover(css = "fill:green;stroke:black")))
    }
    
  })
  
  output$Weather <- renderGirafe({
      
    # filter data to state selection
    full_weather <- accident5 %>%
      group_by(STATENAME, WEATHERNAME) %>%
      summarize(count = n()) %>%
      filter(STATENAME == tolower(input$Location2),
             WEATHERNAME != "Unknown")
    
    # bar plot for weather conditions by state
    w_plot <- ggplot(data = full_weather)+ 
      aes(x = WEATHERNAME,
          y = count,
          fill = WEATHERNAME,
          data_id = WEATHERNAME,
          tooltip = paste("Accident Count:", count))+
      geom_bar_interactive(stat = "identity")+
      scale_fill_paletteer_d("ggthemes::Classic_Cyclic")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
            legend.position = "none")+
      labs(title = paste(str_to_title(input$Location2),
                         " Fatal Accident Cases by Weather Condition"),
           x = "Weather Condition",
           y = "Accident Count")
      
    # generate ggiraph plot
    girafe(ggobj = w_plot,
           options = list(opts_hover(css = "fill:green;stroke:black")))
    
    
  })
  
  output$Month <- renderGirafe({
    
    # Filter to month data by state, keeping fatality count
    full_month <- accident5 %>%
      group_by(STATENAME, MONTHNAME) %>%
      summarize(count = n()) %>%
      mutate(MONTHNAME = factor(MONTHNAME, levels = month.name)) %>%
      filter(STATENAME == tolower(input$Location3))
   
    # plot for monthly trend of fatalities by state 
    w_plot <- ggplot(data = full_month)+ 
      aes(x = MONTHNAME,
          y = count,
          fill = MONTHNAME,
          group = 1,
          data_id = MONTHNAME,
          tooltip = paste("Accident Count:", count))+
      geom_line(linewidth = 1, color = "red")+
      geom_point_interactive(size = 3)+
      ylim(0, NA)+
      scale_x_discrete(limits = month.name) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.75),
            legend.position = "none")+
      labs(title = paste(str_to_title(input$Location3),
                         "Fatal Accident Cases by Month"),
           x = "Month",
           y = "Accident Count")
    
    #generate ggiraph plot
    girafe(ggobj = w_plot,
           options = list(opts_hover(css = "fill:green;stroke:black")))
    
    
  })
    
  # New data for interactive map
    new_data <- reactive({
      accident3 %>%
      filter(ROUTENA %in% c(input$Road),
             FATALS >= input$MinVal,
             FATALS <= input$MaxVal) %>%
      mutate(ST_CASE = as.character(ST_CASE))
    })
  
  # switch to interactive "view" mode
  tmap_mode("view")
  
   # text object for strict user selection with empty data set
    output$none <- renderText({
      
      full_acc <- new_data()
      
      if(nrow(full_acc) == 0){
      
        return("No Data Avaiable")
        
      } else {
        
        return(NULL)
        
      }
      
    })
    
    output$case_map <- renderTmap({
      
      # get new data
      full_acc <- new_data()
    
      if(nrow(full_acc) == 0){
        
          return(NULL)
          
        } else {
          
          # interactive geographic map
          my_map <- tm_shape(full_acc)+
            tm_dots("FATALS",
                    #transparency
                    alpha = 0.5,
                    #color of points
                    col = as.character(input$Fill2),
                    #info displayed on click
                    popup.vars = c("Case: " = "ST_CASE",
                                   "Fatalities: " = "FATALS",
                                   "Weather: " = "WEATHER",
                                   "Month: " = "MONTHNA"),
                    title = "Road Type")+
            # background map
            tm_basemap("Esri.WorldTopoMap")
        }
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
