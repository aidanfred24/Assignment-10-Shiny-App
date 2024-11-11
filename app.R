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
library(ggplot2)
library(ggiraph)
library(tidyverse)
library(mapdata)
library(stringr)
library(RColorBrewer)
library(paletteer)
library(tmap)
library(sf)
library(bslib)
library(gridlayout)
library(shinycssloaders)

#libraries for simple interactive plots

#Data pipelining

# map data for ggiraph map plots

#Text conversion

#Fancy Colors

# More complex geographic plots


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
                        c("Fatalities", "Percent", "Cases"))

subject_choice <- setNames(c("Total", "DAY_WEEKNAME", "LGT_CONDNAME", 
                           "ROUTENAME", "WEATHERNAME"),
                           c("Total","Day of Week", "Light Condition",
                             "Road Type", "Weather Condition"))

road_choices <- unique(accident3$ROUTENA)

fill_choice3 <- setNames(c("MONTHNA", "DAY_WEE", "WEATHER"),
                         c("Month", "Day", "Weather Conditions"))

#------------------------------------

# App UI
ui <- page_navbar(
  
  # Theme choice
  theme = bs_theme(primary = "#000000", 
                   preset = "materia"),
  
  # Application title
  title = "Traffic Accident Data (2021)",
  
  inverse = TRUE,
      
      #First map tab
      nav_panel("Map",
                
                page_sidebar(    
                  sidebar = sidebar(selectInput(inputId = "Location1",
                                                label = "Location",
                                                choices = c("United States",
                                                            state_choices)),
                                    # Conditional panel for US map
                                    conditionalPanel(
                                      condition = "input.Location1 == 'United States'",
                                      
                                      selectInput(inputId = "Subject",
                                                  label = "Subject",
                                                  choices = subject_choice),
                                      
                                      conditionalPanel(
                                        condition = "input.Subject != 'Total'",
                                        uiOutput("sub_subjects")
                                      )
                                    ),
                                    
                                    # Buttons for user selection of stats
                                    radioButtons(inputId = "Fill",
                                                 label = "Statistics",
                                                 choices = fill_choice),
                  ),
                  
                  # plot output with loading symbol and wrapped box
                  withSpinner(girafeOutput("location", height = "85vh"),
                              type = 4,
                              color = "#000000",
                              size = 3) %>%
                    tags$div(
                      style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;")
                ))
      ,
  
  nav_panel(
    title = "Profile",
    page_sidebar(
      sidebar = sidebar(
        title = "Filters/Info",
        
        # multiple select for location data
        selectizeInput(inputId = "Location2",
                       label = "Location(s)",
                       choices = state_choices,
                       multiple = TRUE,          #allows for multiple inputs
                       selected = c("alabama",
                                    "california",
                                    "arizona")),
        
        # Add reset button for quicker transitions
        actionButton(inputId = "reset", 
                     label = "Reset"),
        
        # input to modify y-axis of trendline
        selectInput(inputId = "monthStat",
                     label = "Trend Metric",
                     choices = fill_choice)
            ),
      # Define layout of entire page
      grid_container(
        # area layout of page (3 on top, 1 on bottom)
        layout = c(
          "area1 area2 area3",
          "area0 area0 area0"
        ),
        
        # dimensions of top vs. bottom row
        row_sizes = c(
          "0.5fr",
          "1.68fr"
        ),
        
        # dimensions of grid columns (even along the top)
        col_sizes = c(
          "1fr",
          "1fr",
          "1fr"
        ),
        
        # gaps between grid elements
        gap_size = "10px",
        
        # grid on bottom half of page
        grid_card(
          area = "area0",
          card_body(
            grid_container(
              layout = c(
                "area0 area1"
              ),
              # one row on bottow
              row_sizes = c(
                "1fr"
              ),
              # two columns for two plots
              col_sizes = c(
                "1fr",
                "1fr"
              ),
              # gaps between bottom grid elements
              gap_size = "10px",
              
              # card for first plot
              grid_card(
                area = "area0",
                
                # display plot with loading spinner
                card_body(withSpinner(girafeOutput(outputId = "Weather", height = "53vh"),
                                      type = 4,
                                      color = "#000000"))
              ),
              
              # card for second plot
              grid_card(
                area = "area1",
                
                # display plot with loading spinner
                card_body(withSpinner(girafeOutput(outputId = "Month", height = "53vh"),
                                      type = 4,
                                      color = "#000000"))
              )
            )
          )
        ),
        
        # Top left grid card
        grid_card(
          area = "area1",
          card_body(
            value_box(
              title = "Average Fatalities per Month",
              value = textOutput(outputId = "textOutput1"),                    #display dynamic text output
              showcase = bsicons::bs_icon("car-front-fill", fill = "#000000")  #add icon to left
            )
          )
        ),
        
        # middle grid card
        grid_card(
          area = "area2",
          card_body(
            value_box(
              title = "Top Weather Hazard for Traffic Fatalities",
              value = textOutput(outputId = "textOutput2"),             #display dynamic text output
              showcase = bsicons::bs_icon("sun-fill", fill = "#000000") #add icon
            )
          )
        ),
        
        #top right grid card
        grid_card(
          area = "area3",
          card_body(
            value_box(
              title = "Top Month for Traffic Fatalities",
              value = textOutput(outputId = "textOutput3"),                       #display dynamic text output
              showcase = bsicons::bs_icon("calendar-week-fill", fill = "#000000") #add icon
            )
          )
        )
      )
    )),
      
      # Tab for geographic map of cases
      nav_panel("Cases",
                
                page_sidebar(    
                  sidebar = sidebar(
                    
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
                      
                      sliderInput(inputId = "Range",
                                  label = NULL,
                                  min = 1, max = 15,
                                  value = c(1, 15))
                    )),
                  
                  # wrap output and display plot 
                  tags$div(
                    style = "border: 1px solid #D3D3D3; padding: 15px; border-radius: 5px; background-color: #FFFFFF;",
                    textOutput("none"),
                    withSpinner(tmapOutput("case_map",
                                           height = "85vh"),
                               type = 4,
                               color = "#000000",
                               size = 3),  # Fit plot to 85% height of window
                  )
                ))
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
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
      req(input$Location1, input$Subject, input$Fill)
      
      if(input$Subject != "Total"){
        
        # data manipulation, filter to user input, calculate stats
        accident6 <- accident5 %>%
          group_by(STATENAME, .data[[input$Subject]]) %>%
          summarise(count = n(), total = sum(FATALS)) %>%
          mutate(STATENAME = tolower(STATENAME),
                 fatalcase = total / count,
                 prop = (total / sum(total)) * 100) %>%
          filter(.data[[input$Subject]] == input$Sub_Subjects)
        
      } else {
        
        # Filter to total stats by state
        accident6 <- accident5 %>%
          group_by(STATENAME) %>%
          summarise(count = n(), total = sum(FATALS)) %>%
          mutate(STATENAME = tolower(STATENAME),
                 fatalcase = total / count,
                 prop = (total / sum(total)) * 100)
        
      }
      
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
  
  # Text output for top left card
  output$textOutput1 <- renderText({
    
    # filter and group to just month and total fatals
    full1 <- accident5 %>%
      filter(STATENAME %in% tolower(input$Location2)) %>%
      group_by(MONTHNAME) %>%
      summarize(total = sum(FATALS))
    
    # calculate average
    avg <- round(sum(full1$total) / 12, 0)
    
    return(avg)
  
  })
  
  # text output for top middle card
  output$textOutput2 <- renderText({
    
    # filter and group to weather entry with highest fatals
    full2 <- accident5 %>%
      filter(STATENAME %in% tolower(input$Location2)) %>%
      group_by(WEATHERNAME) %>%
      summarize(total = sum(FATALS)) %>%
      filter(!WEATHERNAME %in% c("Clear", "Cloudy", "Unknown")) %>%
      arrange(desc(total))%>%     # descending order by total fatals
      slice(1)                    # keep only top entry
    
    # grab only weather condition
    highest <- full2$WEATHERNAME[1]
    
    return(highest)
    
  })
  
  # text output for top right card
  output$textOutput3 <- renderText({
    
    # filter and group to month entry with highest fatals
    full3 <- accident5 %>%
      filter(STATENAME %in% tolower(input$Location2)) %>%
      group_by(MONTHNAME) %>%
      summarize(total = sum(FATALS)) %>%
      arrange(desc(total))%>%
      slice(1)
    
    highest2 <- full3$MONTHNAME[1]
    
    return(highest2)
    
  })
  
  output$Weather <- renderGirafe({
      
    # filter data to state selection, add stats
    full_weather <- accident5 %>%
      group_by(STATENAME, WEATHERNAME) %>%
      summarize(count = n()) %>%
      filter(STATENAME %in% tolower(input$Location2),
             WEATHERNAME != "Unknown") %>%
      mutate(prop = round((count / sum(count)) * 100, 2),
             STATENAME = str_to_title(STATENAME))
      
    
    # bar plot for weather conditions by state
    w_plot <- ggplot(data = full_weather)+ 
      aes(x = STATENAME,
          y = count,
          fill = WEATHERNAME,
          data_id = interaction(STATENAME, WEATHERNAME),
          tooltip = paste("Weather Condition: ", WEATHERNAME,
                          "\nAccident Proportion:", prop, "%"))+
      geom_bar_interactive(stat = "identity", position = "fill")+
      scale_fill_paletteer_d("ggthemes::Classic_Cyclic")+
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 0.75,
                                       size = 10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size = 16))+
      labs(title = "Fatal Accident Cases by Weather Condition",
           x = NULL,
           y = "Accident Proportion",
           fill = "Condition")
      
    # generate ggiraph plot
    girafe(ggobj = w_plot,
           options = list(opts_hover(css = "fill:green;stroke:black")))
    
    
  })
  
  # update input to default location values
  observeEvent(input$reset, {
    updateSelectInput(session, "Location2", selected = c("alabama",
                                                      "california",
                                                      "arizona"))
  })
  
  output$Month <- renderGirafe({
    
    # Filter to month data by state, keeping fatality count
    full_month <- accident5 %>%
      group_by(STATENAME, MONTHNAME) %>%
      summarize(fatalcase = sum(FATALS), count = n()) %>%
      mutate(MONTHNAME = factor(MONTHNAME, levels = month.name)) %>%
      filter(STATENAME %in% tolower(input$Location2)) %>%
      mutate(prop = round((count / sum(count)) * 100, 2),
             STATENAME = str_to_title(STATENAME))

    # plot for monthly trend of fatalities by state 
    w_plot <- ggplot(data = full_month)+ 
      aes(x = MONTHNAME,
          y = .data[[input$monthStat]],
          group = STATENAME,
          data_id = interaction(STATENAME, MONTHNAME),
          tooltip = paste("State: ", STATENAME,
                          "\nMonth: ", MONTHNAME,
                          "\n",
                          names(fill_choice[which(fill_choice == input$monthStat)]),
                          ":", .data[[input$monthStat]]))+
      geom_line(aes(color = STATENAME), linewidth = 1)+
      geom_point_interactive(size = 2)+
      ylim(0, NA)+
      scale_x_discrete(limits = month.name) +
      theme_minimal()+
      theme(axis.text.x = element_text(angle = 45,
                                       hjust = 0.75,
                                       size = 10),
            axis.title.y = element_text(size = 15),
            plot.title = element_text(size = 16))+
      labs(title = paste(names(fill_choice[which(fill_choice == input$monthStat)]),
                         "by Month"),
           x = NULL,
           y = names(fill_choice[which(fill_choice == input$monthStat)]),
           color = "State")
    
    #generate ggiraph plot
    girafe(ggobj = w_plot,
           options = list(opts_hover(css = "fill:green;stroke:black"),
                          opts_zoom(min = 1, max = 20, duration = 300),
                          opts_selection(type = "none")))
    
    
  })
    
  # New data for interactive map
    new_data <- reactive({
      accident3 %>%
      filter(ROUTENA %in% c(input$Road),
             FATALS <= max(input$Range),
             FATALS >= min(input$Range)) %>%
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
                    title = names(fill_choice3[which(fill_choice3 == input$Fill2)]))+
            # background map
            tm_basemap("Esri.WorldTopoMap")
        }
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
