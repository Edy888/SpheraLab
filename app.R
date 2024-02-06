library(shiny)
library(bsicons)
library(thematic)
library(gitlink)
library(tidyverse)
library(bslib)
library(readr)
library(plotly)
library(kableExtra)

# Set the default theme for ggplot2 plots
ggplot2::theme_set(ggplot2::theme_minimal())

# Apply the CSS used by the Shiny app to the ggplot2 plots
thematic_shiny()



source("setup.R")

##################################################### UI ##################################################



ui <- page_sidebar(
  # Add github link
  ribbon_css("https://github.com/Edy888"),
  
  # Set the CSS theme
  theme = bs_theme(bootswatch = "darkly",
                   version = 5,
                   success = "#86C7ED",
                   "table-color" = "#81a1aa",
                   base_font = font_google("Lato"),
                   heading_font = font_face(family = "Open Sauce Sans",
                                            src = "url('../OpenSauceSans-Regular.ttf') format('truetype')")),
 
  # Add title
  title = "Shiny app DataBase",
  
  # Add sidebar elements
  sidebar = sidebar(
    class = "bg-secondary",
    sidebar_content,
    HTML('<img src = "serotonin.png", width = "100%", height = "auto">')
  ),
  

    # Layout non-sidebar elements (Main panel / макет вне боковой панели)
    layout_columns(
      card(card_header("Scatterplot: tests by days", class = "h4 text-success"),
           plotlyOutput("scatterplot")),
    
    card(card_header("BoxPlot: tests by weekdays", class = "h4 text-success"),
         plotlyOutput("boxplot")),
    
    card(card_header("Table_test by days", class = "h4 text-success"),
         tableOutput("table")),
    
    col_widths = c(7, 5, 4),
    row_heights = c(2, 1)
  )
)


################## ---SERVER--- ##############################################################################

server <- function(input, output, session) {
  

  
  
################## ---SWITCH--- #########################################################
  
  instrument_data <- reactive({
    switch(input$instrument,
           "Immulite 2000Xpi" = imm_instr,
           "ACL Top 750" = acl_instr,
           imm_instr)
  })

  
  
### Render the scatterplot --- меняется от прибора и интервала (день, месяц, год)########
  
  output$scatterplot <- renderPlotly({
    data <- instrument_data()
    if ("week" %in% input$d_w_m) {
      
        data <- data |> 
          arrange(date_order)  |> 
          mutate(week_number = format(date_order, "%Y-%U"))  |> 
          mutate(week_start_date = date_order - lubridate::wday(date_order) + 1)  |> 
          group_by( week_start_date, week_number)  |> 
          summarise(count = sum(n()), .groups = 'drop')  
     
      x_var <- "week_start_date"
      y_var <- "count"
      plot_title <- "Количество тестов в неделю за выбранный период"
      }
      else if ("month" %in% input$d_w_m){
        
        data <- data |> 
          mutate(month = format(date_order, "%Y-%m")) |> 
          relocate(month, .after = date_order) |> 
          group_by(month) |> 
          summarise(count = n())
        
        x_var <- "month"
        y_var <- "count"
        plot_title <- "Количество тестов (интервал месяц)"
        
        
      }
        
        
    
       else {
        
        data <- data  |>
          arrange(date_order) |> 
          mutate(week = lubridate::isoweek(date_order)) |>
          group_by(date_order) |>
          summarise(count = n(), .groups = 'drop')
      
      x_var <- "date_order"
      y_var <- "count"
      plot_title <- "Количество тестов в день за выбранный период"
    }
    
    # Plotly plot
    plot_ly(data, x =  ~get(x_var), y = ~get(y_var), type = 'scatter', mode = 'markers',
            marker = list(size = 8, opacity = 0.5, color = 'darkgreen')) |> 
      layout(
        title = plot_title,
        xaxis = list(title = 'Date'),
        yaxis = list(title = 'Count'),
        showlegend = FALSE
      )
    
  })
  
################ --- Render Table--- ##############

  output$table <- renderTable({
    data <- instrument_data()
    
    if ("week" %in% input$d_w_m) {
      
      data <- data |> 
        mutate(week_number = format(date_order, "%Y-%U"))  |> 
        mutate(week_start_date = date_order - lubridate::wday(date_order) + 1)  |>
        group_by( week_start_date, week_number)  |> 
        summarise(count = sum(n()), .groups = 'drop')  |> 
        arrange(week_start_date)
      
     } 
    
    else if ("month" %in% input$d_w_m){
        
      data <- data |> 
          mutate(month = format(date_order, "%Y-%m")) |> 
          relocate(month, .after = date_order) |> 
          group_by(month) |> 
          summarise(count = n())
        
      }
      
     else {
      
      data <- data |> 
        group_by(date_order) |> 
        summarise(count = n())
    
    }
    
    return(summary(data))
  })
  

############---Render Boxplot---меняется только от прибора---############################

  
  instrument_data <- reactive({
    switch(input$instrument,
           "Immulite 2000Xpi" = as.data.frame(imm_instr),
           "ACL Top 750" = as.data.frame(acl_instr),
           as.data.frame(imm_instr)) # Convert to data frame
  })
  

  
  output$boxplot <- renderPlotly({
    data <- instrument_data()
    
    boxplot_data <- reactive({
      if (input$instrument == "Immulite 2000Xpi") {
        data %>%
          mutate(weekday = weekdays(date_order, abbreviate = TRUE)) %>%
          relocate(weekday, .before = date_order) %>%
          group_by(date_order, weekday) %>%
          summarise(n = n(), .groups = 'drop') %>%
          as.data.frame() # Convert to data frame
      } else if (input$instrument == "ACL Top 750") {
        data %>%
          mutate(weekday = weekdays(date_order, abbreviate = TRUE)) %>%
          relocate(weekday, .before = date_order) %>%
          group_by(date_order, weekday) %>%
          summarise(n = n(), .groups = 'drop') %>%
          as.data.frame() # Convert to data frame
      } else {
        # Default case or additional instruments
        data %>%
          mutate(weekday = weekdays(date_order, abbreviate = TRUE)) %>%
          relocate(weekday, .before = date_order) %>%
          group_by(date_order, weekday) %>%
          summarise(n = n(), .groups = 'drop') %>%
          as.data.frame() # Convert to data frame
      }
    })
    
    
    # Extract the data frame from the reactive expression
    boxplot_df <- boxplot_data()
    
    plot_ly() %>%
      add_trace(data = boxplot_df,
                x = ~factor(weekday, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")), 
                y = ~n,
                type = "box",
                color = ~weekday,
                colors = c("darkgreen", "blue", "red", "orange", "purple", "cyan", "magenta")
      ) %>%
      layout(title = "Boxplot of Test Counts by Weekday", 
             xaxis = list(title = "Weekday"), 
             yaxis = list(title = "Count"),
             showlegend = FALSE
      )
    
    
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
}

# Run the Shiny app
shinyApp(ui, server)