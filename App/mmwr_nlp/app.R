library(plotly)
library(shiny)
library(tidyverse)
library(haven)
library(tools)

df <- read_dta('data_all.dta')

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Medical Entity Analysis of Mortality & Morbidity Weekly Reports, 1993 - 2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = 'selectCategory',
                    label = h3('Medical Entity Category:'),
                    choices = c('All', 'Medications', 'Medical Conditions', 'Test, Treatments, and/or Procedures'),
                    multiple = FALSE,
                    selected = 'All'
        ),
        sliderInput(inputId = 'selectWordRange',
                    label = h3('Number of Words:'),
                    min = 1,
                    max = 1000,
                    value = c(1, 50),
                    step = 1,
                    ticks = TRUE
        ),
        sliderInput(inputId = 'selectYear',
                    label = h3('Year(s):'),
                    min = min(df$year),
                    max = max(df$year),
                    value = c(min(df$year), max(df$year)),
                    step = 1,
                    ticks = TRUE,
                    sep = ""
        ),
        sliderInput(inputId = 'selectScore',
                    label = h3('AWS Match Score:'),
                    min = 0,
                    max = 1,
                    value = 0.5,
                    step = 0.01,
                    ticks = TRUE
        ),
        selectInput(inputId = 'selectGrouping',
                    label = h3('Calculate Correlation Coefficients by:'),
                    choices = c('Year', 'Quarter', 'Month', 'Issue'),
                    multiple = FALSE,
                    selected = 'year'
        ),
        sliderInput(inputId = 'selectCorThreshold',
                    label = h3('Correlation Coefficient Threshold:'),
                    min = 0,
                    max = 1,
                    value = 0.25,
                    step = 0.01,
                    ticks = TRUE
        )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("mainPlot"),
        br(), br(),
        plotlyOutput("linePlot"),
        br(), br(),
        plotlyOutput("corPlot", height = '100%')#,
        # verbatimTextOutput("select"),
        # verbatimTextOutput("click")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  output$mainPlot <- renderPlotly({
    
    base1 <- plot_ly(df, source = 'main')
    
    if(input$selectCategory == 'All') {
    } else if(input$selectCategory == 'Medications') {
      base1 <- base1 %>%
        filter(category == 'medication')
    } else if(input$selectCategory == 'Medical Conditions') {
      base1 <- base1 %>%
        filter(category == 'medical_condition')
    } else {
      base1 <- base1 %>%
        filter(category == 'test_treatment_procedure')
    }
    
    base1 <- base1 %>%
      filter(year >= input$selectYear[1]) %>% 
      filter(year <= input$selectYear[2]) %>% 
      filter(main_score >= input$selectScore) %>% 
      group_by(text) %>% 
      summarise(total = sum(count)) %>%
      arrange(desc(total)) %>% 
      slice(input$selectWordRange[1]:input$selectWordRange[2])
    
    mainPlot <- base1 %>%  
      add_bars(x = ~text, y = ~total, 
               hoverinfo = 'text',
               text = ~paste0('<b>', text, '</b><br>',
                              'Count: ', total)) %>%
      layout(xaxis = list(title = "", categoryorder = "array", categoryarray = ~text,
                          ticks = 'outside',
                          tickangle = -30), 
             yaxis = list(title = paste0('<b>','Frequency', '</b>')),
             title = paste0('<b>', 'Cumulative Frequency of the Top ', 
                            input$selectWordRange[1], '-', input$selectWordRange[2], ' ', 'Medical Entities, ',
                            input$selectYear[1], ' - ', input$selectYear[2], '</b><br>',
                            '[Select words by dragging cursor to make subplots appear]'),
             dragmode = 'select',
             barmode = 'stack',
             hovermode = 'compare') %>% 
      highlight(on = "plotly_click", off = 'plotly_doubleclick')
      
    mainPlot
    
  })
  
  observe({
    rangeMain <- event_data("plotly_relayout", source = 'main')
    if(is.null(rangeMain)) {
    } else {
      updateSliderInput(session = session, 
                        inputId = 'selectWordRange',
                        value = c(rangeMain$`xaxis.range[0]`,rangeMain$`xaxis.range[1]`))
    }
  })
    
  observe({
      rangeLine <- event_data("plotly_relayout", source = 'line')
      if(is.null(rangeLine)) {
      } else {
        updateSliderInput(session = session, 
                          inputId = 'selectYear',
                          value = c(rangeLine$`xaxis.range[0]`,rangeLine$`xaxis.range[1]`))
      }
  })
    
  output$linePlot <- renderPlotly({

    entitiesSelected <- event_data("plotly_selected", source = 'main')
    corHovered <- event_data('plotly_click', source = 'heatmap')
    
    selected <- c(corHovered$x, corHovered$y)
    
      base2 <- df %>%
        filter(year >= input$selectYear[1]) %>%
        filter(year <= input$selectYear[2]) %>%
        filter(main_score >= input$selectScore) %>%
        filter(text %in% entitiesSelected$x) %>%
        group_by(text, year) %>%
        summarise(total = sum(count))
    
    ymax <- pretty(max(base2$total) * 1.1, min.n = 0, n = 1)[2]
    
    if(is.null(entitiesSelected)) {

    } else if(is.null(corHovered)) {
      
      linePlot <- base2 %>% 
        plot_ly(source = 'line') %>%
        add_trace(x = ~year, y = ~total, type = 'scatter', mode = 'lines+markers',
                  color = ~text, colors = 'Dark2', linetype = ~text, 
                  opacity = 1,
                  line = list(width = 2),
                  marker = list(size = 8),
                  hoverinfo = 'text',
                  text = ~paste0('<b>', text, '</b><br>',
                                 'Year: ', year, '</b><br>',
                                 'Count: ', total)) %>%
        layout(xaxis = list(title = paste0('<b>','Year', '</b>'),
                            ticks = 'outside',
                            range = list(input$selectYear[1], input$selectYear[2]),
                            dtick = 1,
                            showgrid = FALSE,
                            tickangle = -45,
                            dragmode = 'select'),
               yaxis = list(title = paste0('<b>', 'Frequency', '</b>'),
                            range = list(0, ymax)),
               title = paste0('<b>','Frequency of Selected Medical Entities, by Year, ',
                              input$selectYear[1], ' - ', input$selectYear[2], '</b>'),
               dragmode = 'zoom') %>% 
        highlight(on = "plotly_click", off = 'plotly_doubleclick')
      
      linePlot
      
    } else {

      notSelected <- entitiesSelected$x[!entitiesSelected$x %in% selected]
      
      base2NotSelected <- df %>%
        filter(year >= input$selectYear[1]) %>%
        filter(year <= input$selectYear[2]) %>%
        filter(main_score >= input$selectScore) %>%
        filter(text %in% notSelected) %>%
        group_by(text, year) %>%
        summarise(total = sum(count))
      
      base2Selected <- df %>%
        filter(year >= input$selectYear[1]) %>%
        filter(year <= input$selectYear[2]) %>%
        filter(main_score >= input$selectScore) %>%
        filter(text %in% selected) %>%
        group_by(text, year) %>%
        summarise(total = sum(count))
      
      linePlot <- base2NotSelected %>% 
        plot_ly(source = 'line') %>%
        add_trace(x = ~year, y = ~total, type = 'scatter', mode = 'lines+markers',
                  color = ~text, colors = 'Dark2', linetype = ~text, 
                  opacity = 0.1,
                  line = list(width = 2),
                  marker = list(size = 8),
                  hoverinfo = 'text',
                  text = ~paste0('<b>', text, '</b><br>',
                                 'Year: ', year, '</b><br>',
                                 'Count: ', total)) %>%
        add_trace(x = base2Selected$year, y = base2Selected$total, type = 'scatter', mode = 'lines+markers',
                  color = base2Selected$text, colors = 'Dark2', linetype = base2Selected$text, 
                  opacity = 1,
                  line = list(width = 4),
                  marker = list(size = 10),
                  hoverinfo = 'text',
                  text = ~paste0('<b>', base2Selected$text, '</b><br>',
                                 'Year: ', base2Selected$year, '</b><br>',
                                 'Count: ', base2Selected$total)) %>% 
        layout(xaxis = list(title = paste0('<b>','Year', '</b>'),
                            ticks = 'outside',
                            range = list(input$selectYear[1], input$selectYear[2]),
                            dtick = 1,
                            showgrid = FALSE,
                            tickangle = -45,
                            dragmode = 'select'),
               yaxis = list(title = paste0('<b>', 'Frequency', '</b>'),
                            range = list(0, ymax)),
               title = paste0('<b>','Frequency of Selected Medical Entities, by Year, ',
                              input$selectYear[1], ' - ', input$selectYear[2], '</b>'),
               dragmode = 'zoom') %>% 
        highlight(on = "plotly_click", off = 'plotly_doubleclick')
      
      linePlot
      
    }

  })
  
  output$corPlot <- renderPlotly({
    
    d <- event_data("plotly_selected", source = 'main')
    
    if(is.null(d)) {
      
    } else {

    base3 <- df %>%
      filter(year >= input$selectYear[1]) %>%
      filter(year <= input$selectYear[2]) %>%
      filter(main_score >= input$selectScore) %>%
      filter(text %in% d$x)
    
    cor <- data.frame()
     
    if(input$selectGrouping == 'Year') {
      
      base3 <- base3 %>% 
        group_by(year, text) %>% 
        summarise(total = sum(count)) %>% 
        arrange((desc(total))) %>% 
        spread(text, total)
      
      base3[is.na(base3)] <- 0
      cor <- base3[, !(names(base3) %in% c(tolower(input$selectGrouping)))]
      
    } else if(input$selectGrouping == 'Quarter') {
      
      base3 <- base3 %>% 
        group_by(year, quarter, text) %>% 
        summarise(total = sum(count)) %>% 
        arrange((desc(total))) %>% 
        spread(text, total)
      
      base3[is.na(base3)] <- 0
      cor <- base3[, !(names(base3) %in% c('year', tolower(input$selectGrouping)))]
      
    } else if(input$selectGrouping == 'Month') {
      
      base3 <- base3 %>% 
        group_by(year, month, text) %>% 
        summarise(total = sum(count)) %>% 
        arrange((desc(total))) %>% 
        spread(text, total)
      
      base3[is.na(base3)] <- 0
      cor <- base3[, !(names(base3) %in% c('year', tolower(input$selectGrouping)))]
      
    } else {
      
      base3 <- base3 %>% 
        group_by(year, issue, text) %>% 
        summarise(total = sum(count)) %>% 
        arrange((desc(total))) %>% 
        spread(text, total)
      
      base3[is.na(base3)] <- 0
      cor <- base3[, !(names(base3) %in% c('year', tolower(input$selectGrouping)))]
      
    }
    
    mtx <- round(abs(cor(cor)), 2)
    mtx <- ifelse(mtx < input$selectCorThreshold, 0, mtx)
    mtx[upper.tri(mtx, diag = TRUE)] <- NA
    
    mtx <- as.data.frame(mtx)
    
    ylabels <- mtx %>% 
      select(2:(ncol(mtx))) %>% 
      colnames()
    
    mtx <- select(mtx, 1:(ncol(mtx)-1))
    xlabels <- colnames(mtx)
    
    mtx <- slice(mtx, 2:nrow(mtx)) %>% 
      as.matrix()
    
    spikes <- TRUE
    
    if (nrow(mtx) > 10) {
      spikes <- TRUE
    } else {
      spikes <- FALSE
    }
  
    heatMap <- mtx %>% 
      plot_ly(x = xlabels, y = ylabels,
              z = ., zmin = 0, zmax = 1,
              type = 'heatmap',
              source = 'heatmap',
              colors = 'Oranges') %>% 
      layout(title = paste0('<b>Correlations between Selected Medical Entities, by ', toTitleCase(input$selectGrouping), '</b><br>',
                            '[Highlight line plots above by clicking correlation pairs]'),
             xaxis = list(showgrid = FALSE,
                          tickangle = -30,
                          showspikes = spikes),
             yaxis = list(showgrid = FALSE,
                          showspikes = spikes),
             margin = list(t = 75))
    
    heatMap
    
    }
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

