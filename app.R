


library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sortable)


#######

ui <- fluidPage(
  titlePanel("Create a Grant Gantt"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("rowname", "Row Name", value = "Enter a row name"),
      sliderTextInput(
        inputId = "range",
        label = "Choose a range:", 
        choices = seq(0,20),
        selected = c(0, 2)
      ),
      actionButton("enter", "Submit"),
      br(),
      uiOutput("selectDelete"),
      actionButton("delete", "Delete"),
      br(),
      checkboxInput("manualsize", "Adjust Size Manually?"),
      sliderInput("manualheightslider", "Height", min = 50, max = 800, step = 10, value = 300),
      sliderInput("manualwidthslider", "Width", min = 50, max = 1000, step = 10, value = 500),
      numericInput("basefont", "Font Size", min = 10, max = 40, step = 1, value = 16),
      br(),
      textInput("fname", "File Name", value = "gantt-plot"),
      h4("How to download:"),
      p("Adjust size manually. Adjust font. Customize filename. Download."),
      downloadButton("download_plot", "Download Plot"),
      br()
      
    ),
    mainPanel(
      fluidRow(
        column(width = 6,
               uiOutput("rank"),
        ),
        column(width = 6,
               tableOutput("ordered_table")
        )
      ),
      fluidRow(
        uiOutput("plot")
      )
    )
    
  )
)



server <- function(input, output, session) {
  
  timeline = reactiveValues(
    df = tibble(title = "tilerow", 
                start = 0,
                end = 0)
  )
  
  observeEvent(input$enter,{
    i = nrow(timeline$df)+1
    dat = tibble(
      title = input$rowname,
      start = input$range[1],
      end = input$range[2]
    )
    
    timeline$df = bind_rows(timeline$df, dat)
  })
  
  output$selectDelete <- renderUI({
    selectInput("todelete", "Select item to delete", choices = timeline$df$title[-1])
  })
  
  observeEvent(input$delete,{
    timeline$df <- timeline$df %>%
      filter(title != input$todelete)
  })
  
  
  
  
  output$rank <- renderUI({
    labels = timeline$df$title[-1]
    rank_list(
      text = "Drag the items in any desired order",
      labels = labels,
      input_id = "rank_list_basic"
    )
  })
  
  output$results_basic <- renderPrint({
    input$rank_list_basic # This matches the input_id of the rank list
  })
  
  for_chart = reactive({
    req(nrow(timeline$df)>1)
    tibble(title = input$rank_list_basic) %>%
      left_join(timeline$df, by = 'title') %>%
      mutate(order = row_number())
    
  })
  
  output$ordered_table <- renderTable({
    for_chart()
  })
  
  output$plot_ob <- renderPlot({
    ggplot(for_chart()) +
      geom_linerange(aes(x = fct_reorder(title, order, .desc = T), ymin = start, ymax = end), size = 10, color = "grey30") +
      scale_y_continuous(breaks = seq(0.5,19.5,1), minor_breaks = seq(0, 19, 1), labels = seq(1,20,1), limits = c(0,20), expand = c(0.01,0.01)) + 
      scale_x_discrete(labels = wrap_format(40)) +
      geom_hline(aes(yintercept = 0)) +
      geom_hline(aes(yintercept = 4)) +
      geom_hline(aes(yintercept = 8)) +
      geom_hline(aes(yintercept = 12)) +
      geom_hline(aes(yintercept = 16)) +
      geom_hline(aes(yintercept = 20)) +
      coord_flip(clip = "off") + 
      theme_minimal(base_size = input$basefont) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "grey"),
            plot.margin=unit(c(15,1,5,1),"mm")) +
      ylab("Fiscal Quarter") +
      xlab(NULL) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 2, label = "Year 1", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 6, label = "Year 2", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 10, label = "Year 3", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 14, label = "Year 4", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 18, label = "Year 5", vjust = -3.5, size = input$basefont/4)
  })
  
  plotHeight <- reactive(
    if(input$manualsize == TRUE){
      paste0(input$manualheightslider, "px")
    } else {
      paste0(nrow(timeline$df)*50, "px")
    }
  )
  
  plotWidth <- reactive(
    if(input$manualsize == TRUE){
      paste0(input$manualwidthslider, "px")
    } else {
      "auto"
    }
  )
  
  
  plotout <- reactive({
    ggplot(for_chart()) +
      geom_linerange(aes(x = fct_reorder(title, order, .desc = T), ymin = start, ymax = end), size = 10, color = "grey30") +
      scale_y_continuous(breaks = seq(0.5,19.5,1), minor_breaks = seq(0, 19, 1), labels = seq(1,20,1), limits = c(0,20), expand = c(0.01,0.01)) + 
      scale_x_discrete(labels = wrap_format(40)) +
      geom_hline(aes(yintercept = 0)) +
      geom_hline(aes(yintercept = 4)) +
      geom_hline(aes(yintercept = 8)) +
      geom_hline(aes(yintercept = 12)) +
      geom_hline(aes(yintercept = 16)) +
      geom_hline(aes(yintercept = 20)) +
      coord_flip(clip = "off") + 
      theme_minimal(base_size = input$basefont) +
      theme(legend.position = "none",
            panel.grid.major = element_line(color = "white"),
            panel.grid.minor = element_line(color = "grey"),
            plot.margin=unit(c(15,1,5,1),"mm")) +
      ylab("Fiscal Quarter") +
      xlab(NULL) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 2, label = "Year 1", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 6, label = "Year 2", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 10, label = "Year 3", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 14, label = "Year 4", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 18, label = "Year 5", vjust = -3.5, size = input$basefont/4)
  })
  
  output$plot <- renderUI({
    plotOutput("plot_ob", height = plotHeight(), width = plotWidth())
  })
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "gantt-plot.png"
    },
    content = function(file) {
      ggsave(file, plotout(), height = input$plotHeight(), width = input$plotWidth())
    }
  )
  
}



shinyApp(ui = ui, server = server)
