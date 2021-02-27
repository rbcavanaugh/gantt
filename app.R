
###### to do #####
# adjust colors
# adjust number of quarters
# adjust font
# adjust wraping of rownames
# write instructions

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sortable)
library(scales)
library(shinythemes)
library(plotly)
#######


ui <- 
  fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$title("Gantts for Grants")
    ),
    navbarPage(fluid = T,
                 title = div(a(icon("github"),
                               href = "https://github.com/rbcavanaugh/gantt",
                               id = "img-id",
                               target = "_blank"),"Gantts for Grants", style = "color: #EEEEEE;"),
                 id = "navbar", theme = shinytheme("lumen"),
    fluidRow(
      column(width = 6,
    tabsetPanel(
      tabPanel("Instructions",br(), br(),
               tags$ul(
                 tags$li("Under 'Build Plot,' select the number of years for the grant and input each element in the grant timeline. The number of years can be changed at any time."),
                 tags$li("Delete items by selecting the item in the dropdown menu and hitting the delete button."),
                 tags$li("Item order can be modified using the box to the right by dragging items around."),
                 tags$li("Once your items and their order are reasonably complete, click on the adjust plot tab."),
                 tags$li("Check the 'Adjust size manually' box and change the font size, y-axis text wrapping, width, and height until the plot looks like you want it. The final plot size is based on the dimsensions and size of the plot you see, so I recommend maximizing your browser window"),
                 tags$li("When you're satisfied, click the download button in the bottom right and download the plot. If the download doesn't work properly, you can also try to save the plot by right clicking on the plot and using 'save image as' or 'copy image'."),
                 tags$li("If you think you will continue modifying the plot, I recommend hitting 'download csv as well so you can copy and paste next time."),
                 tags$li("Note: If running via R-studio, you must view the application in your web browser (not the R-studio viewer)."),
                 
               )),
      tabPanel("Build Plot",
               column(width = 6,
                      br(),
                      radioGroupButtons("len", "Number of Years",
                                        choices = c(2,3,4,5),
                                        selected = 5),
                      uiOutput("selectDelete"),
                      actionButton("delete", "Delete")
               ),
               column(width = 6, br(),
 
               textInput("rowname", "Create Item", value = ""),
               uiOutput('fiscal'),
               actionButton("enter", "Submit")
               )
               ),
      tabPanel("Adjust Plot",
               column(width = 6,
               checkboxInput("manualsize", "Adjust Size Manually?"),
               checkboxInput("grid", "More gridlines?"),
               sliderInput("basefont", "Font Size", min = 10, max = 40, step = 1, value = 16),
               sliderInput("wrap", "Wrap y-axis", min = 0, max = 100, step = 1, value = 40)
               ),
               column(width = 6,
               textInput("xlab", "X-axis label", "Fiscal Quarter"),
               sliderInput("manualheightslider", "Height", min = 100, max = 1000, step = 20, value = 400),
               sliderInput("manualwidthslider", "Width", min = 500, max = 2000, step = 20, value = 1000)
               )
               )
    )),
    column(width = 6,
           panel(style = "overflow-y:scroll; max-height: 300px;",
               uiOutput("rank")
               ),
      ),
      fluidRow(align = "center",
        uiOutput("plot")
      ),
      fluidRow(
      div(style = "padding:20px; position: fixed;bottom: 0;right:0",
      dropdownButton(up = T, right = T,
        h4("Download:"),
        textInput("imagedownload", "file name (exclude extension)", value = 'gantt-image'),
        downloadButton("save_plot", "Download Plot"),
        textInput("csvdownload", "file name (exclude extension)", value = 'gantt-data'),
        downloadButton("save_csv", "Download CSV"),
        circle = TRUE, status = "primary",
        icon = icon("download"), width = "300px")
      )
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
    selectInput("todelete", "Delete Item", choices = timeline$df$title[-1])
  })
  
  observeEvent(input$delete,{
    timeline$df <- timeline$df %>%
      dplyr::filter(title != input$todelete)
  })
  
  output$rank <- renderUI({
    labels = timeline$df$title[-1]
    rank_list(
      text = "Drag the items in any desired order",
      labels = labels,
      input_id = "rank_list_basic"
    )
  })
  
  output$fiscal <- renderUI({
    sliderTextInput(
      inputId = "range",
      label = "Choose a range (fiscal quaraters):", 
      choices = seq(0,as.numeric(input$len)*4, 1),
      selected = c(0, 4)
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
  
  plot_out <- reactiveValues()
  
  output$plot_ob <- renderPlot({
    len = as.numeric(input$len)*4-1

    gg <- ggplot(for_chart()) +
      geom_linerange(aes(x = fct_reorder(title, order, .desc = T), ymin = start, ymax = end), size = 10, color = "grey30") +
      scale_y_continuous(breaks = seq(0.5,len+.5,1), minor_breaks = seq(0, len, 1), labels = seq(1,len+1,1), limits = c(0,len+1), expand = c(0.01,0.01)) + 
      scale_x_discrete(labels = wrap_format(input$wrap)) +
      geom_hline(aes(yintercept = 0)) +
      geom_hline(aes(yintercept = 4)) +
      geom_hline(aes(yintercept = 8)) +
      geom_hline(aes(yintercept = 12)) +
      geom_hline(aes(yintercept = 16)) +
      geom_hline(aes(yintercept = 20)) +
      coord_flip(clip = "off") + 
      theme_minimal(base_size = input$basefont) +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(color = ifelse(input$grid, "grey", "white")),
            panel.grid.major.x = element_line(color = "white"),
            panel.grid.minor = element_line(color = "darkgrey"),
            plot.margin=unit(c(15,1,5,1),"mm")) +
      ylab(input$xlab) +
      xlab(NULL) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 2, label = "Year 1", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 6, label = "Year 2", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 10, label = "Year 3", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 14, label = "Year 4", vjust = -3.5, size = input$basefont/4) +
      annotate(geom = 'text', x = for_chart()$title[1], y = 18, label = "Year 5", vjust = -3.5, size = input$basefont/4)
    
    plot_out$gg <- gg
    
    print(gg)
  })
  
  
  
     
  
  plotHeight <- reactive(
    if(input$manualsize == TRUE){
      paste0(input$manualheightslider, "px")
    } else {
      paste0(nrow(timeline$df)*65, "px")
    }
  )
  
  plotWidth <- reactive(
    if(input$manualsize == TRUE){
      paste0(input$manualwidthslider, "px")
    } else {
      "90%"
    }
  )
  
  output$plot <- renderUI({
    plotOutput("plot_ob", height = as.character(plotHeight()), width = as.character(plotWidth()))
  })

  output$save_plot <- downloadHandler(
    filename = function(){
      paste0(input$imagedownload, ".png")
    },
    content = function(file) {
      ggsave(filename = file, plot = print(plot_out$gg),
             height = input$manualheightslider/75, width = input$manualwidthslider/75, dpi = 100)
     }
  )
  
  output$save_csv <- downloadHandler(
    filename = function(){
      paste0(input$csvdownload, ".csv")
    },
    content = function(file) {
      write.csv(for_chart(), file, row.names = T)
    }
  )
  
}



shinyApp(ui = ui, server = server)
