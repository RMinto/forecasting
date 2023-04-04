###### TIME SERIES FORECASTING WITH SHINY ###### 

fifa_UI <- function(id){
  
  ns <- NS(id)
  
  fluidPage( 
    # img(src = "EA_Sports.png", height = 250, width = 250,
    #     style="display: block; margin-left: auto; margin-right: auto;"),
    
    #place the WWCE Foreast in the middle
    titlePanel(h1("FIFA Contact Forecast", align = "center")),
    
    fluidRow(  
      sidebarLayout(
        
        sidebarPanel(
          img(src = "EA_fifa.png", height = 250, width = 250, style="display: block; margin-left: auto; margin-right: auto;"),
          tags$hr(),
          ###################
          
          conditionalPanel(
            condition = "input.timeSeriesTabs == 1",
            selectInput(ns("series1"), "Fiscal Month Selection", choices = names(plots.list), selected = tail(names(plots.list), n = 1))
          ),
          
          conditionalPanel(
            condition = "input.timeSeriesTabs == 6",
            selectInput(ns("series2"), "Fiscal Month Selection", choices = names(plots.compare.list), selected = tail(names(plots.compare.list), n = 1))
          ),
          ###################
          br(),br(),
          width = 3
        ),
        
        mainPanel(
          tabsetPanel(
            
            #panel for 18 months rolling forecast
            tabPanel("Rolling 18 Month Forecast", icon = icon("line-chart"),
                     br(), withSpinner(plotlyOutput(ns("p_forecast"), width = '120%'), type = 6), 
                     br(), br(),
                     uiOutput(ns('downloadData')),
                     br(),
                     DT::dataTableOutput(ns("forecast_table")), 
                     value = 1), 
            
            
            #panel for individual forecast visualization by channels
            tabPanel('Channel Based Forecast',
                     icon = icon("line-chart"),
                     selectInput(
                       ns('Channel'),
                       label = '',
                       choices = c('Chat', 'Email', 'Phone', 'TOS Dispute', 'TOS Petition'),
                       selected = plot.channel.list[1],
                       multiple = FALSE),
                     withSpinner(plotlyOutput(ns("channel_plot"), width = '100%'), type = 6),
                     value = 2),


            #panel for language splits
            tabPanel("Language Splits", icon = icon("line-chart"),
                     br(), br(),
                     withSpinner(DT::dataTableOutput(ns("forecast_splits_table")), type = 6),
                     uiOutput(ns('downloadSplitsData')),
                     value = 3),

            #panel for Player Engagement
            tabPanel('Player Engagement',
                     icon = icon("line-chart"),
                     selectInput(
                       ns('Engagement_Metric'),
                       label = '',
                       choices = c('Weekly Average User', 'Year on year Weekly Average User', 'Weekly Contact Rate', 'Year on year Weekly Contact Rate'),
                       selected = plot.engagement.list[1],
                       multiple = FALSE),
                     withSpinner(plotlyOutput(ns("engagement_plot"), width = '100%'), type = 6),
                     br(),
                     #############
                     DT::dataTableOutput(ns("JAAS_tickets_table")),
                     #############
                     value = 4),

            #panel for forecast comparison
            tabPanel("Actual Vs Forecast",
                     icon = icon("line-chart"),
                     selectInput(
                       ns('Accuracy_Plot'),
                       label = '',
                       choices = c('Actual Vs Forecast with Back Projection', 'Actual Vs Forecast'),
                       selected = plot.actvsfor.list[1],
                       multiple = FALSE),
                     br(), withSpinner(plotlyOutput(ns("p_accuracy"), width = '100%'), type = 6), br(),
                     br(),
                     value = 5),

            #panel for forecast comparison
            tabPanel("A/B Forecast Comparison", icon = icon("line-chart"),
                     br(), withSpinner(plotlyOutput(ns("p_compare"), width = '115%'), type = 6),
                     br(),br(),
                     value = 6),

            br(),
            
            id = "timeSeriesTabs"
          ), # end tabsetpanel - end collection of tab panels
          width = 9
        )
      )           
    ))
}

fifa_server <- function(input, output, session) {
  
  #Gdrive url link
  url_contact_forecast <- a("FIFA Contact Forecast", href="https://drive.google.com/drive/folders/1wjr7mU2QjU1sl3kGF4tcO1FIzTAwvQqS", target="_blank")
  url_contact_forecast_splits <- a("FIFA Contact Forecast Language Splits", href="https://drive.google.com/drive/folders/1hrqdwdz7e5ovkkZznPXQVVJKAQaCcU81", target="_blank")
  
  output$p_forecast <- renderPlotly({
    Sys.sleep(2)
    ggplotly(plots.list[[input$series1]], tooltip = c("Date", "Actuals", "Forecast", "upper", "lower")) %>%
      plotly::layout(hovermode = 'compare')
    
  })
  
  output$p_compare <- renderPlotly({
    Sys.sleep(2)
    ggplotly(plots.compare.list[[input$series2]] , tooltip = c("Date", "Actuals", "Forecast", "upper", "lower")) %>%
      plotly::layout(hovermode = 'compare')
  })
  
  output$p_accuracy <- renderPlotly({
    if (input$Accuracy_Plot == 'Actual Vs Forecast with Back Projection'){
      Sys.sleep(2)
      ggplotly(p.histvfrcst.with.fitted)
    }else if(input$Accuracy_Plot == 'Actual Vs Forecast'){
      Sys.sleep(2)
      ggplotly(p.histvfrcst.without.fitted)
    }
  }) 
  
  output$channel_plot <- renderPlotly({
    if (input$Channel == 'Chat'){
      Sys.sleep(2)
      ggplotly(p.chat) %>% layout(height = 1500, width = 2000)
    }else if(input$Channel == 'Email'){
      Sys.sleep(2)
      ggplotly(p.email) %>% layout(height = 1500, width = 2000)
    }else if(input$Channel == 'Phone'){
      Sys.sleep(2)
      ggplotly(p.phone) %>% layout(height = 1500, width = 2000)
    }else if(input$Channel == 'TOS Dispute'){
      Sys.sleep(2)
      ggplotly(p.dispute) %>% layout(height = 1500, width = 2000)
    }else if(input$Channel == 'TOS Petition'){
      Sys.sleep(2)
      ggplotly(p.petition) %>% layout(height = 1500, width = 2000)
    }
  })
  
  output$engagement_plot <- renderPlotly({
    if (input$Engagement_Metric == 'Weekly Average User'){
      Sys.sleep(2)
      ggplotly(p.engagement.wau) %>% layout(height = 1500, width = 2000)
    }else if(input$Engagement_Metric == 'Year on year Weekly Average User'){
      Sys.sleep(2)
      ggplotly(p.engagement.yoy.wau) %>% layout(height = 1500, width = 2000)
    }else if(input$Engagement_Metric == 'Weekly Contact Rate'){
      Sys.sleep(2)
      ggplotly(p.engagement.cr)
    }else if(input$Engagement_Metric == 'Year on year Weekly Contact Rate'){
      Sys.sleep(2)
      ggplotly(p.engagement.yoy.cr) %>% layout(height = 1500, width = 2000)
    }
  })   
  
  
  output$JAAS_tickets_table <- DT::renderDataTable({
  if(input$Engagement_Metric == 'Weekly Contact Rate'){
    df.JAAS.tickets%>%
      datatable(
        filter = 'top',
        options = list(
          scrollX = TRUE,
          paginate = T,
          lengthMenu = c(5, 10, 15),
          pageLength = 5
        )) %>% DT::formatStyle(columns = names(df.JAAS.tickets), color="black")
  }
  })
  
  output$forecast_table <- DT::renderDataTable({
    if(input$series1 == tail(names(plots.list), n = 1)) {
      df.preds %>%
        datatable(
          filter = 'top',
          options = list(
          scrollX = TRUE,
          paginate = T,
          lengthMenu = c(5, 10, 15),
          pageLength = 5
        )) %>% DT::formatStyle(columns = names(df.preds), color="black")
    }
  })
  
  output$forecast_splits_table <- DT::renderDataTable({
    Sys.sleep(2)
    df.preds.splits%>%
      datatable(
        filter = 'top',
        options = list(
        scrollX = TRUE,
        paginate = T,
        lengthMenu = c(5, 10, 15),
        pageLength = 5
      )) %>% DT::formatStyle(columns = names(df.preds.splits), color="black")
  })
  
  output$downloadData <- renderUI({
    tagList("Contact Forecast Download Location:", url_contact_forecast)
  })
  
  output$downloadSplitsData <- renderUI({
    tagList("Contact Forecast Splits Download Location:", url_contact_forecast_splits)
  })
  
}