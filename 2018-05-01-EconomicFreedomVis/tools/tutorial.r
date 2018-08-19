if(FALSE){
  # install pacakges
  devtools::install_github("jphelps13/small.projects",
                           subdir = "2018-05-01-EconomicFreedomVis")
  devtools::install_github("ropensci/plotly")
}

# data and script code
library(utils.economicfreedomvis)

# data manipulation 
library(stringr)
library(magrittr)
library(data.table)

# shiny and plotting libraries
library(shinythemes)
library(shiny)
library(plotly)
library(RColorBrewer)

# create package data
create <- FALSE
if(create){
  evf_data <- cleanData(file = file.path(getwd(), "data/raw", "data.csv"))
  devtools::use_data(evf_data)
}

# lookups used in the shiny app
year_range    <- range(evf_data$Year)
all_countries <- sort(unique(evf_data$Countries))
all_metrics   <- c("FreedomToTradeInternationally", "LegalSystemAndPropertyRights", 
                   "Regulation", "SizeOfGovernment", "SoundMoney", "SummaryIndex")
# display in the UI with spaces
display_metrics <- stringr::str_replace_all(all_metrics,
                                            "([A-Z]{0,1}[^A-Z]*)([A-Z]*)",
                                            "\\1 \\2") %>% trimws(.)
# for radar plot, specify where line breaks are
display_metrics2 <- c("Freedom To\nTrade\nInternationally", "Legal System And\nProperty Rights", 
                      "Regulation", "Size Of\nGovernment", "Sound Money", "Summary Index"
)
# remove summary index for these lookups - metrics for comparing with summaryIndex
m <- charmatch("SummaryIndex", all_metrics)
all_metricsv2     <- all_metrics[-m]
display_metricsv2 <- display_metrics[-m]
display_metrics2v2 <- display_metrics2[-m]

# smooth and impute missing values with splines
evf_data_clean <- preparePlotData(evf_data, all_metrics)


plot_control <- list(
  palette = RColorBrewer::brewer.pal(3, "Set1"),
  font_colour = "white",
  grid_colour = "#5f606b",
  bg_colour = "#37383d",
  paper_colour = "transparent"
)

ui <- fluidPage(
  
  # Top row with information and controls
  fluidRow(
    # app information
    column(5,
           h2("Summary Index Explorer"
           ),
           h4("Choose 2 Metrics & 1-3 Countries", a("Link to original", 
                                                    href="https://www.fraserinstitute.org/economic-freedom/map?geozone=world&page=map&year=2007")),
           h4("Radar chart will show the metrics for the most recent Year in the slider"),
           h4("Data shown is smoothed with natural splines, with max deviation of 0.005 to the raw observation")
    ),
    # interface control for selecting countries, metrics and year range, with defaults
    column(2, 
           HTML("<br/>"),
           selectInput("countries", "Countries", all_countries, multiple = TRUE,
                       selected = c("Peru", "Argentina", "Brazil"))
    ),
    column(2,
           HTML("<br/>"),
           selectInput("dis_metrics", "Metrics", display_metrics, multiple = TRUE,
                       selected = c("Freedom To Trade Internationally",
                                    "Summary Index"))
    ),
    column(3,
           HTML("<br/>"),
           sliderInput("years", "Years", min = year_range[1],
                       max = year_range[2], 
                       value = c(year_range[1], year_range[1]+2), step = 1)
    )
  ), 
  
  # plotly outputs in second row
  fluidRow(
    column(4,
           plotlyOutput("slopePlotly", height = "255px", width = "100%"),
           plotlyOutput("radarPlotly", height = "370px", width = "100%")
    ),
    column(8,
           plotlyOutput("plotPlotly", height = "625px", width = "100%")
    )
  ),
  theme = shinytheme("darkly") # using dark theme
)



server <- function(input, output, session) {
  
  # change year slider range based on countries selected
  observe({
    if(length(input$countries) > 0){
      check <- evf_data_clean[J(input$countries), range(Year)]
      updateSliderInput(session, "years", min = check[1], max = check[2], step = 1)
    }
  })
  
  # reactive data sets based on country, years and metrics
  dt0 <- reactive({
    evf_data_clean[J(input$countries),]
  })
  dt1 <- reactive({
    dt0()[Year %in% input$years[1]:input$years[2],]
  })
  dt2 <- reactive({
    dt0()[Year == input$years[2],]
  })
  c_length <- reactive({
    length(input$countries)
  })
  m_length <- reactive({
    length(input$dis_metrics)
  })
  dt_metrics <- reactive({
    all_metrics[charmatch(input$dis_metrics, display_metrics)]
  })
  
  
  # scatter plot with yearly "snake" trail
  output$plotPlotly <- renderPlotly({
    if(m_length() == 2 & c_length() %in% 1:3){
      plotlyTimeTrackPlot(copy(dt1()), input$countries, input$dis_metrics, dt_metrics(),
                          input$years, plot_control)
    }
  })
  # time series line plot of summary index
  output$slopePlotly <- renderPlotly({
    if(c_length() %in% 1:3){
      plotlySlopeGraph(copy(dt1()), plot_control, input$countries,
                       metric = "SummaryIndex",
                       display_metric = "Summary Index")
    }
  })
  # radar plot of most recent values for each metric
  output$radarPlotly <- renderPlotly({
    if(c_length() %in% 1:3){
      plotlyRadarChart(dt2(), all_metricsv2, display_metrics2v2, input$countries, 
                       plot_control)
    }
  })
}


# run the shiny app in local browser
shinyApp(ui, server)


