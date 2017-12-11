#this shiny app is intended to show 3 ways of fetching data

library(shiny)
library(readr)
library(leaflet)
library(htmltools)
library(plotly)

library(dplyr)
library(DBI)


library(googlesheets)
pittbusiness = read.csv("pitt.csv")
pal1 <- colorNumeric(palette = "Blues",domain = pittbusiness$stars)

mydb = dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "yelp_db",
  host = '45.63.90.29',
  username = "mssp",
  password = "mssp2017"
)
tablelist = dbListTables(mydb)

(mylist = gs_ls())

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("How do I load data for my shiny apps", windowTitle = "Peace"),
  navbarPage(
    "=。=!",
    theme = "bootstrap.min.css",
    tabPanel(
      "within source code",
      sidebarLayout(
        sidebarPanel(
          actionButton(inputId = "no1",label = "Plot!"),
          width = 3
          ),
        mainPanel(
          p("The simplest way to get data into an application is by uploading a CSV, 
            RData or other data file directly with the application source code.
            This is usually best for data files that do not change very often as updating or adding data requires redeploying the application. 
            Uploads of this type are done via secure HTTPS by default. 
            Currently there is a limit of 100MB for application uploads."),
          hr(),
          leafletOutput("map1")
          )
      )
    ),

    tabPanel(#upload tab
      "Upload Local File",
      sidebarLayout(
        sidebarPanel(
          width = 4,
          fileInput("file1", "Choose CSV File",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),

          tags$hr(),
          actionButton(inputId = "no2", label = "Plot!")
          ),
        mainPanel(
          p("Another simple approach is to use the shiny file upload functionally
            allowing the end user of an application to upload a CSV or other data file.
            In this model, the end user can directly upload a file to the application
            which can be analyzed. This is usually best for small data sets.
            Uploads of this type are always done via secure HTTPS.
            Currently limit uploads of this type to 32MB."),
          hr(),
          tableOutput("table1"),
          plotOutput("plot1"),
          plotlyOutput("plotly1"),
          verbatimTextOutput("movie")
        )
      )
    ),#end upload tab
    
    tabPanel(#fetching from remote
      "from SQL db",
      sidebarLayout(
        sidebarPanel(
          tableOutput("tablelist"),
          hr(),
          selectInput("choosetbl",label = "show tbl's col", choices = tablelist)
        ),
        mainPanel(
          p("The most complex approach would be fetching the data
            from a remote database or other data source.
            Shiny applications can be written to dynamically pull data
            from a SQL database or other API. "),
          hr(),
          tableOutput("tbl"),
          hr(),
          tableOutput("headoftbl"),
          hr(),
          p("businesses in Cleveland"),
          leafletOutput("Cleveland")
        )
      )
    ),
    tabPanel(
      "Google Sheets",
      sidebarLayout(
        sidebarPanel(
          tableOutput("sheetlist"),
          hr(),
          selectInput("select1",label = "Which sheet",choices = mylist$sheet_title),
          actionButton("no3", "Print!")
        ),
        mainPanel(
          verbatimTextOutput("sheet_info"),
          tableOutput("googlesheettable")
          #DT::dataTableOutput("the_data")
        )
      )
    )
  )# END navbarPage
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  v1 <- reactiveValues(data = NULL)
  observeEvent(input$no1, {
    v1$data <- pittbusiness
  })   
  
  output$map1 <- renderLeaflet({
    if(is.null(v1$data))return()
    leaflet(v1$data) %>% addTiles() %>% 
      setView( lng = -79.99589, lat = 40.44062, zoom = 12 ) %>% 
      addTiles() %>% 
      addCircleMarkers(
        ~longitude,~latitude,
        radius = 1,
        color = ~pal1(stars),
        fillOpacity = 0.5, label = ~htmlEscape(name)) %>%
      addLegend("bottomright", pal = pal1, values = ~stars,
                title = "Stars",
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  
  v2 <- reactiveValues(data = NULL)
  observeEvent(input$no2,{
    #Ensure that values are available 
    #("truthy"–see Details) before proceeding with a calculation or action.
    req(input$file1)
    movie <- read.csv(input$file1$datapath)
    movie$X = NULL
    movie$yearlabel = as.character(movie$year)
    movie$yearlabel = gsub(pattern = "-.*", replacement = "", movie$year)
    movie$year = as.Date(movie$yearlabel,"%Y")
    colnames(movie)[4] = "n/million"
    
    v2$data <- movie
    
  })
  
  output$table1 <- renderTable({
    if(is.null(v2$data))return()
    head(v2$data[,-3])
  })
  
  output$plot1 <- renderPlot({
    if(is.null(v2$data))return()
    ggplot(v2$data) + geom_point(aes(x = year, y = rating, col = `n/million`))    
  })
  
  output$plotly1 <- renderPlotly({
    if(is.null(v2$data))return()
    ggplot(v2$data) + geom_point(aes(x = year, y = rating, col = `n/million`))    
  })
  
  output$movie <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else {
      datapoint = as.numeric(d$pointNumber)[1] + 1
      v2$data[datapoint,-3]
    }
  })
  
  output$tablelist <- renderTable({
    dbListTables(mydb)
  })
  output$tbl <- renderTable({
    dbListFields(mydb, input$choosetbl)
  })
  output$headoftbl <- renderTable({
    mydb %>% dbSendQuery(paste0("select * from ", input$choosetbl," limit 5")) %>% fetch(n = -1)
  })
  
  output$Cleveland <- renderLeaflet({
    a = mydb %>% dbGetQuery("select * from business;") %>% filter(state == "OH")
    long = mean(a$longitude)
    lat = mean(a$latitude)
    
    leaflet(a) %>% addTiles() %>% 
      setView( lng = long, lat = lat, zoom = 10 ) %>% 
      addTiles() %>% 
      addCircleMarkers(
        ~longitude,~latitude,
        radius = 1,
        fillOpacity = 0.5, label = ~htmlEscape(name))
  })

   output$sheetlist <- renderTable({
     mylist[,c(1,2)]
   })
  
   v3 <- reactiveValues(data = NULL)
   observeEvent(input$no3, {
     key <- mylist$sheet_key[mylist$sheet_title == input$select1]
     v3$data <- gs_key(key)
   })
#   
   output$sheet_info <- renderPrint({
     if(is.null(v3$data))return()
    print(v3$data)
  })

  output$googlesheettable <- renderTable({
    if(is.null(v3$data))return()
    gs_read(v3$data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

