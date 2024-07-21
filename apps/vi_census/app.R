#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr,quietly = TRUE, warn.conflicts = F)


r_root <- paste0("libs/")
data_root <- paste0("data/")
img_root <- "./images/"

source(paste0(r_root,"mid_year_census.R"))

df_bins <- readRDS(paste0(data_root,"age_bins.rds")) %>% mutate(name = gsub("yr_"," years - ",name))
bin_nms <- c("None", "Single year",df_bins %>% pull(name) %>% unique())

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Census Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 2,
                 sliderInput("year",
                             "Years:",
                             min = 1990,
                             max = 2030,
                             value = c(2021,2021),sep = ""
                 )
    ),

    # Show a plot of the generated distribution
    mainPanel(img(src='doh.png', align = "right"),
      tabsetPanel(
        tabPanel("Decennial"),
        tabPanel("Mid-year",
                 fluidPage(
                   fluidRow(
                     column(width=5, style = "font-size: 30px;",
                            textOutput("title_mid")
                     ),
                     column(width=5,
                            selectInput("bins",label = "Age Grouping:",choices = bin_nms)
                     ),
                     column(width=1,
                            actionButton("download", label = "Download", icon = icon("download") )
                     )
                   ),
                   fluidRow(
                     tableOutput("midyear_tbl")
                   )
                 ))

      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$title_mid <- renderText(paste0(input$year, " Mid-Year Census"))

  output$midyear_tbl <- renderTable(
    {
      my_data()
    }
  )

  my_data <- reactive({
    df <- download_midyear(input$year)
    df_bins <- my_bins()

    if(nrow(df_bins)>0) {

      index  <-  sapply(df$AGE, function(age){
        min(which(df_bins$max_age>=age))
      })
      df <- df %>%
        mutate(bin = index )%>%
        group_by(bin) %>%
        summarise(Male = sum(Male), Female = sum(Female), Total = sum(Total)) %>%
        left_join(df_bins, by = c("bin" = "index")) %>%
        rename_with(~"Ages",.cols = matches("description")) %>%
        select(Ages, Female, Male, Total)
    } else if(grepl("^None",input$bins)) {
      df <- df  %>%
        summarise(Ages = "Total", Male = sum(Male), Female = sum(Female), Total = sum(Total)) %>%
        # rename_with(~"Ages",.cols = matches("AGE")) %>%
        select(Ages, Female, Male, Total)

    } else if(grepl("^Single",input$bins)) {
      df <- df %>%
        rename_with(~"Ages",.cols = matches("AGE")) %>%
        select(Ages, Female, Male, Total)
    }
    df
  })


  my_bins <- reactive({
    nm <- input$bins

    df_bins %>% filter(name == nm)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
