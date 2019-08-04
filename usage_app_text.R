library("shiny")
library("tidyverse")


our_data <- read.csv("usage_main.csv", header=T, sep = ",")

#the code below works by 'ignoring' all information up to the 8th point in cell when the date begins. This is regex.
our_data$datetime <- sub('...........', '', our_data$datetime)
calculate_range <- range(our_data$dayNumber)

# Rename columns
names(our_data)[names(our_data) == "participant"] <- "Participant"
names(our_data)[names(our_data) == "dayNumber"] <- "DayNumber"
names(our_data)[names(our_data) == "datetime"] <- "Time"
names(our_data)[names(our_data) == "status"] <- "Status"

total <- max(our_data$dayNumber)


ui <- fluidPage(
  # Application title
  titlePanel(h1(id="Title","Mixed Methods App")),
  tags$style(HTML("#Title {color: #1F45FC;}")),
  # Sidebar  
  sidebarLayout(
    sidebarPanel(
      selectInput("personId", "Participant ID:", 
        choices=(unique(our_data$Participant))),
      selectInput("statuses", "Status ID:",
        choices=(unique(our_data$Status)))
      ),
    mainPanel(
      tabsetPanel(
        id = "region",
        tabPanel("usage", DT::dataTableOutput("MyTable1")),
        tabPanel("times", DT::dataTableOutput("MyTable2")),
        tabPanel("themes", DT::dataTableOutput("MyTable3"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$MyTable1 <- DT::renderDataTable({
    DT::datatable(df <- our_data %>%
                    group_by(Status) %>%
                    filter(Participant == input$personId) %>%
                    summarise(Beeps = n()) %>%
                    mutate("Usage in %" = Beeps/ sum(Beeps) * 100) %>%
                    adorn_totals("row"))
    
      # adorn totals = adds in extra row calculating previous parameters into a new row #
    
      
 #   DT::loretable <- DT::datatable() %>% 
      
    })

  output$MyTable2 <- DT::renderDataTable({
    DT::datatable(our_data %>%
                    select(Participant, DayNumber, Time, Status) %>%
                    group_by(Time) %>%
                    filter(Status == input$statuses, Participant == input$personId)) 
  
  })
  
  # Selects themes which are listed one theme per row in tidy format #
  
  output$MyTable3 <- DT::renderDataTable({
    DT::datatable(our_data %>%
                    group_by(Participant) %>%
                    select(Themes) %>%
                    filter(Participant == input$personId)) 
    
  })
  
}
  
shinyApp(ui,server)

