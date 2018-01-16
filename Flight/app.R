library(dplyr)
library(treemap)
library(MASS)
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp")
git_pkgs = git_pkgs_load = c("streamgraph","DT")
load_pkgs = c(load_pkgs, git_pkgs_load)
pkgs_loaded = lapply(load_pkgs, require, character.only=T)
air = fread('data/2008_subset_trial.csv')
air <- as.data.frame(air)
save(air, file = 'air.RDdata')
load('air.RDdata')
flights <- read.csv("data/2008_subset_trial.csv", header = TRUE)
flights <- air



#Total no. of flights by Unique Carrier
UniqueCarrier.freq <- sort(table(flights$UniqueCarrier))

color<- c("red","blue","green","orange","white","black")

#Cancellation by Month
cancelled <- flights[,"Cancelled"] == 1
cancel <- flights[cancelled,]

#print(dim(cancel)[1]/dim(data)[1])

cancel %>% 
  group_by(Month) %>% 
  tally() -> month

cancel %>% 
  group_by(UniqueCarrier) %>% 
  tally() -> carrier

#average delay time for each carrier arrival delay
count <- table(air$UniqueCarrier)
DTsum <- aggregate(ArrDelay ~ UniqueCarrier, air[which(air$ArrDelay > 0),], sum)
DTsum[2] <- DTsum[2]/count
sortedDTsum <- DTsum[order(DTsum$ArrDelay),]
sortedDTsum
summary(sortedDTsum$ArrDelay)


#average delay time for each carrier departure delay
DTsum_dep <- aggregate(DepDelay ~ UniqueCarrier, air[which(air$DepDelay > 0),], sum)
DTsum_dep[2] <- DTsum_dep[2]/count
sortedDTsum_dep <- DTsum_dep[order(DTsum_dep$DepDelay),]



#cancellation by day of week
cancel %>% 
  group_by(DayOfWeek) %>% 
  tally() -> day


#Average Arrival Delay at Departure Airport
DTsum_dest <- aggregate(ArrDelay ~ Dest, air[which(air$ArrDelay > 0),], sum)
DTsum_dest[2] <- DTsum_dest[2]/count
sortedDTsum_dest <- DTsum_dest[order(DTsum_dest$ArrDelay,decreasing = T),]


#Average Departure Delay at Origin Airport
DTsum_origin <- aggregate(DepDelay ~ Dest, air[which(air$DepDelay > 0),], sum)
DTsum_origin[2] <- DTsum_origin[2]/count
sortedDTsum_origin <- DTsum_origin[order(DTsum_origin$DepDelay,decreasing = T),]


#Cancellation at Origin Airport
cancel %>% 
  group_by(Origin) %>% 
  tally() -> Origin

head(flights)
library(maps)
library(mapproj)
source("helpers.R")



ui <- navbarPage(
  title="Flight Data For United States - 2008",
  
  tabPanel("Home",
           
           
           fluidPage(
             titlePanel("Maths For Data Analytics"),
             sidebarLayout(
               sidebarPanel(
                 h2("Assignment"),
                 p("This application has been designed for the purpose of helping users choose a flight and/or analyze the data."),
                 code('Submitted by Nishant Gupta & Kartik Nagras'),
                 br(),
                 br(),
                 br(),
                 br(),
                 img(src = "georgian.png", height = 70, width = 200),
                 br(),
                 "Designed and Coded by",code( "Nishant Gupta"), "and powered by ", 
                 span("RStudio", style = "color:blue")
               ),
               mainPanel(
                 h1("Airplane Data For US "),
                 p("The Data was obtained from Bureau of Transportation Statistics US.The data gives the information regarding all flights flying in the US for the year 2008,representing 20 unique commercial airlines and 7,00,9728 (Approx ~7 million) obsservations. "), 
                    
                 
                 br(),
                 p(strong("Data Summary: "),br(),br(),
                   "No.of Unique Carriers : 20",
                   br(),
                   "No. of Unique flights: 7131",
                   br(),
                   "No : of Airports: 286",br(),br(),
                   a("Click Here For Data Source",href = "http://stat-computing.org/dataexpo/2009/",target="_blank"))
                
                )
             )))
           
           
           
           
           
  
  
  
  ,
  tabPanel('Data',     
  
  fluidRow(
    column(4,
           selectInput("ori",
                       "Origin:",
                       c("All",
                         unique(as.character(flights$Origin))))
    ),
    column(4,
           selectInput("mon",
                       "Month:",
                       c("All",
                         unique(as.character(flights$Month))))
    ),
    column(4,
           selectInput("cari",
                       "Carrier:",
                       c("All",
                         unique(as.character(flights$UniqueCarrier))))
    )
  ),
  
  fluidRow(
    DT::dataTableOutput("table")
  )),

  tabPanel('Data Display',
           fluidPage(
             sidebarLayout(
             
             # Copy the line below to make a select box 
             sidebarPanel( radioButtons("radio", label = h3("Graphs"),
                          choices = list("Number of Flights for each Commercial Carrier" = 1, "Month wise Cancellations " = 2, "Commercial Carrier wise Cancellations" = 3, "Commercial Carrier wise Arrival Delay" = 4, "Commercial Carrier wise Departure Delay" = 5, "Day of the Week wise Cancellations" = 6, "Arrival Delay at Destination Airport" = 7, "Departure Delay at Origin Airport" = 8, "Cancellations at Origin Airport" = 9), 
                          selected = 1)
             
             
             
           ),
           
           
             
  
  
   mainPanel(        
             
  plotOutput(outputId = "main_plot", height = "100px")
  
  )))))
  
  

server <-function(input, output) {
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- flights
    if (input$ori != "All") {
      data <- data[data$Origin == input$ori,]
    }
    if (input$cari != "All") {
      data <- data[data$UniqueCarrier == input$cari,]
    }
    if (input$mon != "All") {
      data <- data[data$Month == input$mon,]
    }
    data
  }))
  
  
  
  
  
  
  output$main_plot <- renderPlot({
    
    if(input$radio == '1')
    {
    barplot(UniqueCarrier.freq, col = color,main = "Numer of Flights for each Commercial Carrier",
            xlab = "Commercial Carrier", ylab = "Cont of Flights")
    }
  
    
    if(input$radio == '2')
    {
    colnames(month)<- c("Month","No. of Cancellations")
    month$MonthAbb <- month.abb
    treemap(month,
            index=c("MonthAbb"),
            vSize="No. of Cancellations",
            vColor = "No. of Cancellations",
            type="value",
            title = "Month wise Cancellations - Sorted by Number of Cancellations")
 
    }
    if(input$radio == '3')
    {
      colnames(carrier) <- c("Commercial Carrier","No. of Cancellations")
    treemap(carrier,
            index=c("Commercial Carrier"),
            vSize="No. of Cancellations",
            vColor = "No. of Cancellations",
            type="value",
            title = "Commercial carrier wise Cancellations - Sorted by Number of Cancellations")
    
    }
  
    if(input$radio == '4')
    {
      colnames(DTsum) <- c("Commercial Carrier","Average Arrival Delay")
    treemap(DTsum,
            index=c("Commercial Carrier"),
            vSize="Average Arrival Delay",
            vColor="Average Arrival Delay",
            type="value",
            title = "Commercial Carrier wise Arrival Delay - Sorted by Average Arrival Delay")
    
    
    }
    if(input$radio == '5')
    {colnames(DTsum_dep) <- c("Commercial Carrier","Average Departure Delay")
    
    
    treemap(DTsum_dep,
            index=c("Commercial Carrier"),
            vSize="Average Departure Delay",
            vColor="Average Departure Delay",
            type="value",
            title = "Commercial Carrier wise Departure Delay - Sorted by Average Departure Delay")
    
    }
 
    
    
    if(input$radio == '6')
    { 
      day$Day = c("Monday", "Tuesday", "Wednesday", 
                  "Thursday", "Friday", "Saturday", "Sunday")
      colnames(day) = c("Day of the week","Cancellations","Day")
      treemap(day,
              index=c("Day"),
              vSize="Cancellations",
              vColor = "Cancellations",
              type="value",
              title = "Day Of the Week wise Cancellations - Sorted by No. of Cancellations")
      
    }
    
    
 
    
    if(input$radio == '7')
    {
    
    DTsum_dest$avg <- (DTsum_dest$ArrDelay/(nrow(DTsum_dest)+280)*100)
    colnames(DTsum_dest) <- c("Destination Airport","Arrival Delay","Average Arrival Delay")
    treemap(DTsum_dest,
            index=c("Destination Airport"),
            vSize="Average Arrival Delay",
            vColor="Average Arrival Delay",
            type="value",
            title="Arrival Delay at Destination Airport - Sorted by Average Arrival Delay")#actual value or percentage value
    
    }
    if(input$radio == '8')
    {
      colnames(DTsum_origin) <- c("Origin Airport","Average Departure Delay")
    treemap(DTsum_origin,
            index=c("Origin Airport"),
            vSize="Average Departure Delay",
            vColor="Average Departure Delay",
            type="value",
            title="Departure Delay at Origin Airport - Sorted by Average Departure Delay")#actual value or percentage value
    
    
    }
    
  
    
    if(input$radio == '9')
    {  
    colnames(Origin) <- c("Origin Airport","Number of Cancellations")
    treemap(Origin,
            index=c("Origin Airport"),
            vSize="Number of Cancellations",
            vColor = "Number of Cancellations",
            type="value",
            title = "Number of Cancellations at Origin Airport - Sorted by Number of Cancellations")
  
    }
      },width=800, height=600)
  
}

shinyApp(ui = ui, server = server)