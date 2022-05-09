library(fpp3)

library(shiny)
library(plotly)
library(ggeasy)
library(ggpubr)
library(shinydashboard)
file_path <- "/Users/Pmoney/Desktop/bas 475/multiTimeline.csv"
g_trends <- read.csv(file_path, skip = 2)
names(g_trends) <- c("Month","Apple")
g_trends$Month <- yearmonth(g_trends$Month)
g_trends <- tsibble(g_trends)
g_trends$Apple <- as.numeric(
  ifelse(g_trends$Apple == "<1", 0, g_trends$Apple))
TrendsDF <- as.data.frame(g_trends)
p <- ggplot(TrendsDF,aes(Month,Apple)) + geom_line(color = "red")
ggplotly(p)
iphone11 <- g_trends %>%
  filter( year(Month) == 2019) %>%
  autoplot()
iphone <- g_trends %>%
  filter( year(Month) == 2007) %>%
  autoplot()
macbook <- g_trends %>%
  filter( year(Month) == 2006) %>%
  autoplot()
ipad <- g_trends %>%
  filter( year(Month) == 2010) %>%
  autoplot()
airpod <- g_trends %>%
  filter( year(Month) == 2016) %>%
  autoplot()




library(shiny)
library(plotly)
library(ggeasy)
library(ggpubr)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- fluidPage(
  dashboardPage(skin = "green",
    dashboardHeader(title = "Apple's Google Trends " ,
                    dropdownMenu(type = "messages",
                     messageItem(
                     from = "Peyton Hampton",
                     message = "Be sure to check out all of the feature. Hope you enjoy!"
                    ))
                   ),
    dashboardSidebar(width = 275,
      sidebarMenu(
        menuItem("Instructions Page",tabName = "main",icon = icon("cog", lib = "glyphicon") ),
        menuItem("Time Series", tabName = "time", icon = icon("dashboard")),
        menuItem("Major Release Dates", tabName = "compare", icon = icon("calendar")),
        menuItem("Choose a graph",tabName = "trend", icon = icon("folder")),
        menuItem("Plot Interpretations", tabName = "plotint", icon = icon("check")),
        menuItem("Simple Models", tabName = "simp", icon = icon("calendar")),
        menuItem("Exponential Smoothing", tabName = "ETS", icon = icon("check")),
        menuItem("ARIMA Model",tabName = "ARIMA",icon = icon("check"))
      )),
   dashboardBody(tabItems(tabItem(tabName = "main", h2("Instructions on Shiny App"),
                                  verbatimTextOutput("value")),
                          tabItem(tabName = "time", h2("Time Series Plot"), plotlyOutput("apptime"),
                                  verbatimTextOutput("print")),
                          tabItem(tabName = "trend",h2("Choose a graph"),  selectInput("select", "Graph select", 
                           choices=c("Additive Decomp","Multiplicative Decomp","Autocorrelation", "Seasonality")),
                          plotOutput("choices")),
                          tabItem(tabName = "compare",h2("Apple Google trends in years of Major Releases"),
                           selectInput("chooseone", "Select an apple product", 
                                              choices=c("Iphone","Iphone 11","Ipad", "Macbook", "Airpods")),
                                  plotlyOutput("choose")),
                          tabItem(tabName = "plotint", h2("Time Series Plot Interpretations"),
                                  verbatimTextOutput("seasonal"),
                                  verbatimTextOutput("decomp"),
                                  verbatimTextOutput("acf")),
                          tabItem(tabName = "simp", h2("Simple Models"),
                                  selectInput("choosesimp","Select a Graph", choices = c("Naive","Seasonal Naive", "Mean","Drift")),
                                  plotOutput("SIMPOUT")),
                          tabItem(tabName = "ETS",h2("Exponential Smoothing Graphs"),
                                  selectInput("chooseETS","Select A Graph", choices = c("Holts", "Holts/Winters")),
                                  plotOutput("ETSOUT")),
                          tabItem(tabName = "ARIMA",h2("ARIMA Models"), 
                                  selectInput("chooseARIMA", "Select a Graph",choices = c("ARIMA AUTO", "ARIMA MANUAL211")),
                          plotOutput("ARIMAOUT"))
                          
                          
     
   ))))


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$value <- renderText({ "This is my Midterm Shiny App Project. There are 4 tabs on the side 
    that will help you navigate the app. This is app was designed to show the google search trend of the company
    Apple. 
    Tab 1: Instructions
    The first, which is this one, is a simple instructions tab
    that shows what features are in the app.
    
    Tab 2: Time Plot
    In this tab, you will be able to see the entire time series of the search trends.
    
    Tab 3: Major Release Date
    This gives an user to see a singular year's google trends of years with different releases of Apple Products.
    
    Tab 4: Choose a graph
    This tab allows users to see the seasonality, autocorrelation, and decomposition of the google trends of Apple.
    
    Tab 5: Plot Interpretations:
    
    THis tab shows an in-depth interpretation of the time series objects. 
    "})
    
output$apptime <- renderPlotly(
  p + labs(title= "Apple's Google Search Trends", 
                                      y = "Amount of Interest", x = "Months") + geom_line(color = "green")
)


output$choices <- renderPlot({
  
  if(input$select == "Additive Decomp"){g_trends %>% 
      model(classical_decomposition(Apple, type = "additive")) %>%
      components() %>%
      autoplot() + labs(title = "Classical additive decomposition of Apple Google Trends")
  }else if
  (input$select == "Multiplicative Decomp"){g_trends %>% 
      model(classical_decomposition(Apple, type = "multiplicative")) %>%
      components() %>%
      autoplot() + labs(title = "Classical multiplicative decomp of Apple Google Trends.")}else if
  (input$select == "Autocorrelation"){acf(g_trends)} else if
  (input$select == "Seasonality"){gg_season(g_trends) + labs(title = " Seasonality of Apple's Google Trends")} 
})

output$print <- renderText({"Time Series Interpretation:
  This time series covers data from January 2004 to March 2022. See the Plot interpretations tab for an in-depth interpretation
  of the time series objects. 
  "})


output$choose <- renderPlotly({
  if(input$chooseone == "Iphone"){
    iphone + labs(title = "Iphone Release Date:June 29th") + geom_line(color = "blue")
  }else if
  (input$chooseone == "Iphone 11"){
    iphone11 + labs(title = "Iphone 11 Release Date: September 20th") + geom_line(color = "red")
  }else if
  (input$chooseone == "Ipad"){
    ipad +labs(title = "Ipad release date: April 3rd ") + geom_line(color = "black")
  } else if
  (input$chooseone == "Macbook"){
    macbook + labs(title = "MacBook release date: May 16th")+ geom_line(color = "orange")
  } else if
  (input$chooseone == "Airpods"){
    airpod + labs(title = "Airpods Relase date: December 13th") + geom_line(color = "purple")
  }
})

output$seasonal <- renderText({
  "Seasonality:
  
  Apple's googles trends does experiences seasonality. In the range of August - October, the interest
  level tends to be higher than the average amount of interest.Also, in December, the interest seems to rise
  above average consistently. This may have to do with the holiday season and Apple products being common gifts given."
})

output$acf <- renderText({
  "Autocorrelation:
  
  The variation in the g_trends data does not seem to be white noise. White noise is the variation
  in the data that is not explained by the regression model. It seems there is very large variation that is
  not explanied by the regression model. One thing we can do to change this is to gather more data points.
  We are only collecting only 1 data point per month, and if we had more data points, it would be possible that the
  data would become a white noise series. 
  "
})

output$decomp <- renderText({
  "Decomposition:
  
  We ran 2 different decomposition models : Additive and Multiplicative.
  
  One of the main differences in the axis in the random between the 2 decomps. The multiplicative is a much smaller difference
  meaning that random variation accounts for a much smaller amound of variation which a good thing. 
  
  This is also the case for the seasonal tab. Multiplicative decomp has a much smaller axis than the additive variation. THe gray bar size
  seems to be similar for both ( its all about perspective.). Since the axis size is smaller for multiplicative,
  this means that the change is not as large. 
  
  Both decomps show the trend is increasing over time and there is a peak in the Apple row around 2019 ( we see this in the autoplot).
  
  After seeing both decomps, the multiplicative decomposition is better in this case since a majority of the variation is not based on randomness unlike
  what we see in the additive decomposition. 
  
  "
})

output$ETSOUT <- renderPlot({
  if(input$chooseETS == "Holts"){g_trends %>%
      model(ETS( ~ error("A") + trend("A") + season("N"))) %>%
      forecast(h = 12) %>%
      autoplot(g_trends)}else
        g_trends %>%
    model(ETS( ~ error("A") + trend("A") + season("A"))) %>%
    forecast(h = 12) %>%
    autoplot(g_trends)
})


output$SIMPOUT <- renderPlot({
  if(input$choosesimp == "Naive"){g_trends %>% naive(h=12) %>% autoplot()}else
    if(input$choosesimp == "Seasonal Naive"){g_trends %>% snaive(h=12) %>% autoplot()}else
      if(input$choosesimp == "Mean"){g_trends %>% model(MEAN()) %>% forecast(h= 12) %>% autoplot(g_trends)}else
      if(input$choosesimp == "Drift"){g_trends %>% model(RW(~drift())) %>% forecast(h = 12) %>% autoplot(g_trends)}
  
})



output$ARIMAOUT <- renderPlot({
  if(input$chooseARIMA == "ARIMA AUTO"){g_trends %>% model(ARIMA()) %>%  forecast(h = 12) %>% autoplot(g_trends)}else
    if(input$chooseARIMA == "ARIMA MANUAL211"){g_trends %>% model(ARIMA( ~ pdq(2,1,1))) %>% forecast(h = 12) %>% autoplot(g_trends)}
  
})

}

# Run the application 
shinyApp(ui = ui, server = server)
