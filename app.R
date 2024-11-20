
library(shiny)
library('plot.matrix')
library(viridisLite)

v <- readRDS("./www/v.RData")
P <- readRDS("./www/P.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Optimal policy visualiser"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("leg1",
                        "Inventory leg 1:",
                        min = 0,
                        max = 20,
                        value = 20),
            sliderInput("leg2",
                        "Inventory leg 2:",
                        min = 0,
                        max = 40,
                        value = 40),
            sliderInput("day",
                        "days:",
                        min = 1,
                        max = 100,
                        value = 1)
        ),

        # Show a plot of the generated policy
        mainPanel(
          tabsetPanel(
            tabPanel("Optimal Policy",plotOutput("policyPlot1")),
            tabPanel("Value function heatmap",plotOutput("heatmap"),
                     textOutput("exp_val"))
          )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$policyPlot1 <- renderPlot({
      par(mfrow=c(3,1))
      plot(x=seq(1,100),y=P[input$leg1+1,input$leg2+1,1:100,1],lwd=2,col="red",type = "lines",
           ylab="Reject=0, Accept=1",xlab="",main="Optimal policy for booking FF to LDN",
           ylim = c(0,1),yaxt='n')
      axis(2, labels = c(0,1), at = c(0,1))
      abline(v=input$day,lwd=2,lty=2,col="red")
      plot(x=seq(1,100),y=P[input$leg1+1,input$leg2+1,1:100,2],lwd=2,col = "blue",type="lines",
           ylab="Reject=0, Accept=1",xlab="",main="Optimal policy for booking LDN to NY",
           ylim = c(0,1),yaxt='n')
      axis(2, labels = c(0,1), at = c(0,1))
      abline(v=input$day,lwd=2,lty=2,col="blue")
      plot(x=seq(1,100),y=P[input$leg1+1,input$leg2+1,1:100,3],lwd=2,col="black",type="lines",
           ylab="Reject=0,Accept =1",xlab="",main="Optimal policy for booking FF to NY",
           ylim=c(0,1),yaxt='n')
      axis(2, labels = c(0,1), at = c(0,1))
      abline(v=input$day,lwd=2,lty=2,col="black")
    })
    
    output$heatmap <- renderPlot({
      par(mar=c(5.1, 4.1, 4.1, 5.1)) # adapt margins
      plot(v[,,input$day], breaks = range(v[,,input$day]),fmt.key = "%#.3g",
           xlab = "Leg 2 inventory",
           ylab = "Leg 1 inventory",
           main = paste("Expected values at time ",input$day),
           #col = rainbow(11))
           col = viridis(11))
      rect(xleft = input$leg2+1-0.5, xright = input$leg2+1+0.5, ybottom = 21.5-1-input$leg1, ytop = 22.5-1-input$leg1,border="red",lwd=2)
    })
    
    output$exp_val <- renderText({paste("Expected value = $",
                                       round(v[input$leg1+1,input$leg2+1,input$day],2))})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
