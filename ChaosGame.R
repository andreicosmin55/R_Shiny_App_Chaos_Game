library(shiny)
library(shinythemes)


# user interface

ui <- fluidPage(theme = shinytheme("yeti"),
                navbarPage(
                  "Chaos Game",
                  tabPanel(
                    "About",
                    h2("How It Works"),
                    p("An algorithm originally described by Barnsley in 1988. 
                    Pick a point at random inside a regular n-gon. 
                    Then draw the next point a fraction r of the distance between it 
                    and a polygon vertex picked at random. 
                    Continue the process (after throwing out the first few points). 
                    The result of this 'chaos game' is sometimes, but not always, a fractal. 
                    The results of the chaos game are shown below for several values of (n,r).\n"),
                    uiOutput("img",
                             height = "400px", 
                             width = "400px"),
                    p("Source: https://mathworld.wolfram.com/ChaosGame.html")
                    
                  ),
                  tabPanel(
                    "Simulation",
                      sidebarPanel(
                        sliderInput(
                          inputId = "no_vertex",
                          label = "Number of vertexes: ",
                          min = 3,
                          max = 20,
                          value = 3),
                        sliderInput(
                          inputId = "r",
                          label = "Fraction: ",
                          min = 0.00,
                          max = 1.00,
                          value = 0.5),
                        sliderInput(
                          inputId = "n",
                          label = "Number of Points: ",
                          min = 5000,
                          max = 20000,
                          value = 10000,
                          step = 500),
                        selectInput("prev", 
                                    label = "Current vertex cannot be chosen in the next iteration:",
                                    choices = list("No" = "no","Yes" = "yes"),
                                    selected = TRUE)
                    ),
                    
                      mainPanel(
                        h1("Scatter Plot"),
                        plotOutput(outputId = "distPlot", 
                                   height = "600px", 
                                   width = "600px")
                      )
                    )
                  
                ) 
)

# server

server <- function(input, output){
  
  output$img <- renderUI({
    tags$img(src = "https://mathworld.wolfram.com/images/eps-svg/ChaosGame_1000.png")
  })
  
  output$distPlot <- renderPlot({
    
    width = 500;
    height = 500;
    
    vertexx = vector();
    vertexy = vector();
    
    centrex = 0;
    centrey = 0;
    
    vertexx[1] = centrex;
    vertexy[1] = centrey + height/2;
    
    angle = (2 * pi) / input$no_vertex;
    
    for (i in 2:input$no_vertex){
      
      vertexx[i] = vertexx[i-1] * cos(angle) - (vertexy[i-1] * sin(angle));
      vertexy[i] = vertexx[i-1] * sin(angle) + (vertexy[i-1] * cos(angle));
    }
    
    # generate a random start point 
    
      x = runif(1,-width/2,width/2);
      y = runif(1,-height/2,height/2);
      
    # 2 arrays that have the coordinates we will use for the plot
    
    coordx = c(x,vertexx[1:input$no_vertex]);
    coordy = c(y,vertexy[1:input$no_vertex]);
      
    previous = -1;
        
      
    # generating n points using the algorithm
      
    for(i in 1:input$n) {
      
      # picking a random vertex of the polygon
      # calculating the new coordinates
      
      edge = floor(runif(1,0,input$no_vertex)) + 1;
      
      if(edge != previous && input$prev == "yes"){
        
        x = x * input$r + (1 - input$r) * vertexx[edge];
        y = y * input$r + (1 - input$r) * vertexy[edge];
        
        coordx = c(coordx, x);
        coordy = c(coordy, y);
        
      }else if(input$prev == "no"){
        x = x * input$r + (1 - input$r) * vertexx[edge];
        y = y * input$r + (1 - input$r) * vertexy[edge];
        
        coordx = c(coordx, x);
        coordy = c(coordy, y);
      }
      previous = edge;
    }  
  
    
    # plot-ul
    
    plot(x = coordx, 
         y = coordy,
         type = "p",
         main = "Chaos Game",
         xlab = "",
         ylab = "",
         col = c("red",rep("black", input$no_vertex + input$n)),
         pch = c(15,rep(20, input$no_vertex + input$n)),
         cex = 1,
         axes = FALSE,
         frame.plot = TRUE);
    lines(c(vertexx,vertexx[1]), c(vertexy,vertexy[1]), col = "peachpuff4")
  })
}

shinyApp(ui = ui, server = server)