library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(top = 100, right = 100,
                 selectInput("var","Select Political Party",
                             choices=c("Democrats","Republican","Independent"))
    ),
    mainPanel(
      plotlyOutput("mapping")
    )
  ))


server <- function(input, output,session) {
  output$mapping <- renderPlotly({
    #check for the input variable
    if (input$var == "Democrats") {
      states <- D_states
      states_center <- D_states_center
      color_low <- "lightblue1"
      color_high <- "blue"
      mytitle <- "Donors to Massachusetts Democrats"
      mybreaks <- c(4,5.5,7)
    }
    else {
      if(input$var=="Independent"){
        states<-I_states
        states_center<-I_states_center
        color_low<-"yellow1"
        color_high<-"purple"
        mytitle<-"Donors to Massachusetts Independent"
        mybreaks<-c(4,5.5,7)
      }
      else{states <- R_states
      states_center <- R_states_center
      color_low <- "pink1"
      color_high <- "red4"
      mytitle <- "Donors to Massachusetts Republicans"
      mybreaks <- c(3.4,4.7,6)
      }}
    
    p <- ggplot(data = states) + 
      geom_polygon(aes(x = long, y = lat, fill = log10(Donations), 
                       group = group), color = "white") + 
      coord_fixed(1.3) +
      labs(title= mytitle, caption = "Number of Donors by State") +
      scale_fill_gradient("Donations", low =  color_low, 
                          high = color_high,
                          breaks=mybreaks,
                          labels=c("low","","high")) +
      
      geom_text(data = states_center,aes(x=long, y=lat, label = Donors),size=2, color="white") +
      annotate("text", x = -66, y = 42, label = paste("MA:",don_ma), size=3)  +
      annotate("text", x = -66, y = 41, label = paste("RI:", don_ri), size=3) +
      annotate("text", x = -66, y = 40, label = paste("CT:", don_ct), size=3) +
      theme(legend.position = c(0.5,0.5),legend.text=element_text(size=10)) +
      theme(text = element_text(size=15),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.x =element_blank(),
            axis.line.y = element_blank(),
            panel.background = element_blank())
    
  })
}


shinyApp(ui = ui,server = server)
