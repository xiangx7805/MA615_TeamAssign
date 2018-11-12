#library
library(tidyverse)
library(readxl)
library(ggmap)
library(magrittr)
library(maps)
library(shiny)
library(ggplot2)
library(plotly)

#data

dono <- read.csv("11-5 MASSCONTRIBUTIONS-csv.csv")

donor_Q08aa <- dono %>% 
  group_by(city, state, party) %>% 
  summarize(total = sum(amount), num = n()) %>% 
  arrange(desc(total))

dmap <- spread(donor_Q08aa, party, party)

dmap_stR <- dmap %>% filter(R==R)
dmap_stD <- dmap %>% filter(D==D)
dmap_stI <- dmap %>% filter(I==I)

## Republican donor 
dmapR <- dmap_stR %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

states <- map_data("state")
states %<>% 
  dplyr::select(long,lat,group,order,region) %>% 
  rename(state=region)
st_name <- unique(states$state)

R_st_abrev <- dmapR$state

st <- read.csv("states.csv")
st %<>% rename(state=st_name)

R_states <- left_join(states, st, by="state")
R_states %<>% rename(R_st_abrev=st_abrev)

dmapR %<>% rename(R_st_abrev=state)

R_states <- left_join(R_states, dmapR, by="R_st_abrev")
R_states$Donors <- as.character(R_states$Donors)

R_states_center <- R_states %>% 
  group_by(R_st_abrev) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = R_st_abrev)

R_states_data <- R_states %>% 
  dplyr::select(R_st_abrev,Donations, Donors) %>% 
  unique()

R_states_center <- left_join(R_states_center, R_states_data, by=c("R_st_abrev"))
#####################################

## capture number of donors before these rows are deleted from states_center
##states_center[states_center$st_abrev=="MA",]
don_ma <- R_states_center[R_states_center$R_st_abrev=="MA",]$Donors
don_ri <- R_states_center[R_states_center$R_st_abrev=="RI",]$Donors
don_ct <- R_states_center[R_states_center$R_st_abrev=="CT",]$Donors

## remove rows for MA, Ri, CT

R_states_center %<>% filter(!(R_st_abrev=="MA" | R_st_abrev=="RI" | R_st_abrev=="CT"))

### Adjust location of state labels

R_states_center[R_states_center$R_st_abrev=="ID",]$long=-115.5
R_states_center[R_states_center$R_st_abrev=="MI",]$long=-84.7
R_states_center[R_states_center$R_st_abrev=="MI",]$lat=43
R_states_center[R_states_center$R_st_abrev=="VT",]$long=-72.7
R_states_center[R_states_center$R_st_abrev=="VT",]$lat=44.4
R_states_center[R_states_center$R_st_abrev=="NH",]$lat=43.6
R_states_center[R_states_center$R_st_abrev=="FL",]$long=-81.7
R_states_center[R_states_center$R_st_abrev=="LA",]$long=-92.5


##Democrats Data

## Democrats
dmapD <- dmap_stD %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

states <- map_data("state")
states %<>%
  dplyr::select(long,lat,group,order,region) %>%
  rename(state=region)
st_name <- unique(states$state)

D_st_abrev <- dmapD$state

st <- read.csv("states.csv")
st %<>% rename(state=st_name)

D_states <- left_join(states, st, by="state")
D_states %<>% rename(D_st_abrev=st_abrev)

dmapD %<>% rename(D_st_abrev=state)

D_states <- left_join(D_states, dmapD, by="D_st_abrev")

D_states$Donors <- as.character(D_states$Donors)

D_states_center <- D_states %>% group_by(D_st_abrev) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = D_st_abrev)

D_states_data <- D_states %>% 
  dplyr::select(D_st_abrev,Donations,Donors) %>% 
  unique()

D_states_center <- left_join(D_states_center, D_states_data, by=c("D_st_abrev"))


## capture number of donors before these rows are deleted from states_center
don_ma <- D_states_center[D_states_center$D_st_abrev=="MA",]$Donors
don_ri <- D_states_center[D_states_center$D_st_abrev=="RI",]$Donors
don_ct <- D_states_center[D_states_center$D_st_abrev=="CT",]$Donors

## remove rows for MA, Ri, CT

D_states_center %<>% filter(!(D_st_abrev=="MA" | D_st_abrev=="RI" | D_st_abrev=="CT"))

### Adjust location of state labels

D_states_center[D_states_center$D_st_abrev=="ID",]$long=-115.5
D_states_center[D_states_center$D_st_abrev=="MI",]$long=-84.7
D_states_center[D_states_center$D_st_abrev=="MI",]$lat=43
D_states_center[D_states_center$D_st_abrev=="VT",]$long=-72.7
D_states_center[D_states_center$D_st_abrev=="VT",]$lat=44.4
D_states_center[D_states_center$D_st_abrev=="NH",]$lat=43.6
D_states_center[D_states_center$D_st_abrev=="FL",]$long=-81.7
D_states_center[D_states_center$D_st_abrev=="LA",]$long=-92.5

##Independent 

## Republican donor 
dmapI <- dmap_stI %>% 
  group_by(state) %>% 
  summarize(Donations = sum(total), Donors = sum(num))

states <- map_data("state")
states %<>% 
  dplyr::select(long,lat,group,order,region) %>% 
  rename(state=region)
st_name <- unique(states$state)

I_st_abrev <- dmapI$state

st <- read.csv("states.csv")
st %<>% rename(state=st_name)

I_states <- left_join(states, st, by="state")
I_states %<>% rename(I_st_abrev=st_abrev)

dmapI %<>% rename(I_st_abrev=state)

I_states <- left_join(I_states, dmapI, by="I_st_abrev")
I_states$Donors <- as.character(I_states$Donors)

I_states_center <- I_states %>% 
  group_by(I_st_abrev) %>% 
  summarise(lat = mean(c(max(lat),min(lat))),
            long = mean(c(max(long),min(long)))) %>% 
  mutate(state_c = I_st_abrev)

I_states_data <- I_states %>% 
  dplyr::select(I_st_abrev,Donations, Donors) %>% 
  unique()

I_states_center <- left_join(I_states_center, I_states_data, by=c("I_st_abrev"))
#####################################

## capture number of donors before these rows are deleted from states_center
##states_center[states_center$st_abrev=="MA",]
don_ma <- I_states_center[I_states_center$I_st_abrev=="MA",]$Donors
don_ri <- I_states_center[I_states_center$I_st_abrev=="RI",]$Donors
don_ct <- I_states_center[I_states_center$I_st_abrev=="CT",]$Donors

## remove rows for MA, Ri, CT

I_states_center %<>% filter(!(I_st_abrev=="MA" | I_st_abrev=="RI" | I_st_abrev=="CT"))

### Adjust location of state labels

I_states_center[I_states_center$I_st_abrev=="ID",]$long=-115.5
I_states_center[I_states_center$I_st_abrev=="MI",]$long=-84.7
I_states_center[I_states_center$I_st_abrev=="MI",]$lat=43
I_states_center[I_states_center$I_st_abrev=="VT",]$long=-72.7
I_states_center[I_states_center$I_st_abrev=="VT",]$lat=44.4
I_states_center[I_states_center$I_st_abrev=="NH",]$lat=43.6
I_states_center[I_states_center$I_st_abrev=="FL",]$long=-81.7
I_states_center[I_states_center$I_st_abrev=="LA",]$long=-92.5


#shiny
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(top = 100, right = 100,
                 selectInput("var","Select Political Party",
                             choices=c("Democrats","Republican","Independent")),
                 h4("A Work by:"),
                 h4("Jianhao(Miller) Yan"),
                 h4("Xiang(Rachel) XU"),
                 h4("Jing(Mira) Tang"),
                 h4("Ningze(Summer) ZU")
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
