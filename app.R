#A Shiny Democracy Map

# Load packages and data
library(shiny)
library(tidyverse)
library(sf)
library(showtext)



world <- read_rds("www/world.rds")

col_low <-  c("#cfdceb","#d6f2f6","#dbe8e6","#fee0d0","#fad9ea","#ebe4d1")
col_high <- c("#0F509B","#32BED2","#4B8C82","#FA6414","#E64196","#9B7819")  
indicator <- c("v2x_polyarchy","v2x_libdem","v2x_partipdem","v2x_delibdem" ,"v2x_egaldem","v2pepwrort")

df_colors <- data.frame(indicator, col_high, col_low)

choices_vars <- c("Yttrandefrihet" = 'v2x_libdem', 
             "Rättvisa val" = "v2x_polyarchy",
             "Majoritetsstyre" = "v2x_polyarchy",
             "Öppenhet" = "v2x_libdem", 
             "Jämlikhet" = "v2x_egaldem",
             "Att alla deltar" = "v2x_partipdem",
             "Samförstånd" = "v2x_delibdem", 
             "Att få sticka ut" = "v2pepwrort")

#Font stuff
#font_add("Georgia", "Georgia.ttf")
#font_families()


# Define UI for application that draws a map
ui <- fluidPage(
  #theme = "style.css",
  
  #here are some style things to costum fonts etc. 
  
  #Title in 
  
  #Text body font in Georgia
  tags$style(HTML('body {font-family:"Georgia",Georgia,Serif; background-color:white}')),
  
  #Header font in Knock out - don't know how to do this!
  tags$style(HTML('h1 {font-family:"Georgia",Georgia,Serif}')),
  
  
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #32BED2}")),
  
  
  
  plotOutput(outputId = "map", height = 700), 
  
  hr(), 
  
  
    fluidRow(
      column(4,
             
     
  
  
  #tags$head(
  #  tags$style(HTML("
  #    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
  #    
  #    h1 {
  #      font-family: 'Lobster';
  #      font-weight: 200;
  #      line-height: 1.1;
  #    }
#
  #  "))
  #),
  
  #tags$head(
    # Include custom CSS
   # includeCSS("style.css")
  #  ),
    
    # Application title
   # titlePanel("Demokrati i tid och rum"),
    
    #"Demokrati är mer än regelbundna val. Demokrati är även",
    #  tags$strong(tags$span(style="color:#32BED2", "Yttrandefrihet")), ",", 
    #  tags$strong(tags$span(style="color:#0F509B", "Rättvisa val")), ",", 
    #  tags$strong(tags$span(style="color:#F5B41E", "Majoritetsstyre")), ",", 
    #  tags$strong(tags$span(style="color:#5AF53C", "Öppenhet")), ",",
    #  tags$strong(tags$span(style="color:#E64196", "Jämlikhet")), ",",
    #  tags$strong(tags$span(style="color:#4B8C82", "Att alla deltar")), ",",
    #  tags$strong(tags$span(style="color:#FA6414", "Samförstånd")), " och ",
    #  tags$strong(tags$span(style="color:#9B7819", "Att få sticka ut")), "."
    #, 
    
    
    # Sidebar with a slider input for number of bins 
    #idebarLayout(
    # position = "left",
    #   sidebarPanel(
         "Demokrati är mer än regelbundna val.",tags$br(),
         tags$strong(tags$span(style="color:#32BED2", "Yttrandefrihet")), tags$br(), 
         tags$strong(tags$span(style="color:#0F509B", "Rättvisa val")), tags$br(), 
         tags$strong(tags$span(style="color:#F5B41E", "Majoritetsstyre")),tags$br(), 
         tags$strong(tags$span(style="color:#5AF53C", "Öppenhet")),tags$br(),
         tags$strong(tags$span(style="color:#E64196", "Jämlikhet")), tags$br(),
         tags$strong(tags$span(style="color:#4B8C82", "Att alla deltar")), tags$br(),
         tags$strong(tags$span(style="color:#FA6414", "Samförstånd")), tags$br(),
         tags$strong(tags$span(style="color:#9B7819", "Att få sticka ut")),tags$br(),tags$br()
         ), 
    #     width = 3,
    
    column(4,      
          selectInput(inputId = "variable", 
                      label = "Välj en dimension av demokrati:",
                      choices = unique(world$indicator),
                      #choices = choices_vars, 
                      selected = unique(world$indicator)[2]
                      ),
                      
            
            
    ), 
  
  
    column(4,
           
           sliderInput("animation", "Looping Animation:",
                       inputId = "year",
                       label = "Välj ett år och tryck play:",
                       min = 1900, max = 2019,
                       value = 2018, step = 1,
                       animate = animationOptions(interval = 600, loop = FALSE),
                       sep = ""
           ),
           
           helpText("Data från världens största demokratimätningsprojekt, The Varieties of Democracy Institute vid Göteborgs Universitet."), 
    )
          
        )
)
    
    # Show the map
  
    
  #column(
 
 #   mainPanel(
     # width = 11,  
     # plotOutput(outputId = "map")
    #  )
    #)


# 
server <- function(input, output) {
    
 
    output$map <- renderPlot({
      
      world %>% 
        filter(year == input$year) %>% 
        filter(indicator == input$variable) %>% 
        
        ggplot() +
          geom_sf(aes(fill=value), color="white", size=.1) +
          theme_void() +
          scale_fill_gradient(low = as.character(df_colors$col_low[indicator==input$variable]),
                            high = as.character(df_colors$col_high[indicator==input$variable]),
                            name =  as.character(input$year),
                            breaks = c(seq(0,1, .1)),
                            labels=c("Ingen Demokrati", rep("", 9), "Mycket Demokrati"),
                            limits = c(0,1),
                            guide = guide_legend(
                            direction = "horizontal",
                            #  keyheight = unit(3, units = "mm"),
                            #  keywidth = unit(50/length(labels), units = "mm"),
                            title.position = 'top',
                            title.hjust	= .5,
                            #  #title.hjust = 0.5,
                            label.hjust = 2,
                            nrow = 1,
                            #  #byrow = T,
                            label.position = "bottom"
                             
                            
                            )
                            ) +
          coord_sf() +
          theme(legend.position = "bottom", 
                legend.title=element_text(size=30)#, 
                #text = element_text(family = "Georgia")
                )
      
    }, #height="auto", width = "auto"
    )
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

