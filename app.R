#A Shiny Democracy Map

# Load packages and data
library(shiny)
library(tidyverse)
library(sf)
library(showtext)

world <- read_rds("www/world-test.rds")

col_low <-  c("#cfdceb","#d6f2f6","#dbe8e6","#fee0d0","#fad9ea")
col_high <- c("#0F509B","#32BED2","#4B8C82","#FA6414","#E64196")  
indicator <- c("v2x_polyarchy","v2x_libdem","v2x_partipdem","v2x_delibdem" ,"v2x_egaldem")

df_colors <- data.frame(indicator, col_high, col_low)

choices_vars <- c("Yttrandefrihet" = 'v2x_libdem', 
             "Rättvisa val" = "v2x_polyarchy",
             "Majoritetsstyre" = "v2x_polyarchy",
             "Öppenhet" = "v2x_libdem", 
             "Jämlikhet" = "v2x_egaldem",
             "Att alla deltar" = "v2x_partipdem",
             "Samförstånd" = "v2x_delibdem")

var_info <- c("The electoral principle of democracy seeks to embody the core value of making rulers
responsive to citizens, achieved through electoral competition for the electorate’s approval
under circumstances when suffrage is extensive; political and civil society organizations can
operate freely; elections are clean and not marred by fraud or systematic irregularities; and
elections affect the composition of the chief executive of the country. In between elections,
there is freedom of expression and an independent media capable of presenting alternative
views on matters of political relevance.", 
" The liberal principle of democracy emphasizes the importance of protecting individual
and minority rights against the tyranny of the state and the tyranny of the majority. The liberal
model takes a negative view of political power insofar as it judges the quality of democracy by
the limits placed on government. This is achieved by constitutionally protected civil liberties,
strong rule of law, an independent judiciary, and effective checks and balances that, together,
limit the exercise of executive power.", 
"The participatory principle of democracy emphasizes active participation by citizens
in all political processes, electoral and non-electoral. It is motivated by uneasiness about
a bedrock practice of electoral democracy: delegating authority to representatives. Thus,
direct rule by citizens is preferred, wherever practicable. This model of democracy thus takes
suffrage for granted, emphasizing engagement in civil society organizations, direct democracy,
and subnational elected bodies.", 
"The deliberative principle of democracy focuses on the process by which decisions
are reached in a polity. A deliberative process is one in which public reasoning focused on the
common good motivates political decisions—as contrasted with emotional appeals, solidary
attachments, parochial interests, or coercion. According to this principle, democracy requires
more than an aggregation of existing preferences. There should also be respectful dialogue
at all levels—from preference formation to final decision—among informed and competent
participants who are open to persuasion.", 
"The egalitarian principle of democracy holds that material and immaterial inequalities inhibit the exercise of formal rights and liberties, and diminish the ability of citizens from
all social groups to participate. Egalitarian democracy is achieved when rights and freedoms
of individuals are protected equally across all social groups; and resources are distributed
equally across all social groups; and groups and individuals enjoy equal access to power")

#Font stuff
#font_add("Georgia", "Georgia.ttf")
#font_families()


# Define UI for application that draws a map
ui <- fluidPage(
  #theme = "style.css",
  
  #here are some style things to costum fonts etc. 
  
  #Title in 
  
  #Text body font in Georgia
  #tags$style(HTML('body {font-family:"Georgia",Georgia,Serif; background-color:white}')),
  
  #Header font in Knock out - don't know how to do this!
  #tags$style(HTML('h1 {font-family:"Georgia",Georgia,Serif}')),
  
  
  #liberal 
  tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #32BED2}")),
  #just grey
  #tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge {background: #737373} .js-irs-0 .irs-bar {background: #737373}")),
  
  tags$head(
    includeCSS("www/style.css")),
  
  plotOutput(outputId = "map", height = 900), 
  
  #test with leaflet controls
  
  absolutePanel(id = "dimension", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 20, left = 60, right = "auto", bottom = "auto",
                width = 300, height = "auto", 
                
                #h2("Dimension av demokrati"),
                
                radioButtons(inputId = "variable", 
                             label = h4("Välj en dimension av demokrati"),
                             choiceNames = list(tags$strong(tags$span(style="color:#0F509B", "Rättvisa val")),
                                                tags$strong(tags$span(style="color:#32BED2", "Yttrandefrihet")),
                                                tags$strong(tags$span(style="color:#4B8C82", "Att alla deltar")),
                                                tags$strong(tags$span(style="color:#FA6414", "Samförstånd")),
                                                tags$strong(tags$span(style="color:#E64196", "Jämlikhet"))
                             ),
                             choiceValues = c(1,2,3,4,5),
                             selected = 2
                ),
                
  ), 
  
  absolutePanel(id = "information", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = "auto", left = 60, right = "auto", bottom = 140,
                width = 300, height = "auto", 
                
                h4("Information"),
                
                textOutput(outputId = "var_text"), 
                
                hr(), 
                
                "Data från världens största demokratimätningsprojekt, The Varieties of Democracy Institute vid Göteborgs Universitet.",
                
  ), 
  
  absolutePanel(id = "slider", class = "panel panel-default", fixed = TRUE,
                draggable = FALSE, top = 20, left = "auto", right = 60, bottom = "auto",
                width = 700, height = "auto", 
              
                sliderInput("animation", "Looping Animation:",
                            inputId = "year",
                            label = h4("Välj ett år och tryck play"),
                            min = 1900, max = 2019,
                            value = 2019, step = 1,
                            animate = animationOptions(interval = 800, loop = FALSE),
                            sep = "", 
                            width = "100%"
                            ),  
                
                
  )
  
  
)
  
  
# 
server <- function(input, output) {
  
  world_df <- reactive({

    a <- world[c(1,2, (as.numeric(input$variable)+2))]
    names(a)[3] <- "value"
    
    return(a)
  })
    
 
  output$var_text <- renderText({
    var_info[as.numeric(input$variable)]
    
    })
  
  
    output$map <- renderPlot({
      
      world_df() %>% 
        filter(year == input$year) %>% 
        ggplot() +
          geom_sf(aes(fill=value), color="white", size=.1) +
          theme_void() +
          scale_fill_gradient(low = col_low[as.numeric(input$variable)],
                            high = col_high[as.numeric(input$variable)],
                            name =  as.character(input$year),
                            breaks = c(seq(0,1, .1)),
                            labels=c("Ingen Demokrati", rep("", 9), "Mycket Demokrati"),
                            limits = c(0,1),
                            na.value = "white",
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
          theme(legend.position = c(0.5, 0.025), 
                legend.title=element_text(size=30), 
                text = element_text(family = "Georgia") # eventuellt måste jag kommentera ut denna 
                )
      
    }, #height="auto", width = "auto"
    )
    

    
}

# Run the application 
shinyApp(ui = ui, server = server)

