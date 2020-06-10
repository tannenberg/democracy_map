#A Shiny Democracy Map

# Load packages and data
library(shiny)
library(tidyverse)
library(shinyWidgets)


world_grey <- read_rds("www/world2-grey.rds")
world <- read_rds("www/world2.rds")

col_low <-  c("#cfdceb","#d6f2f6","#dbe8e6","#defdd8","#fad9ea")
col_high <- c("#0F509B","#32BED2","#4B8C82","#5af53c","#E64196")  
indicator <- c("v2x_polyarchy","v2x_libdem","v2x_partipdem","v2x_delibdem" ,"v2x_egaldem")

df_colors <- data.frame(indicator, col_high, col_low)

var_title <- list(c("Rättvisa val", "Frihet", "Att alla deltar", "Öppenhet", "Jämlikhet"), 
                  c("Fair elections", "Freedom", "Everyone participates", "Transparency", "Equality")
)


vdem <- c("Data från världens största demokratimätningsprojekt, The Varieties of Democracy Institute vid Göteborgs Universitet.",
          "Data from the worlds largest democracy measurement project, The Varieties of Democracy Instutute at the University of Gothenburg") 

var_info <- list(
  c("Betyder att de val som hålls är schyssta och att valresultatet respekteras. Grundläggande för rättvisa val är att ledare och regeringar lyssnar på sina medborgare. Rättvisa val innebär även en utbredd rösträtt, att politiska och andra organisationer från samhället kan verka fritt, att valen inte kantas av bedrägeri och oegentligheter samt att valen faktiskt avgör vilka som håller den verkställande makten. För att valen ska vara rättvisa krävs även yttrandefrihet mellan valen. Rättvisa val kallas med ett annat ord elektoral demokrati.",
              "Frihet står för individens rätt att stå upp för sig själv och för andra. Demokrati som bygger på frihet handlar om vikten av rättigheter för den enskilda individen gentemot den styrande makten. Frihetsbaserad demokrati uppnås genom skyddade medborgerliga friheter som yttrandefrihet, en stark rättsstatsprincip, ett oberoende rättsväsende och effektiva kontrollmekanismer. Det är även viktigt med möjlighet att granska och begränsa den verkställande makten. Den frihetsbaserade demokratin betonar vikten av att skydda den enskilda individen och minoriteters rättigheter mot statens och majoritetens styre. Detta kallas med ett annat ord för den liberala demokratiprincipen.",
              "Här har alla möjlighet att vara med och aktivt påverka samhället och beslut som fattas. Den deltagande principen om demokrati ser till medborgarnas deltagande i alla politiska processer, inte enbart i val. Deltagande demokrati premierar medborgarnas direkta påverkan, närhelst det är möjligt. Denna modell av demokrati tar således rösträtt för givet, betonar engagemang i civilsamhällets organisationer, direkt demokrati, folkomröstningar, och möjlighet att påverka och delta i lokala politiska organ.", 
              "Respektfulla samtal, öppen debatt samt öppet beslutsfattande ingår i en öppenhetsbaserad demokrati. Demokrati där öppenhet är i fokus handlar om att beslutsfattande processer är öppna för samtal och dialog. En öppen process där resonemang baserade på det allmännas bästa avgör politiska beslut . Enligt denna princip kräver demokrati mer än att beslut följer medborgarnas opinion. Det bör också finnas respektfull dialog på alla nivåer - från opinionsbildning till slutgiltig beslut - med informerade och kompetenta deltagare som är öppna för att låta sig övertalas. Denna demokratiform kallas även för deliberativ eller samtalande demokrati.",
              "Alla ska ha samma faktiska möjligheter att delta i samhället och att alla kan ta del av samma rättigheter. En jämlik demokrati uppnås när samtliga samhällsgrupper åtnjuter samma rättigheter och friheter; och att resurser fördelas lika mellan dessa; samt att samtliga samhällsgrupper och individer har en jämlik tillgång till politisk makt. Den jämlika principen om demokrati tar i beaktning att materiella och immateriella ojämlikheter utgör hinder för formella rättigheter och friheter och underminerar deltagande från samhällets samtliga sociala grupperingar. Detta är den egalitära demokratin."
    ), 
  
  c("The electoral principle of democracy seeks to embody the core value of making rulers
responsive to citizens, achieved through electoral competition for the electorate’s approval
under circumstances when suffrage is extensive; political and civil society organizations can
operate freely; elections are clean and not marred by fraud or systematic irregularities; and
elections affect the composition of the chief executive of the country. In between elections,
there is freedom of expression and an independent media capable of presenting alternative
views on matters of political relevance.", 
              
"The liberal principle of democracy emphasizes the importance of protecting individual
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
equally across all social groups; and groups and individuals enjoy equal access to power"
)
)

#slider_titel <- c("Välj ett år och tryck play", "Choose a year and press play")

no_dem <- c("Ingen Demokrati", "No Democracy")
high_dem <- c("Mycket Demokrati", "Full Democracy")


ui <- fluidPage(
  
  tags$head(includeCSS("www/style.css")),
           
  plotOutput(outputId = "map", height = 800), 
  
  
  absolutePanel(id = "language", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 5, left = "auto", right = 5, bottom = "auto",
                width = 100, height = "auto",
  
                radioButtons(inputId = "language", 
                             label = "",
                             choiceNames = list("SWE", "ENG"),
                             choiceValues = c(1,2),
                             selected = 1
                ),
  ),
  
  
  absolutePanel(id = "dimension", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "650px", left = 60, right = "auto", bottom = "auto",
                width = 300, height = "auto", 
                
                #tags$strong("Välj en dimension av demokrati"),
                
                radioButtons(inputId = "variable", 
                             label = "",
                             choiceNames = list(tags$strong(tags$span(style="color:#0F509B", textOutput(outputId = "choice_1"))),
                                                tags$strong(tags$span(style="color:#32BED2", textOutput(outputId = "choice_2"))),
                                                tags$strong(tags$span(style="color:#4B8C82", textOutput(outputId = "choice_3"))),
                                                tags$strong(tags$span(style="color:#5af53c", textOutput(outputId = "choice_4"))),
                                                tags$strong(tags$span(style="color:#E64196", textOutput(outputId = "choice_5"))) 
                             ),
                             choiceValues = c(1,2,3,4,5),
                             selected = 5
                ),
                
                tags$strong(tags$span(style="color:#b8b8b8", "Länder i grå saknar data")),
           
  ), 
  
  absolutePanel(id = "information", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "650px", left = "auto", right = 40, bottom = "auto",
                width = 600, height = "auto", 
                
                tags$strong(textOutput(outputId = "var_title_out")), 
                
                textOutput(outputId = "var_text"), 
                
                
  ), 
  
  absolutePanel(id = "vdem", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "auto", left = "auto", right = 5, bottom = 5,
                width = 600, height = "auto", 
                
                textOutput(outputId = "vdem_out"),
                
  ), 
  
  
  
  setSliderColor("#5c5c5c", 1),
  
  tags$head(tags$style(type='text/css', ".slider-animate-button {font-size: 40pt !important; color: #5c5c5c}")),
  
  
  absolutePanel(id = "slider", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = "650px", left = "380px", right = "auto", bottom = "auto",
                width = 540, height = "auto", 
              
                #tags$strong(textOutput(outputId = "slider_titel_out")),
                
                sliderInput("obs", "animation", "Looping Animation:",
                            inputId = "year",
                            label = "",
                            min = 1900, max = 2019,
                            value = 2019, step = 1,
                            animate = animationOptions(interval = 800, loop = FALSE),
                            sep = "", 
                            width = "100%"
                            ),
                
              
  ) 
) 
  
  
# 
server <- function(input, output, session) {
  
  world_df <- reactive({

    a <- world[c(1,2, (as.numeric(input$variable)+2))]
    names(a)[3] <- "value"
    
    return(a)
  })
    
 
  #output$var_col <- renderText({
  #  as.numeric(input$variable)
  #  })  
  
  
  output$choice_1 <- renderText({
    var_title[[as.numeric(input$language)]][1]
    
  })
  
  output$choice_2 <- renderText({
    var_title[[as.numeric(input$language)]][2]
    
  })
  
  output$choice_3 <- renderText({
    var_title[[as.numeric(input$language)]][3]
    
  })
  
  output$choice_4 <- renderText({
    var_title[[as.numeric(input$language)]][4]
    
  })
  
  output$choice_5 <- renderText({
    var_title[[as.numeric(input$language)]][5]
    
  })
  
  
  output$slider_col_out <- renderPrint({
    slider_col[as.numeric(input$variable)]
    
  })
  
  #output$slider_titel_out <- renderText({
  #  slider_titel[as.numeric(input$language)]
  #  
  #})
  
  output$vdem_out <- renderText({
    vdem[as.numeric(input$language)]
    
  })
  
  output$var_title_out <- renderText({
    var_title[[as.numeric(input$language)]][as.numeric(input$variable)]
    
  })
  
  output$var_text <- renderText({
    var_info[[as.numeric(input$language)]][as.numeric(input$variable)]
    
    })
  
  
    output$map <- renderPlot({
      
      world_df() %>% 
        filter(year == input$year) %>% 
        ggplot() +
          geom_sf(data=world_grey, fill= "#e2e2e2", color= "white", size=.01) +
          geom_sf(aes(fill=value), color="white", size=.1) +
          theme_void() +
          scale_fill_gradient(low = col_low[as.numeric(input$variable)],
                            high = col_high[as.numeric(input$variable)],
                            name =  as.character(input$year),
                            breaks = c(seq(0,1, .1)),
                            labels=c(no_dem[as.numeric(input$language)], rep("", 9),
                                     high_dem[as.numeric(input$language)]),
                            limits = c(0,1),
                            na.value = "#e2e2e2",
                            guide = guide_legend(
                            direction = "horizontal",
                            #keyheight = unit(3, units = "mm"),
                            #keywidth = unit(3, units = "mm"),
                            title.position = 'top',
                            title.hjust	= .5,
                            title.vjust	= -2,
                            #label.vjust = -7,
                            nrow = 1,
                            #  #byrow = T,
                            label.position = "top"
                            )
                             ) +
          coord_sf() +
          theme(#legend.position = c(0.40, 0.05), 
                legend.position = "bottom",            
                legend.title=element_text(family = "Georgia", size=30), 
                text = element_text(family = "Georgia", size = 16) # eventuellt måste jag kommentera ut denna 
                )
      
    }, #height="auto", width = "auto"
    )

}

# Run the application 
shinyApp(ui = ui, server = server)

