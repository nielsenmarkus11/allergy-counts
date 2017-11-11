library(shiny)
library(dplyr)
library(plotly)
library(lubridate)
library(broom)

load("data/pollen-final.Rdat")

coldat <- data.frame(variable = c("grass", "mold", "trees", "weeds"),
                     fill = c('rgba(0, 100, 0, .05)',
                              'rgba(100, 100, 100, .05)',
                              'rgba(63, 42, 20, .05)',
                              'rgba(165, 165, 0, .05)'),
                     color = c('rgba(0, 100, 0, 1)',
                               'rgba(100, 100, 100, 1)',
                               'rgba(63, 42, 20, 1)',
                               'rgba(165, 165, 0, 1)'))


wksum <- final %>%
  filter(variable %in% c("grass", "mold", "trees", "weeds")) %>% 
  mutate(mnth = month(created_time, label = TRUE, abbr = TRUE)) %>% 
  select(mnth, week, year, variable, value) %>% 
  group_by(year, mnth, week, variable) %>% 
  dplyr::summarize(pollen_cnt = mean(value, na.rm=TRUE)) %>% 
  data.frame()

wksum$dt <- as.Date(paste(wksum$year, wksum$week, 1, sep = "-"),"%Y-%U-%u")



# Define UI for application that draws a histogram
ui <- fluidPage(theme="cerulean",
                
                # Application title
                titlePanel("Seasonal Allergen Trends"),
                
                # Buttons on Top
                fluidRow(HTML('<div id="pollen" class="btn-group btn-group-justified shiny-input-radiogroup">
                              <label class="btn btn-default">
                              <input type="radio" name="pollen" value="grass" checked="checked" style="display:none;">
                              <img src="grass1.png" style="width: 100px"></img><p style="color: rgb(0,100,0)">Grass</p>
                              </label>
                              <label class="btn btn-default">
                              <input type="radio" name="pollen" value="mold" style="display:none;">
                              <img src="mold1.png" style="width: 100px"></img><p style="color: rgb(100,100,100)">Mold</p>
                              </label>
                              <label class="btn btn-default">
                              <input type="radio" name="pollen" value="trees" style="display:none;">
                              <img src="tree1.png" style="width: 100px"></img><p style="color: rgb(63,42,20)">Tree</p>
                              </label>
                              <label class="btn btn-default">
                              <input type="radio" name="pollen" value="weeds" style="display:none;">
                              <img src="weeds.png" style="width: 50px"></img><p style="color: rgb(165,165,0)">Weeds</p>
                              </label>
                              </div>
                              ')),
                
                # Sidebar with a slider input for number of bins 
                fluidRow(
                  column(width = 6,
                         plotlyOutput("boxplot")
                  ),
                  column(width = 6,
                         plotlyOutput("loesschart")
                  )
                ),
                HTML('<p><b>Source:</b> Intermountain Allergy & Asthma</p>')
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$loesschart <- renderPlotly({
      
      ptype <- input$pollen
      
      wksum$dt <- as.Date(paste(wksum$year, wksum$week, 1, sep = "-"),"%Y-%U-%u")
      
      loessMod <- loess(pollen_cnt~week, span = .6, data= wksum[wksum$variable==ptype,])
      
      plot_ly(data=wksum[wksum$variable==ptype,], 
              x=~week, y=~pollen_cnt, color = I("black")) %>% 
        add_markers(text = ~variable, showlegend=FALSE) %>% 
        add_lines(y=~ifelse(fitted(loess(pollen_cnt~week, span = .6))>0,
                            fitted(loess(pollen_cnt~week, span = .6)),0),
                  line = list(color = coldat$color[coldat$variable==ptype]),
                  name = "Loess Smoother") %>% 
        add_ribbons(data = augment(loessMod),
                    ymin = ~ifelse(.fitted - 1.96*.se.fit>0,.fitted - 1.96*.se.fit,0),
                    ymax = ~ifelse(.fitted + 1.96*.se.fit>0,.fitted + 1.96*.se.fit,0),
                    line = list(color = coldat$fill[coldat$variable==ptype]),
                    name = "Standard Error") %>%
        layout(xaxis = list(title = 'Week'),
               yaxis = list(title = 'Average Pollen Count Severity'),
               legend = list(x = 0.80, y = 0.90))
    })
    
    output$boxplot <- renderPlotly({
      
      ptype <- input$pollen
      
      plot_ly(data=wksum[wksum$variable==ptype,],
              x=~mnth, y=~pollen_cnt, type = 'box',
              marker = list(color=coldat$color[coldat$variable==ptype]),
              line = list(color=coldat$color[coldat$variable==ptype])) %>%
        layout(xaxis = list(title = ''),
               yaxis = list(title = 'Average Pollen Count Severity'),
               showlegend = FALSE)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

