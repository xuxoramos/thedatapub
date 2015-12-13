library(dplyr)
library(googleVis)
library(shinydashboard)
library(corrplot)

suppressPackageStartupMessages(library(googleVis))

server <- function(input, output) {

    # Plot for means by discipline
  output$meanScoreByDiscipline <- renderGvis({
    meanPlot <- gvisBarChart(chartid = 'meanScoreByDiscipline', meandiscipline,
                             options = list(legend = "none")
                             )
    meanPlot
  })
  
  # Gender plot
  output$genderProportion <- renderGvis({
    genPlot <- gvisColumnChart(chartid = 'genderProportion', gen, 
                               options = list(legend = "none")
                               )
    genPlot
  })
  
  output$genderDisciplineProportion <- renderGvis({
    datSK <- data.frame(
      from = c(rep('female', 5), rep('male', 5), rep('NA', 5)),
      to = c(rep(c('stats','cs','swdev','dataviz','biz'), 3)),
      weight = c(
        round(mean(filter(attendees, gendr == 'female')$stats), 2),
        round(mean(filter(attendees, gendr == 'female')$cs), 2),
        round(mean(filter(attendees, gendr == 'female')$swdev), 2),
        round(mean(filter(attendees, gendr == 'female')$dataviz), 2),
        round(mean(filter(attendees, gendr == 'female')$biz), 2),
        round(mean(filter(attendees, gendr == 'male')$stats), 2),
        round(mean(filter(attendees, gendr == 'male')$cs), 2),
        round(mean(filter(attendees, gendr == 'male')$swdev), 2),
        round(mean(filter(attendees, gendr == 'male')$dataviz), 2),
        round(mean(filter(attendees, gendr == 'male')$biz), 2),
        round(mean(filter(attendees, gendr == 'NA')$stats), 2),
        round(mean(filter(attendees, gendr == 'NA')$cs), 2),
        round(mean(filter(attendees, gendr == 'NA')$swdev), 2),
        round(mean(filter(attendees, gendr == 'NA')$dataviz), 2),
        round(mean(filter(attendees, gendr == 'NA')$biz, 2))
        )
    )
    
    genDiscPlot <- gvisSankey(chartid = 'genderDisciplineProportion', 
                              datSK, from = "from", to = "to", weight = "weight",
                              options = list(legend = "none")
                                )
    genDiscPlot                  
  })
  
  # General correlation plot
  output$corrPlot <- renderPlot({
    rho <- cor(select(attendees, c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rho, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })
  
  # Correlation plot for women
  output$corrFemalePlot <- renderPlot({
    rhofem <- cor(select(filter(attendees, gendr == 'female'), c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rhofem, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })

    # Correlation plot for men
  output$corrMalePlot <- renderPlot({
    rhomal <- cor(select(filter(attendees, gendr == 'male'), c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rhomal, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })
}