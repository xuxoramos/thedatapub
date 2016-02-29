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
        round(mean(filter(attendeesclean, gendr == 'female')$stats), 2),
        round(mean(filter(attendeesclean, gendr == 'female')$cs), 2),
        round(mean(filter(attendeesclean, gendr == 'female')$swdev), 2),
        round(mean(filter(attendeesclean, gendr == 'female')$dataviz), 2),
        round(mean(filter(attendeesclean, gendr == 'female')$biz), 2),
        round(mean(filter(attendeesclean, gendr == 'male')$stats), 2),
        round(mean(filter(attendeesclean, gendr == 'male')$cs), 2),
        round(mean(filter(attendeesclean, gendr == 'male')$swdev), 2),
        round(mean(filter(attendeesclean, gendr == 'male')$dataviz), 2),
        round(mean(filter(attendeesclean, gendr == 'male')$biz), 2),
        round(mean(filter(attendeesclean, gendr == 'NA')$stats), 2),
        round(mean(filter(attendeesclean, gendr == 'NA')$cs), 2),
        round(mean(filter(attendeesclean, gendr == 'NA')$swdev), 2),
        round(mean(filter(attendeesclean, gendr == 'NA')$dataviz), 2),
        round(mean(filter(attendeesclean, gendr == 'NA')$biz, 2))
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
    rho <- cor(select(attendeesclean, c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rho, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })
  
  # Correlation plot for women
  output$corrFemalePlot <- renderPlot({
    rhofem <- cor(select(filter(attendeesclean, gendr == 'female'), c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rhofem, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })

    # Correlation plot for men
  output$corrMalePlot <- renderPlot({
    rhomal <- cor(select(filter(attendeesclean, gendr == 'male'), c(stats,cs,swdev,dataviz,biz)))
    cp <-
      corrplot.mixed(rhomal, lower = "ellipse", upper = 'number', tl.pos = 'd')
    cp
  })
}