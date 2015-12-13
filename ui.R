library(shinydashboard)

dashboardPage(
  skin = 'yellow', dashboardHeader(title = 'TheDataPub'),
  dashboardSidebar(sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon("dashboard")),
    menuItem("Correlaciones", tabName = "cor", icon = icon("th"))
  )),
  dashboardBody(
    h1('Conociendo a los Parroquianos'),
    helpText(
      'Con este pequeño dashboard podrás explorar las afinidades expresadas por los parroquianos. La afinidad es con una de las 5 disciplinas que forman la verdadera analítica de datos: 1) Estadística, 2) Ciencias Computacionales, 3) Ingeniería de SW, 4) Visualización y Storytelling, 5) Conexión de Negocio.'
    ),
    tabItems(
      # First tab content
      tabItem(tabName = "home",
              fluidRow(column(
                12,
                box(
                  title = "Promedios de Afinidad por Disciplina",
                  width = 12,
                  htmlOutput(outputId = 'meanScoreByDiscipline')
                )
              ),
              column(
                12,
                box(
                  title = 'Conteo de Género', width = 6, 
                  htmlOutput(outputId = 'genderProportion')
                ),
                box(title = 'Proporción de Género con Disciplinas', width = 6,
                    htmlOutput('genderDisciplineProportion'))
                ))),
              # Second tab content
              tabItem(tabName = "cor",
                      fluidRow(
                                column(
                                  12,
                                  box(
                                    title = "Correlaciones Entre Disciplinas",
                                    width = 12,
                                    plotOutput('corrPlot'),
                                    helpText('Este plot se puede interpretar identificando en la diagonal superior los coeficientes de correlación (rojo en negativa, azul en positiva) y en la diagonal inferior la forma general de la -nube de datos-, donde entre más delgada y más intensa en color la correlación es mayor. Pero recuerden: CORRELACIÓN NO IMPLICA CAUSALIDAD!')
                                  )
                                ),
                                column(
                                  12,
                                  box(
                                    title = "Correlaciones para Mujeres",
                                    width = 6,
                                    plotOutput('corrFemalePlot')
                                  ),
                                  box(
                                    title = "Correlaciones para hombres",
                                    width = 6,
                                    plotOutput('corrMalePlot')
                                  )
                                )
                              )
                      )
            )
      )
    )
  
  