library(shinydashboard)

dashboardPage(dashboardHeader(title = 'TheDataPub'),
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Home", tabName = "home", icon = icon("dashboard")),
                  menuItem("Relaciones Lineales", tabName = "lm", icon = icon("th"))
                )
              ),
              dashboardBody(
                h1('Conociendo a los Parroquianos'),
                helpText('Con este pequeño dashboard '),
                tabItems(
                  # First tab content
                  tabItem(tabName = "home",
                          fluidRow(
                            box(title = "Promedios de Afinidad por Disciplina", plotOutput("meanScore", width = 500, height = 400)),
                            box(title = "Histograma de afinidad para Estadística", plotOutput("histStats", width = 300, height = 300)),
                            box(title = "Histograma de afinidad para Ciencias Comp", plotOutput("histCompSci", width = 300, height = 300)),
                            box(title = "Histograma de afinidad para Ingeniería de SW", plotOutput("histSW", width = 300, height = 300)),
                            box(title = "Histograma de afinidad para Visualización", plotOutput("histViz", width = 300, height = 300)),
                            box(title = "Histograma de afinidad para Disciplinas Verticales", plotOutput("histBiz", width = 300, height = 300))
                          )
                  ),
                  # Second tab content
                  tabItem(tabName = "lm",
                          h2("Relaciones Lineales")
                  )
                )              
              )
)
              
