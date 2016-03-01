library(shiny)

shinyUI(navbarPage(
    "Ant colony simulation",
    position = "static-top",
    tabPanel("Simulation app",
#             style = "text-align:center;
#                      margin-bottom:20px;
#                      padding:10px;
#                      background-color:#dddddd;
#                      border:3px outline #aaaaaa;
#                      border-radius: 5px"
    sidebarPanel(
        h3("Simulation settings:"),
        div(
            h4("World"),
            numericInput("width", "Width (max x-coord):", 30, min = 10, max = 50,
                         step = 1),
            numericInput("height", "Height (max y-coord):", 30, min = 10, max = 50,
                         step = 1),
            style = "background-color: #dfd;
                     padding: 2px 10px;
                     margin: 5px 10px "
        ),
        div(
            h4("Hive"),
            numericInput("hivex", "The x-coord of the hive:", 20, min = 1, max = 50, step = 1),
            numericInput("hivey", "The y-coord of the hive:", 20, min = 1, max = 50, step = 1),
            numericInput("hivebearrate", "Ants born each tick:", 2, min = 1, max = 5, step = 1),
            style = "background-color: #fdd;
                     padding: 2px 10px;
                     margin: 5px 10px
            "
        ),

        actionButton("play", "Run simulation"),
        actionButton("stop", "Stop!")
    ),
    mainPanel(
        h3(textOutput("tick"),
           style = "text-align: center"),
        plotOutput("plot")
    )),
tabPanel("Help")
))
