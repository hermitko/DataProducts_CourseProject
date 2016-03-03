library(shiny)
library(shinyjs)
library(markdown)

shinyUI(
    navbarPage(
    "Ant colony simulation",
    position = "static-top",
    tabPanel(
        "Simulation app",
        tags$head(
            tags$style(".navbar-brand {font-size:200%; padding-right:150px;
                       margin-right:50px;
                       background-image:url('ant.png');
                       background-position:right center;
                       background-repeat: no-repeat}")
        ),
        fluidRow(column(
            width = 5,
            h3("Simulation settings:",
               style = "text-align: center")
        ),
        column(
            width = 7,
            h3(textOutput("tick"),
               style = "text-align: center")
        )),
        fluidRow(
            column(
                width = 2,
                div(
                    h4("World"),
                    sliderInput(
                        "width", "Width:", 30, min = 10, max = 50,
                        step = 1
                    ),
                    sliderInput(
                        "height", "Height:", 30, min = 10, max = 50,
                        step = 1
                    ),
                    numericInput("maxantsatplace",
                                 "Maximum ants at one place:",
                                 5, min = 2, max = 10),
                    style = "background-color: #dfd;
                    padding: 2px 5px;
                    margin: 8px 0px;
                    border-radius: 5px;",
                    id = "winput"
                ),
                div(
                    h4("Hive"),
                    numericInput(
                        "hivex", "The x-coord:", 10, min = 1, max = 50, step = 1
                    ),
                    numericInput(
                        "hivey", "The y-coord:", 10, min = 1, max = 50, step = 1
                    ),
                    numericInput(
                        "hivebearrate", "Ants born each tick:", 2, min = 1, max = 5, step = 1
                    ),
                    numericInput(
                        "antlivelength", "Lifespan:", 100, min = 50, max = 200, step = 10
                    ),
                    numericInput("hivepheromonerate", "Hive pheromone rate:",
                                 3, min = 1, max = 10, step = 1),
                    style = "background-color: #fdd;
                    padding: 2px 5px;
                    margin: 8px 0px;
                    border-radius: 5px",
                    id = "hinput"
                ),
                style = "margin-left:0px; margin-right:0px"
            ),
            column(
                width = 3,
                div(
                    h4("Food sources"),
                    sliderInput(
                        "foodscount", "Count of food sources:", 1, min = 1, max = 3, step = 1
                    ),
                    div(
                        div(
                            numericInput(
                                "food1x", "First x:", 20, min = 2, max = 49, step = 1
                            ),
                            style = "width:48%; float:left;"
                        ),
                        div(
                            numericInput(
                                "food1y", "First y:", 20, min = 2, max = 49, step = 1
                            ),
                            style = "width:48%; float:right;"
                        ),
                        sliderInput(
                            "food1amount", "First amount:", 100, min = 10, max = 1000, step = 10
                        ),
                        style = "background-color: rgba(127, 127, 127, 0.2);
                                 border-radius: 5px;
                                 padding: 0px 5px"
                    ),
                    conditionalPanel(
                        "input.foodscount > 1",
                        div(
                            div(
                                numericInput(
                                    "food2x", "Second x:", 10, min = 2, max = 49, step = 1
                                ),
                                style = "width:48%; float:left;"
                            ),
                            div(
                                numericInput(
                                    "food2y", "Second y:", 20, min = 2, max = 49, step = 1
                                ),
                                style = "width:48%; float:right;"
                            )
                        ),
                        sliderInput(
                            "food2amount", "Second amount:", 100, min = 10, max = 1000, step = 10
                        ),
                        style = "background-color: rgba(127, 127, 127, 0.2);
                                     border-radius: 5px;
                                     padding: 0px 5px"
                    ),
                    conditionalPanel(
                        "input.foodscount > 2",
                        div(
                            div(
                                numericInput(
                                    "food3x", "Third x:", 20, min = 2, max = 49, step = 1
                                ),
                                style = "width:48%; float:left;"
                            ),
                            div(
                                numericInput(
                                    "food3y", "Third y:", 10, min = 2, max = 49, step = 1
                                ),
                                style = "width:48%; float:right;"
                            )
                        ),
                        sliderInput(
                            "food3amount", "Third amount:", 100, min = 10, max = 1000, step = 10
                        ),
                        style = "background-color: rgba(127, 127, 127, 0.2);
                        border-radius: 5px;
                        padding: 0px 5px"
                    ),
                    style = "background-color: #ddf;
                    padding: 2px 5px;
                    margin: 8px 0px;
                    border-radius: 5px",
                    id = "finput"
                ),
                style = "margin-left:0px; margin-right:0px"
            ),
            column(
                width = 7,
                plotOutput("plot"),
                wellPanel(
                    div(
                    actionButton("play", img(src = "play.png", width = "36px"), style = "margin:0px 20px"),
                    actionButton("pause", img(src = "pause.png", width = "36px"), style = "margin:0px 20px"),
                    actionButton("stop", img(src = "stop.png", width = "36px"), style = "margin:0px 20px"),
                    style = "text-align:center"
                    ),
                    style = "margin:10px 40px 30px"
                ),
                wellPanel(
                    h3("Statistics"),
                    tableOutput("statistics")
                )
            )
        )
    ),
    tabPanel("Help & Info",
        includeMarkdown("help.md")
    ),
    useShinyjs()
))
