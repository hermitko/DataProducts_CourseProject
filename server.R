library(shiny)

running <- FALSE
counter_play <- 0
counter_stop <- -1
ticks <- 0

shinyServer(
    function(input, output, session){
        tick <- reactive({
            if (input$play > counter_play & !running){
                running <<- TRUE
            }
            if (input$stop > counter_stop | !running){
                world <<- World(
                    hive = Hive(x = input$hivex, y = input$hivey, bearRate = 2L,
                                antLiveLength = 100L, broughtFood = 0L, pheromoneRate = 1)
                )
                world$tick()
                ticks <<- 0
                running <<- FALSE
            }
            counter_play <<- input$play
            counter_stop <<- input$stop
            if (running) {
                invalidateLater(1000, session)
                world$tick()
                ticks <<- ticks + 1
                #print(world$map$homePheromones)
            }
        }
        )
        output$plot <- renderPlot({
            validate(
                need(input$hivex <= input$width, "Hive's x-coord is greater than world width!"),
                need(input$hivey <= input$height, "Hive's y-coord is greater than world height!")
            )
            tick()
            world$displayGG()
            })
        output$tick <- renderText({
            tick()
            paste("World after", world$timeElapsed, "ticks.")
            })
    }
)
