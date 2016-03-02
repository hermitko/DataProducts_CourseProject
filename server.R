library(shiny)

shinyServer(
    function(input, output, session){
        running <- FALSE
        paused <- FALSE
        counter_play <- 0
        counter_pause <- 0
        counter_stop <- -1
        #world <- "foo"
        tick <- reactive({
            if (input$pause > counter_pause & running){
                running <<- FALSE
                paused <<- TRUE
            }
            if (input$play > counter_play & !running){
                running <<- TRUE
                paused <<- FALSE
                disable("winput")
                disable("hinput")
                disable("finput")
            }
            if (input$stop > counter_stop | (!running & !paused)){
                running <<- FALSE
                paused <<- FALSE
                enable("winput")
                enable("hinput")
                enable("finput")
                foods_info <- data.frame(
                    x = c(input$food1x, input$food2x, input$food3x),
                    y = c(input$food1y, input$food2y, input$food3y),
                    food_remaining = c(input$food1amount,
                                       input$food2amount,
                                       input$food3amount)
                )
                foods <- lapply(
                    1:input$foodscount,
                    function(i){
                        Food(x = foods_info$x[i],
                             y = foods_info$y[i],
                             foodRemaining = foods_info$food_remaining[i],
                             pheromoneRate = 1)
                     }
                )
                world <<- World(
                    map = Map(width = input$width, height = input$height),
                    hive = Hive(x = input$hivex, y = input$hivey, bearRate = input$hivebearrate,
                                antLiveLength = 100L, broughtFood = 0L, pheromoneRate = input$hivepheromonrate),
                    foods = foods,
                    maxAntsAtPlace = input$maxantsatplace
                )
                world$tick()
            }
            counter_play <<- input$play
            counter_pause <<- input$pause
            counter_stop <<- input$stop
            if (running) {
                invalidateLater(1000, session)
                world$tick()
            }
        }
        )
        output$plot <- renderPlot({
            validate(
                need(input$hivex < input$width, "Hive's x-coord is greater or equal than world width!"),
                need(input$hivey < input$height, "Hive's y-coord is greater or equal than world height!"),
                need(input$hivex > 1, "Hive's x-coord is less or equal than 1!"),
                need(input$hivey > 1, "Hive's y-coord is less or equal than 1!"),
                need(input$hivex > 1 & input$hivey > 1 & input$hivex < input$width & input$hivey < input$height,
                     "Hive has to be in the inner of the map (not even on the border)!")
            )
            tick()
            world$displayGG()
            })
        output$tick <- renderText({
            validate(
                need(input$hivex > 1 & input$hivey > 1 & input$hivex < input$width & input$hivey < input$height,
                     "Error:")
            )
            tick()
            paste("World after", world$timeElapsed, "ticks:")
            })
        output$statistics <- renderTable({
            tick()
            data.frame(
                Statistics = c(
                    "Total number of living ants",
                    "Food brought to hive",
                    "Food items remaining to collect"),
                Value = c(
                    length(world$ants),
                    world$hive$broughtFood,
                    sum(sapply(world$foods,
                           function(f){f$foodRemaining}))
                )
            )
            },
            include.rownames = FALSE)
    }
)
