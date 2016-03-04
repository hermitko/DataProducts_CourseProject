library(shiny)

shinyServer(
    function(input, output, session){
        running <- FALSE
        paused <- FALSE
        counter_play <- 0
        counter_pause <- 0
        counter_stop <- -1
        world <- "foo"
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
                                       input$food3amount),
                    pheromone_rate = c(1, 1, 1)
                )
                world <<- create_world(
                    width = input$width,
                    height = input$height,
                    hive_x = input$hivex,
                    hive_y = input$hivey,
                    bear_rate = input$hivebearrate,
                    ant_live_length = input$antlivelength,
                    brought_food = 0L,
                    hive_pheromone_rate = input$hivepheromonerate,
                    foods_data = foods_info[1:input$foodscount, ],
                    max_ants_at_place = input$maxantsatplace
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
                need(input$hivex < input$width,
                     "Hive's x-coord is greater or equal than world width!"),
                need(input$hivey < input$height,
                     "Hive's y-coord is greater or equal than world height!"),
                need(input$hivex > 1,
                     "Hive's x-coord is less or equal than 1!"),
                need(input$hivey > 1,
                     "Hive's y-coord is less or equal than 1!"),
                need(input$hivex > 1 & input$hivey > 1 & input$hivex < input$width & input$hivey < input$height,
                     "Hive has to be in the inner of the map (not even on the border)!"),
                need(input$food1x < input$width,
                     "First food source's x-coord is greater or equal than world width!"),
                need(input$food1x > 1,
                     "First food source's x-coord is less or equal than 1!"),
                need(input$food1y < input$height,
                     "First food source's y-coord is greater or equal than world height!"),
                need(input$food1y > 1,
                     "First food source's y-coord is less or equal than 1!"),
                need(input$food2x < input$width | input$foodscount <= 1,
                     "Second food source's x-coord is greater or equal than world width!"),
                need(input$food2x > 1 | input$foodscount <= 1,
                     "Second food source's x-coord is less or equal than 1!"),
                need(input$food2y < input$height | input$foodscount <= 1,
                     "Second food source's y-coord is greater or equal than world height!"),
                need(input$food2y > 1 | input$foodscount <= 1,
                     "Second food source's y-coord is less or equal than 1!"),
                need(input$food3x < input$width | input$foodscount <= 2,
                     "Third food source's x-coord is greater or equal than world width!"),
                need(input$food3x > 1 | input$foodscount <= 2,
                     "Third food source's x-coord is less or equal than 1!"),
                need(input$food3y < input$height | input$foodscount <= 2,
                     "Third food source's y-coord is greater or equal than world height!"),
                need(input$food3y > 1 | input$foodscount <= 2,
                     "Third food source's y-coord is less or equal than 1!"),
                need(input$food1x < input$width & input$food1y < input$height &
                         (input$foodscount <= 1 | input$food2x < input$width &
                              input$food2y < input$height & input$food2x > 1 & input$food2y > 1) &
                         (input$foodscount <= 2 | input$food3x < input$width &
                              input$food3y < input$height & input$food3x > 1 & input$food3y > 1),
                     "Food sources have to be in the inner of the map!")
            )
            tick()
            world$display()
            })
        output$tick <- renderText({
            validate(
                need(input$hivex > 1 & input$hivey > 1 & input$hivex < input$width & input$hivey < input$height,
                     "Error:"),
                need(input$food1x < input$width & input$food1y < input$height &
                         (input$foodscount <= 1 | input$food2x < input$width &
                              input$food2y < input$height & input$food2x > 1 & input$food2y > 1) &
                         (input$foodscount <= 2 | input$food3x < input$width &
                              input$food3y < input$height & input$food3x > 1 & input$food3y > 1),
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
