library(ggplot2)
library(grid)

directions <- data.frame(
    x = c(0L, 1L, 1L, 1L, 0L,-1L,-1L,-1L),
    y = c(1L, 1L, 0L,-1L,-1L,-1L, 0L, 1L),
    row.names = c("N", "NE", "E", "SE", "S", "SW", "W", "NW"))

dirProbs = matrix(
    c(
        0.34, 0.23, 0.1, 0, 0, 0, 0.1, 0.23,
        .2, .2, .2, 0, 0, 0, .2, .2
    ),
    nrow = 2,
    byrow = TRUE
)

create_map <-
    function(
        width = 30L,
        height = 30L,
        hive = c(10L, 10L),
        foods_coord = data.frame(x = 20L, y = 20L),
        pheromone_filter =  list(
            filter = matrix(c(0, .02, 0, .02, .9, .02, 0, .02, 0), nrow = 3, ncol = 3),
            xOffset = 2,
            yOffset = 2
        )
    )
    {
        map <- Map(width = width, height = height, pheromoneFilter = pheromone_filter)
        map$environ[hive[1], hive[2], "foodsAndHives"] <- 1
        for (i in 1:nrow(foods_coord)){
            map$environ[foods_coord$x[i], foods_coord$y[i], "foodsAndHives"] <- -1
        }
        map
    }

create_world <-
    function(
        width = 30L,
        height = 30L,
        hive_x = 10L,
        hive_y = 10L,
        bear_rate = 2L,
        ant_live_length = 100L,
        brought_food = 0L,
        hive_pheromone_rate = 1,
        foods_data = data.frame(x = 20L, y = 20L, food_remaining = 1000L, pheromone_rate = 1),
        pheromone_filter =
            list(
                filter = matrix(c(0, .02, 0, .02, .9, .02, 0, .02, 0), nrow = 3, ncol = 3),
                xOffset = 2,
                yOffset = 2
            ),
        max_ants_at_place = 5L
    ) {
        hive <- Hive(x = hive_x, y = hive_y, bearRate = bear_rate,
                    antLiveLength = ant_live_length, broughtFood = brought_food,
                    pheromoneRate = hive_pheromone_rate)
        foods <- lapply(1:nrow(foods_data),
                         function(i){
                             Food(x = foods_data[i, 1], y = foods_data[i, 2],
                                  foodRemaining = foods_data[i, 3], pheromoneRate = foods_data[i, 4])
                         })
        map <- create_map(width = width, height = height, hive = c(hive_x, hive_y),
                    foods_coord = foods_data[, 1:2], pheromone_filter = pheromone_filter)
        World(hive = hive, foods = foods, map = map, ants = list(), maxAntsAtPlace = max_ants_at_place)
    }


apply_matrix_filter <- function(input, filter, x_offset, y_offset) {
    filtered <-
        matrix(
            0, nrow = nrow(input) + nrow(filter) - 1, ncol = ncol(input) + ncol(filter) - 1
        )
    for (i in 1:nrow(filter)) {
        for (j in 1:ncol(filter)) {
            filtered_col <-
                rbind(
                    matrix(
                        0, nrow = nrow(filter) - i, ncol = ncol(input)
                    ),
                    input * filter[i, j],
                    matrix(0, nrow = i - 1, ncol = ncol(input))
                )
            filtered_row <-
                cbind(
                    matrix(
                        0, nrow = nrow(input) + nrow(filter) - 1, ncol = ncol(filter) - j
                    ),
                    filtered_col,
                    matrix(
                        0, nrow = nrow(input) + nrow(filter) - 1, ncol = j - 1
                    )
                )
            filtered <- filtered + filtered_row
        }
    }
    filtered[1:nrow(input) + x_offset - 1, 1:ncol(input) + y_offset - 1]
}


add_RGBA_filters <- function(f1, f2){
    width <- dim(f1)[1]
    height <- dim(f1)[2]
    res <- f1
    for (i in 1:width){
        for (j in 1:height){
            res[i, j, 1:3] <-
                0.005 + 0.99 * ((1 - f1[i, j, 4]) * f1[i, j, 1:3] + (1 - f2[i, j, 4]) * f2[i, j, 1:3]) / (2 - f1[i, j, 4] - f2[i, j, 4])
            res[i, j, 4] <- 1 - (1 - f1[i, j, 4]) * (1 - f2[i, j, 4])
        }
    }
    res
}

Ant <- setRefClass(
    "Ant",
    fields = list(
        x = "integer",
        y = "integer",
        dirCode = "integer",
        explore = "logical",
        ticksToDie = "integer"
    ),
    methods = list(
        chooseDirCode = function(neighborhood, max_ants_at_place) {
            home_pheromons <- neighborhood[, , "homePheromones"]
            food_pheromons <- neighborhood[, , "foodPheromones"]
            ant_counts <- neighborhood[, , "antCounts"]
            hive_and_food <- neighborhood[, , "foodsAndHives"]
            ant_count_filter <-
                sapply(1:nrow(directions),
                       function(i){
                           if (ant_counts[2 + directions$x[i], 2 + directions$y[i]] >= max_ants_at_place &
                               hive_and_food[2 + directions$x[i], 2 + directions$y[i]] != 1) # There is the hive
                           0.00001 else 1
                       }
                )
            if (explore) {
                dir_probs <- dirProbs[1, ]
                phers <-
                    sapply(
                        1:nrow(directions),
                        function(i) {
                            food_pheromons[2 + directions$x[i], 2 + directions$y[i]]
                        }
                    )
                dir_probs_rotated <-
                    sapply(
                        1:length(dir_probs),
                        function(i) {
                            d <- (i - dirCode) %% length(dir_probs) + 1
                            dir_probs[d]
                        }
                    )
                probs <- dir_probs_rotated * (1 + phers) * ant_count_filter
                sample(1:8, 1, FALSE, probs)
            }
            else {
                dir_probs <- dirProbs[2, ]
                phers <-
                    sapply(
                        1:nrow(directions),
                        function(i) {
                            home_pheromons[2 + directions$x[i], 2 + directions$y[i]]
                        }
                    )
                dir_probs_rotated <- sapply(1:length(dir_probs),
                                            function(i) {
                                                d <- (i - dirCode) %% length(dir_probs) + 1
                                                dir_probs[d]
                                            })
                probs <- dir_probs_rotated * (1 + phers) * ant_count_filter
                which.max(probs)
            }
        },
        setBestDirCode = function(neighborhood){
            pher_code = if (explore) 2 else 1
            phers <-
                sapply(
                    1:nrow(directions),
                    function(i) {
                        neighborhood[2 + directions$x[i], 2 + directions$y[i], pher_code]
                    }
                )
            dirCode <<- which.max(phers)
        },
        move = function(neighborhood, max_ants_at_place) {
            dirCode <<- chooseDirCode(neighborhood, max_ants_at_place)
            ticksToDie <<- ticksToDie - 1L
            x <<- x + directions$x[dirCode]
            y <<- y + directions$y[dirCode]
        }
    )
)

Map <- setRefClass(
    "Map",
    fields = list(
        width = "integer",
        height = "integer",
        environ = "array", #homePheromones, foodPheromones, antCounts, foodsAndHives
        pheromoneFilter = "list"
    ),
    methods = list(
        initialize =
            function(
                width = 30L,
                height = 30L,
                pheromoneFilter =  list(
                    filter = matrix(c(0, .02, 0, .02, .9, .02, 0, .02, 0), nrow = 3, ncol = 3),
                    xOffset = 2,
                    yOffset = 2
                )
            )
            {
                width <<- width
                height <<- height
                environ <<- array(0, dim = c(width, height, 4))
                dimnames(environ) <<- list(NULL, NULL,
                                           c("homePheromones", "foodPheromones",
                                             "antCounts", "foodsAndHives"))
                pheromoneFilter <<- pheromoneFilter
            },
        addHomePheromones = function(x, y, amount){
            environ[x, y, "homePheromones"] <<-
                environ[x, y, "homePheromones"] + amount
        },
        addFoodPheromones = function(x, y, amount){
            environ[x, y, "foodPheromones"] <<-
                environ[x, y, "foodPheromones"] + amount
        },
        moveAnt = function(from_x, from_y, to_x, to_y){
            environ[from_x, from_y, "antCounts"] <<-
                environ[from_x, from_y, "antCounts"] - 1
            environ[to_x, to_y, "antCounts"] <<-
                environ[to_x, to_y, "antCounts"] - 1
        },
        spreadPheromones = function() {
            environ[, , "homePheromones"] <<- apply_matrix_filter(
                environ[, , "homePheromones"],
                pheromoneFilter$filter,
                pheromoneFilter$xOffset,
                pheromoneFilter$yOffset
            )
            environ[, , "foodPheromones"] <<- apply_matrix_filter(
                environ[, , "foodPheromones"],
                pheromoneFilter$filter,
                pheromoneFilter$xOffset,
                pheromoneFilter$yOffset
            )
        }
    )
)

Hive <- setRefClass(
    "Hive",
    fields = list(
        x = "integer",
        y = "integer",
        bearRate = "integer",
        antLiveLength = "integer",
        broughtFood = "integer",
        pheromoneRate = "numeric"
    ),
    methods = list(
        bearAnts = function() {
            sapply(1:bearRate,
                   function(i) {
                       ant <- Ant$new(
                           x = x,
                           y = y,
                           dirCode = sample.int(8, 1),
                           explore = TRUE,
                           ticksToDie = antLiveLength
                       )
                       ant$setBestDirCode
                       ant
                   })
        },
        deployFood = function() {
            broughtFood <<- broughtFood + 1L
        }
    )
)

Food <- setRefClass(
    "Food",
    fields = list(
        x = "integer",
        y = "integer",
        foodRemaining = "integer",
        pheromoneRate = "numeric"
    ),
    methods = list(
        eat = function() {
            foodRemaining <<- foodRemaining - 1L
        }
    )
)

World <- setRefClass(
    "World",
    fields = list(
        hive = "Hive",
        foods = "list",
        map = "Map",
        ants = "list",
        timeElapsed = "integer",
        maxAntsAtPlace = "integer"
    ),
    methods = list(
        initialize = function(
            hive = Hive(
                x = 10L, y = 10L, bearRate = 2L, antLiveLength = 100L, broughtFood = 0L, pheromoneRate = 1
            ),
            foods = list(Food(x = 10L, y = 10L, foodRemaining = 1000L, pheromoneRate = 1)),
            map = Map(),
            ants = list(),
            maxAntsAtPlace = 5L
        ) {
            hive <<- hive
            foods <<- foods
            map <<- map
            ants <<- ants
            timeElapsed <<- -1L
            maxAntsAtPlace <<- maxAntsAtPlace
        },
        moveAnts = function() {
            for (ant in ants){
                old_x = ant$x
                old_y = ant$y
                ant$move(map$environ[ant$x + -1:1, ant$y + -1:1, ], maxAntsAtPlace)
                map$moveAnt(old_x, old_y, ant$x, ant$y)
            }
        },
        checkAntsLive = function() {
            living_ants <- list()
            for (ant in ants) {
                if (ant$ticksToDie > 0 &
                    ant$x > 1 &
                    ant$x < map$width &
                    ant$y > 1 & ant$y < map$height)
                    living_ants <- c(living_ants, list(ant))
                else
                    map$environ[ant$x, ant$y, "antCounts"] <<-
                        map$environ[ant$x, ant$y, "antCounts"] - 1
            }
            ants <<- living_ants
        },
        checkAntsTask = function(){
            for (ant in ants) {
                if (!ant$explore & ant$x == hive$x & ant$y == hive$y){
                    hive$deployFood()
                    ant$explore = TRUE
                    ant$setBestDirCode(map$environ[ant$x + -1:1, ant$y + -1:1, ])
                }
                if (ant$explore){
                    for (food in foods){
                        if (ant$x == food$x & ant$y == food$y & food$foodRemaining > 0) {
                            food$eat()
                            ant$explore = FALSE
                            ant$setBestDirCode(map$environ[ant$x + -1:1, ant$y + -1:1, ])
                        }
                    }
                }
            }
        },
        bearAnts = function() {
            ants <<- c(ants, hive$bearAnts())
            map$environ[hive$x, hive$y, "antCounts"] <<-
                map$environ[hive$x, hive$y, "antCounts"] + hive$bearRate
        },
        display = function() {
            ant_frame <- data.frame(t(sapply(ants, function(a) {
                c(x = a$x, y = a$y)
                })),
                explore = sapply(ants, function(a) {
                    a$explore
                })
            )
            plot(
                jitter(ant_frame$x[ant_frame$explore]),
                jitter(ant_frame$y[ant_frame$explore]),
                xlim = c(0, map$width + 1),
                ylim = c(0, map$height + 1)
            )
            points(jitter(ant_frame$x[!ant_frame$explore]),
                   jitter(ant_frame$y[!ant_frame$explore]),
                   col = 2)
            image(
                x = 1:30, y = 1:30, map$environ[, , "homePheromones"], add = TRUE,
                col = rainbow(255, alpha = .5)
            )
        },
        displayGG = function(){
            ant_frame <- data.frame(t(sapply(ants, function(a) {
                    c(x = a$x, y = a$y)
                })),
                explore = sapply(ants, function(a) {
                    a$explore
                }))
            max_home <- max(map$environ[, , "homePheromones"])
            homeRGBA <- array(
                c(
                    rep(0, map$width * map$height),
                    rep(1, map$width * map$height),
                    rep(1, map$width * map$height),
                    apply(map$environ[, , "homePheromones"], 1, rev) / (2 * max_home)),
                c(map$height, map$width, 4)
            )
            max_food <- max(map$environ[, , "foodPheromones"])
            foodRGBA <- array(
                c(
                    rep(1, map$width * map$height),
                    rep(0, map$width * map$height),
                    rep(0, map$width * map$height),
                    apply(map$environ[, , "foodPheromones"], 1, rev) / (2 * max_food)),
                c(map$height, map$width, 4)
            )
            grobRGBA <- add_RGBA_filters(homeRGBA, foodRGBA)
            foodsLabs <-
                data.frame(t(sapply(foods, function(f) {
                    c(x = f$x, y = f$y)
                })),
                foodRemaining = sapply(foods, function(f) {
                    f$foodRemaining
                }))
            g <- ggplot(data = ant_frame, aes(x = jitter(x), y = jitter(y))) +
                 annotation_custom(
                     rasterGrob(
                         grobRGBA,
                         width=unit(1, "npc"),
                         height=unit(1, "npc"),
                         interpolate = FALSE), -Inf, Inf, -Inf, Inf) +
                coord_cartesian(xlim = c(.5, map$width + .5),
                                ylim=c(.5, map$height + .5)) +
                geom_point(aes(color = !explore)) +
                theme_bw() +
                theme(
                    plot.background = element_blank(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    axis.text = element_blank(),
                    axis.ticks = element_blank(),
                    axis.title = element_blank(),
                    legend.position = "none"
                ) +
                annotate("text", x = hive$x, y = hive$y, label = hive$broughtFood, color = rgb(0.6, 0, 0)) +
                annotate("text", x = foodsLabs$x, y = foodsLabs$y, label = foodsLabs$foodRemaining, color = rgb(0, 0.6, 0.6))
            g
        },
        deployFoodAndHivePheromones = function () {
            for (f in foods)
                if (f$foodRemaining > 0)
                    map$addFoodPheromones(f$x, f$y, f$pheromoneRate)
            map$addHomePheromones(hive$x, hive$y, hive$pheromoneRate)
            for (ant in ants) {
                if (ant$explore) {
                    map$addHomePheromones(ant$x, ant$y, 1)
                } else {
                    map$addFoodPheromones(ant$x, ant$y, 1)
                }
            }
        },
        tick = function(count = 1L) {
            for (i in 1:count) {
                checkAntsLive()
                moveAnts()
                checkAntsTask()
                deployFoodAndHivePheromones()
                map$spreadPheromones()
                bearAnts()
            }
            timeElapsed <<- timeElapsed + count
        }
    )
)

