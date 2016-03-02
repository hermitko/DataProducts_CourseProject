library(ggplot2)
library(grid)

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
        moveDir = function(dx, dy) {
            world$map$antCounts[x, y] <- world$map$antCounts[x, y] - 1L
            x <<- x + dx
            y <<- y + dy
            world$map$antCounts[x, y] <-
                world$map$antCounts[x, y] + 1L
        },
        chooseDirCode = function() {
            ant_count_filter <-
                sapply(1:nrow(world$directions),
                       function(i){
                           if (world$map$antCounts[x + world$directions$x[i], y + world$directions$y[i]] >= world$maxAntsAtPlace &
                               x + world$directions$x[i] != world$hive$x & y + world$directions$y[i] != world$hive$y)
                           0.00001 else 1
                       }
                )
            if (explore) {
                dir_probs <- world$dirProbs[1, ]
                phers <-
                    sapply(1:nrow(world$directions),
                           function(i) {
                               world$map$foodPheromones[x + world$directions$x[i], y + world$directions$y[i]]
                           })
                dir_probs_rotated <- sapply(1:length(dir_probs),
                                            function(i) {
                                                d <- (i - dirCode) %% length(dir_probs) + 1
                                                dir_probs[d]
                                            })
                probs <- dir_probs_rotated * (1 + phers) * ant_count_filter
                sample(1:8, 1, FALSE, probs)
            }
            else {
                dir_probs <- world$dirProbs[2, ]
                phers <- sapply(1:nrow(world$directions),
                                function(i) {
                                    world$map$homePheromones[x + world$directions$x[i], y + world$directions$y[i]]
                                })
                dir_probs_rotated <- sapply(1:length(dir_probs),
                                            function(i) {
                                                d <- (i - dirCode) %% length(dir_probs) + 1
                                                dir_probs[d]
                                            })
                probs <- dir_probs_rotated * (1 + phers) * ant_count_filter
                which.max(probs)
            }
        },
        releasePheromones = function() {
            if (explore)
                world$map$homePheromones[x, y] <-
                    world$map$homePheromones[x, y] + 1
            else
                world$map$foodPheromones[x, y] <-
                    world$map$foodPheromones[x, y] + 1
        },
        checkFoodAndRelease = function() {
            found_food = FALSE
            for (food in world$foods) {
                if (explore &  x == food$x & y == food$y & food$foodRemaining > 0) {
                    found_food <- TRUE
                    food_founded <- food
                }
            }
            if (found_food) {
                food_founded$eat()
                explore <<- FALSE
                dirCode <<-
                    which.max(sapply(1:nrow(world$directions),
                                     function(i) {
                                         world$map$homePheromones[x + world$directions$x[i], y + world$directions$y[i]]
                                     }))
            }
            if (!explore & x == world$hive$x & y == world$hive$y) {
                world$hive$deployFood()
                explore <<- TRUE
                dirCode <<-
                    which.max(sapply(1:nrow(world$directions),
                                     function(i) {
                                         world$map$foodPheromones[x + world$directions$x[i], y + world$directions$y[i]]
                                     }))
            }
            releasePheromones()
        },
        move = function() {
            dir_code <- chooseDirCode()
            moveDir(world$directions$x[dir_code], world$directions$y[dir_code])
            ticksToDie <<- ticksToDie - 1L
        }
    )
)

Map <- setRefClass(
    "Map",
    fields = list(
        width = "integer",
        height = "integer",
        antCounts = "matrix",
        homePheromones = "matrix",
        foodPheromones = "matrix"
    ),
    methods = list(
        initialize = function(width = 30L,
                              height = 30L) {
            width <<- width
            height <<- height
            antCounts <<- matrix(0, nrow = width, ncol = height)
            homePheromones <<- matrix(0, nrow = width, ncol = height)
            foodPheromones <<- matrix(0, nrow = width, ncol = height)
        },
        show = function() {
            image(antCounts)
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
            world$map$antCounts[x, y] = world$map$antCounts[x, y] + bearRate
            sapply(1:bearRate,
                   function(i) {
                       ant <- Ant$new(
                           x = x,
                           y = y,
                           dirCode = sample.int(8, 1),
                           explore = TRUE,
                           ticksToDie = antLiveLength
                       )
                       phers <- sapply(1:nrow(world$directions),
                                       function(i) {
                                           world$map$foodPheromones[x + world$directions$x[i], y + world$directions$y[i]]
                                       })
                       ant$dirCode <- sample(1:8, size = 1, prob = 0.01 + phers)
                       ant
                   })
        },
        deployFood = function() {
            broughtFood <<- broughtFood + 1L
        },
        deployPheromones = function () {
            world$map$homePheromones[x, y] = world$map$homePheromones[x, y] + pheromoneRate
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
        },
    deployPheromones = function() {
            if (foodRemaining > 0)
                world$map$foodPheromones[x, y] <- world$map$foodPheromones[x, y] + pheromoneRate
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
        dirProbs = "matrix",
        pheromoneFilter = "list",
        directions = "data.frame",
        timeElapsed = "integer",
        maxAntsAtPlace = "integer"
    ),
    methods = list(
        initialize = function(directions = data.frame(
            x = c(0L, 1L, 1L, 1L, 0L,-1L,-1L,-1L),
            y = c(1L, 1L, 0L,-1L,-1L,-1L, 0L, 1L),
            row.names = c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
        ),
        dirProbs = matrix(
            c(
                0.34, 0.23, 0.1, 0, 0, 0, 0.1, 0.23,
                .3, .25, .1, 0, 0, 0, .1, .25
            ),
            nrow = 2,
            byrow = TRUE
        ),
        hive = Hive(
            x = 20L, y = 20L, bearRate = 2L, antLiveLength = 100L, broughtFood = 0L, pheromoneRate = 1
        ),
        foods = list(Food(
            x = 10L, y = 10L, foodRemaining = 1000L, pheromoneRate = 1
        )),
        map = Map(),
        ants = list(),
        pheromoneFilter =
            list(
                filter = matrix(c(0, .02, 0, .02, .9, .02, 0, .02, 0), nrow = 3, ncol = 3),
                xOffset = 2,
                yOffset = 2
            ),
        maxAntsAtPlace = 5L
        ) {
            directions <<- directions
            dirProbs <<- dirProbs
            hive <<- hive
            foods <<- foods
            map <<- map
            ants <<- ants
            pheromoneFilter <<- pheromoneFilter
            timeElapsed <<- -1L
            maxAntsAtPlace <<- maxAntsAtPlace
        },
        spreadPheromones = function() {
            map$homePheromones <<- apply_matrix_filter(
                map$homePheromones,
                pheromoneFilter$filter,
                pheromoneFilter$xOffset,
                pheromoneFilter$yOffset
            )
            map$foodPheromones <<- apply_matrix_filter(
                map$foodPheromones,
                pheromoneFilter$filter,
                pheromoneFilter$xOffset,
                pheromoneFilter$yOffset
            )
            for (ant in ants)
                ant$checkFoodAndRelease()
        },
        moveAnts = function() {
            for (ant in ants)
                ant$move()
        },
        checkAnts = function() {
            living_ants <- list()
            for (ant in ants) {
                if (ant$ticksToDie > 0 &
                    ant$x > 1 &
                    ant$x < map$width &
                    ant$y > 1 & ant$y < map$height)
                    living_ants <- c(living_ants, list(ant))
                else
                    map$antCounts[ant$x, ant$y] <<-
                        map$antCounts[ant$x, ant$y] - 1
            }
            ants <<- living_ants
        },
        bearAnts = function() {
            ants <<- c(ants, hive$bearAnts())
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
                x = 1:30, y = 1:30, map$homePheromones, add = TRUE,
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
            max_home <- max(map$homePheromones)
            homeRGBA <- array(
                c(
                    rep(0, map$width * map$height),
                    rep(1, map$width * map$height),
                    rep(1, map$width * map$height),
                    apply(map$homePheromones, 1, rev) / (2 * max_home)),
                c(map$height, map$width, 4)
            )
            max_food <- max(map$foodPheromones)
            foodRGBA <- array(
                c(
                    rep(1, map$width * map$height),
                    rep(0, map$width * map$height),
                    rep(0, map$width * map$height),
                    apply(map$foodPheromones, 1, rev) / (2 * max_food)),
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
                    #panel.border = element_blank(),
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
            for (f in foods) f$deployPheromones()
            hive$deployPheromones()
        },
        tick = function(count = 1L) {
            for (i in 1:count) {
                checkAnts()
                moveAnts()
                deployFoodAndHivePheromones()
                spreadPheromones()
                bearAnts()
            }
            timeElapsed <<- timeElapsed + count
        }
    )
)

