## Quick user quide
### Background

The purpose of this application is to simulate ants living on the flat
rectangular world divided into rectangular cells.
They are looking for food sources. When they found food, they try to bring it
to the hive. In every time interval, each ant move one step in one of eight
possible directions (i.e. to one of the adjacent cells sharing at least a
vertex with the current one).

Ants aren't communicating directly, they use *pheromones* to mark their path
to food / to home. Exploring ants are likely to follow the path marked by
*food pheromones*, ants that bring food back home follow the path marked by
*home pheromones*.

The app is very simple -- it doesn't not include any kind of obstacles or
terrain information. However with not a huge effort, these features can
be added (as well as e.g. hexagonal world, etc.).

### Controls

When the simulation is stopped (which is the default), user can change parameters
of the world where simulation is to take place.

#### World parameters
- `Width` -- the horizontal size of the world; an integer from [10, 50]
- `Height` -- the vertical size of the world; an integer from [10, 50]
- `Maximum ants at one place` -- ant won't move to the *overcrowded* cell,
  this parameter is the number of ant in a cell to call it *overcrowded*

#### Hive parameters
- `The x-coord` -- self-descriptive (isn't it?); has to be greater than one and 
  less than the width of the world
- `The y-coord` -- like above; 
  has to be greater than one and less than world's height; the hive hase to be in
  the interior of the world (not even on the border)
- `Ants born each tick` -- the number of newborn ants each time interval of the 
  simulation; integer from [1, 5]
- `Lifespan` -- the number of time intervals until the death (and deletion) 
  of each ant; integer from [50, 200]
- `Hive pheromone rate` -- the hive is deploying this amount of home pheromones 
  each time interval of the simulation; integer from [1, 10]

#### Food sources
- `Count of food sources` -- self-descriptive; {1, 2, 3}; when greater than one, 
  box for specifying controls specific to food source appears
- `n-th x` -- the *x*-coord of *n*-th food source
- `n-th y` -- the *y*-coord of *n*-th food source; 
  again, it has to be in the interior of the world
- `n-th amount` -- the amount of food in specified food source (each ant can 
  carry one unit of food)
  
#### Simulation controls
I suppose everyone has seen some kind of either software or hardware player:-).

### Simulation image
The simulation displays the hive, the food sources, ants (red colored are
exploring, azure ones are carrying food to hive) and the pheromone background
(shades of red represent home pheromones, shades of azure represent food 
pheromones).

It realizes as a graph plotted by `ggplot2` package. The pheromone background is 
constructed as a `rasterGrob` from `grid` package.

### Statistics
A few numbers describing the actual state of the world. Merely as an ilustration,
that the app is able to track a state and display it.

## Remarks 
The application was written as a course project for Coursera's
[Developing Data Products](https://www.coursera.org/learn/data-products) class.

All the source files can be found at https://github.com/hermitko/DataProducts_CourseProject.

The logic of the application is included in the `global.R` file. The reason for
choosing the theme was to learn some OOP in R. After exploring S3 and S4 systems
(that are not suitable for this kind of object manipulating),
I chose RC system.

The images are properly licensed:
- <a href="https://commons.wikimedia.org/wiki/File:Ant_(Jacob_Eckert).svg">Ant</a> from Jacob Eckert
- [Buttons](https://openclipart.org/detail/17304/deck-or-vcr-button) from czara1

Created by Jan Herman in 2016.
