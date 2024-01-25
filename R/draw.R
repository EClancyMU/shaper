
#' Draw/Visualize shapes in 2D and 3D.
#'
#' This  provides functions to visually draw various shapes,
#' including 2D shapes such rectangles, squares, circles, triangles,
#' and 3D shapes such as cuboids, cubes, spheres, and cylinders.
#'
#' @param shape An object of class 'shape'.
#' @param color The border color of the shape (default is "blue" for 2D shapes
#' and "red" for 3d shapes but can be easily changed.).
#' @param fillColor The fill color of the shape (default is "transparent" but
#' can be easily changed).
#'
#' @details The function checks if the input object is of class 'shape'. If not,
#' it raises an error indicating that the input must be an object of class
#' 'shape'.If the input is of class 'shape', it dispatches the drawing to the
#'  appropriate method based on the type of shape.
#'
#'
#' @family draw functions
#'
#' @examples
#' # Create a rectangle
#' my_rectangle <- rectangle(height = 4, width = 6)
#'
#' # Create a sphere
#' my_sphere <- sphere(radius = 2)
#'
#' # Draw the sphere
#' draw(my_sphere)
#'
#'@export
draw <- function(shape, color = "blue", fillColor = "transparent") {
  if(!inherits(shape,"shape")){
    stop("Draw can only be passed objects of class shape")
  }
  else{
    UseMethod("draw")
  }
}

#' Draw a rectangle in 2d.
#'
#' @param shape An object of class 'shape' representing a rectangle.
#' @param color The border color of the rectangle (default is "blue").
#' @param fillColor The fill color of the rectangle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "aes" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
#'
#' @family draw functions
#' @seealso \code{\link{draw}}
#'
#' @examples
#' # Create a rectangle
#' my_rectangle <- rectangle(height = 4, width = 6)
#'
#' # Draw the rectangle
#' draw.rectangle(my_rectangle)
#'
#' @export
draw.rectangle <- function(shape, color = "blue", fillColor = "transparent") {
  x <- y <- NULL  # Dummy definitions

  rectangle_data <- data.frame(
    x = c(0, shape$width, shape$width, 0),
    y = c(0, 0, shape$height, shape$height)
  )

  max_dim <- max(shape$height, shape$width)

  p <- ggplot(rectangle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Width", y = "Height", title = "Rectangle") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, max_dim), y = c(0, max_dim))

  print(p)
}


#' Draw a square in 2D.
#'
#' @param shape An object of class 'shape' representing a square.
#' @param color The border color of the square (default is "blue").
#' @param fillColor The fill color of the square (default is "transparent").
#' @importFrom ggplot2 "ggplot" "aes" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
#' @family draw functions
#' @seealso \code{\link{draw}}
#'
#' @examples
#' # Create a square
#' my_square <- square(sideLength = 5)
#'
#' # Draw the square
#' draw.square(my_square)
#'
#' @export
draw.square <- function(shape, color = "blue", fillColor = "transparent") {
  x <- y <- NULL  # Dummy definitions

  square_data <- data.frame(
    x = c(0, shape$sideLength, shape$sideLength, 0),
    y = c(0, 0, shape$sideLength, shape$sideLength)
  )

  p <- ggplot(square_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Side Length", y = "Side Length", title = "Square") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, shape$sideLength), y = c(0, shape$sideLength))

  print(p)
}

#' Draw a triangle in 2D.
#'
#' @param shape An object of class 'shape' representing a triangle.
#' @param color The border color of the triangle (default is "blue").
#' @param fillColor The fill color of the triangle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "aes" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
#' @family draw functions
#'
#' @examples
#' # Create a triangle
#' my_triangle <- triangle(a = 3, b = 4, c = 5)
#'
#' # Draw the triangle
#' draw.triangle(my_triangle)
#'
#' @export
draw.triangle <- function(shape, color = "blue", fillColor = "transparent") {
  x <- y <- NULL  # Dummy definitions

  triangle_data <- data.frame(
    x = c(0, shape$a, shape$b),
    y = c(0, 0, shape$c)
  )

  max_dim <- max(shape$a, shape$b, shape$c)

  p <- ggplot(triangle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Base", y = "Height", title = "Triangle") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, max_dim), y = c(0, max_dim))

  print(p)
}

#' Draw a circle in 2D.
#'
#' @param shape An object of class 'shape' representing a circle.
#' @param color The border color of the circle (default is "blue").
#' @param fillColor The fill color of the circle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "aes" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
#'
#'
#' @family draw functions
#'
#' @examples
#' # Create a circle
#' my_circle <- circle(radius = 3)
#'
#' # Draw the circle
#' draw.circle(my_circle)
#'
#' @export
draw.circle <- function(shape, color = "blue", fillColor = "transparent") {
  x <- y <- NULL  # Dummy definitions

  circle_data <- data.frame(
    x = shape$radius * cos(seq(0, 2 * pi, length.out = 100)),
    y = shape$radius * sin(seq(0, 2 * pi, length.out = 100))
  )

  p <- ggplot(circle_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "X-axis", y = "Y-axis", title = "Circle") +
    theme_minimal() +
    coord_fixed(ratio = 1)

  print(p)
}

#' Draw a sphere in 3D.
#'
#' @param shape An object of class 'shape' representing a sphere.
#' @param color The color of the sphere (default is "#FF0000 or 'red').
#' @param ... Additional arguments passed to `sphere3d` and other
#'  underlying functions.
#' @importFrom rgl "open3d" "spheres3d" "decorate3d" "cylinder3d"
#' "mesh3d" "qmesh3d"
#' "shade3d" "wire3d"
#'
#' @family draw functions
#'
#' @examples
#' # Create a sphere
#' my_sphere <- sphere(radius = 2)
#'
#' # Draw the sphere
#' draw.sphere(my_sphere)
#'

#' @export
draw.sphere <- function(shape, color =  "#FF0000",...) {
  open3d()

  spheres3d(0, 0, 0, radius = shape$radius, color = color, alpha = 0.7)
  decorate3d(box = FALSE ,axes = TRUE)
}


#' Draw a cuboid in 3D using rgl.
#'
#' @param shape An object of class 'shape' representing a cube.
#' @param color The color of the cube (default is "red").
#' @param ... Additional arguments passed to underlying rgl functions.
#' @importFrom rgl "open3d" "spheres3d" "decorate3d" "cylinder3d"
#'  "mesh3d" "qmesh3d"
#' "shade3d" "wire3d"
#'
#' @family draw functions
#'
#'
#' @examples
#' # Create a cube
#' my_cuboid <- cuboid(height = 3, width = 3, depth = 3)
#'
#' # Draw the cube
#' draw.cuboid(my_cuboid)
#'
#' @export draw cuboid
draw.cuboid <- function(shape, color =  "red", ...) {
  height <- shape$height
  width <- shape$width
  depth <- shape$depth

  vertices <- cbind(
    c(0,0,0),
    c(0,0, height),
    c(0, depth,0),
    c(0, depth, height),
    c( width,0,0),
    c( width,0, height),
    c( width, depth,0),
    c( width, depth, height)
  )

  indices <- cbind(
    c(1, 5, 7, 3),
    c(2, 6, 8, 4),
    c(1, 2, 4, 3),
    c(5, 6, 8, 7),
    c(3, 7, 8, 4),
    c(1, 5, 6, 2)
  )

  cuboid <- qmesh3d(
    vertices = vertices,
    indices = indices,
    homogeneous = FALSE
  )

  open3d()
  shade3d(cuboid, color = color, alpha = 0.7)
  wire3d
  decorate3d()
}


#' Draw a cube in 3D using rgl.
#'
#' @param shape An object of class 'shape' representing a cube.
#' @param color The color of the cube (default is "red").
#' @param ... Additional arguments passed to underlying rgl functions.
#' @importFrom rgl "open3d" "spheres3d" "decorate3d" "cylinder3d" "mesh3d"
#'  "qmesh3d"
#' "shade3d" "wire3d"
#'
#' @family draw functions
#'
#'
#' @examples
#' # Create a cube
#' my_cube <- cube(sideLength = 5)
#'
#' # Draw the cube
#' draw.cube(my_cube)
#'
#' @export
draw.cube <- function(shape, color = "red", ...) {
  sideLength <- shape$sideLength

  vertices <- cbind(
    c(0,0,0),
    c(0,0, sideLength),
    c(0, sideLength,0),
    c(0, sideLength, sideLength),
    c(sideLength,0,0),
    c(sideLength,0, sideLength),
    c(sideLength, sideLength,0),
    c(sideLength, sideLength, sideLength)
  )

  indices <- cbind(
    c(1, 5, 7, 3),
    c(2, 6, 8, 4),
    c(1, 2, 4, 3),
    c(5, 6, 8, 7),
    c(3, 7, 8, 4),
    c(1, 5, 6, 2)
  )

  cube_mesh <- qmesh3d(
    vertices = vertices,
    indices = indices,
    homogeneous = FALSE
  )

  open3d()
  shade3d(cube_mesh, color = color, alpha = 0.7)
  wire3d
  decorate3d()
}

#' Draw a cylinder in 3D.
#'
#' @param shape An object of class 'shape' representing a cylinder.
#' @param color The color of the cylinder (default is "red").
#' @param ... Additional arguments passed to `cylinder3d` and
#' other underlying functions.
#' @export
#' @importFrom rgl "open3d" "spheres3d" "decorate3d" "cylinder3d" "qmesh3d"
#' "shade3d" "mesh3d" "wire3d"
#'
#' @family draw functions
#'
#' @examples
#' # Create a cylinder
#' my_cylinder <- cylinder(radius = 2, height = 5)
#'
#' # Draw the cylinder
#' draw.cylinder(my_cylinder)
#'
#' @export
draw.cylinder <- function(shape, color = "red", ...){
  center <- matrix(c(0, 0, 0, 0, 0, shape$height), ncol = 3, byrow = TRUE)
  radius <- shape$radius
  cylinder_mesh <- cylinder3d(center = center, radius = radius, sides = 1000,closed=-2)
  open3d()
  shade3d(cylinder_mesh, color = color, alpha = 0.7)
  decorate3d()
}

