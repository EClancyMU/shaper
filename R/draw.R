
#' Draw/Visualize shapes in 2D and 3D.
#'
#' This  provides functions to visually draw various shapes,
#' including 2D shapes such rectangles, squares, circles, triangles,
#' and 3D shapes such as cuboids, cubes, spheres, and cylinders.
#'
#' @param shape An object of class 'shape'.
#' @param color The border color of the shape (default is "blue" but can be easily
#' changed.).
#' @param fillColor The fill color of the shape (default is "transparent" but can be
#' easily changed).
#'
#' @details The function checks if the input object is of class 'shape'. If not,
#' it raises an error indicating that the input must be an object of class 'shape'.
#' If the input is of class 'shape', it dispatches the drawing to the appropriate
#' method based on the type of shape.
#'
#'
#' @family draw functions
#'
#' @examples
#' # Create a rectangle
#' my_rectangle <- rectangle(height = 4, width = 6)
#'
#' # Draw the rectangle
#' draw(my_rectangle, color = blue, fillColor = "transparent
#' )
#'
#' # Create a sphere
#' my_sphere <- sphere(radius = 2)
#'
#' # Draw the sphere
#' draw(my_sphere)
#' }
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

#' Draw a rectangle in 2D.
#'
#' @param rec An object of class 'shape' representing a rectangle.
#' @param color The border color of the rectangle (default is "blue").
#' @param fillColor The fill color of the rectangle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
#'
#' @family draw functions
#' @seealso \code{\link{draw}}
#'
#' @examples
#' \dontrun{
#' # Create a rectangle
#' my_rectangle <- rectangle(height = 4, width = 6)
#'
#' # Draw the rectangle
#' draw.rectangle(my_rectangle)
#' }
draw.rectangle <- function(rec, color = "blue", fillColor = "transparent") {
  rectangle_data <- data.frame(
    x = c(0, rec$width, rec$width, 0),
    y = c(0, 0, rec$height, rec$height)
  )

  max_dim <- max(rec$height, rec$width)

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
#' @param squ An object of class 'shape' representing a square.
#' @param color The border color of the square (default is "blue").
#' @param fillColor The fill color of the square (default is "transparent").
#' @importFrom ggplot2 "ggplot" "geom_polygon" "labs" "theme_minimal"
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
draw.square <- function(squ, color = "blue", fillColor = "transparent") {
  square_data <- data.frame(
    x = c(0, squ$sideLength, squ$sideLength, 0),
    y = c(0, 0, squ$sideLength, squ$sideLength)
  )

  p <- ggplot(square_data, aes(x, y)) +
    geom_polygon(fill = fillColor, color = color, size = 2) +
    labs(x = "Side Length", y = "Side Length", title = "Square") +
    theme_minimal() +
    coord_fixed(ratio = 1) +
    lims(x = c(0, squ$sideLength), y = c(0, squ$sideLength))

  print(p)
}

#' Draw a triangle in 2D.
#'
#' @param triangle An object of class 'shape' representing a triangle.
#' @param color The border color of the triangle (default is "blue").
#' @param fillColor The fill color of the triangle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "geom_polygon" "labs" "theme_minimal"
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
draw.triangle <- function(triangle, color = "blue", fillColor = "transparent") {
  triangle_data <- data.frame(
    x = c(0, triangle$a, triangle$b),
    y = c(0, 0, triangle$c)
  )

  max_dim <- max(triangle$a, triangle$b, triangle$c)

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
#' @param circle An object of class 'shape' representing a circle.
#' @param color The border color of the circle (default is "blue").
#' @param fillColor The fill color of the circle (default is "transparent").
#' @importFrom ggplot2 "ggplot" "geom_polygon" "labs" "theme_minimal"
#' "coord_fixed" "lims"
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
draw.circle <- function(circle, color = "blue", fillColor = "transparent") {
  circle_data <- data.frame(
    x = circle$radius * cos(seq(0, 2 * pi, length.out = 100)),
    y = circle$radius * sin(seq(0, 2 * pi, length.out = 100))
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
#' @param sph An object of class 'shape' representing a sphere.
#' @param color The color of the sphere (default is "#FF0000 or 'red').
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
draw.sphere <- function(sph, color =  "#FF0000",..) {
  open3d()

  spheres3d(0, 0, 0, radius = sph$radius, color = color, alpha = 0.7)
  decorate3d(box = FALSE ,axes = TRUE)
}



draw.cuboid <- function(cub, color =  "red", ...) {
  height <- cub$height
  width <- cub$width
  depth <- cub$depth

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
#' @param cube An object of class 'shape' representing a cube.
#' @param color The color of the cube (default is "red").
#'
#' @family draw functions
#'
#' @examples
#' \dontrun{
#' # Create a cube
#' my_cube <- cube(sideLength = 5)
#'
#' # Draw the cube
#' draw.cube(my_cube)
#' }
#'
#' @export
draw.cube <- function(cube, color = "red", ...) {
  sideLength <- cube$sideLength

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
#' @param cyl An object of class 'shape' representing a cylinder.
#' @param color The color of the cylinder (default is "red").
#'
#' @importFrom rgl "open3d" "sphere3d" "decorate3d" "cylinder" "qmesh3d"
#' "shade3d" and "wire3d"
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
draw.cylinder <- function(cyl, color = "red", ..){
  center <- matrix(c(0, 0, 0, 0, 0, cyl$height), ncol = 3, byrow = TRUE)
  radius <- cyl$radius
  cylinder_mesh <- cylinder3d(center = center, radius = radius, sides = 1000,closed=-2)
  open3d()
  shade3d(cylinder_mesh, color = color, alpha = 0.7)
  decorate3d()
}

