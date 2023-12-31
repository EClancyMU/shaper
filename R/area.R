#' Calculate the area of various 2D and 3D shapes.
#'
#' This provides functions to calculate the area of various 2D and 3D shapes,
#' specifically rectangles, squares, circles, triangles, cuboids, cubes, spheres, and cylinders.
#' The area can be calculated based on the dimensions of each shape.
#'
#' @param shp An object of class 'shape'.
#' @details The function checks if the input object is of class 'shape'. If not, it raises
#' an error indicating that the input must be an object of class 'shape'. If the input is
#' of class 'shape', it dispatches the calculation to the appropriate method based on the
#' type of shape.
#'
#' @family area functions
#'
#' @export
area <- function(shp) {
  if(!inherits(shp,"shape")){
    stop("area can only be passed objects of class shape")
  }
  else{
    UseMethod("area")
  }
}

#' Calculate the area of a rectangle.
#'
#' @param rect An object of class 'shape' representing a rectangle.
#' @return The area of the rectangle.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a rectangle object
#' my_rectangle <- rectangle(height = 2,width = 4)
#'
#' # Calculate the area of the rectangle
#' area.rectangle(my_rectangle)
#'
#' @export

area.rectangle <- function(rect) {
  return(rect$height * rect$width)
}



#' Calculate the area of a square.
#'
#' @param squ An object of class 'shape' representing a square.
#' @return The area of the square.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a square object
#' my_square1 <- square(sideLength = 5)
#'
#'
#' # Calculate the area of the square
#' area.square(my_square1)
#'
#'
#' @export
area.square <- function(squ){
  return(squ$sideLength * squ$sideLength)
}



#' Calculate the area of a circle.
#'
#' @param circ An object of class 'shape' representing a circle.
#' @return The area of the circle.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a circle object
#' my_circle <- circle(radius = 3)
#'
#' # Calculate the area of the circle
#' area.circle(my_circle)
#'
#' @export
area.circle <- function(circ) {
  return(pi*circ$radius^2)
}

#' Calculate the area of a triangle using Heron's formula.
#'
#' @param tri An object of class 'shape' representing a triangle with sides a, b, and c.
#' @return The area of the triangle.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a triangle object
#' my_triangle <- triangle(a = 3, b = 4, c = 5)
#'
#' # Calculate the area of the triangle
#' area.triangle(my_triangle)
#'
#'
#' @export
area.triangle <- function(tri) {
  s <- perimeter(tri)/2
  Area <- sqrt(s*(s-tri$a)*(s-tri$b)*(s-tri$c)) #Heron's formula
  return(Area)
}

#3D - these are surface areas
#' Calculate the surface area of a cuboid.
#'
#' @param cu An object of class 'shape' representing a cuboid with dimensions depth, width, and height.
#' @return The surface area of the cuboid.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a cuboid object
#' my_cuboid1 <- cuboid(height = 3, width = 4, depth = 5)
#'
#' # Calculate the surface area of the cuboid
#' area.cuboid(my_cuboid1)
#'
#' @export
area.cuboid <- function(cu){
  return(2 * (cu$depth * cu$width + cu$depth * cu$height + cu$height * cu$width))
}


#' Calculate the surface area of a cube.
#'
#' @param cub An object of class 'shape' representing a cube with side length.
#' @return The surface area of the cube.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a cube object
#' my_cube1 <- cube(sideLength = 5)
#'
#' # Calculate the surface area of the cube
#' area.cube(my_cube1)
#'
#' @export
area.cube <- function(cub){
  return(6 * cub$sideLength * cub$sideLength)
}


#' Calculate the surface area of a sphere.
#'
#' @param sph An object of class 'shape' representing a sphere with radius.
#' @return The surface area of the sphere.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a sphere object
#' my_sphere <- sphere(radius = 2)
#'
#' # Calculate the surface area of the sphere
#' area.sphere(my_sphere)

#' @export
area.sphere <- function(sph){
  return(4*pi*sph$radius^2)
}


#' Calculate the surface area of a cylinder.
#'
#' @param cyl An object of class 'shape' representing a cylinder with radius and height.
#' @return The surface area of the cylinder.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a cylinder object
#' my_cylinder <- cylinder(radius = 2, height = 5)
#'
#' # Calculate the surface area of the cylinder
#' area.cylinder(my_cylinder)
#'
#' @export
area.cylinder <- function(cyl){
  return(2*pi*cyl$radius*(cyl$radius+cyl$height))
}
