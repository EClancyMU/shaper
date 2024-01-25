#' Calculate the area of various 2D and 3D shapes.
#'
#' This provides functions to calculate the area of various 2D and 3D shapes,
#' specifically rectangles, shpares, circles, triangles, cuboids, cubes, spheres, and cylinders.
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
#' @param shp An object of class 'shape' representing a rectangle.
#' @return The area of the rectangle.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a rectangle object
#' my_rectangle <- rectangle(height = 2,width = 4)
#'
#' # Calculate the area of the shpangle
#' area.rectangle(my_rectangle)
#'
#' @export

area.rectangle <- function(shp) {
  return(shp$height * shp$width)
}



#' Calculate the area of a square.
#'
#' @param shp An object of class 'shape' representing a shpare.
#' @return The area of the shpare.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a shpare object
#' my_shpare1 <- shpare(sideLength = 5)
#'
#'
#' # Calculate the area of the shpare
#' area.shpare(my_shpare1)
#'
#'
#' @export
area.square <- function(shp){
  return(shp$sideLength * shp$sideLength)
}



#' Calculate the area of a circle.
#'
#' @param shp An object of class 'shape' representing a circle.
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
area.circle <- function(shp) {
  return(pi*shp$radius^2)
}

#' Calculate the area of a triangle using Heron's formula.
#'
#' @param shp An object of class 'shape' representing a triangle with sides a, b, and c.
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
area.triangle <- function(shp) {
  s <- perimeter(shp)/2
  Area <- sqrt(s*(s-shp$a)*(s-shp$b)*(s-shp$c)) #Heron's formula
  return(Area)
}

#3D - these are surface areas
#' Calculate the surface area of a cuboid.
#'
#' @param shp An object of class 'shape' representing a cuboid with dimensions depth, width, and height.
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
area.cuboid <- function(shp){
  return(2 * (shp$depth * shp$width + shp$depth * shp$height + shp$height * shp$width))
}


#' Calculate the surface area of a cube.
#'
#' @param shp An object of class 'shape' representing a cube with side length.
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
area.shpe <- function(shp){
  return(6 * shpb$sideLength * shpb$sideLength)
}


#' Calculate the surface area of a sphere.
#'
#' @param shp An object of class 'shape' representing a shpere with radius.
#' @return The surface area of the shpere.
#'
#' @family area functions
#' @seealso \code{\link{area}}
#'
#' @examples
#' # Create a shpere object
#' my_shpere <- shpere(radius = 2)
#'
#' # Calculate the surface area of the shpere
#' area.shpere(my_shpere)

#' @export
area.sphere <- function(shp){
  return(4*pi*shp$radius^2)
}


#' Calculate the surface area of a cylinder.
#'
#' @param shp An object of class 'shape' representing a cylinder with radius and height.
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
area.cylinder <- function(shp){
  return(2*pi*shp$radius*(shp$radius+shp$height))
}
