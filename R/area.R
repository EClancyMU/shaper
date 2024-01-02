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
#' @examples
#' # Create a rectangle object
#' rectangle <- list(height = 4, width = 6)
#' class(rectangle) <- "shape"
#'
#' # Calculate the area of the rectangle
#' area(rectangle)
#'
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

#2D
area.rectangle <- function(rect) {
  return(rect$height * rect$width)
}

area.square <- function(squ){
  return(squ$sideLength * squ$sideLength)
}

area.circle <- function(circ) {
  return(pi*circ$radius^2)
}

area.triangle <- function(tri) {
  s <- perimeter(tri)/2
  Area <- sqrt(s*(s-tri$a)*(s-tri$b)*(s-tri$c)) #Heron's formula
  return(Area)
}

#3D - these are surface areas
area.cuboid <- function(cu){
  return(2 * (cu$depth * cu$width + cu$depth * cu$height + cu$height * cu$width))
}

area.cube <- function(cub){
  return(6 * cub$sideLength * cub$sideLength)
}

area.sphere <- function(sph){
  return(4*pi*sph$radius^2)
}

area.cylinder <- function(cyl){
  return(2*pi*cyl$radius*(cyl$radius+cyl$height))
}
