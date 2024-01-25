
#' Calculate the perimeter of 2D shapes.
#'
#' This package provides functions to calculate the perimeter of various 2D shapes,
#' including rectangles, squares, circles, and triangles. The perimeter can be
#' calculated based on the dimensions of each shape.
#'
#' @param shp An object of class '2dShape'.
#' @details The function checks if the input object is of class '2dShape'. If not,
#' it raises an error indicating that the input must be an object of class '2dShape'.
#' If the input is of class '2dShape', it dispatches the calculation to the appropriate
#' method based on the type of shape.
#'
#' @family perimeter functions
#' @seealso \code{\link{perimeter.rectangle}}, \code{\link{perimeter.square}},
#' \code{\link{perimeter.circle}}, \code{\link{perimeter.triangle}}
#'
#'
#' @export
perimeter <- function(shp) {
  if(!inherits(shp, "2dShape")){
    stop("perimeter can only be passed objects of class 2dShape")
  }
  else{
    UseMethod("perimeter")
  }
}

#' Calculate the perimeter of a rectangle.
#'
#' @param shp An object of class '2dShape' representing a rectangle.
#' @return The perimeter of the rectangle.
#'
#' @family perimeter functions
#' @seealso \code{\link{perimeter}}
#'
#' @examples
#' # Create a rectangle
#' my_rectangle <- rectangle(height = 2, width = 4)
#'
#' # Calculate the perimeter of the rectangle
#' perimeter.rectangle(my_rectangle)
#'
#' @S3method perimeter rectangle
perimeter.rectangle <- function(shp) {
  return(2 * shp$height + 2 * shp$width)
}

#' Calculate the perimeter of a square.
#'
#' @param shp An object of class '2dShape' representing a square.
#' @return The perimeter of the square.
#'
#' @family perimeter functions
#' @seealso \code{\link{perimeter}}
#'
#' @examples
#' # Create a square
#' my_square <- square(sideLength = 5)
#'
#' # Calculate the perimeter of the square
#' perimeter.square(my_square)
#'
#' @S3method perimeter square
perimeter.square <- function(shp){
  return(4 * shp$sideLength)
}

#' Calculate the perimeter of a circle.
#'
#' @param shp An object of class '2dShape' representing a circle.
#' @return The perimeter of the circle.
#'
#' @family perimeter functions
#' @seealso \code{\link{perimeter}}
#'
#' @examples
#' # Create a circle
#' my_circle <- circle(radius = 3)
#'
#' # Calculate the perimeter of the circle
#' perimeter.circle(my_circle)
#'
#' @S3method perimeter circle
perimeter.circle <- function(shp) {
  return(2 * pi * shp$radius)
}

#' Calculate the perimeter of a triangle.
#'
#' @param shp An object of class '2dShape' representing a triangle.
#' @return The perimeter of the triangle.
#'
#' @family perimeter functions
#' @seealso \code{\link{perimeter}}
#'
#' @examples
#' # Create a triangle
#' my_triangle <- triangle(a = 3, b = 4, c = 5)
#'
#' # Calculate the perimeter of the triangle
#' perimeter.triangle(my_triangle)
#'
#' @S3method perimeter triangle
perimeter.triangle <- function(shp) {
  return(shp$a + shp$b + shp$c)
}

