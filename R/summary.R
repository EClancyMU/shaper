#' Generate a summary for shapes.
#'
#' This function provides a summary for objects of class 'shape'. The summary
#' includes information such as the shape's class, perimeter (for 2D shapes),
#' surface area (for 3D shapes), and volume (for 3D shapes).
#'
#' @param shp An object of class 'shape'.
#' @details The function checks if the input object is of class 'shape'. If not,
#' it raises an error indicating that the input must be an object of class 'shape'.
#' If the input is of class 'shape', it dispatches the summary calculation to the
#' appropriate method based on the type of shape.
#'
#' @family summary functions
#'
#' @export
summary <- function(shp){
  if(!inherits(shp,"shape")){
    stop("This summary function can only be passed objects of class shape")
  }
  else{
    UseMethod("summary")
  }
}

#' Generate a summary of the parameters for 2D shapes.
#'
#' @param shp An object of class 'shape' representing a 2D shape.
#'
#' @family summary functions
#'
#' @examples
#' my_square <- square(sideLength = 5)
#'
#' # Generate a summary for the square
#' summary.2dShape(my_square)
#'
#'
#' @exportS3Method summary 2dShape
summary.2dShape <- function(shp){
  cat("Shape:", class(shp)[3], "\nPerimeter:", perimeter(shp), "\nArea:", area(shp))
}

#' Generate a summary of the parameters for 3D shapes.
#'
#' @param shp An object of class 'shape' representing a 3D shape.
#'
#' @family summary functions
#'
#' @examples
#' # Create a cylinder
#' my_cylinder1 <- cylinder(radius = 2, height = 5)
#'
#' # Generate a summary for the cylinder
#' summary.3dShape(my_cylinder1)
#'
#' @exportS3Method summary 3dShape
summary.3dShape <- function(shp){
  cat("Shape:", class(shp)[3], "\nSurface-area:", area(shp), "\nVolume:", volume(shp))
}
