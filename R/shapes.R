#' Function to check if the input is numeric.
#'
#' @param x Input value to be checked.
#' @return Logical value indicating whether the input is numeric.
#'
#' @export
is.numeric.value <- function(x) {
  is.numeric(x)
}

#' Create a rectangle object.
#'
#' @param height Height of the rectangle.
#' @param width Width of the rectangle.
#' @return An object of class 'shape', '2dShape', and 'rectangle'.
#'
#' @family 2D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_rectangle <- rectangle(height = 2,width = 4)
#' }
#' @export
rectangle <- function(height, width) {
  if(!is.numeric.value(height) | !is.numeric.value(width)) {
    stop("Height and width must be numeric values.")
  }
  rect <- list(height = height, width = width)
  class(rect) <- c("shape","2dShape", "rectangle")
  return(rect)
}

#' Create a square object.
#'
#' @param sideLength Side length of the square.
#' @return An object of class 'shape', '2dShape', and 'square'.
#'
#' @family 2D Shape Classes
#' @seealso \code{\link{rectangle}}
#'
#' @examples
#' \dontrun{
#' my_square1 <- square(sideLength = 5)
#' my_square2 < rectangle(height = 5, width = 5)
#' }
#' @export
square <- function(sideLength){
  if(!is.numeric.value(sideLength)) {
    stop("Side Length must be a numeric value.")
  }
  squ <- rectangle(sideLength, sideLength)
  class(squ) <- c("shape","2dShape","square")
  # Add 'sideLength' attribute for convenience in square-specific calculations
  squ$sideLength <- sideLength
  return(squ)
}

#' Create a circle object.
#'
#' @param radius Radius of the circle.
#' @return An object of class 'shape', '2dShape', and 'circle'.
#'
#' @family 2D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_circle <- circle(radius = 3)
#' }
#'
#' @export
circle <- function(radius){
  if(!is.numeric.value(radius)) {
    stop("Radius must be a numeric value.")
  }
  circ <- list(radius = radius)
  class(circ) <- c("shape","2dShape", "circle")
  return(circ)
}


#' Create a triangle object.
#'
#' @param a Length of side a.
#' @param b Length of side b.
#' @param c Length of side c.
#' @return An object of class 'shape', '2dShape', and 'triangle'.
#'
#' @family 2D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_triangle <- triangle(a = 3, b = 4, c = 5)
#' }
#'
#' @export
triangle <- function(a, b, c) {
  if(!is.numeric.value(a) | !is.numeric.value(b) | !is.numeric.value(c)) {
    stop("a, b and c must be numeric values.")
  }
  ##triangle inequality theorem
  if(a+b>c & b+c>a & c+a>b){
    tri <- list(a = a, b = b, c = c)
    class(tri) <- c("shape","2dShape", "triangle")
    return(tri)
  }
  stop("Invalid side combos")
}

#' Create a cuboid object.
#'
#' @param height Height of the cuboid.
#' @param width Width of the cuboid.
#' @param depth Depth of the cuboid.
#' @return An object of class 'shape', '3dShape', and 'cuboid'.
#'
#' @family 3D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_cuboid1 <- cuboid(height = 3, width = 4, depth = 5)
#' }
#'
#' @export
cuboid <- function(height, width, depth) {
  if(!is.numeric.value(height) | !is.numeric.value(width) | !is.numeric.value(depth)) {
    stop("Height, width and depth must be numeric values.")
  }
  cu <- list(width = width, depth = depth, height = height)
  class(cu) <- c("shape","3dShape", "cuboid")
  return(cu)
}

#' Create a cube object.
#'
#' @param sideLength Side length of the cube.
#' @return An object of class 'shape', '3dShape', and 'cube'.
#'
#' @family 3D Shape Classes
#' @seealso \code{\link{cuboid}}
#'
#' @examples
#' \dontrun{
#' my_cube1 <- cube(sideLength = 5)
#' my_cube2 <- cuboid(height = 3, width = 3, depth = 3)
#' }
#' @export
cube <- function(sideLength){
  if(!is.numeric.value(sideLength)) {
    stop("Side Length must be a numeric value.")
  }
  cub <- cuboid(sideLength, sideLength, sideLength)
  class(cub) <- c("shape","3dShape","cube")
  # Add 'sideLength' attribute for convenience in cube-specific calculations
  cub$sideLength <- sideLength
  return(cub)
}

#' Create a sphere object.
#'
#' @param radius Radius of the sphere.
#' @return An object of class 'shape', '3dShape', and 'sphere'.
#'
#' @family 3D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_sphere <- sphere(radius = 2)
#' }
#' @export
sphere <- function(radius){
  if(!is.numeric.value(radius)) {
    stop("Radius must be a numeric value.")
  }
  sph <- list(radius = radius)
  class(sph) <- c("shape","3dShape", "sphere")
  return(sph)
}


#' Create a cylinder object.
#'
#' @param radius Radius of the cylinder.
#' @param height Height of the cylinder.
#' @return An object of class 'shape', '3dShape', and 'cylinder'.
#'
#' @family 3D Shape Classes
#'
#' @examples
#' \dontrun{
#' my_cylinder <- cylinder(radius = 2, height = 5)
#' }
#' @export
cylinder <- function(radius, height){
  if(!is.numeric.value(radius) | !is.numeric.value(height)) {
    stop("Radius and height must be numeric values.")
  }
  cyl <- list(radius = radius, height = height)
  class(cyl) <- c("shape","3dShape", "cylinder")
  return(cyl)
}
