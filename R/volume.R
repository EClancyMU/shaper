
#' Calculate the volume of 3D shapes.
#'
#' This provides functions to calculate the volume of various 3D shapes,
#' specifically cuboids, cubes, spheres, and cylinders. The volume can be calculated
#' based on the dimensions of each shape.
#'
#' @param shp An object of class '3dShape'.
#' @details The function checks if the input object is of class '3dShape'. If not,
#' it raises an error indicating that the input must be an object of class '3dShape'.
#' If the input is of class '3dShape', it dispatches the calculation to the appropriate
#' method based on the type of shape.
#'
#' @family volume functions
#'
#' @export
volume <- function(shp) {
  if(!inherits(shp, "3dShape")){
    stop("Volume can only be passed objects of class 3dShape")
  }
  else{
    UseMethod("volume")
  }
}

#' Calculate the volume of a cuboid.
#'
#' @param cu An object of class '3dShape' representing a cuboid with dimensions
#' height, width, and depth.
#' @return The volume of the cuboid.
#'
#' @family volume functions
#'
#' @examples
#' # Create a cuboid object
#' my_cuboid <- cuboid(height = 3, width = 4, depth = 5)
#'
#' # Calculate the volume of the cuboid
#' volume.cuboid(my_cuboid)
#'
#' @export
volume.cuboid <- function(cu){
  return(cu$depth * cu$height * cu$width)
}

#' Calculate the volume of a cube.
#'
#' @param cub An object of class '3dShape' representing a cube with side length.
#' @return The volume of the cube.
#'
#' @family volume functions
#'
#' @examples
#' # Create a cube object
#' my_cube1 <- cube(sideLength = 5)
#'
#' # Calculate the volume of the cube
#' volume.cube(my_cube1)
#'
#' @export
volume.cube <- function(cub){
  return(cub$sideLength * cub$sideLength * cub$sideLength)
}

#' Calculate the volume of a sphere.
#'
#' @param sph An object of class '3dShape' representing a sphere with radius.
#' @return The volume of the sphere.
#'
#' @family volume functions
#'
#' @examples
#' # Create a sphere object
#' my_sphere <- sphere(radius = 2)
#'
#' # Calculate the volume of the sphere
#' volume.sphere(my_sphere)
#'
#' @export
volume.sphere <- function(sph){
  return((4/3) * pi * sph$radius^3)
}

#' Calculate the volume of a cylinder.
#'
#' @param cyl An object of class '3dShape' representing a cylinder with radius and height.
#' @return The volume of the cylinder.
#'
#' @family volume functions
#'
#' @examples
#' # Create a cylinder object
#' my_cylinder <- cylinder(radius = 2, height = 5)
#'
#' # Calculate the volume of the cylinder
#' volume.cylinder(my_cylinder)
#'
#' @export
volume.cylinder <- function(cyl){
  return(pi * cyl$radius^2 * cyl$height)
}
