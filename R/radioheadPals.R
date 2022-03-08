#' list of radiohead colour palettes
#'
#' @export
radiohead_palettes <- list(
  `pabloHoney` = c('#3d1964', '#f9c000', '#f1e8e1', '#030202'),
  `Bends` = c('#020001', '#e20612', '#fcd397', '#fcd8a3', '#ce7f14'),
  `okComputer` = c('#ffffff', '#0079c2', '#cce9fb', '#809ba9', '#c65737', '#0a0001'),
  `KID_A` = c('#070705', '#4e2d24', '#382822', '#e51d1d',  '#854a14', '#bcbaae','#1e7fbe', '#8e9397'),
  `Amnesiac` = c('#000000',  '#db1a1b', '#e31818', '#e23634', '#f5a664', '#ffffff'),
  `hailToTheThief` = c('#78a0be', '#4f84a1', '#37392e', '#ce161d', '#e46a12', '#0e4490', '#f5a814'),
  `inRainbows` = c('#020202', '#f4ef4a', '#4785c6', '#f46523', '#48b54a', '#eab51f', '#ed2325', '#a5dce7', '#ca4a26', '#df893d'),
  `theKingOfLimbs` = c('#2d312f', '#3f4231', '#21211d', '#0e0e0e', '#b95b39', '#c8ae35', '#875521',
    '#5a4e25', '#323b1f', '#374021', '#cacccd'),
  `MoonShapedPool` = c('#fbfbfd', '#eae9ee', '#cac9ce', '#a4a2a7', '#98969b', '#545255', '#292524', '#201c1b')

)

#' Function to interpolate a color palette
#'
#' @param palette Character name of palette in tswift_palettes
#' @param reverse Boolean true if palette should be reversed
#' @param ... Additional arguments to pass to colorRampPalette()
#'
#' @return A vector of colors
#' @export
radiohead_pal <- function(palette = "pabloHoney", reverse = FALSE, ...){
  pal <- radiohead_palettes[[palette]]

  if(reverse){
    pal <- rev(pal)
  }

  grDevices::colorRampPalette(pal, ...)
}

#' Color scale for Radiohead colors
#'
#' @param palette Character name of palette in radiohead_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean indicating whether palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#'
#' @return No return value. Called for side effects
#' @export
#' @examples
#' library(ggplot2)
#' data <- data.frame(c = LETTERS[1:3],x = c(1,5,7),y = c(5,9,13))
#' ggplot(data, aes(x,y,color = c)) + geom_point() + scale_color_radiohead()
scale_color_radiohead <- function(palette = "pabloHoney",
                               discrete = TRUE, reverse=FALSE,...){

  pal <- radiohead_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("colour", paste0("radiohead_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Fill scale for Radiohead colors
#'
#' @param palette Character name of palette in radiohead_palettes
#' @param discrete Boolean if color aesthetic is discrete
#' @param reverse Boolean if palette should be reversed
#' @param ... Additional arguments used to discrete_scale() or scale_fill_gradientn()
#'   to automatically interpolate between colours.
#'
#' @return No return value. Called for side effects
#' @export
#' @examples
#' library(ggplot2)
#' data <- data.frame(c = LETTERS[1:3],x = c(1,5,7),y = c(5,9,13))
#' ggplot(data, aes(x,fill=c)) + geom_bar() + scale_fill_radiohead()
scale_fill_radiohead <- function(palette = "pabloHoney",
                              discrete = TRUE, reverse = FALSE, ...){
  pal <- radiohead_pal(palette = palette, reverse = reverse)

  if(discrete){
    ggplot2::discrete_scale("fill", paste0("radiohead_", palette), palette = pal, ...)
  }else{
    ggplot2::scale_fill_gradientn(colours = pal(256),...)
  }
}
