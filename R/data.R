#------------------------------------------------------------------------------#
#' Virtual sparse wheat canopy
#'
#' Wheat architectural data that were originally generated using the framework
#' OpenAlea.
#'
#' The \code{dataflow} tutorial, available in OpenAlea
#' (alinea/adel/tutorials/AdelRMonoRun), was used to generate the original plot
#' of wheat canopy. A subset of only 7 plants was then selected and randomly
#' located within a new plot of 1 m x 1 m. The input data used to simulate wheat
#' development came from the Caphorn variety (1200 degrees Celsius.day after
#' sowing), with leaf curvature data corresponding to the Soissons variety.
#'
#' @format A data frame with 2466 rows and 7 variables:
#' \tabular{rll}{
#'     [, 1] \tab primitiveType \tab Primitive ID which should be "p" for polygon. \cr
#'     [, 2] \tab nData \tab Number of data which should be 1 everywhere in this data set. \cr
#'     [, 3] \tab speciesID \tab Species ID is a way to make distinctions between plants (species, senescence,...). \cr
#'     [, 4] \tab plantID \tab Plant ID ranging from 0 to 6 in this data set. \cr
#'     [, 5] \tab leafID \tab Leaf ID. \cr
#'     [, 6] \tab polygonID \tab Polygon ID. \cr
#'     [, 7] \tab vertices \tab The coordinates (x, y, z) of the three vertices each polygon (i.e. triangle) is made of. \cr
#' }
#'
#' @source Data set provided by Tiphaine Vidal, and altered by Christophe Gigot.
#------------------------------------------------------------------------------#
"plants"



