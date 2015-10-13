#------------------------------------------------------------------------------#
#' Read a canopy file (.can)
#'
#' Read .can files from .can files and save the information in a data.frame.
#'
#' The data are stored in a data frame where the first columns correspond to
#' ID and characteristics of the polygons. The last columns contains the points
#' coordinates, one column per point. The list-columns in data frames.
#'
#' @param filePath The name of the file which the data are to be read from.
#'
#'
#' @export
#------------------------------------------------------------------------------#
readCan <- function(filePath) {
    rawData <- read.table(filePath, header = FALSE, comment.char = "#")

    # Some checks
    if (ncol(rawData) != 13)
        stop("Not the right number of columns in the input file.")
    if (length(unique(rawData[,4])) != 1)
        stop(paste("Not only one sort of polygon in the input file.",
                   "Do not work with more than one kind of polygon."))
    if (unique(rawData[,4]) != 3)
        stop("Need only triangles.")

    # ??
    rawData <- setNames(rawData, c("primitive_type", "nData",
                                   "bunchInfo", "nVertices",
                                   "v1x", "v1y", "v1z",
                                   "v2x", "v2y", "v2z",
                                   "v3x", "v3y", "v3z"))

    # We do not take into account the 2 first columns.
    # 3rd column is more complicated.
    # To figure out and retrieve information contained in this 3rd column,
    # the file entited "Canopy File (.can) Description", created on 2009-10-27
    # was used. Below are the information:
    # bunchInfo (col #3) = e * 1e11 + p * 1e6 + f * 1e3 + t
    # e: species ID
    # p: plant ID
    # f: leaf ID
    # t: polygon ID
    rawData <- rawData %>%
        tidyr::separate(bunchInfo,
                 into = c("speciesID", "plantID", "leafID", "polygonID"),
                 sep =  c(-12, -7, -4),
                 convert = TRUE)

    fun <- function(x) {
        res <- matrix(x, nrow=3, ncol=3, byrow=TRUE)
        dimnames(res) <- list(NULL, c("x", "y", "z"))
        return(list(res))
        }

    vertices <- do.call(c, apply(rawData %>% select(v1x:v3z), 1, fun))

    rawData <- rawData %>% select(primitive_type:nVertices)
    rawData$vertices <- vertices

    #writeOBJ("visuCan.obj")
    return(rawData)
}


#------------------------------------------------------------------------------#
#' 3D canopy visualization
#'
#' 3D canopies visualized using rgl package features.
#'
#' @param data A data frame containing canopy data.
#' @param fraction Which fraction of the data should be displayed. This is
#'     useful when you deal with a huge amount of polygon in order to render
#'     them in a reasonible amount of time.
#'
#' @export
#------------------------------------------------------------------------------#
canopy3d <- function(data, fraction = 1.0, ...) {
    # TODO: check if Class = can...
    subData  <- data %>% dplyr::sample_frac(fraction)
    vertices <- subData$vertices
    indices  <- 1:3 # Deals with triangles only

    tmesh3ds <- lapply(vertices, function(x){
        normal  <- 1.0 # TODO: À approfondir!
        polygon <- cbind(x, normal = normal)
        mesh3d  <- rgl::tmesh3d(as.vector(t(polygon)), indices)
        return(mesh3d)
    })

    return(rgl::shapelist3d(tmesh3ds, ...))#col = rgb(0, 0.7, 0)))#, alpha = 0.2)
}
