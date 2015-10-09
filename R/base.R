#------------------------------------------------------------------------------#
#' Read a canopy file (.can)
#'
#' Read .can files from .can files and save the information in a data.frame.
#'
#' @param filePath The name of the file which the data are to be read from.
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
can3d <- function(data, fraction = 1.0) {
    vertices <- data %>% dplyr::select(v1x:v3z) # Deals with triangles only
    vertices <- vertices %>% dplyr::sample_frac(fraction)
    indices  <- 1:3 # Deals with triangles only

    tmesh3ds <- apply(vertices, 1, function(x){
        normal <- 1.0 # TODO: À approfondir!
        x <- c(x[1:3], normal, x[4:6], normal, x[7:9], normal)
        rgl::tmesh3d(x, indices)
    })

    return(rgl::shapelist3d(tmesh3ds, col = rgb(0, 0.7, 0)))#, alpha = 0.2)
}
