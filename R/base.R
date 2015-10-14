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

    vertices <- do.call(c, apply(rawData %>% dplyr::select(v1x:v3z), 1, fun))

    rawData <- rawData %>% dplyr::select(primitive_type:nVertices)
    #rawData[] <- lapply(rawData, as.factor)
    rawData <- rawData %>% dplyr::mutate_each(dplyr::funs(as.factor))
    rawData$vertices <- vertices

    #class(rawData) <- "Canopy" # plus data.frame... effet indésirable avec dplyr?
    comment(rawData) <- "CanopyDescription"

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
canopy3d <- function(data, fraction = 1.0, colBy, ...) {

    # TODO: check if Class = can...
    if (comment(data) != "CanopyDescription") stop("data: wrong object.")

    if (!missing(colBy)) {
        if (colBy %in% names(data)) {
            #RColorBrewer::brewer.pal(8, "Dark2")
            cols <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                      "#66A61E", "#E6AB02", "#A6761D", "#666666")
            attr(cols, "length") <- length(cols)
        } else {
            warning("colBy does not exist.")
        }
    }

    subData  <- data %>% dplyr::sample_frac(fraction)
    indices  <- 1:3 # Deals with triangles only

    # Because apply() change every factor as a characher,
    # and we want the levels (numeric, not letters,...)
    i <- sapply(subData, is.factor)
    subData[i] <- lapply(subData[i], as.numeric)

    tmesh3ds <- apply(subData, 1, function(x){
        vertices <- x$vertices
        normal  <- 1.0 # TODO: À approfondir!
        polygon <- cbind(vertices, normal = normal)
        if (exists("cols")) {
            id <- x[[colBy]] # apply a générer une liste d'éléments
            mesh3d  <- rgl::tmesh3d(as.vector(t(polygon)), indices,
                                    material = list(color = cols[(id %% attr(cols, "length")) + 1]))
        } else {
            mesh3d  <- rgl::tmesh3d(as.vector(t(polygon)), indices)
        }
        return(mesh3d)
    })

    return(rgl::shapelist3d(tmesh3ds, ...))#col = rgb(0, 0.7, 0)))#, alpha = 0.2)
}
