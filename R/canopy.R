#------------------------------------------------------------------------------#
#' @include canis.R
#------------------------------------------------------------------------------#
NULL

#------------------------------------------------------------------------------#
#' Data input: canopy file (.can)
#'
#' Reads a canopy file, with a .can extension, and creates a data frame from it.
#'
#' The data are stored in a data frame where each row corresponds to only one
#' constituent polygon of the canopy. Only triangle shapes can currently be
#' imported. The first columns contain "labels" linked to the polygons, i.e.
#' characteristics (other than the shape) and IDs. The last column is a
#' list-column containing all the vertices of the polygons. Each element of this
#' column is a matrix. Each row of this matrix corresponds to the spatial
#' coordinates of a vertex.
# For more information about the can file structure, take a look at the related
# vignette: \code{vignette("can-format", package = "canis")}
#'
#' @param file The name of the can file which the data are to be read from.
#' @param label Not yet implemeted.
#' @param col3Name Vector containing the names of the different elements in the
#'   third columne of the can file.
#' @param col3Sep Positions to split at in the third column. It is mandatory
#'   that \code{length(col3Sep)} equals \code{length(col3Name) - 1}.
#'
#' @return An object of class \code{c("data.frame", "Canopy")}.
#'
#' @examples
#' plants <- readCan("/path/to/file/field.can")
#'
#' @export
#------------------------------------------------------------------------------#
readCan <- function(file, label = NA,
                    col3Name = c("speciesID", "plantID", "leafID", "polygonID"),
                    col3Sep  = c(12, 7, 4)) {
    rawData <- read.table(file, header = FALSE, comment.char = "#",
                          stringsAsFactors = FALSE)

    # Some checks
    if (ncol(rawData) != 13)
        stop("Not the right number of columns in the input file.")
    if (length(unique(rawData[,4])) != 1)
        stop(paste("Not only one sort of polygon in the input file.",
                   "Do not work with more than one kind of polygon."))
    if (unique(rawData[,4]) != 3)
        stop("Need only triangles.")

    # ??
    rawData <- setNames(rawData, c("primitiveType", "nData",
                                   "bunchInfo", "nVertices",
                                   "v1x", "v1y", "v1z",
                                   "v2x", "v2y", "v2z",
                                   "v3x", "v3y", "v3z"))
    .vertice <- lapply(colnames(rawData)[5:13], as.symbol)

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
                        into = col3Name,
                        sep = - col3Sep,
                        convert = TRUE)

    .labels <- lapply(colnames(rawData)[1:(length(col3Name) + 2)], as.symbol)

    fun <- function(x) {
        res <- matrix(x, nrow=3, ncol=3, byrow=TRUE)
        dimnames(res) <- list(NULL, c("x", "y", "z"))
        return(list(res))
    }

    vertices <- do.call(c, apply(rawData %>% dplyr::select_(.dots = .vertice), 1, fun))
    rawData <- rawData %>% dplyr::select_(.dots = .labels)
    rawData <- rawData %>% dplyr::mutate_each(dplyr::funs(as.character))
    rawData$vertices <- I(vertices)
    class(rawData) <- c("Canopy", "data.frame")
    return(rawData)
}

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
str.Canopy <- function(object, ...) NextMethod(max.level = 1)
