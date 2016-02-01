#------------------------------------------------------------------------------#
#' @include canopy.R
#------------------------------------------------------------------------------#
NULL

#------------------------------------------------------------------------------#
#' Flatten a data frame with list-columns
#'
#' Flattens a data frame (or an inherited object, such as \code{Canopy}) with
#' list-columns in order to get a regular data frame. This is for instance required
#' to export a data frame in a file.
#'
#' @param data The data frame to be flattened.
#' @param ... Not currently used.
#'
#' @examples
#' plants <- readCan("/path/to/file/field.can")
#' flatPlants <- flatten(plants)
#' filename <- tempfile(fileext = ".csv")
#' write.csv(flatPlants, filename, row.names = FALSE)
#'
#' @export
#------------------------------------------------------------------------------#
flatten <- function(data, ...) UseMethod("flatten")

#------------------------------------------------------------------------------#
#' @rdname flatten
#' @export
#------------------------------------------------------------------------------#
flatten.Canopy <- function(data, ...) {
    flatVertices <- do.call(rbind.data.frame,
                            lapply(data$vertices, function(x) {as.vector(t(x))}))
    colnames(flatVertices) <- c("v1x", "v1y", "v1z",
                                "v2x", "v2y", "v2z",
                                "v3x", "v3y", "v3z")
    data <- data %>%
        dplyr::select(-vertices) %>%
        dplyr::bind_cols(flatVertices) %>%
        as.data.frame()
    return(data)
}
