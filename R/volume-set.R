#------------------------------------------------------------------------------#
#' @include canis.R
#------------------------------------------------------------------------------#
NULL

#------------------------------------------------------------------------------#
# @keywords internal
#------------------------------------------------------------------------------#
getVolumeCenter <- function(data) {
    # Faire le cas si pas liste (le parametre data)
    # res = milieu des différents volumes --> intéressant pour text3D
    res <- t(sapply(data, function(X) c(mean(X[, "x"]),
                                        mean(X[, "y"]),
                                        mean(X[, "z"]))))
    dimnames(res) <- list(NULL, c("x", "y", "z"))
    return(res)
}

#------------------------------------------------------------------------------#
# @keywords internal
#------------------------------------------------------------------------------#
giveVolumeID <- function(data) {
    # Faire le cas si pas liste (le parametre data)
    res <- getVolumeCenter(data)

    # res2 = rank dans chaque dimension
    res2 <- data.frame(xrank = as.numeric(as.factor(res[, "x"])), # to get rank
                       yrank = as.numeric(as.factor(res[, "y"])), # to get rank
                       zrank = as.numeric(as.factor(res[, "z"]))) # to get rank
    label <- (res2 %>% dplyr::transmute(label = paste(xrank, yrank, zrank, sep="-")))[["label"]]
    return(label)
}

#------------------------------------------------------------------------------#
# @keywords internal
#------------------------------------------------------------------------------#
buildGrid3d <- function(xlim, ylim, zlim, intervals) {

    if ((length(xlim) != 2) | (length(ylim) != 2) | (length(ylim) != 2))
        stop("Error.......")
    if (length(intervals) != 3) stop("Error.......")
    if (!all(intervals >= 1))   stop("Error.......")

    getBreaks <- function(lim, interval)
        seq(lim[1], lim[2], length = (interval + 1))
    xbreaks <- getBreaks(xlim, intervals[1])
    ybreaks <- getBreaks(ylim, intervals[2])
    zbreaks <- getBreaks(zlim, intervals[3])

    volumes <- list()

    for (ix in seq_len(length(xbreaks) - 1)) {
        for (iy in seq_len(length(ybreaks) - 1)) {
            for (iz in seq_len(length(zbreaks) - 1)) {

                xbounds <- xbreaks[c(ix, ix + 1)]
                ybounds <- ybreaks[c(iy, iy + 1)]
                zbounds <- zbreaks[c(iz, iz + 1)]

                volume <- matrix(c(xbounds[1], ybounds[1], zbounds[1],
                                   xbounds[2], ybounds[1], zbounds[1],
                                   xbounds[1], ybounds[2], zbounds[1],
                                   xbounds[2], ybounds[2], zbounds[1],
                                   xbounds[1], ybounds[1], zbounds[2],
                                   xbounds[2], ybounds[1], zbounds[2],
                                   xbounds[1], ybounds[2], zbounds[2],
                                   xbounds[2], ybounds[2], zbounds[2]),
                                 ncol = 3, byrow = TRUE)

                dimnames(volume) <- list(NULL, c("x", "y", "z"))
                volumes[[length(volumes) + 1]] <- volume
            }
        }
    }
    return(volumes)
}

#------------------------------------------------------------------------------#
#' Build a set of regular volumes
#'
#' Useful
#'
#' Note that the plotting process may be time-consuming with a large number of
#' polygons. Consider using the parameter \code{fraction} to plot only a random
#' subset of all the polygons. You can have only one volume in your
#' set of volumes.
#'
#' @param rangeX,rangeY,rangeZ Min and max for each dimension.
#' @param intervals A numeric vector of length 3, with numbers (greater than or
#'     equal to 1) giving the numbers of intervals into which x, y ans z are to
#'     be cut.
#' @param label Do the volumes need to be labelized? Either "auto" or a character
#' vector of length equals to the number of volumes must be provided.
#' @param dataFrame return as data frame. Useful to work with \code{dplyr} package.
#'
#' @return An object of class \code{c("VolumeSet", "data.frame")}.
#'
#' @examples
#' canopy  <- readCan("/path/to/file/field.can")
#' rangeDim <- function(can, dim) {
#'     range(sapply(can$vertices, function(i) range(i[, dim])))
#' }
#' volumes <- makeVolumeSet(rangeDim(canopy, "x"),
#'                          rangeDim(canopy, "y"),
#'                          rangeDim(canopy, "z"),
#'                          intervals = rep(3, 3))
#'
#' @export
#------------------------------------------------------------------------------#
makeVolumeSet <- function(rangeX, rangeY, rangeZ, intervals = rep(1, 3),
                         label = "auto", onlyVertices = FALSE){
    vertices <- buildGrid3d(rangeX, rangeY, rangeZ, intervals)

    if (onlyVertices) {
        if (length(vertices) == 1) return(vertices[[1]])
        else                       return(vertices)
    } else {
        if (length(label) == 1 & label[[1]] == "auto") {
            volumeID  <- giveVolumeID(vertices)
        } else {
            if (length(label) != length(vertices)) {
                stop("Error...!!!")
            } else {
                volumeID <- label
            }
        }
        volumes   <- data.frame(volumeID = volumeID,
                                vertices = I(vertices),
                                stringsAsFactors = FALSE)

        class(volumes) <- c("VolumeSet", "data.frame")
        return(volumes)
    }
}

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
str.VolumeSet <- function(object, ...) NextMethod(max.level = 1)

