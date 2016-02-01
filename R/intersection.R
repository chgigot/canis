#------------------------------------------------------------------------------#
#' @include canopy.R
#' @include volume-set.R
#------------------------------------------------------------------------------#
NULL

#------------------------------------------------------------------------------#
# Heron's formula
#
# Compute the area of a triangle knowing only the lengths of its three sides.
#
# @param x A vector containing the lengths of the triangle sides.
#
# @keywords internal
#------------------------------------------------------------------------------#
heron <- function(x) {
    stopifnot(length(x) == 3)
    s <- sum(x) / 2
    m <- s * (s - x[1]) * (s - x[2]) * (s - x[3])
    # Even if it's not supposed to occur in theory, we need to check that m > 0.
    # Due to numeric limit, we can get negative numbers.
    if (m < 0) return(0)
    else       return(sqrt(m))
}

#------------------------------------------------------------------------------#
# Triangle area
#
# To do.
#
# @param x A matrix containing the coordinates of the triangle. Each row
#     corresponds to a points, and each column to a dimension (x, y and z).
#
# @keywords internal
#------------------------------------------------------------------------------#
area <- function(x) {
    distances <- as.vector(dist(x))
    return(heron(distances))
}

#------------------------------------------------------------------------------#
# Compute coordinates of the interesect point
#
# To do.
#
# @keywords internal
#------------------------------------------------------------------------------#
getIntersectPoint <- function(p1, p2, p3part) {
    if (length(na.omit(p3part)) != 1) stop("Error in ppart.")
    i <- which(!is.na(p3part))
    k <- (p3part[i] - p1[i]) / (p2[i] - p1[i])
    return(p1 + k * (p2 - p1))
}


#------------------------------------------------------------------------------#
# Compute coordinates of the middle point
#
# To do.
#
# @keywords internal
#------------------------------------------------------------------------------#
getMidPoint <- function(p1, p2) return((p1 + p2) / 2)

#------------------------------------------------------------------------------#
# is in volume ?
#
# To do.
#
# @keywords internal
#------------------------------------------------------------------------------#
belongToVolume <- function(volume, point) {

    if ((point["x"] >= xs[1]) & (point["x"] <= xs[2]) &
        (point["y"] >= ys[1]) & (point["y"] <= ys[2]) &
        (point["z"] >= zs[1]) & (point["z"] <= zs[2])) {
        return(TRUE)
    } else {
        return(FALSE)
    }
}

#------------------------------------------------------------------------------#
# Interection between canopy polygons and a cube
#
# Check if each triangle is inside or outside the volume.
# Shred a triangle into four smaller triangles
#
# The "cube" should have all its sides parallel to an origin axis (current
# restriction).
#
# @param x A matrix containing the coordinates of the triangle. Each row
#     corresponds to a points, and each column to a dimension (x, y and z).
# @param volume A 3-component matrix of coordinates of a "cube".
# @param epsilon The area limit below which no triangle is to be shreded
#     anymore.
#
# @keywords internal
#------------------------------------------------------------------------------#
intersectPolygon <- function(polygon, volume, shred = TRUE, epsilon = 1e-6) {

    # One situation is not taken into account here:
    #
    #              | /| A
    #       in     |/ |
    #              |  |
    #             /|  |
    # -----------/-+  |   out
    #           /     |
    #        C /______| B
    #

    # Récupération des mins et maxs du volume
    xs <- unique(volume[,1])
    ys <- unique(volume[,2])
    zs <- unique(volume[,3])
    if ((length(xs) != 2) | (length(ys) != 2) | (length(zs) != 2))
        stop("The volume must be a cube, with sides // to the x, y and z axes.")

    # Matrix lower
    lower  <- matrix(rep(c(min(xs), min(ys), min(zs)), 3),
                     nrow = 3, ncol = 3, byrow = TRUE)
    dimnames(lower) <- list(NULL, c("x", "y", "z"))

    # Matrix upper
    upper <- matrix(rep(c(max(xs), max(ys), max(zs)), 3),
                    nrow = 3, ncol = 3, byrow = TRUE)
    dimnames(upper) <- list(NULL, c("x", "y", "z"))

    if(!all(dim(polygon) == c(3, 3))) stop("Polygon must be a triangle in a 3D space.")

    is.inside <- function(element, lower, upper) {
        if ((length(element) != length(lower)) |
            (length(element) != length(upper)) |
            (length(lower)   != length(upper)))
            stop("Error...")

        # if (x < left) & (x > right) .... error !!!!
        # left == FALSE et right == FALSE => Error

        elementAboveLower <- (element >= lower)
        elementBelowUpper <- (element <= upper)
        elementInside     <- (elementAboveLower & elementBelowUpper)

        inside <- NULL
        if (is.matrix(element)) {
            inside <- apply(elementInside, 1, all)
        } else if (is.vector(element)) {
            inside <- all(elementInside)
        }
        return(inside)
    }

    verticesInside <- is.inside(polygon, lower, upper)
    polygonInside <- NA
    newPolygons <- list()

    if (all(verticesInside))       polygonInside <- TRUE
    else if (all(!verticesInside)) polygonInside <- FALSE
    else if (shred & (area(polygon) > epsilon)) { ## sinon, reste à NA

        # On va faire une recherche dichotomique

        idVertexAlone <- idVertexDuo <-
            idVertexInside  <- idVertexOutside <- NULL

        if(sum(verticesInside) == 1) {# TRUE est tout seul
            idVertexAlone <- idVertexInside  <- which(verticesInside == TRUE)
            idVertexDuo   <- idVertexOutside <- which(verticesInside == FALSE)
        } else {
            idVertexAlone <- idVertexOutside <- which(verticesInside == FALSE)
            idVertexDuo   <- idVertexInside  <- which(verticesInside == TRUE)
        }

        if (length(idVertexInside) == 1) idVertexInside <- rep(idVertexInside, 2)
        if (length(idVertexOutside) == 1) idVertexOutside <- rep(idVertexOutside, 2)

        # Le plus facile
        midDuo <- getMidPoint(polygon[idVertexDuo[1], ],
                              polygon[idVertexDuo[2], ])

        # Plus dur
        intersectVertices <- matrix(rep(0, 6),
                                    ncol = 3, nrow = 2,
                                    byrow = TRUE)
        dimnames(intersectVertices) <- list(NULL, c("x", "y", "z"))

        for (i in 1:2) { # for (i in 1:nrow(intersectVertices))
            # Recherche dichotomique
            intersectVertices[i, ] <- intersectCopy <-
                getMidPoint(polygon[idVertexAlone, ],
                            polygon[idVertexDuo[i], ])
            diff <- TRUE
            while (diff) {

                case <- NA
                if ((is.inside(intersectVertices[i, ], lower[1,], upper[1,])) &
                    (is.inside(intersectCopy, lower[1,], upper[1,]))) {
                    case <- "bothInside" # point et copie dans le volume
                } else if ((!is.inside(intersectVertices[i, ], lower[1,], upper[1,])) &
                          (!is.inside(intersectCopy, lower[1,], upper[1,]))) {
                    case <- "bothOutside" # point et copie dans le volume
                } else {
                    case <- "oneInsideOneOutside" # Un point dedans, l'autre dehors
                }

                # Traitement
                if (case == "bothInside") {
                    tmp <- getMidPoint(intersectVertices[i, ],
                                       polygon[idVertexOutside[i], ])
                    intersectCopy <- intersectVertices[i, ]
                    intersectVertices[i, ] <- tmp

                }
                if (case == "bothOutside") {
                    tmp <- getMidPoint(intersectVertices[i, ],
                                       polygon[idVertexInside[i], ])
                    intersectCopy <- intersectVertices[i, ]
                    intersectVertices[i, ] <- tmp

                }
                if (case == "oneInsideOneOutside") {
                    tmp <- getMidPoint(intersectVertices[i, ],
                                       intersectCopy)

                    tmpInside  <- (is.inside(tmp, lower[1,], upper[1,]))
                    copyInside <- (is.inside(intersectCopy, lower[1,], upper[1,]))
                    interVertInside <- (is.inside(intersectVertices[i, ], lower[1,], upper[1,]))

                    if (identical(tmpInside, copyInside)) {# ici
                        #intersectVertices[i, ] <- intersectVertices[i, ]
                        intersectCopy <- tmp
                    }
                    if (identical(tmpInside, interVertInside)) {# ici
                        #intersectCopy <- intersectCopy
                        intersectVertices[i, ] <- tmp
                    }
                }

                distance <- as.vector(dist(rbind(intersectVertices[i, ], intersectCopy)))
                if (distance < epsilon) diff <- FALSE
            }
        }

        newPolygons <- list(poly1 = matrix(c(polygon[idVertexAlone, ],
                                             intersectVertices[1, ],
                                             intersectVertices[2, ]),
                                           nrow = 3, ncol = 3, byrow = TRUE),
                            poly2 = matrix(c(polygon[idVertexDuo[1], ],
                                             intersectVertices[1, ],
                                             midDuo),
                                           nrow = 3, ncol = 3, byrow = TRUE),
                            poly3 = matrix(c(polygon[idVertexDuo[2], ],
                                             intersectVertices[2, ],
                                             midDuo),
                                           nrow = 3, ncol = 3, byrow = TRUE),
                            poly4 = matrix(c(intersectVertices[1, ],
                                             intersectVertices[2, ],
                                             midDuo),
                                           nrow = 3, ncol = 3, byrow = TRUE))

        # mettre x, y et z en nom des colonnes
        newPolygons <- lapply(newPolygons, function(x) {
            dimnames(x) <- list(NULL, c("x", "y", "z"))
            return(x)
        })

        # Créer deux autres points à l'intersection avec le volume
        # On a un nouveau triangle, avec le point tout seul
        # chercher le milieu du côté avec les deux points pas tout seuls
        # Ce milieu + les deux points à l'intersection avec le volume = un nouveau traingle
        # les deux derniers triangles se trouvent facilement

        # fourTriangles <- shred(x, volume, verticesInside, both)
        # LIST <- do.call(c, lapply(fourTriangles, contain, volume, shred, epsilon))
    }

    res <- NULL
    if (length(newPolygons) != 0) {
        res <- do.call(c, lapply(newPolygons, intersectPolygon, volume, shred, epsilon))
    } else {
        ##res <- list(vertices = x, inside = triangleInside)
        res <- list(list(polygon = polygon, inside = polygonInside))
    }
    return(res)
}

#------------------------------------------------------------------------------#
# Interection between canopy polygons and a cube
#
# Check if each triangle is inside or outside the volume.
# Shred a triangle into four smaller triangles
#
# The "cube" should have all its sides parallel to an origin axis (current
# restriction).
#
# @param x A matrix containing the coordinates of the triangle. Each row
#     corresponds to a points, and each column to a dimension (x, y and z).
# @param volume A 3-component matrix of coordinates of a "cube".
# @param epsilon The area limit below which no triangle is to be shreded
#     anymore.
#
# @keywords internal
#------------------------------------------------------------------------------#
intersectCanopy <- function(can, volume, shred = TRUE, epsilon = 1e-6) {

    env <- environment()
    counter <- 0
    progress <- txtProgressBar(min = 0, max = nrow(can), style = 3)

    res <- apply(can, 1, function(x) {
        #browser()
        val <- intersectPolygon(x$vertices, volume, shred, epsilon)
        x$vertices <- NULL
        x <- data.frame(x, stringsAsFactors = FALSE)
        x <- x[rep(1, length(val)), ]
        x$vertices <- do.call(list, lapply(1:length(val), function(x) val[[x]]$polygon))
        x$inside   <- do.call(c, lapply(1:length(val), function(x) val[[x]]$inside))

        i <- get("counter", envir = env)
        assign("counter", i + 1, envir = env)
        setTxtProgressBar(get("progress", envir = env), i + 1)

        return(x)
        })
    #res <- do.call(rbind, res) ### Très très long !!!!!
    res <- dplyr::bind_rows(res)
    class(res$vertices) <- "AsIs"
    #res <- res %>% dplyr::mutate_each(dplyr::funs(as.factor))
    return(res)
}


#------------------------------------------------------------------------------#
#' Split a whole canopy in accordance with a set of volumes
#'
#' Each polygon of the canopy that is included in one of the provided volumes is
#' allocated to this volume. If a polygon crosses one or several sides of a
#' volume and if the parameter \code{shred} is set to \code{TRUE}, then the
#' polygon is recursively shreded into smaller polygons until none of them (with
#' an area > \code{epsilon}) crosses the volume sides anymore.
#'
#' @param canopy A \code{Canopy} object.
#' @param volumeSet A \code{VolumeSet} object.
#' @param shred Logical. Should an intersected polygon be shreded into smaller
#'   polygons?
#' @param epsilon Minimal polygon area to take into account.
#' @param parallel Logical. Parallelization activation.
#' @param nCores Number of cores to use if \code{parallel} is set to \code{TRUE}.
#'
#' @return A \code{c("Canopy", "data.frame")} object containing only the
#' polygons that were successfully allocated to a volume.
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
#' allocatedPolygons <- splitCanopy(canopy, volumes, shred = TRUE,
#'                                  parallel = TRUE, nCores = 8)
#'
#' @export
#------------------------------------------------------------------------------#
splitCanopy <- function(canopy, volumeSet, shred = TRUE, epsilon = 1e-6,
                  parallel = FALSE, nCores = 2) {

    case <- NA

    if (requireNamespace("parallel", quietly = TRUE)) {
        if (parallel) {
            warning(paste0("Parallelization is an experimental feature. ",
                           "In addition, progress bar may not be ",
                           "displayed when activated."))
            case <- "parallel"
        } else {
            case <- "no-parallel"
        }
    } else {
        if (parallel) {
            warning("Package 'parallel' needs to be installed to activate parallelization feature.")
        }
        case <- "no-parallel"
    }

    switch (case,
        "no-parallel" = {
            res <- lapply(seq_len(nrow(volumeSet)), function(i) {
                res <- intersectCanopy(canopy, volumeSet[i, ]$vertices[[1]], shred, epsilon) # Check if volume != list
                res <- res %>% dplyr::filter(inside == TRUE) %>% dplyr::select(-inside)
                res$volumeID <- volumeSet[i, ]$volumeID

                tmp <- res$vertices
                res <- res %>% dplyr::select(-vertices)
                res$vertices <- tmp

                return(res)
            })
        },
        "parallel"    = {
            res <- parallel::mclapply(seq_len(nrow(volumeSet)), function(i) {
                res <- intersectCanopy(canopy, volumeSet[i, ]$vertices[[1]], shred, epsilon)  # Check if volume != list
                res <- res %>% dplyr::filter(inside == TRUE) %>% dplyr::select(-inside)
                res$volumeID <- volumeSet[i, ]$volumeID

                tmp <- res$vertices
                res <- res %>% dplyr::select(-vertices)
                res$vertices <- tmp

                return(res)
            }, mc.cores = nCores)
        }
    )

    res <- res %>% dplyr::bind_rows()
    class(res$vertices) <- "AsIs"
    class(res) <- c("Canopy", "data.frame")
    return(res)
}

