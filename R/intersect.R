#------------------------------------------------------------------------------#
#' Heron's formula
#'
#' Compute the area of a triangle knowing only the lengths of its three sides.
#'
#' @param x A vector containing the lengths of the triangle sides.
#------------------------------------------------------------------------------#
heron <- function(x) {
    if (length(x) != 3) stop("Error.")
    s <- sum(x) / 2
    return(sqrt(s * (s - x[1]) * (s - x[2]) * (s - x[3])))
}

#------------------------------------------------------------------------------#
#' Triangle area
#'
#' ...
#'
#' @param x A matrix containing the coordinates of the triangle. Each row
#'     corresponds to a points, and each column to a dimension (x, y and z).
#'
#' @export
#------------------------------------------------------------------------------#
area <- function(x) {
    distances <- as.vector(dist(x)) ### Vérifier manuellement pour qq triangles. # Distances between the rows of a data matrix
    return(heron(distances))
}

#------------------------------------------------------------------------------#
#' Compute coordinates of the interesect point
#'
#' TODO.
#'
#' @keywords internal
#------------------------------------------------------------------------------#
getIntersectPoint <- function(p1, p2, p3part) {
    if (length(na.omit(p3part)) != 1) stop("Error in ppart.")
    i <- which(!is.na(p3part))
    k <- (p3part[i] - p1[i]) / (p2[i] - p1[i])
    return(p1 + k * (p2 - p1))
}


#------------------------------------------------------------------------------#
#' Compute coordinates of the middle point
#'
#' TODO.
#'
#' @keywords internal
#------------------------------------------------------------------------------#
getMidPoint <- function(p1, p2) return((p1 + p2) / 2)

#------------------------------------------------------------------------------#
#' is in volume ?
#'
#' TODO.
#'
#' @keywords internal
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
#' Shred a triangle into pieces
#'
#' Shred a triangle into four smaller triangles
#'
#' @export
#------------------------------------------------------------------------------#
shred <- function(x, volume, verticesInside, both, PPART3) {
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

}

#------------------------------------------------------------------------------#
#' Interection between canopy polygons and a cube
#'
#' Check if each triangle is inside or outside the volume.
#' Shred a triangle into four smaller triangles
#'
#' The "cube" should have all its sides parallel to an origin axis (current
#' restriction).
#'
#' @param x A matrix containing the coordinates of the triangle. Each row
#'     corresponds to a points, and each column to a dimension (x, y and z).
#' @param volume A 3-component matrix of coordinates of a "cube".
#' @param epsilon The area limit below which no triangle is to be shreded
#'     anymore.
#'
#' @export
#------------------------------------------------------------------------------#
containPolygon <- function(polygon, volume, shred = TRUE, epsilon = 1e-6) {

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
        res <- do.call(c, lapply(newPolygons, containPolygon, volume, shred, epsilon))
    } else {
        ##res <- list(vertices = x, inside = triangleInside)
        res <- list(list(polygon = polygon, inside = polygonInside))
    }
    return(res)
}

#------------------------------------------------------------------------------#
#' Interection between canopy polygons and a cube
#'
#' Check if each triangle is inside or outside the volume.
#' Shred a triangle into four smaller triangles
#'
#' The "cube" should have all its sides parallel to an origin axis (current
#' restriction).
#'
#' #@param x A matrix containing the coordinates of the triangle. Each row
#'     corresponds to a points, and each column to a dimension (x, y and z).
#' #@param volume A 3-component matrix of coordinates of a "cube".
#' #@param epsilon The area limit below which no triangle is to be shreded
#'     anymore.
#'
#' #@export
#------------------------------------------------------------------------------#
#contain <- function(x, volume, shred = TRUE, epsilon = 1e-6) UseMethod("contain")

#------------------------------------------------------------------------------#
#' @export
#------------------------------------------------------------------------------#
containCanopy <- function(can, volume, shred = TRUE, epsilon = 1e-6) {

    env <- environment()
    counter <- 0
    progress <- txtProgressBar(min = 0, max = nrow(can), style = 3)

    res <- apply(can, 1, function(x) {
        #browser()
        val <- containPolygon(x$vertices, volume, shred, epsilon)
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
    #res <- res %>% dplyr::mutate_each(dplyr::funs(as.factor))
    return(res)
}



