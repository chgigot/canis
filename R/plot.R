#------------------------------------------------------------------------------#
#' @include canopy.R
#' @include volume-set.R
#------------------------------------------------------------------------------#
NULL

#------------------------------------------------------------------------------#
#' Plot a canopy or a set of volumes in 3D
#'
#' Canopies or Volumes viewed in 3D using \code{rgl} package features.
#'
#' This function is basically a wrapper of \code{\link[rgl]{shapelist3d}}
#' (package \code{rgl}) with a seamless interface with \code{Canopy} objects
#' (returned by the function \code{\link{readCan}}) or \code{VolumeSet} objects
#' (returned by the function \code{\link{makeVolumeSet}}).
#'
#' Note that the plotting process may be time-consuming with a large number of
#' polygons. Consider using the parameter \code{fraction} when plotting a
#' \code{Canopy} to only select a random subset of all the polygons.
#'
#' @param data A \code{c("data.frame", "Canopy")} or \code{c("data.frame",
#'   "VolumeSet")} object.
#' @param fraction The fraction of rows to select.
#' @param color A color character or a polygon label to use to colorize the 3D
#'   scene.
#' @param label Display ("levels" or "indices") or no display ("no") labels for
#'   each volume. "levels" displays "labels" consisting of the volume ID and
#'   "indices", just a number, corresponding to the row number.
#' @param ... Further potential arguments passed to
#'   \code{\link[rgl]{shapelist3d}}.
#'
#' @return An object of class \code{c("shapelist3d", "shape3d")}.
#'
#' @examples
#' display3d(plants)
#' display3d(plants, frac = 0.1)
#' display3d(plants, col = rgb(0, 0.7, 0))
#' display3d(plants, col = "green4", alpha = 0.3)
#' display3d(plants, col = "plantID")
#'
#' rangeDim <- function(can, dim) {
#'     range(sapply(can$vertices, function(i) range(i[, dim])))
#' }
#' volumes <- makeVolumeSet(rangeDim(plants, "x"),
#'                          rangeDim(plants, "y"),
#'                          rangeDim(plants, "z"),
#'                          intervals = rep(3, 3))
#' display3d(volumes)
#' display3d(volumes, color = rgb(0.7, 0, 0.7), alpha = 0.3)
#' display3d(volumes, color = "red", alpha = 0.3)
#' display3d(volumes, color = "volumeID")
#'
#' @seealso \code{\link[rgl]{shapelist3d}} in the \code{rgl} package for a more
#'   detailed explanation.
#'
#' @name display3d
#' @export
#------------------------------------------------------------------------------#
display3d <- function(data, ...) UseMethod("display3d")

#------------------------------------------------------------------------------#
#' @rdname display3d
#' @export
#------------------------------------------------------------------------------#
display3d.Canopy <- function(data, color, fraction = 1.0, ...) {

    #old <- options(stringsAsFactors = FALSE)
    #on.exit(options(old), add = TRUE)

    ## Sécurité : vérifier si vertices est présent avant tout
    ## Sécurité : vérifier que tout le reste == facteur !!! Très important pour couleur !!

    dots <- list(...)

    if (!missing(color)) {
        if (color %in% names(data)) {
            if (requireNamespace("RColorBrewer", quietly = TRUE)) { ## use palette() instead?
                cols <- RColorBrewer::brewer.pal(8, "Dark2")
            } else {
                cols <- rainbow(8) ## use palette() instead
            }
            attr(cols, "length") <- length(cols)
            data[[color]] <- as.factor(data[[color]])
        } else {
            dots$color <- color
            #dots <- list(...)
            #ndots <- length(dots)
            #stuff
            #

            # http://stackoverflow.com/questions/2436688/append-an-object-to-a-list-in-r-in-amortized-constant-time

            #
            #        list_ = {
            #           a <- list(0)
            #           for(i in 1:n) {a <- list(a, list(i))}
            #       },

            # Initial list:
            #List <- list()

            # Now the new experiments
            #for(i in 1:3){
            #    List[[length(List)+1]] <- list(sample(1:3))
            #}

            #List
        }
    }

    subData  <- data %>% dplyr::sample_frac(fraction)
    indices  <- 1:3 # Deals with triangles only

    # Because apply() coerce each factor element to a characher element
    # and we want the levels (numeric, not letters,...)
    i <- sapply(subData, is.factor)
    subData[i] <- lapply(subData[i], as.numeric)

    tmesh3ds <- apply(subData, 1, function(x){
        vertices <- x$vertices ## Sécurité : vérifier si vertices est présent avant tout
        normal  <- 1.0 # TODO: À approfondir!
        polygon <- cbind(vertices, normal = normal)
        if (exists("cols")) {
            id <- x[[color]] # apply a générer une liste d'éléments
            args <- list(as.vector(t(polygon)), indices,
                         material = list(color = cols[(id %% attr(cols, "length")) + 1]))
        } else {
            args <- list(as.vector(t(polygon)), indices)
        }
        mesh3d <- do.call(rgl::tmesh3d, args)
        return(mesh3d)
    })
    LIST <- append(list(shapes = tmesh3ds), dots)
    return(do.call(rgl::shapelist3d, LIST))
}

#------------------------------------------------------------------------------#
#' @rdname display3d
#' @export
#------------------------------------------------------------------------------#
display3d.VolumeSet <- function(data, color, label = "no", ...) {

    dots <- list(...)

    if (!missing(color)) {
        if (color %in% names(data)) {
            if (requireNamespace("RColorBrewer", quietly = TRUE)) { ## use palette() instead?
                cols <- RColorBrewer::brewer.pal(8, "Dark2")
            } else {
                cols <- rainbow(8) ## use palette() instead
            }
            attr(cols, "length") <- length(cols)
            data[[color]] <- as.factor(data[[color]])
            if ((label == "levels") | (label == "indices")) LAB <- data[[color]] # Une sauvegarde car après on garde que les numéros
            data[[color]] <- as.numeric(data[[color]]) # Pour récupérer des nombre uniquement (à ajouter dans display3d.Canopy)
        } else {
            dots$color <- color
        }
    }

    volume3ds <- apply(data, 1, function(x){
        vertices <- t(x$vertices) ## Sécurité : vérifier si vertices est présent avant tout
        normal  <- 1.0 # TODO: À approfondir!
        volume <- rbind(vertices, normal = normal)
        dimnames(volume) <- c(NULL, NULL)

        volume3d <- list(vb = volume,
                         ib = matrix(c(1, 3, 2, 1, 1, 5,
                                       3, 7, 4, 5, 2, 6,
                                       4, 8, 8, 7, 6, 8,
                                       2, 4, 6, 3, 5, 7),
                                     nrow = 4, byrow = TRUE),
                         primitivetype = "quad",
                         material = list(),
                         normals = NULL,
                         texcoords = NULL)
        attr(volume3d, "class") <- c("mesh3d", "shape3d")

        if (exists("cols")) {
            id <- x[[color]] # apply a générer une liste d'éléments
            volume3d$material = list(color = cols[(id %% attr(cols, "length")) + 1])
        }
        return(volume3d)
    })

    #LIST <- volume3ds
    #LIST[[length(LIST) + 1]] <- dots
    LIST <- append(list(shapes = volume3ds), dots)
    #return(LIST)
    if (label == "no" || !(color %in% names(data))) {
        return(do.call(rgl::shapelist3d, LIST))
    } else {
        res <- getVolumeCenter(data$vertices)
        # Gérer le cas si alpha n'existe pas ou trop faible,
        # alors forcer un alpha = 0.9 pour voir les labels
        if (label == "levels")  LAB <- LAB
        if (label == "indices") LAB <- as.numeric(as.factor(LAB))
        return(invisible(list(
            do.call(rgl::shapelist3d, LIST),
            rgl::text3d(x = res[, "x"],
                        y = res[, "y"],
                        z = res[, "z"],
                        texts = LAB)
        )))
    }
}
