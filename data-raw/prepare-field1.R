library(canis)
library(dplyr)
plants <- readCan("data-raw/field1.can")
plantIDs <- unique(plants$plantID)[1:7] # We just keep 7 plants
newLocs <- data.frame(plantID = 0:6,
                      xOrigin = c(14.84501, 20.84089, 41.27688, -25.274821,
                                  -16.136680, -39.489095, -34.623309),
                      yOrigin = c(36.67440, 18.52380, -34.14191, -28.805722,
                                  -26.084791, -49.005557, -17.264172))
plants <- do.call(rbind.data.frame, lapply(plantIDs, function(id) {
    res <- plants %>% filter(plantID == id)
    lagToOrigin <- do.call(rbind.data.frame,
                           lapply(res$vertices, function(mat) {
                               data.frame(x = mean(mat[, "x"]),
                                          y = mean(mat[, "y"]))
                               }))
    lagToOrigin <- colMeans(lagToOrigin)
    vertices <- lapply(res$vertices, function(mat) {
        mat[, "x"] <- mat[, "x"] - lagToOrigin["x"] +
            newLocs[newLocs$plantID == id, "xOrigin"]
        mat[, "y"] <- mat[, "y"] - lagToOrigin["y"] +
            newLocs[newLocs$plantID == id, "yOrigin"]
        mat
    })
    res$vertices <- I(vertices)
    res
}))
class(plants) <- c("Canopy", "data.frame")
devtools::use_data(plants, overwrite = TRUE)

