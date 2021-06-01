# ajusta função gama a pontos no raster

library(raster)
library(fitdistrplus)

dados <- brick('data/prec_decendial_xavier_1980_2015.tif')


get_gamma <- function(dado, decendio, lat, lon) {
    pt <- cbind(lon, lat)
    serie <- as.numeric(raster::extract(dado, pt))
    
    serie <- data.frame(dec = rep(1:36, 36),
                        prec = serie)
    
    fit <- fitdist(serie[serie$dec == decendio, 'prec'],
                   'gamma', method = 'mme')
    
}

pt <- cbind(-47, -23)

serie <- extract(dados, pt)

decendios <- rep(1:36, 36)

serie <- data.frame(dec = decendios,
                    prec = as.numeric(serie))

# dec 1
fit1 <- fitdist(serie[serie$dec == 1, 'prec'],
                'gamma')
plot(fit1)
cdfcomp(fit1)

# dec 18
fit18 <- fitdist(serie[serie$dec == 18, 'prec'],
                'gamma')
plot(fit18)
cdfcomp(fit18)

# testando no raster
# temos que calcular o shape e scale separadamente
prec_dec1 <- dados[[which(decendios == 1)]]

mascara <- sum(prec_dec1)
prec_dec1_m <- mask(prec_dec1, mascara == 0, maskvalue = TRUE)

prec_menor <- crop(prec_dec1, extent(-48, -47, -24, -23))

gamma_shape <- function(x, ...) {
    if (mean(x) == 0) {
        return(0)
    } else {
        fitdist(x, 'gamma', method = 'mme')$estimate[1] 
    }    
}

gamma_rate <- function(x) {
    if (mean(x) == 0) {
        return(0)
    } else {
        fitdist(x, 'gamma', method = 'mme')$estimate[2] 
    }    
}

shape <- overlay(prec_dec1, fun = gamma_shape)
rate <- overlay(prec_dec1, fun = gamma_rate)


# prob da chuva ser > 100 no dec1
prob <- overlay(shape, rate, fun = function(x, y) {
    pgamma(100, shape = x, rate = y, lower.tail = FALSE)
})

shape.dec <- stackApply(dados, indices = decendios,
                     fun = gamma_shape)
