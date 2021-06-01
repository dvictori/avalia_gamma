# calcula chuva em decendios acumulados
# interesse em trabalharmos com chuva em 1, 2, 3 até 6 decendios (2 meses)

library(raster)

dados_dec <- brick('data/prec_decendial_xavier_1980_2015.tif')

decAcum <- function(x, dec) {
    calc(x,
            fun = function(x) {
                movingFun(x, n = dec, fun = sum, type = 'from',
                          circular = TRUE)
                })
}

# 4 minutos por agregação
beginCluster()
for (d in 2:6) {
    dec_acum <- clusterR(dados_dec, decAcum, args = list(dec = d))
    writeRaster(dec_acum,
                sprintf('data/prec_decendial_acum_%s_xavier_1980_2015.tif', d))
}
endCluster()


# Usando o cluster foi mais rápido para mais camadas
# 10 camadas: 21s vs 14s (cluster vs. single)
# 50 camadas: 72s vs 66s
# 100 camadas: 131s vs 132s
# 150 camadas: 220s vs 358s

ptm <- proc.time()
beginCluster()
dec2 <- clusterR(dados_dec[[1:100]], decAcum, args = list(dec = 2))
endCluster()
proc.time() - ptm

ptm <- proc.time()
teste <- calc(dados_dec[[1:100]],
                 fun = function(x) movingFun(x, n = 2, fun = sum, type = 'from', circular = TRUE))
proc.time() - ptm

