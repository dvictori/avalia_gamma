# Calcula base decendial de chuva
# a partir dos dados diários Xavier

library(raster)
source('aux_func.R')

infolder <- '/home/daniel/geodb/xavier/prec_v2.2/'
infiles <- dir(infolder, full.names = T)

# dados estão divididos em 4 blocos
data_list <- vector('list', 4)

for (i in 1:length(infiles)) {
    dados <- brick(infiles[i])
    
    datas <- dados@z
    decs <- decendio(datas[[1]])
    anos <- lubridate::year(datas[[1]])
    
    indices <- as.integer(paste0(anos, decs))
    
    teste <- stackApply(dados, indices, fun = sum)
    
    data_list[[i]] <- teste
}

writeRaster(stack(data_list),
            'data/prec_decendial_xavier_1980_2015.tif',
            overwrite = TRUE)
