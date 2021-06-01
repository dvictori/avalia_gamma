# funcoes auxiliares

decendio <- function(d) {
    mes <- lubridate::month(d)
    dia <- lubridate::day(d)
    
    dec_n <- ifelse(dia <= 10, 1, 
                    ifelse(dia <= 20, 2, 3))
    
    dec <- (mes - 1)*3 + dec_n
    return(dec)
}
