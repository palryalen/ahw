# Replaces NA values in vec with last non-NA element
# Assumes the first position in vec is not NA
# Can be replaced by zoo::na.locf0
naReplace <- function(vec){
        naIds <- which(!is.na(vec))
        tab <- data.table(vec=vec,notNa=0)
        tab[,notNa := cumsum(!is.na(vec))]
        tab[,filledVec := vec[1],by=notNa]
        vec <- tab$filledVec
        return(vec)
}