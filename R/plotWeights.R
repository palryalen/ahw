plotWeights <- function(w,t,g,n=100)
{   
    ug <- unique(g)[1:n]
    plot(NULL, xlim=c(min(t),  min(max(t),2990)), 
         ylim=c(0.4,3), xlab='time', ylab='weights')
    for (v in ug) {
       J <- which(v == g)
       lines(t[J],w[J], type='s', col='#00000022')
    }
}


