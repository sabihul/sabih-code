plot(sme_wr$count)

ts_cln<-tsclean(sme_wr$count)

plot(ts_cln)

library(tseries)


plot(diff(sme_wr$count))


adf.test(sme_wr$count, alternative="stationary", k=0)

adf.test(diff(sme_wr$count), alternative="stationary", k=0)

plot(diff(sme_wr$count))


