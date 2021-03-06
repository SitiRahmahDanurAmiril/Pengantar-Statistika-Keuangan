Bunga <- function(num, A, k, t, m=TRUE)
  switch(num,
         satu = {
           bunga_tunggal = (A/k-1)/t
           print(bunga_tunggal)
         },
         dua = {
           bunga_majemuk_nominal = ((A/k)^(1/(m*t))-1)*m
           print(bunga_majemuk_nominal)
         },
         tiga = {
           bunga_majemuk_kontinu = log(A/k)/t
           print(bunga_majemuk_kontinu)
         }
  )