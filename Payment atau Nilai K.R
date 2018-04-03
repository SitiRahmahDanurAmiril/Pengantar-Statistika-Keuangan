K <- function(num, nilai, i, t, m=TRUE){
  j=i/m
  n=t*m
  v=1/(1+j)
  switch(num,
         satu = { 
           k = nilai/((1-v^n)/(j))
           print(k)
         },
         dua = {
           k = nilai/((((1+j)^n)-1)/j)
           print(k)
         },
         tiga = {
           k = nilai/((1-v^n)/(j*v))
           print(k)
         },
         empat = {
           k = nilai/((((1+j)^n)-1)/(j*v))
           print(k)
         }
        
  )
}