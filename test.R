
#* @post /async_sqrt
#* @param n
function(n) {
  future::future({
    #Sys.sleep(5)
    x<-sqrt(as.numeric(n))
    x
  })
}
#* @get /not_async_sqrt
#* @param n
function(n) {
  Sys.sleep(5)
  x<-sqrt(as.numeric(n))
  x
}
#* @get /sqrt
#* @param n
function(n) {
  x<-sqrt(as.numeric(n))
  x
}