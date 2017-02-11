library(geosphere)
distance = 1200
#calcDensity = function(dataset, distance = 1500){
cdist = c()
for (i in  1:length(dataset[,1])) {
  lat0 = dataset[i,]$latitude
  lot0 = dataset[i,]$longtitude
  c0 = c(as.numeric(lat0), as.numeric(lot0))
  count = 0
  for (j in 1:20) {
    if (i != j) {
      lat1 = dataset[j,]$latitude
      lot1 = dataset[j,]$longtitude
      c1 = c(as.numeric(lat1), as.numeric(lot1))
      d = distm(c0, c1, fun = distHaversine)
      print(d)
      if(d < distance && d!=0){
        count = count + 1
      }
    }
  }
  cdist = c(cdist,count)
  print(i)
  if(i>1) break
}
#}