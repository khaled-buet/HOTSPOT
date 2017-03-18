temposignal = data.frame(read.csv('temporalsignal.csv'))
consistency = consistencymatrix$consistency
uniquecheckin = data.frame(read.csv('uniquecheckins.csv'))
row.names(uniquecheckin) = uniquecheckin$venuename
fcheckindata = data.frame(read.csv('finalcheckindata.csv'))
row.names(fcheckindata) = fcheckindata$venuenames
morning = temporalsignal$morning
noon = temporalsignal$noon
afternoon = temporalsignal$afternoon
evening = temporalsignal$evening
consistent = c()
openness = c()
for(i in 1:length(fcheckindata[,1])){
  venueid = fcheckindata[i,2]
  s = uniquecheckin[venueid, 'identity']
  v = as.numeric(fcheckindata$ncheckins[i])/as.numeric(fcheckindata$density[i])
  #print(v)
  consistent = c(consistent, s)
  openness = c(openness, v)
}
openness = as.vector(unlist(lapply(openness, function(x){if(is.infinite(x)){0}else{x}})))
featurizedcheckins = data.frame(fcheckindata, consistent, morning, noon, afternoon, evening,openness)
write.csv(featurizedcheckins,'finalfeaturizedcheckins.csv')