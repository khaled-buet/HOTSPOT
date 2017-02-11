file = "dataset_TSMC2014_NYC.txt"
inputfile = file(file, "r")
checkins = readLines(inputfile)
baseTime = 1333476009
venueschecks = list()
c = 0
nchecks = c()
latitude = c()
longtitude = c()
venuetype = c()
for(i in 1:length(checkins)){
  a = strsplit(toString(checkins[i]),"\t", fixed = TRUE, useBytes = TRUE)[[1]][8]
  v = strsplit(toString(checkins[i]),"\t", fixed = TRUE, useBytes = TRUE)[[1]][2]
  lat = as.numeric(strsplit(toString(checkins[i]),"\t", fixed = TRUE, useBytes = TRUE)[[1]][5])
  lot = as.numeric(strsplit(toString(checkins[i]),"\t", fixed = TRUE, useBytes = TRUE)[[1]][6])
  d = strptime(a, format = "%a %b %d %H:%M:%S %z %Y")
  
  if(as.numeric(d) > baseTime + 86400){
    baseTime = baseTime + 86400
    nchecks = c(nchecks, c)
    #print(c)
    c = 0
  }else{
    if(v %in% names(venueschecks)){
      t = as.numeric(venueschecks[v])
      venueschecks[v] = t + 1
    }else{
      latitude = c(latitude, lat)
      longtitude = c(longtitude, lot)
      vt = strsplit(toString(checkins[i]),"\t", fixed = TRUE, useBytes = TRUE)[[1]][3]
      venuetype = c(venuetype, vt)
      venueschecks[v] = 1
    }
  }
  c = c + 1
}
close(inputfile)