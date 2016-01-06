# This script calls to googlemaps api to get (walking) distances [km] and 
# durations [minutes] between the best hot chocolate cafes in Van (according 
# to Van City Buzz: http://vancitybuzz.com/2015/12/best-hot-chocolate-vancouver/ )
# End result is a matrix of distances and a matrix of duations.


library("XML")
library("repmis")
library("RCurl")
source("Google_Maps_cafe_distance_retrieval-FUNCTIONS.R")
APIkey = "PUT YOUR KEY HERE"

cafes = c("100+W+Hastings+St,+Vancouver,+BC+V6B",
"4208+Fraser+St,+Vancouver,+BC+V5V+4E8","1938+Commercial+Dr,+Vancouver,+BC+V5N+4A7",
"2539+W+Broadway,+Vancouver,+BC+V6K+2E9","1088+Homer+St,+Vancouver,+BC+V6B+2W9",
"319+Carrall+St,+Vancouver,+BC+V6B+2J4","863+W+Hastings+St,+Vancouver,+BC+V6C+3N9",
"1059+Alberni+St,+Vancouver,+BC+V6E+1A1","1849+W+1st+Ave,+Vancouver,+BC+V6J+5B8",
"801+W+Georgia+St,+Vancouver,+BC+V6C+2E5","767+Seymour+St,+Vancouver,+BC+V6B+5J3",
"508+W+Hastings+St,+Vancouver,+BC+V6B+1L6",
"603+Abbott+St,+Vancouver,+BC+V6B+6N7","550+Burrard+St,+Vancouver,+BC+V6C+2B5",
"401+Burrard+St,+Vancouver,+BC+V6C+3R2","338+Helmcken+St,+Vancouver,+BC+V6B+6C5",
"999+Granville+St,+Vancouver,+BC+V6Z+1L3","2506+Granville+St,+Vancouver,+BC+V6H+3G8",
"695+W+Broadway,+Vancouver,+BC+V5Z+1G6","521+W+Broadway,+Vancouver,+BC+V5Z+1E6",
"1201+Robson+St,+Vancouver,+BC+V6E+1C2","1203+Davie+St,+Vancouver,+BC+V6E+1N4",
"1806+Commercial+Dr,+Vancouver,+BC+V5N+4A5","4295+Main+Street,+Vancouver,+BC")

cafenames = c("Prado Café","Prado Café","Prado Café",
              "Thomas Haas","Small Victory","East Van Roasters",
              "Mink Chocolate Café","Thierry",
              "Koko Monk Chocolates","Bel Café",
              "Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz","Blenz",
              "Continental Coffee","Continental Coffee"
              )
mode = "walking"

urlformat = list("https://maps.googleapis.com/maps/api/distancematrix/xml?origins=",
                   "&destinations=","&mode=","&units=metric&language=fr-FR&key="
  )

# Ask googlemaps for the distances
page = list()
N = length(cafes)
for(lp in 1:(N-1)){
  
  url = paste(urlformat[[1]],cafes[lp],urlformat[[2]],paste(cafes[(lp+1):N],collapse='|'),
              urlformat[[3]],mode,urlformat[[4]],APIkey,sep="")
  
  # Read page
  page[[lp]] = getURL(url)
  # if 'page' is not right:
  # Troubleshooting #1
  # copy 'url' from the console into your clipboard and pasting it into the location bar of a browser.
  # Troubleshooting #2
  # note that the url must be perfect and blank spaces are an easy way to have a malformed url.  
  # If the object 'page' gives an error or isn't as expected then try copying the output of 'url' 
  # from the console into a text editor and search for blank space.  Copying a bad url into a 
  # browser might work but it won't work here
  #
  
}

# here's the [empty] Distance and Duration Matrices:
DurMatrix = DistMatrix = matrix(0,nrow=N,ncol=N,dimnames = list(cafes,cafes))

# Now let's fill in the elements
for(lp in 1:(N-1)){
  node = xmlTreeParse(page[[lp]], getDTD = F)[[1]]
  D = Distances(node, name="row")
  # Note that symmetry in distance and duration only holds for cycling and walking.
  DistMatrix[cafes[lp],cafes[(lp+1):N]] = D[,"km"]
  DistMatrix[cafes[(lp+1):N],cafes[lp]] = D[,"km"]
  
  DurMatrix[cafes[(lp+1):N],cafes[lp]]   = D[,"minutes"]
  DurMatrix[cafes[lp],cafes[(lp+1):N]]   = D[,"minutes"]
}





