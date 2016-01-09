# This script calls to googlemaps api to get (walking) distances [km] and 
# durations [minutes] between the best hot chocolate cafes in Van (according 
# to Van City Buzz: http://vancitybuzz.com/2015/12/best-hot-chocolate-vancouver/ )
# End result is a matrix of distances and a matrix of duations to be used as
# part of the Travelling Salesman Problem.
#
# If you have errors, it might be because googlemaps will return an error if you
# have performed too many querries (or too many too fast) for your (free) API key.
# Go to the line "page[[lp]] = getURL(url)" (line 70ish)
# Look at the last value of the page list.  This is a common place for errors to occur
# It should be easy to tell if the googlemaps querry came down correctly or if it broke.
#
#
# These are the steps to this script.
# 1- querry googlemaps for (street) distances and travel times (ignoring time of day)
# 2- geocode the addresses into lattitude and longitude (easier to plot in R)
# 3- Perform the asymetric travelling salesman problem from an optimal starting point.
# 4- Make some plots
#rm(list=ls())
library("XML")
library("repmis")
library("RCurl")
source("Google_Maps_cafe_distance_retrieval-FUNCTIONS.R")



############### 1 ############### ############### ############### ############### ###############
# 1- querry googlemaps for (street) distances and travel times (ignoring time of day)
# Set up the cafe locations
# Querry google about the distances between pairs
# Since googlemaps limits querries (for free users) only run this if a saved version doesn't already exist.
############### 1 ############### ############### ############### ############### ###############

if(!file.exists("CafeDistances.Rdata")){
  APIkey = "YOUR_KEY_GOES_HERE"
  
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
                     "&destinations=","&mode=","&units=metric&language=fr-FR&key=")
  
  # Ask googlemaps for the distances
  page = list()
  N = length(cafes)
  for(lp in 1:(N-1)){
    
    url = paste(urlformat[[1]],cafes[lp],urlformat[[2]],paste(cafes[(lp+1):N],collapse='|'),
                urlformat[[3]],mode,urlformat[[4]],APIkey,sep="")
    # Read the webpage
    page[[lp]] = getURL(url)
    # View the contents of page[[lp]] and make sure it gives you what you need.
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
    # Note that symmetry in distance and duration 
    # matrices generally only hold for cycling and walking.
    #
    DistMatrix[cafes[lp],cafes[(lp+1):N]] = D[,"metres"]
    DistMatrix[cafes[(lp+1):N],cafes[lp]] = D[,"metres"]
    
    DurMatrix[cafes[(lp+1):N],cafes[lp]]   = D[,"seconds"]
    DurMatrix[cafes[lp],cafes[(lp+1):N]]   = D[,"seconds"]
  }
  
  save.image("CafeDistances.Rdata")
}else{
  load("CafeDistances.Rdata")
}


############### 2 ############### ############### ############### ############### ###############
#
# 2- geocode the addresses into lattitude and longitude (easier to plot in R)
# If the cafe locations and names already exist (as per above) then use them, otherwise define them
#
#
# Geocode the addresses into Lattitude and Longitude
############### 2 ############### ############### ############### ############### ###############


if(!file.exists("CafeLatLong.Rdata")){
  APIkey = "AIzaSyBujHC5i5UX-uyKPeaHkgocaflAqckWXZ0"
  if(!exists("cafes")){
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
  }
  
  urlformat = list("https://maps.googleapis.com/maps/api/geocode/xml?address=",
                   "&key=")
  
  # Ask googlemaps for the lattitude and longitude
  page = list()
  N = length(cafes)
  for(lp in 1:N){
    url = paste(urlformat[[1]],paste(cafes[lp],collapse='|'),urlformat[[2]],APIkey,sep="")
    # Read the webpage
    page[[lp]] = getURL(url)
    # View the contents of page[[lp]] and make sure it gives you what you need.
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
  
  # here's the [empty] Lattitude and longitude matrix
  LatLong = matrix(0,nrow=N,ncol=2,dimnames = list(cafes,c("Lat","Long")))
  
  # Now let's fill in the elements
  for(lp in 1:N){
    node = xmlTreeParse(page[[lp]], getDTD = F)[[1]]
    LatLong[lp,] = GetLatLong(node)
  }
  
  save.image("CafeLatLong.Rdata")
}else{
  load("CafeLatLong.Rdata")
}




############### 3 ############### ############### ############### ############### ###############
# 3- Perform the asymetric travelling salesman problem from an optimal starting point.
# Note that we need to also choose the single best locations of cafes that are chains.
# We don't need to visit repeats of stores at all of their various locations.
# Do the Travelling Salesman problem for each combination of unique cafes
#
# Note that to run the TSP I tried 3 methods.  
# Method 1: nearest_insertion starts with an arbitrary cafe then adds 
#   a new city to the tour location that is the smallest detour from the current tour.  For example, if 
#   the current tour consists of cafes in the order [A,E,C,D], we insert cafe B in the location that is 
#   the shortest detour.  So it might then become the tour: [A,E,C,B,D].
# Method 2: repetitive_nn Gives each cafe the chance to be the starting point then builds up the tour
#   by adding in the closest as of yet not included cafe
# Method 3: 2-opt refinement, start with a random tour then refine it by swapping 2 cafes. Continue until 
#   all possible pairs of swappers extend rather than shrinnk the tour. 
#         
# In each case repeat the process 100 times.  This is excessive for repetitive_nn, nearest_insertion
# because they use individual cafes as initializers, but for the 2-opt refinement this is reasonable (or small)
# because it begins with a random tour.
#
############### 3 ############### ############### ############### ############### ###############
library(TSP)
library(doParallel)
registerDoParallel()
# find out how many duplicates there are:
cafenames = as.factor(cafenames)
CountOfEachCafe = table(cafenames)

sum(CountOfEachCafe>1)
print('non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert!')
print(' You have been warned.  To use this code for anything else you will need to')
print('change the number of non-unique cafes and how that is dealt with')


### You've been warned.  This part of the code needs hand tuning.
# There are 3 with more than 1 count
Bind = which(cafenames=="Blenz")
Pind = which(cafenames=="Prado Café")
Cind = which(cafenames=="Continental Coffee")
ALLOTHERS = 1:length(cafenames)
ALLOTHERS=sort(ALLOTHERS[-c(Bind,Pind,Cind)])
# Now I have all appropriate indices for the specific cafe chains and the unique cafes

# Now calculate the shortest path and keep the duration for all combinations of the non-unique cafes
# I use the Distance Matrix (with units of metres) but you could use Duration (units of time).
index = 0
Loc.Used = matrix(0,nrow=3*72,ncol=length(unique(cafenames)))
out = list()
TotalDist = rep(0,72*3)
tour = matrix(0,nrow=3*72,ncol=length(unique(cafenames)))
for(blenz in Bind){
  for(prado in Pind){
    for(continental in Cind){
      index = index+1
      # Keep the cafes in question
      L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
      # make the distance matrix between the places in use
      #rownames(Dist.Use) = cafenames[L]
      Dist.Use = DistMatrix[L,L]
      # Solve the asymetric TSP
      out[[index]]  = solve_TSP(ATSP(Dist.Use),method = "nearest_insertion",control=list(repetitions=250))
      #Define the order
      tour[index,] = L[as.integer(TOUR(out[[index]]))]
      # Get the Total Distance Travelled
      TotalDist[index] = tour_length(out[[index]])
      
      
      ####### Since the method used can make a big difference let's retry it with 
      
      index = index+1
      # Keep the cafes in question
      L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
      # make the distance matrix between the places in use
      Dist.Use = DistMatrix[L,L]
      # Solve the TSP
      out[[index]]  = solve_TSP(ATSP(Dist.Use),method = "repetitive_nn",control=list(repetitions=250))
      #Define the order
      tour[index,] = L[as.integer(TOUR(out[[index]]))]
      # Get the Total Distance Travelled
      TotalDist[index] = tour_length(out[[index]])
        
      index = index+1
      # Keep the cafes in question
      L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
      # make the distance matrix between the places in use
      Dist.Use = DistMatrix[L,L]
      # Solve the TSP
      out[[index]]  = solve_TSP(ATSP(Dist.Use),method = "two_opt",control=list(repetitions=250,tour=out[[index-1]]))
      #Define the order
      tour[index,] = L[as.integer(TOUR(out[[index]]))]
      # Get the Total Distance Travelled
      TotalDist[index] = tour_length(out[[index]])
    }
  }
}





############### 4 ############### ############### ############### ############### ###############
# 4- Make some plots
# A) Make some plots to look at the algorithm performance.  
# B) Obtain the best tour and plot it using Lattitudes and Longitudes on top of a google maps image
#        To do that you need to go to maps.google.com.  Use the menu on the left to get to "my maps"
#        From there you can place points at Lattitude and Longitude points to define a bounding box.  
#        Take a screen shot and trim it to the bounding box.  We will use that as the background for 
#        the R plot. 
############### 4 ############### ############### ############### ############### ###############
# A) First let's see if the methods agreed.  
# If so they should all fall along the diagonal line, which of course they don't
par(mfrow=c(3,1))
plot(TotalDist[seq(1,3*72,by=3)],TotalDist[seq(2,3*72,by=3)],ylab="2-opt refinement",xlab="nearest_insertion")
abline(0,1)
plot(TotalDist[seq(1,3*72,by=3)],TotalDist[seq(3,3*72,by=3)],ylab="repetitive_nn",xlab="nearest_insertion")
abline(0,1)
plot(TotalDist[seq(3,3*72,by=3)],TotalDist[seq(2,3*72,by=3)],ylab="2-opt refinement",xlab="repetitive_nn")
abline(0,1)


# B) Find the best tour.  The results may depend on your random seed, but I found that I usually had multiple 
# equal best tours found

# Best tour and sanity checks:
best = which(TotalDist==min(TotalDist))
# Here is the path
cafenames[tour[best[1],]]
cafes[tour[best[1],]]
# Here is the Total Distance Travelled
TotalDist[best]
# SANITY CHECK
# You can do a sanity check for the routing using google maps
# You can do this by manually plotting the route, in one case this meant this resulting pathway:
# https://www.google.ca/maps/dir/4295+Main+St,+Vancouver,+BC+V5V+319+Carrall+St,+Vancouver,+BC+V6B+2J4/319+Carrall+St,+Vancouver,+BC+V6B+2J4/100+W+Hastings+St,+Vancouver,+BC+V6B/863+W+Hastings+St,+Vancouver,+BC+V6C+3N9/1059+Alberni+St,+Vancouver,+BC+V6E+1A3/801+W+Georgia+St,+Vancouver,+BC+V6C+2E5/767+Seymour+St,+Vancouver,+BC+V6B+5J3/1088+Homer+St,+Vancouver,+BC+V6B/1849+W+1st+Ave,+Vancouver,+BC+V6J+5B8/2539+W+Broadway,+Vancouver,+BC+V6K/@49.2667593,-123.1618164,13z/data=!4m62!4m61!1m5!1m1!1s0x548673f7730ef9fd:0x70a8e7c52620dd0b!2m2!1d-123.1011643!2d49.247242!1m5!1m1!1s0x5486717a08025b43:0x58663f2d510c7744!2m2!1d-123.1044716!2d49.28207!1m5!1m1!1s0x548671798fe2be07:0xa3a0f3f26378dc6d!2m2!1d-123.1073206!2d49.2820263!1m5!1m1!1s0x548671825efa437d:0x2f3ba621fb761124!2m2!1d-123.1156229!2d49.2864491!1m5!1m1!1s0x54867181b2c10189:0x6c7ac6c84ff38577!2m2!1d-123.1226288!2d49.2848765!1m5!1m1!1s0x5486717f5e265569:0xbb8ca81bc6061324!2m2!1d-123.1190438!2d49.2834886!1m5!1m1!1s0x5486717e54a24733:0xead9d573c873d81c!2m2!1d-123.1188476!2d49.2810814!1m5!1m1!1s0x548673d665aef4d5:0x8941725ef20af80f!2m2!1d-123.1212133!2d49.2764917!1m5!1m1!1s0x548673ca69e42b69:0xb6efd41d2761fe3c!2m2!1d-123.1473779!2d49.2708342!1m5!1m1!1s0x548673adde224ef3:0x28729bff19a02699!2m2!1d-123.1632614!2d49.2640033!3e2?hl=en
# Or you can automatically make the url to visit,
# just be sure to change the mode of transportation to waling:
urlformat = "https://www.google.ca/maps/dir/"
url = paste(urlformat[[1]],paste(cafes[tour[best[1],]],collapse='/'),sep="")
# Visit that url and look for obvious lack of efficiencies
#
#
#  Now onto plotting the locations on a map with a map image in the background.
#   See the section header for how to obtain the map in the background to suit
#   the coordinate system.
library(calibrate)
library(png)
par(mfrow=c(1,1))
#Replace the directory and file information with your info
ima <- readPNG("google_map_background.png")

#Set up the plot area
plot(LatLong[tour[best[1],],2],-LatLong[tour[best[1],],1],type='n',ylab="Longitude",xlab="Lattitude",
     xlim=c(-123.165,-123.1),ylim=c(49.24750,49.29000))

lim <- par()
rasterImage(ima, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
grid()

rbPal <- colorRampPalette(c('blue','red'))

textxy(LatLong[tour[best[1],],2],LatLong[tour[best[1],],1],
       letters[1:length(tour[best[1],])], cex=1,col=2)

points(LatLong[tour[best[1],],2],LatLong[tour[best[1],],1],type='b'
       ,col=2)

legend(lim$usr[1]+.02,abs(lim$usr[3]-lim$usr[4])*.4+lim$usr[3],
  pch = letters[1:length(tour[best[1],])],
  cafenames[tour[best[1],]], cex=.8,col=2)#,bty='n')

