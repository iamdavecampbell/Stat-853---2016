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






######### Now set up the TSP


###############
# Now do the Travelling Salesman problem for each combination of unique cafes
#####
library(TSP)
library(doParallel)
registerDoParallel()
# find out how many duplicates there are:
cafenames = as.factor(cafenames)
CountOfEachCafe = table(cafenames)

sum(CountOfEachCafe>1)
print('non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert!')

print('non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert!')

print('non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert! non-automation alert!')

### You've been warned.  This part needs hand tuning.
# There are 3 with more than 1 count
Bind = which(cafenames=="Blenz")
Pind = which(cafenames=="Prado Café")
Cind = which(cafenames=="Continental Coffee")
ALLOTHERS = 1:length(cafenames)
ALLOTHERS=sort(ALLOTHERS[-c(Bind,Pind,Cind)])
# Now I have all appropriate indices.

# Now calculate the shortest path and keep the duration for all combinations of the non-unique cafes
index = 0
Loc.Used = matrix(0,nrow=3*72,ncol=length(unique(cafenames)))
out = list()
time = rep(0,72*3)
tour = matrix(0,nrow=3*72,ncol=length(unique(cafenames)))
for(blenz in Bind){
    for(prado in Pind){
        for(continental in Cind){
            index = index+1
            # Keep the cafes in question
            L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
            # make the distance matrix between the places in use
            Dur.Use = DurMatrix[L,L]
            # Solve the asymetric TSP
            out[[index]]  = solve_TSP(ATSP(Dur.Use),method = "nearest_insertion",control=list(repetitions=100))
            #Define the order
            tour[index,] = L[as.integer(TOUR(out[[index]]))]
            # Get the time
            time[index] = tour_length(out[[index]])
            
            
            ####### Since the method used can make a big difference let's retry it with
            
            index = index+1
            # Keep the cafes in question
            L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
            # make the distance matrix between the places in use
            Dur.Use = DurMatrix[L,L]
            # Solve the TSP
            out[[index]]  = solve_TSP(ATSP(Dur.Use),method = "repetitive_nn",control=list(repetitions=100))
            #Define the order
            tour[index,] = L[as.integer(TOUR(out[[index]]))]
            # Get the time
            time[index] = tour_length(out[[index]])
            
            
            
            
            index = index+1
            # Keep the cafes in question
            L = Loc.Used[index,] = c(ALLOTHERS,blenz,prado,continental)
            # make the distance matrix between the places in use
            Dur.Use = DurMatrix[L,L]
            # Solve the TSP
            out[[index]]  = solve_TSP(ATSP(Dur.Use),method = "two_opt",control=list(repetitions=100,tour=out[[index-1]]))
            #Define the order
            tour[index,] = L[as.integer(TOUR(out[[index]]))]
            # Get the time
            time[index] = tour_length(out[[index]])
        }
    }
}
# First let's see if the two methods agreed.
# If so they should all fall along the diagonal line, which of course they don't
par(mfrow=c(3,1))
plot(time[seq(1,3*72,by=3)],time[seq(2,3*72,by=3)],ylab="2-opt refinement",xlab="nearest_insertion")
abline(0,1)
plot(time[seq(1,3*72,by=3)],time[seq(3,3*72,by=3)],ylab="repetitive_nn",xlab="nearest_insertion")
abline(0,1)
plot(time[seq(3,3*72,by=3)],time[seq(2,3*72,by=3)],ylab="2-opt refinement",xlab="repetitive_nn")
abline(0,1)


#### Now look at the tour times

best = which(time==min(time))
Loc.Used[best,]
time[best]/60
tour[best,]
cafenames[tour[best,]]
cafes[tour[best,]]

### Compare with teh duration for the given permutation based on the raw duration data:


time[1]
Loc.Used[1,]
L = tour[1,]
cafes[tour[1,]]
cafes[Loc.Used[1,]]
temp = rep(0,10)

temp[1]=DurMatrix[L[1],L[2]]
print(paste(rownames(DurMatrix)[L[1]],colnames(DurMatrix)[L[2]]))
for(lp in 2:(length(L)-1)){
    temp[lp] = DurMatrix[L[lp],L[lp+1]]
    print(paste(rownames(DurMatrix)[L[lp]],colnames(DurMatrix)[L[lp+1]]))
}
temp
sum(temp)/60



#### Note that they don't coincide with anything from the data durations... :^(
# There seems to be a problem in the way that library(TSP) works.
