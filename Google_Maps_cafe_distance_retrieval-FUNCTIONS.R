#
# Last modified Dec 30, 2015
# Dave Campbell
#
# CHANGELOG:
#
#
#
#Functions in this file:
#
# Distances = obtains the distances and duration of travel between the origins and destinations
# Destinations = Obtains the names of the destinations 
# TourDist = obtains the total distance for a given route



########################################################
##############  Destinations
########################################################

Destinations <-  function(node, name="destination_address"){
  # Extracts the destination addresses from the googlemaps output.
  # This gives a vector of addresses that can be used to name columns (or rows) 
  # in the distance matrix
  # If you already have the list of locations you don't need to use this function
  #
  ########
  # Last update: Dec 30, 2015, Dave Campbell
  # 
  # This navigates the XML output from a call to the google maps API
  # and extracts the addresses in teh destinations
  #
  # Use this function to: 
  # Given the google maps output, obtain the set of destinations that were used
  #
  # CALLED BY / WITHIN:
  # The Script: Google_Maps_cafe_distance_retrieval.R
  #==============================================================
  #                         REQUIRED INPUTS                      
  #==============================================================
  # node = xmlTreeParse(page[[lp]], getDTD = F)[[1]]
  #        This is the main body of the xml output.  Troubleshoot 
  #        by making sure checking that names(node) actually contains
  #        the appropriate target elements
  # name="destination_address" #should be generally left at the 
  #       default but you could alternatively use "origin_address"
  #
  #
  #===============================================================%
  #                           OUTPUTS                             %
  #===============================================================%
  # just a vector of addresses.
  #===============================================================%
  #                          MODEL DETAILS                        %
  #===============================================================%
  # 
  # This code is specific to googlemaps api
  #===============================================================%
  datavec = node$children[names(node)==name]
  addys = list()
  for(klp in 1:length(datavec)){
    addys[klp] = unlist(datavec[klp])[3]
    #note that the third element is specific to extracting
    # the address element from the googlemaps output.
  }
  return(unlist(addys))
}




########################################################
##############  Distances
########################################################

Distances <-  function(node, name="row"){
  # Extracts the Distances from the googlemaps output.
  # This gives a vector of distances that can be used to name columns (or rows) 
  # in the distance matrix
  
  ########
  # Last update: Dec 30, 2015, Dave Campbell
  # 
  # This navigates the XML output from a call to the google maps API
  # and extracts the duration of travel and distance
  #
  # Use this function to: 
  # Given the google maps output, obtain the set of distances and duration of travel
  #
  # CALLED BY / WITHIN:
  # The Script: Google_Maps_cafe_distance_retrieval.R
  #==============================================================
  #                         REQUIRED INPUTS                      
  #==============================================================
  # node = xmlTreeParse(page[[lp]], getDTD = F)[[1]]
  #        This is the main body of the xml output.  Troubleshoot 
  #        by making sure checking that names(node) actually contains
  #        the appropriate target elements
  # name="row #should be generally left at the default
  #      This extracts the "row" of data.
  #
  #
  #===============================================================%
  #                           OUTPUTS                             %
  #===============================================================%
  # just a matrix with elements for duration and distance.  
  # These are assumed to be minutes and km
  #===============================================================%
  #                          MODEL DETAILS                        %
  #===============================================================%
  # 
  # This code is specific to googlemaps api
  #===============================================================%
  

  data = node$children[names(node)==name]
  datavec = data[[1]]$children
  # preallocate:
  dur = dist = rep(0,length(datavec))
  
  for(klp in 1:length(datavec)){
    #Extracting numbers from the start of a string:
    #See http://www.endmemo.com/program/R/gsub.php for syntax
    #I'm actually deleting everything that isn't a digit
    dur[klp] = as.numeric(gsub("\\D","",unlist(datavec[klp][[1]]$children["duration"][[1]]$children$text)[3]))
    # Note that the locations of the data entries are specific to digging within the googlemaps api output.  
    number.no.comma = gsub(",",".",unlist(datavec[klp][[1]]$children["distance"][[1]]$children$text)[3])
    dist[klp]  = as.numeric(gsub("[A-z]","",number.no.comma))    
  }
  return(cbind(minutes=dur,km = dist))
}








########################################################
##############  TourDist
########################################################

TourDist <-  function(DistMatrix, tour){
  # Extracts the distance travelled for a given tour
  # The tour can be the travel matrix or a permutation vector
  #
  ########
  # Last update: Dec 30, 2015, Dave Campbell
  # 
  #
  # Use this function to: 
  # Obtain the total distance travelled for the given route
  #
  # CALLED BY / WITHIN:
  # The Script: Google_Maps_cafe_distance_retrieval.R
  #==============================================================
  #                         REQUIRED INPUTS                      
  #==============================================================
  # DistMatrix = The matrix of distances between all locations.  Can be asymetric
  #
  # tour = The permutation (vector) or the travel (matrix)
  #
  #
  #===============================================================%
  #                           OUTPUTS                             %
  #===============================================================%
  # just a scalar of the distance
  #===============================================================%
  #                          MODEL DETAILS                        %
  #===============================================================%
  # 
  # Works for any distance matrix
  #===============================================================%
  if(is.vector(tour)){
    time = DistMatrix[tour[1],tour[2]]
    for(lp in 2:(length(tour)-1)){
      time = time + DistMatrix[tour[lp],tour[lp+1]]
    }
  }else{# it is a travel matrix
    time = sum(sum(DistMatrix * tour))
  }
    return(time)
}





########################################################
##############  Tour2TravelMatrix
########################################################

Tour2TravelMatrix = function(tour){
  # Transforms a tour permutation vector into a travel matrix
  #
  ########
  # Last update: Dec 30, 2015, Dave Campbell
  # 
  #
  # Use this function to: 
  # Convert tour formats
  #
  # CALLED BY / WITHIN:
  # The Script: Google_Maps_cafe_distance_retrieval.R
  #==============================================================
  #                         REQUIRED INPUTS                      
  #==============================================================
  #
  # tour = The permutation vector of locations visited
  #
  #===============================================================%
  #                           OUTPUTS                             %
  #===============================================================%
  # A transition matrix
  #===============================================================%
  #                          MODEL DETAILS                        %
  #===============================================================%
  # 
  # 
  #===============================================================%
  TravelMatrix = matrix(0,length(tour),length(tour))
  TravelMatrix[tour[1],tour[2]] = 1
  for(lp in 2:(length(tour)-1)){
    TravelMatrix[tour[lp],tour[lp+1]] = 1
  }
}








########################################################
##############  TravelMatrix2Tour
########################################################

TravelMatrix2Tour = function(TravelMatrix){
  # Transforms a travel matrix into a tour permutation vector
  #
  ########
  # Last update: Dec 30, 2015, Dave Campbell
  # 
  #
  # Use this function to: 
  # Convert tour formats
  #
  # CALLED BY / WITHIN:
  # The Script: Google_Maps_cafe_distance_retrieval.R
  #==============================================================
  #                         REQUIRED INPUTS                      
  #==============================================================
  #
  # TravelMatrix = The transition travel matrix
  #
  #===============================================================%
  #                           OUTPUTS                             %
  #===============================================================%
  # A permutation vector of locations visited
  #===============================================================%
  #                          MODEL DETAILS                        %
  #===============================================================%
  # 
  # 
  #===============================================================%
  tour = rep(0,dim(TravelMatrix)[1])
  TravelMatrix[tour[1],tour[2]] = 1
  for(lp in 2:(length(tour)-1)){
    TravelMatrix[tour[lp],tour[lp+1]] = 1
  }
}







Indices = which(TravelMatrix==1,arr.ind = TRUE)
SortedPath = rep(0,dim(TravelMatrix)[1])
if(length(intersect(0:4,2:5)) == length(tour)){
  #Symmetric path so it doesn't matter where it starts
  SortedPath[1]=1
  SortedPath[Indices[,"col"]]
  SortedPath[Indices[,"col"]] = Indices[,"row"]
  
}



#TravelMatrix=matrix(c(0,0,0,1,0,0,1,0,1,0,0,0,0,1,0,0),nrow=4)