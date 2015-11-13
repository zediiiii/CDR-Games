

####################
# function:   	countcds()
# purpose:		Generate and count all possible gamestates and moves for CDS games with l through
#               r elements
#               Only returns reasonable values for n>3 - takes several minutes for values of n>10
# parameters:	l:Starting number of elements inclusive - must be <=r  (natural number),
#               r:Ending number of elements inclusive - must be >=l (natural number),
#               pretty:Default TRUE - if false, returns no text in output
#               timed:Default TRUE - Count execution time
#               countgamestates:Default TRUE - returns number of gamestates for each set of elements
#               Example:countcdr(3,5,pretty=F) returns a named list with the total number of 
#                       states and moves possible for sets of 1 to 3,4,5 along with exectution time.
# Author:       Joshua Watson Oct 2015
# Dependancies: listed at start of code

#install.packages('combinat'); install.packages('R.matlab') #run the first time only to get the package on your computer
library(combinat) 
library(R.matlab)


countcds <- function(l,r,pretty=TRUE,timed=TRUE,countgamestates=TRUE){
    
    if(timed==TRUE){
    #Start the timer if timer is on.
    proc.time()->timer
    }
    
    #Instantiate some variables needed later
    movecount =0         
    countlist <-list()   
    list_out  <-list()
    statelist <-list()
    
    these<-c(l:r) #range of games to analyze, l:r
    for(k in these){
        set   <- c(1:k) #the particular set to analyze for this iteration in the loop
                        #We need to note that 1-5 is the same as 2-6 for the sake of the game
                        #and it's functions. The two sets provide the same game.
    
        base  <- permn(set)     #get the permutations and call the list base
        states<-length(base)    #count the list of permutations (number of gamestates)    
    #Count the branches of the gamestate tree
        for(i in 1:length(base)){
            temp <- base[[i]] #make temp the ith element of the permutation list
      
            ############
            #LOGIC for counting possible CDS moves
            #temp is a list for a particular gamestate, and temp[[n]] indicates the 
            #nth element of that list.
            distance<-length(temp)
            for(n in 1:(distance-2)){           #Testloop for pointer to the right of n - last two can't have any right pointers
                for(j in 2:(distance-n)){       #get the list of distances from n to check in right direction - the last two won't have pointer to the right of them
                    if(temp[[n]]==(temp[n+j]-1)){ #check if swappable pointers exist for each number at least two to the right of n
                        movecount<-movecount+1      #if so, add one to the counter. 
                    }
                }  
            } #End the positive test loop
            for(m in 3:distance){                  # Testloop for pointers to the left of m - first two can't have any left pointers
                for(p in 2:(m-1)){               #get the list of distances to m to check in left direction
              
                    if(temp[[m]]==(temp[(m-p)]-1)){ #check if swappable pointers exist for each number at least two to the left of m
                        movecount<-movecount+1       #if so, add one to the counter. 
                    }
                }
            }
        } # end of loop through all permutations for n
        
    countlist[[length(countlist)+1]] <- movecount #add movecount for this iteration to a list
    
    ###Following handles the T/F arguments for the function
        
        if(countgamestates==TRUE){
            statelist[[length(statelist)+1]] <- states #add states count for this iteration to a list
        } 
    
    } # end of loop through all permutations for l:r
    
    if(pretty==TRUE){
        names(countlist)<-paste("For ",c(l:r)," elements.","") #rename each iteration with words
        names(statelist)<-paste("For ",c(l:r)," elements.","") #rename state list with words
    }
        else{
            names(countlist)<-c(l:r) #rename each iteration with only numbers
            if(countgamestates==TRUE){
                names(statelist)<-c(l:r)
            }
        }
    
    if(timed==TRUE){
    time<-(proc.time()-timer)
    }
    
    returnme<-list("Total Possible States"=statelist, "Total Possible Moves" = countlist,"Execution Time" = time)    
    if(timed==FALSE){
        returnme <- returnme[-3] #Take out this element from this list if not requested
    }
    if(countgamestates==FALSE){
        returnme <- returnme[-1] #Take out this element from this list if not requested
    }
    returnme
}