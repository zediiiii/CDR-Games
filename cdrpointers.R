####################
# function:     cdrpointers()
# purpose:    	Finds the pointers of a particular CDR gamestate and outputs a list of pointer vectors
#               Note that the code works so that the second number is always the negative number.
# parameters:	game: a cdr gamestate in vector form, ie, c(1,2,3,4,5)
#               Example: cdrpointers(c(-2,1,3,-4,5))
# Author:       Joshua Watson & Alyssa Seidman 2015
# Dependancies: none

cdrpointers <- function(game){
    poscount<-0
    pointerlist<-list()
    
    for(i in 1:length(game)){  #Check for all positive or all negative
        if(game[i]>0){
            poscount<-(poscount+1)
        }
    }
    if((poscount==length(game)) || (poscount==0)){
        print("No pointers.")
    }
    
    for(i in 1:length(game)){
        pointer<-game[i]
        if(pointer>0){
            for(j in 1:length(game)){
                pointercheck<-game[j]
                if((pointercheck==-(pointer+1)) | (pointercheck==-(pointer-1))){
                    pointerlist[[length(pointerlist)+1]] <- c(game[i],game[j])
                }
            }
        }
    }
    pointerlist
}