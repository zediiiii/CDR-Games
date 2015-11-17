####################
# function:     cdrpointers()
# purpose:    	Finds the pointers of a particular CDR gamestate and outputs a list of pointer vectors
#               Note that the code works so that the second number is always the negative number.
# parameters:	game: a cdr gamestate in vector form, ie, c(1,2,3,4,5)
#               Example: cdrpointers(c(-2,1,3,-4,5))
# Author:       Joshua Watson & Alyssa Seidman 2015
# Dependancies: none

cdrpointers <- function(gamestate){
    poscount<-0
    pointerlist<-list()
    
    for(i in 1:length(gamestate)){  #Check for all positive or all negative
        if(gamestate[i]>0){
            poscount<-(poscount+1)
        }
    }
    if((poscount==length(gamestate)) || (poscount==0)){
        print("No pointers.")
        break
    }
    
    for(i in 1:length(gamestate)){
        pointer<-gamestate[i]
        if(pointer>0){
            for(j in 1:length(gamestate)){
                pointercheck<-gamestate[j]
                if((pointercheck==-(pointer+1)) | (pointercheck==-(pointer-1))){
                    pointerlist[[length(pointerlist)+1]] <- c(gamestate[i],gamestate[j])
                }
            }
        }
    }
    pointerlist
}