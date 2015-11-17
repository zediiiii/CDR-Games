####################
# function:     cdrmove()
# purpose:        
# parameters:    l:Starting number of elements inclusive - must be <=r  (natural number),
#               Example:countcdr(3,5,pretty=F) returns a named list with the total number of 
#                       states and moves possible for sets of 1 to 3,4,5 along with exectution time.
# Author:       Joshua Watson & Alyssa Seidman 2015
# Dependancies: combinat

cdrmove <- function(game,pointer){
    
    ##### This block of code finds the index of the block to have CDR performed on it ####
    
    which(unlist(pointer)[1] == game)[[1]]->p1
    which(unlist(pointer)[2] == game)[[1]]->p2
    
    if(abs(p1-p2)==1){ #ie the two numbers are adjacent
        if(p1<p2){ #ie p1 is left of p2
            if(abs(game[p1]>abs(game[p2]))){
                block<-p1
            } else {
                block<-p2
            }
        } else {   #ie p1 is right of p2
            if(abs(game[p1]>abs(game[p2]))){
                block<-p2
            } else {
                block<-p1 
            } #end else            
        } #end else
        
    } else {
        if(p1<p2){  #ie, if p1 is left of p2 not adjacent
            block<-c((p1+1):p2)
        } else {    #ie p1 is right of p2 not adjacent
            block<-c((p2:p1-1))
        } #end else
    } #end else
    
    #### Performs the flip and negation and overwrites block index elements with new neg.block ####
    neg.block<-rev(game[block])*-1
    new.game<-game
    new.game[block]<-neg.block
    new.game
}