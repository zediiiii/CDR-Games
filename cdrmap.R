####################
# function:     cdrmap()
# purpose:      Generate a complete map for a CDR Game given an initial gamestate. 
# parameters:	gamestate:starting gamestate in the form c(1,2,3)
#               Example:cdrmap(c(1,2,3))
# Author:       Joshua Watson & Alyssa Seidman 2015
# Dependancies: 

cdrmap <- function(gamestate){
    statelist<-list()
    namelist <-list()
    k=0
    length<-length(gamestate)
    index<-cdrindex(gamestate)
    pointers<-cdrpointers(gamestate)
    if(length(pointers)<1){
        print("No pointers.")
        break
    }
    
        statelist[[(length(statelist)+1)]] <- gamestate   # add the first gamestate to the list of gamestates
            namelist[[(length(namelist)+1)]]   <-Node$new(expression('R'[index]^length))

                                            # add the expression for the 
                                            # first gamestate node to the list of node name commands
    
        for(i in 1:length(pointers)){
            nextstate<-cdrmove(gamestate,pointers[i]) #get the next gamestate using the ith pointer
            tempindex<-cdrindex(nextstate)
            
            namelist[[length(namelist)+1]]   <- paste("b",k,"i","<-a",k,"$AddChild([expression('R'[tempindex]^length))",sep="") #add name to list of node name commands
            statelist[[length(statelist)+1]] <- nextstate                               #add state to state list
            
            length(1:i)<-temp   #get length of how many gamestates were just added
        }
    
        for(i in (length(statelist)-temp):length(statelist)){ #index the newly added statelist elements for next iteration
        
        #This begins a loop over gamestates in the second generation
        pointers<-cdrpointers(statelist[i])
        tempindex<-cdrindex(statelist[i])
        nextstate<-statelist(i)    
            if(length(pointers)<1){
            print("Done")
            break
            }
        
            #this block is a repeat of lines 27-35 except the naming for nesting
        
            for(j in 1:length(pointers)){  #iterate over the pointers for each gamestate of 2nd gen
            nextstate<-cdrmove(nextstate,pointers[j]) #get the next gamestate using the ith pointer
            tempindex<-cdrindex(nextstate)
            
            namelist[[length(namelist)+1]]   <- paste("c",k,"i","<-a",k,"$AddChild([expression('R'[tempindex]^length))",sep="") #add name to list of node name commands
            statelist[[length(statelist)+1]] <- nextstate                               #add state to state list
            
            length(1:i)<-temp   #get length of how many gamestates were just added
            
            length(1:i)<-temp   #get length of how many states were just added
            }
        
        nextstate<-cdrmove(gamestate,pointers[i])
        
        namelist[[length(namelist)+1]]   <- statelist$AddChild([expression('R'[tempindex]^length))
        
        nextstate<-cdrmove(statelist[i],cdrpointers(gamestate))
        tempindex<-cdrindex(nextstate)
        
        namelist[[length(namelist)+1]]   <- statelist$AddChild([expression('R'[tempindex]^length))
        statelist[[length(statelist)+1]] <- nextstate
        
        length(1:i)<-temp
        
        
        statelist[i]
        
    }  
    
}