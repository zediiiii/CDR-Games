####################
# function:     sort.listss()
# purpose:		Sorts a list of lists in ascending order.
# parameters:	x:A list of lists. This function
# Author:       Joshua Watson Oct 2015

sort.listss <- function(list){
    list->list.of.lists    
    list.of.lists[order(sapply(list.of.lists,'[[',1))]

}