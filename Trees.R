#Scratch for Trees!

#install.packages('data.tree')
#install.packages('igraph')
install.packages('networkD3')
library(data.tree)
library(igraph)
library(networkD3)


#text based
acme <- Node$new("Acme Inc.")
accounting <- acme$AddChild("Accounting")
software <- accounting$AddChild("New Software")
standards <- accounting$AddChild("New Accounting Standards")
research <- acme$AddChild("Research")
newProductLine <- research$AddChild("New Product Line")
newLabs <- research$AddChild("New Labs")
it <- acme$AddChild("IT")
outsource <- it$AddChild("Outsource")
agile <- it$AddChild("Go agile")
goToR <- it$AddChild("Switch to R")

print(acme)


#igraph based

library(igraph)
plot(as.igraph(acme, directed = TRUE, direction = "climb"))


#radial network
useRdf <- read.csv('R15.csv', stringsAsFactors = FALSE)
#define the hierarchy (Session/Room/Speaker)
useRdf$pathString <- paste("useR", useRdf$session, useRdf$room, useRdf$speaker, sep="|")
#convert to Node
useRtree <- as.Node(useRdf, pathDelimiter = "|")

#plot with networkD3
useRtreeList<-list()
useRtreeList <- ToListExplicit(useRtree, unname = TRUE)
radialNetwork( useRtreeList)



#movable one
library(networkD3)
acmeNetwork <- ToDataFrameNetwork(acme, "name")
simpleNetwork(acmeNetwork[-3], fontSize = 12)





#igraph

#Subset the data. If we want to exclude people who are in the network only tangentially 
#(participate in one or two relationships only)
# we can exclude the by subsetting the graph on the basis of the 'degree':

bad.vs<-V(bsk.network)[degree(bsk.network)<3] #identify those vertices part of less than three edges
bsk.network<-delete.vertices(bsk.network, bad.vs) #exclude them from the graph

# Plot the data.Some details about the graph can be specified in advance.
# For example we can separate some vertices (people) by color:

V(bsk.network)$color<-ifelse(V(bsk.network)$name=='CA', 'blue', 'red') #useful for highlighting 
#certain people. Works by matching the name attribute of the vertex to 
#the one specified in the 'ifelse' expression

# We can also color the connecting edges differently depending on the 'grade': 

E(bsk.network)$color<-ifelse(E(bsk.network)$grade==9, "red", "grey")

# or depending on the different specialization ('spec'):

E(bsk.network)$color<-ifelse(E(bsk.network)$spec=='X', "red", ifelse(E(bsk.network)$spec=='Y', "blue", "grey"))

# Note: the example uses nested ifelse expressions which is in general a bad idea but does the job in this case
# Additional attributes like size can be further specified in an analogous manner, either in advance or when the plot function is called:

V(bsk.network)$size<-degree(bsk.network)/10#here the size of the vertices is specified by the degree of the vertex, so that people supervising more have get proportionally bigger dots. Getting the right scale gets some playing around with the parameters of the scale function (from the 'base' package)

# Note that if the same attribute is specified beforehand and inside the function, the former will be overridden.
# And finally the plot itself:
par(mai=c(0,0,1,0))     		#this specifies the size of the margins. the default settings leave too much free space on all sides (if no axes are printed)
plot(bsk.network,				#the graph to be plotted
     layout=layout.fruchterman.reingold,	# the layout method. see the igraph documentation for details
     main='Organizational network example',	#specifies the title
     vertex.label.dist=0.5,			#puts the name labels slightly off the dots
     vertex.frame.color='blue', 		#the color of the border of the dots 
     vertex.label.color='black',		#the color of the name labels
     vertex.label.font=2,			#the font of the name labels
     vertex.label=V(bsk.network)$name,		#specifies the lables of the vertices. in this case the 'name' attribute is used
     vertex.label.cex=1			#specifies the size of the font of the labels. can also be made to vary
)

# Save and export the plot. The plot can be copied as a metafile to the clipboard, or it can be saved as a pdf or png (and other formats).
# For example, we can save it as a png:
png(filename="org_network.png", height=800, width=600) #call the png writer
#run the plot
dev.off() #dont forget to close the device
#And that's the end for now.

#adding superscripts

> plot(1,1, main=expression('title'[6]^2)) #gives super 2 and sub 6'

#using superscripts in trees
nam <- paste("step", j, sep = "")
assign(nam, Node$new(expression('R'[length]^index)))