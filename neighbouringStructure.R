
nb.district <- poly2nb(BD.gen, queen=TRUE, row.names=as.character(BD.gen[["NAME_2"]]))
neiCT <- nb2WB(nb.district)
nb2INLA("BD.graph", nb.district)
nb2INLA("BD.txt", nb.district)
inla.geobugs2inla(neiCT$adj,neiCT$num,graph.file = "BD.graph")
H <- inla.read.graph(filename="BD.txt")
image(inla.graph2matrix(H),xlab="",ylab="")

