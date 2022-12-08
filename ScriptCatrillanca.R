#################################
##     Cargar Packages         ##
#################################

rm(list=ls())
require(here)
library(igraph)
library(readr)
library(readxl)
library(tidyverse)
library(triangle)
aqui <- here()


###############################
##############################

#RED
base <- read.csv(paste0(aqui,"/B.csv"), sep=";")
relaciones <- cbind(base$emisor,base$receptor)
red1 <- graph_from_data_frame(relaciones, 
                              directed = T)

plot(red1, edge.arrow.size=.01,vertex.label=NA, vertex.color="red",
     vertex.frame.color="red",vertex.shape="circle", vertex.size=2)

hist(degree(red1), breaks = 400)

summary(degree(red1))

count_components(red1, mode = c("weak", "strong"))
# esto calcula la densidad de los enlases
edge_density(red1)

# diámetro de la red
diameter(red1, directed = T) 

# plot de la red general - sin pesos en los nodos
plot(red1, edge.arrow.size=.01,vertex.label=NA, vertex.color="blue",
     vertex.frame.color="blue",vertex.shape="circle", vertex.size=2)

# plot de la red general - con pesos en los nodos
size_nodo <- degree(red1)
plot(red1, edge.arrow.size=.01,vertex.label=NA, vertex.color="#10B502",
     vertex.frame.color="#10B502",vertex.shape="circle", vertex.size=size_nodo*0.04)


# centralidad promedio
degree_centrality <- mean(degree (red1))

#contar cantidad de nodos y de links
length(E(red1))
length(V(red1))

# para ver si un grafo es dirigido o no
is_directed(red1)
mean(degree(red1)) #todos los grados de la red
mean(degree(red1, mode="in")) # solo grados in
mean(degree(red1, mode="out")) #solo grados out


#centralidad de la red
centr_degree(
  red1,
  mode = c("all", "out", "in", "total"),
  loops = TRUE,
  normalized = TRUE
)

#histogramas de la red
hist(degree(red1), breaks = 100)
hist(degree(red1, mode = "in"), breaks = 100)
hist(degree(red1, mode = "out"), breaks = 100)

# acá sacamos la cantidad de triadas
triad.census(red1)

#lo que viene es para sacar el componente principal de la red, y luego
# plotearlo
componentes <- clusters(red1)
componentes
g <- which.max(componentes$csize) # identificamos el gigante
red2 <- induced.subgraph(red1, which(componentes$membership == g)) # nos quedamos con el componente gigante

#plot de la neuva red

size_nodo <- degree(red2)
plot(red2, edge.arrow.size=.01,vertex.label=NA, vertex.color="red",
     vertex.frame.color="red",vertex.shape="circle", vertex.size=size_nodo*0.04)


