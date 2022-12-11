# Tarea tweets

require(here)
library(igraph)
library(readr)
library(readxl)
library(tidyverse)
library(triangle)


# Primero cargamos la base de datos

db <- read_csv("Tweets filtrados para cargar en R.csv")
red <- graph_from_data_frame(db, directed = T)
count_components(red, mode = c("weak", "strong"))


# esto calcula la densidad de los enlases
edge_density(red)

# vamos a calcular la distnacia media entre sujetos
mean_distance(red) # esta no me sirve

# diámetro de la red
diameter(red, directed = T) 

# plot de la red general
plot(red, vertex.label=NA,
     edge.arrow.size=.5)

#acá de plotea la red general, de acuerdo al tamaño de los degree
plot(red, vertex.label=NA, vertex.size=degree(red)*0.2,
     edge.arrow.size=.5)

# Parámetros

centr_degree(red)
centr_eigen(red)
E(red)
V(red)
degree(red) #todos los grados de la red
degree(red, mode="in") # solo grados in
degree(red, mode="out") #solo grados out

#centralidad de la red
centr_degree(
  red,
  mode = c("all", "out", "in", "total"),
  loops = TRUE,
  normalized = TRUE
)

#histogramas de la red
hist(degree(red), breaks = 100)
hist(degree(red, mode = "in"), breaks = 100)
hist(degree(red, mode = "out"), breaks = 100)

#no se que hace esto
table(sapply(decompose(red), vcount))
table(sapply(decompose(red), diameter))

#distancia
distancia <- round(mean_distance(red),3) 
distancia


# acá sacamos la cantidad de triadas
triad.census(red)


#lo que viene es para sacar el componente principal de la red, y luego
# plotearlo
componentes <- clusters(red)
componentes
g <- which.max(componentes$csize) # identificamos el gigante
red1 <- induced.subgraph(red, which(componentes$membership == g)) # nos quedamos con el componente gigante

#plot de la neuva red

plot(red1, vertex.label=NA,
     edge.arrow.size=.5)


#plot de la nueva red de componentes principales, segíun degree
plot(red1, vertex.label=NA, vertex.size=degree(red)*0.2,
     edge.arrow.size=.5)

