rm(list=ls())
require(here)
library(igraph)
aqui <- here()

################################################################################
# Una red se puede generar de varias formas.
################################################################################
# 1a. Juntando un conjunto de links representados como números o conceptos

red1 <- graph_from_literal(1-2,
                          2-3,
                          3-4,
                          4-5,
                          1-5,
                          2-3)
plot(red1)

red2 <- graph_from_literal(Alfa-Beta, # en este caso toma los nombres como factores
                          Gamma-Delta,
                          Delta-Alfa,
                          Omega-Beta)
plot(red2)

red3 <- graph_from_literal(Alfa-+Beta, # aquí es digida
                           Gamma+-Delta,
                           Delta++Alfa,
                           Omega-+Beta)
plot(red3)

# 1b se puede combinar un edgelist con atributos

relaciones <- matrix(c("Alfa","Beta",  #edlist
                 "Gamma","Delta",
                 "Delta","Alfa",
                 "Omega","Beta"), ncol=2, byrow = T)
atributos <- matrix(c("Alfa","Beta","Gamma","Delta","Omega", #atributos
                    "4","4","5","5","5", # letras
                    "2","1","2","1","1"), # veces que tienen la letra "a"
                    ncol=3, nrow = 5, byrow=F)
red4 <- graph_from_data_frame(relaciones, 
                              vertices=atributos,
                              directed = F)
plot(red4)

# 1c se puede importar en diversos formatos
x <- read.csv(paste0(aqui,"/red_ilustracion.csv"), sep=";")
relaciones5 <- cbind(x$nombre,x$empresa)
red5 <- graph_from_data_frame(relaciones5, 
                              directed = T)
plot(red5)

# 1d importar la red desde algun formato de red
red6 <- read_graph(paste0(aqui,"/red_empresas.igraph"))
plot(red6)

################################################################################
# Caracteristicas del objeto igraph
################################################################################

red2$name <- "Grafo en clase"

# Vertices
V(red2)

# Edges
E(red2)

# ambos datos
print_all(red2) # o simplemente
red2

# atributos de los vertices y edges
V(red2)$name # ya estaban 

E(red2)$name # no hay
E(red2)$name <- c("a","b","c","d") 

V(red2)$color <- c("blue","red","red","red","pink")

################################################################################
# Operaciones con el objeto igraph
################################################################################

class(red2)
is_weighted(red2)
is_simple(red2)

# Se puede trabajar con subgrafos de la red

red2_a <- induced_subgraph(red2, 1:3) # los nodos según el orden dado
print_all(red2_a)

red2_b <- red2 - vertices(c(1)) # se lee, red2_b es la red2 menos los vértices indicados

# Se pueden agregar nodos y edges
red2_c <- red2 + vertices(c("Epsilon","Eta"))
plot(red2_c)
red2_d <- red2_c + edges(c("Epsilon","Gamma"),
                         c("Eta","Beta"),
                         c("Alfa","Epsilon"))
plot(red2_d)

# o unir redes
red2_e <- red2 + red2_c
plot(red2_e)

# podemos crear una red con pesos a partir de la anterior
red2_f <- red2
E(red2_f)$weight <- c(0.1,0.2,1,1.3)
is_weighted(red2_f)
plot(red2_f)

################################################################################
# visualizando la red
################################################################################

# ahora una red un poco mas grande 
nodos <- seq(1:18)
links <- matrix(c(1,12,3,14,5,6,17,8,9,1,10,4,
                  12,13,14,15,16,17,18,9,11,10,14,7,
                  7,7,17,6,18,14,2,5,2,8,5,8,8,5,
                  4,4,11,11),nrow=21,ncol=2)
red2 <- graph_from_data_frame(d=links, vertices=nodos, directed=T) 
plot(red2)
red2 <- simplify(red2, remove.multiple = F, remove.loops = T) 
plot(red2)

# se puede cambiar el tamaño de la flecha, de las etiquetas, Cambiar colores, etc
plot(red2, edge.arrow.size=.2,vertex.label.cex=0.5)
plot(red2, edge.arrow.size=.2,vertex.label=NA)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red")
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",vertex.frame.color="yellow")
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",
     vertex.frame.color="yellow",vertex.shape="square")
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",
     vertex.frame.color="yellow",vertex.shape="square", vertex.size=50)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",
     vertex.frame.color="yellow",vertex.shape="square", vertex.size=5,edge.curved=.2)

# cambiando los tamaños de los nodos de a uno
size_nodo <- c(55,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,5)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo)

b <- degree(red2)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color="red",
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=b*10)

# misma lógica para cambiar colores, por ejemplo
color_nodo <- c("black","red","red","red","red","red","red","red","red","red",
                "red","red","red","red","red","red","red","yellow")
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo)

# cada vector puede a su vez ser una funcion de algo. Por ejemplo de grado.
size_nodo <- degree(red2)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo*10)

##############
# Layouts

plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo*10,
     layout=layout_randomly)

l.circulo <- layout_in_circle(red2) # podemos hacer c?lculos previos y luego ponerlos en el plot
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo*10,
     layout=l.circulo)

# Fruchterman-Reingold 
l.fr <- layout_with_fr(red2)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo*10,
     layout=l.fr)

# Kamada Kawai
l.kk <- layout_with_kk(red2)
plot(red2, edge.arrow.size=.2,vertex.label=NA, vertex.color=color_nodo,
     vertex.frame.color="black",vertex.shape="circle", edge.curved=.2,
     edge.color="green", vertex.size=size_nodo*10,
     layout=l.kk)

layouts <- grep("^layout_", ls("package:igraph"), value=TRUE)[-1] 
layouts

# Ver http://kateto.net/network-visualization para diferentes variaciones para la visualizacion

################################################################################
# redes bimodales
################################################################################

# redes bipartitas
red_b <- graph_from_literal(Alfa-LET, 
                            Perro-ANI,
                            Beta-LET,
                            Gato-ANI,
                            Gamma-LET)
# asignemos categorias a cada nodo
V(red_b)$type <- c(FALSE,TRUE,FALSE,TRUE,FALSE,FALSE,FALSE)
print_all(red_b, v=T)

red_b_proyecciones <- bipartite_projection(red_b) 

red_b_proyecciones$proj1  # proyeccion en nombres
red_b_proyecciones$proj2  # proyección en paises

plot(red_b_proyecciones$proj1)
plot(red_b_proyecciones$proj2)

################################################################################
# Grupos especiales de redes
################################################################################

# red completa
red_completa <- make_full_graph(10, directed = F)
plot(red_completa)

# red estrella
red_estrella <- make_star(10, mode="undirected")
plot(red_estrella)

# redes regulares
red_anillo <- make_ring(10, directed = F)
plot(red_anillo)

red_regular <- sample_k_regular(10, k=5, directed = FALSE, multiple = FALSE)
plot(red_regular)

# redes árboles 
red_arbol <- make_tree(10, mode = "out")
plot(red_arbol, edge.arrow.size=)


################################################################################
# Propiedades de la red
################################################################################

# genera la matriz de adyacencia
as_adjacency_matrix(red2)

# analiza la ego network del nodo especificado
neighbors(red2,2)
neighbors(red2,1)

# grado de la red
degree(red2)
degree(red2, mode="in")
degree(red2, mode="out")
hist(degree(red2))
# cuando es una red con pesos se usa strength(_nombrered_)

# caminos y conectividad en la red
# completa o incompleta
is_connected(red2)
decompose(red2) # muestra los componentes
table(sapply(decompose(red2), vcount)) # tabla resumen de los componentes

# una medida de conectividad interesante es saber si hay puntos vulnerables que
# de no existir se rompe la red:

articulation_points(red2)

# para ver lo mismo en una red con más nodos, usemos el paquete
# igraphdata que contiene una colección de grafos

require(igraphdata)
data(package="igraphdata")

data(USairports)
d.aeropuertos <- degree(USairports)
hist(d.aeropuertos, col="blue")
# por la rapidez con que decae la distribución es mejor usar log-log
dist.aeropuertos <- degree_distribution(USairports)
d <- 1:max(d.aeropuertos) - 1 # generamos una secuencia que la usaremos en el gráfico
val <- (dist.aeropuertos != 0) # creamos un vector lógico de valores con grado !=0
plot(d[val],dist.aeropuertos[val], 
     log="xy", 
     col="red", 
     ylab="log f()", 
     xlab="log k",
     pch = 19)

# cliques 
# Estos algoritmos son lentos porque son censos de toda la red karate
data(karate)
plot(karate)
# así que lo haremos con la red2
# te entrega una tabla resumen de la cantidad de cliques de tamaño 1, 2, 3,...
table(sapply(cliques(karate), length)) 

# tambien se puede buscar el clique más grande, denominado "the clique number"
clique_num(karate)

# k-cores
cores <- coreness(karate) # entrega el grado máximo del core en que puede estar cada nodo

require(sna)
require(intergraph)
karate.sna <- intergraph::asNetwork(karate)
sna::gplot.target(karate.sna,cores,circ.lab=F,
                  circ.col="skyblue",usearrows=F,
                  vertex.col=cores) # mientras más al medio + alto el k-core
detach("package:sna") # porque tiene funciones con nombres iguales que igraph

# dyads
dyad_census(karate)

# triads --> motifs
triad_census(karate)  # usamos karate porque es una red dirigida (i.e. más variedad)
# para ver los códigos de cada motif https://igraph.org/r/doc/triad_census.html

transitivity(karate) # clustering de la red (# tríadas/ # potencial tríadas)
reciprocity(karate) # como es una red no dirigida va a ser 1 siempre
reciprocity(red2)

# clusters
clusters(karate)

diameter(karate, weights=NA)
diameter(USairports, weights=NA)


################################################################################
################################################################################
################################################################################
# red de los medici
################################################################################
################################################################################
################################################################################

library(network) # network, sna, igraph
data(flo) # es una matriz de adyacencia
medici <- igraph::graph_from_adjacency_matrix(flo,mode = "undirected")

##############
# el objeto igraph
# la clase igraph entrega info basica 
medici 
E(medici)
V(medici)
plot(medici)
as_edgelist(medici, names=T)
as_adjacency_matrix(medici)

plot(medici)

# distribucion de grado
grado.dist <- degree_distribution(medici)
grado.tabla <- matrix(c(seq(0:6),100*grado.dist),byrow=F,ncol=2)
grado.tabla <- as.data.frame(grado.tabla)
colnames(grado.tabla) <- c("Grado","Porcentaje")
grado.tabla
plot(grado.tabla$Grado,grado.tabla$Porcentaje)
hist(grado.dist)

t <- paste("Histograma - red Medici - ",length(V(medici))," nodos")
plot(density(grado.dist), main = t, xlim=c(0,0.5))

# componente gigante
componentes <- clusters(medici)
componentes
g <- which.max(componentes$csize) # identificamos el gigante
red <- induced.subgraph(medici, which(componentes$membership == g)) # nos quedamos con el componente gigante
V(medici)
  V(red)

# densidad
edge_density(red)

# distancia media
distancia <- round(mean_distance(red),3) 
distancia

# geodesicas
m <- as_adjacency_matrix(red, sparse = F)
class(m) 
t(m)%*%m # matrix de dos grados
distance_table(red)
shortest_paths(red,"Peruzzi","Pazzi")
all_shortest_paths(red,"Peruzzi")

# conteo de triadas
triad_census(red) # entrega un censo según clasificación: https://igraph.org/r/doc/triad_census.html

# centralidad de grado
centralidad_grado <- degree(red)
centralidad_grado

# centralidad closeness
centralidad_cercania <- closeness(red,normalized=T)
centralidad_cercania

# centralidad eigenvalue
centralidad_eigen <- eigen_centrality(red)
centralidad_eigen$vector

# centralidad betweenness
centralidad_intermediacion <- betweenness(red, normalized = T)
centralidad_intermediacion

centralidades <- cbind(centralidad_grado,
                       centralidad_cercania,
                       centralidad_eigen$vector,
                       centralidad_intermediacion) 
colnames(centralidades) <- c("grado", "cercania", "eigen", "intermediacion")
centralidades

# centralization
centr_degree(red)
centr_clo(red)
centr_eigen(red)
centr_betw(red)


