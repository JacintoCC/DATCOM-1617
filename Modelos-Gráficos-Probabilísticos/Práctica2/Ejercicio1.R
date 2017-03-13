# Modelos gráficos probabilísticos
# Ejercicio 1
#
# Jacinto Carrasco Castillo



##
# Construcción de una red bayesiana.
##

require(bnlearn)

## Creación de la red 1
dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
modelstring(dag) <- "[A][S][E|A:S][O|E][R|E][T|O:R]"

## Creación de la red 3
dag2 <- empty.graph(nodes = nodes(dag))
arc.set <- matrix(c("A","E",
                    "S","E",
                    "E","O",
                    "E","R",
                    "O","T",
                    "R","T"),
                  byrow = T, ncol = 2,
                  dimnames = list(NULL, c("from", "to")))
arcs(dag2) <- arc.set

## Creación de la red 3
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")

## Visualización de la red
plot(dag)
graphviz.plot(dag)



## Propiedades de la red

A.st <- c("young","adult","old")
S.st <- c("M","F")
E.st <- c("high", "uni")
O.st <- c("emp", "self")
R.st <- c("small", "big")
T.st <- c("car", "train","other")

## Especificación de probabilidades

A.prob <- array(c(0.30, 0.5, 0.2), dim = 3,
                dimnames = list(A=A.st))

S.prob <- array(c(0.6, 0.4), dim=2,
                dimnames = list(S=S.st))

O.prob <- array(c(0.96,0.04, 0.92, 0.08), dim = c(2,2),
                dimnames = list(O = O.st, E=E.st))

R.prob <- array(c(0.25,0.75,0.2,0.8), dim = c(2,2),
                dimnames = list(R = R.st, E=E.st))

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2),
                dimnames = list(E = E.st, A = A.st, S = S.st))

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2),
                dimnames = list(T = T.st, O = O.st, R = R.st))

# Construcción de red bayesiana
cpt <- list(A=A.prob, S=S.prob, E=E.prob, O=O.prob,R=R.prob, T=T.prob)
bn <- custom.fit(dag,cpt)
bn



###
# Estimación de los parámetros
# Distribuciones de probabilidad condicional
###

survey <- read.table("survey.txt", header = T)

bn.mle <- bn.fit(dag, data=survey, method = "mle")
bn.bayes <- bn.fit(dag,data = survey, method = "bayes",iss=10)


# Estimación del DAG

learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")

learned2 <- hc(survey, score = "bde")
modelstring(learned2)
score(learned, data = survey, type = "bde")

###
#     Inferencia en redes bayesianas
###

dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")

path(dag, from = "S", to = "R")
dsep(dag, x = "S", y = "R", z = "E")

dsep(dag, x = "S", y = "T", z = c("O","R"))
dsep(dag, x = "O", y = "R", z = c("E"))

dsep(dag, x="A", y="S")
dsep(dag, x="A", y="S", z="E")


# Consultas de probabilidad a posteriori 

library(gRain)
junction <- compile(as.grain(bn))

# Consulta 1. P(R|S=F, T = "other")
querygrain(junction, nodes="R")$R
jres <- setEvidence(junction, node="S",states="F")
querygrain(jres, nodes="R")$R
jres <- setEvidence(jres, node="T",states="other")
querygrain(jres,nodes="R")$R


cpquery(bn, event = (R == "small"),
        evidence = (S == "F" & T == "other"))
cpquery(bn, event = (R == "small"),
        evidence = list(S = "F", T= "other"),
        method = "lw")


dsep(bn, x = "R", y="T", z="S")

# Vemos cómo la probabilidad de vivir en una ciudad pequeña disminuye si 
# el usuario es una mujer y el medio de transporte es uno distinto al coche o el tren.
# Como la probabilidad ha variado una vez que hemos incluido el medio de transporte, 
# no se cumple que el tamaño de la ciudad sea independiente del género dado el 
# medio de transporte.


# Consulta 2. P(O|E="high", R = "small")
querygrain(junction, nodes="O")$O
jocc <- setEvidence(junction, node="E",states="high")
querygrain(jocc,nodes="O")$O
jocc <- setEvidence(jocc, node="R",states="small")
querygrain(jocc,nodes="O")$O

cpquery(bn, event = (O == "self"),
        evidence = (E == "high" & R == "small"))
cpquery(bn, event = (O == "self"),
        evidence = list(E = "high", R = "small"),
        method = "lw")

dsep(bn, x = "O", y="R", z="E")

# Vemos como la probabilidad de que sea un empleado aumenta dado que la educación es 
# superior, pero que esta no varía una vez que incluimos que vive en una pequeña ciudad
# lo que, como hemos comprobado, significa que la ocupación es independiente del tamaño 
# de la ciudad dado el nivel de educación.

#  Consulta 3. P(O|R="small", T="car")

querygrain(junction, nodes="O")$O
jocc <- setEvidence(junction, node="R",states="small")
querygrain(jocc,nodes="O")$O
jocc <- setEvidence(jocc, node="T",states="car")
querygrain(jocc,nodes="O")$O

cpquery(bn, event = (O == "self"),
        evidence = (T == "car" & R == "small"))
cpquery(bn, event = (O == "self"),
        evidence = list(T = "car", R = "small"),
        method = "lw")
dsep(bn, x = "O", y="T", z="R")

# Vemos como la probabilidad de que sea un empleado disminuye ligeramente si usa coche y 
# vive en una ciudad pequeña. Al no cumplirse P(O|R) = P(O|R,T), no existe la relación 
# de independencia entre O y T dado R. 


