# Práctica 2
# Modelos gráficos probabilísticos
# 
# Jacinto Carrasco Castillo

require(bnlearn)

dag <- empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))

dag

dag <- set.arc(dag, from = "A", to = "E")
dag <- set.arc(dag, from = "S", to = "E")

dag <- set.arc(dag, from = "E", to = "O")
dag <- set.arc(dag, from = "E", to = "R")

dag <- set.arc(dag, from = "R", to = "T")
dag <- set.arc(dag, from = "O", to = "T")

dag

# Otra manera de establecer los arcos
modelstring(dag)
modelstring(dag) <- "[A][S][E|A:S][O|E][R|E][T|O:R]"

# Otra manera de definir una red
dag3 <- model2network("[A][S][E|A:S][O|E][R|E][T|O:R]")
all.equal(dag, dag3)

# Obtención de la estructura de la red
nodes(dag)
arcs(dag)

# Forma alternativa para arcos
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

all.equal(dag,dag2)

try(set.arc(dag, from = "T", to = "E"))

# Sección 4

plot(dag)
graphviz.plot(dag)

# Sección 5: Especificación de las propiedades

A.st <- c("young","adult","old")
S.st <- c("M","F")
E.st <- c("high", "uni")
O.st <- c("emp", "self")
R.st <- c("small", "big")
T.st <- c("car", "train","other")

A.prob <- array(c(0.30, 0.5, 0.2), dim = 3,
                dimnames = list(A=A.st))
A.prob

S.prob <- array(c(0.6, 0.4), dim=2,
                dimnames = list(S=S.st))
S.prob

O.prob <- array(c(0.96,0.04, 0.92, 0.08), dim = c(2,2),
                dimnames = list(O = O.st, E=E.st))
O.prob


R.prob <- array(c(0.25,0.75,0.2,0.8), dim = c(2,2),
                dimnames = list(R = R.st, E=E.st))
R.prob

E.prob <- array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                  0.36, 0.70, 0.30, 0.90, 0.10), dim=c(2, 3, 2),
                dimnames = list(E = E.st, A = A.st, S = S.st))

T.prob <- array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                  0.24, 0.18, 0.70, 0.21, 0.09), dim=c(3, 2, 2),
                dimnames = list(T = T.st, O = O.st, R = R.st))

# Sección 6  - Creación de la red bayesiana

cpt <- list(A=A.prob, S=S.prob, E=E.prob, O=O.prob,R=R.prob, T=T.prob)
bn <- custom.fit(dag,cpt)

nparams(bn)
arcs(bn)

bn$R

R.cpt <- coef(bn$R)
bn

# Sección 7 - Estimación de los parámetros: Distribuciones de probabilidad condicional

survey <- read.table("survey.txt", header = T)
head(survey)

bn.mle <- bn.fit(dag, data=survey, method = "mle")
prop.table(table(survey[,c("O","E")]), margin = 2)

bn.mle$O

bn.bayes <- bn.fit(dag,data = survey, method = "bayes",iss=10)
bn.bayes$O

# Sección 8 

learned <- hc(survey)
modelstring(learned)
score(learned, data = survey, type = "bic")

learned2 <- hc(survey, score = "bde")
modelstring(learned2)
score(learned, data = survey, type = "bde")

# Sección 9

dsep(dag, x = "S", y = "R")
dsep(dag, x = "O", y = "R")

path(dag, from = "S", to = "R")
dsep(dag, x = "S", y = "R", z = "E")

dsep(dag, x = "S", y = "T", z = c("O","R"))
dsep(dag, x = "O", y = "R", z = c("E"))

dsep(dag, x="A", y="S")
dsep(dag, x="A", y="S", z="E")

library(gRain)
junction <- compile(as.grain(bn))
querygrain(junction, nodes="T")$T
jsex <- setEvidence(junction, node="S",states="F")
querygrain(jsex,nodes="T")$T

jres <- setEvidence(junction,nodes="R",states="small")
querygrain(jres,nodes = "T")$T

jedu <- setEvidence(junction, nodes = "E", states = "high")
SxT.cpt <- querygrain(jedu, nodes = c("S","T"),type = "joint")
SxT.cpt

querygrain(jedu, nodes = c("S", "T"), type = "marginal")
querygrain(jedu, nodes = c("S", "T"), type = "conditional")

dsep(bn, x="S", y="T",z="E")

## Inferencia aproximada

cpquery(bn, event = (S== "M") & (T=="car"),
        evidence = (E=="high"))
cpquery(bn, event = (S== "M") & (T=="car"),
        evidence = (E=="high"), n = 10^6)

cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = list("E" = "high"), method = "lw")

cpquery(bn, event = (S == "M") & (T == "car"),
        evidence = ((A == "young") & (E == "uni")) | (A == "adult"))
SxT <- cpdist(bn, nodes = c("S","T"),
              evidence = (E == "high"))
head(SxT)
prop.table(table(SxT))

