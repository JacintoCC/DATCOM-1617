# Trabajo opcional.
# Jacinto Carrasco Castillo


##
# Construcción de una red bayesiana.
##

require(bnlearn)

# T: Terreno
# A: Abono
# C: Cosecha
# P: Plaga
# V: Variedad
# W: Clima
# X: Plaguicida

dag <- model2network("[T][V][W][X][A|T:W][P|V:X][C|V:W:A:P]")
graphviz.plot(dag)

# Descripción de la red

T.st <- c( "Duro", "Arcilloso", "Arenoso")
V.st <- c("Variedad A", "Variedad B")
W.st <- c("Positivo", "Negativo") 
X.st <- c("Sí", "No")
A.st <- c("Sí", "No")
P.st <- c("Sí", "No")
C.st <- c("Sí", "No")

## Especificación de probabilidades


T.prob <- array(c(0.3, 0.6, 0.1), dim = 3,
                dimnames = list(T=T.st))
V.prob <- array(c(0.6, 0.4), dim = 2,
                dimnames = list(V=V.st))
W.prob <- array(c(0.7, 0.3), dim = 2,
                dimnames = list(W=W.st))
X.prob <- array(c(0.8, 0.2), dim = 2,
                dimnames = list(X=X.st))
A.prob <- array(c(0.65,0.35,0.4,0.6,
                  0.5,0.5,0.85,0.15,
                  0.6,0.4,0.7,0.3), dim = c(2,3,2),
                dimnames = list(A = A.st, T = T.st, W = W.st))
P.prob <- array(c(0.2, 0.8, 0.3, 0.7,
                  0.6, 0.4 ,0.9, 0.1), dim = c(2,2,2),
                dimnames = list(P = P.st, V = V.st, X = X.st))
C.prob <- array(c(0.4,0.6,0.5,0.5,
                  0.2,0.8,0.25,0.75,
                  0.35,0.65,0.25,0.75,
                  0.05,0.95,0.07,0.93,
                  0.95,0.05,0.9,0.1,
                  0.35,0.65,0.4,0.6,
                  0.80,0.2,0.7,0.3,
                  0.3,0.7,0.2,0.8), dim = c(2,2,2,2,2),
                dimnames = list(C = C.st, V = V.st, W = W.st, A = A.st, P = P.st))

# Construcción de red bayesiana
cpt <- list(T=T.prob, V=V.prob, W=W.prob, X=X.prob, A=A.prob, P=P.prob, C=C.prob)
bn <- custom.fit(dag,cpt)
bn



# Consultas de probabilidad a posteriori 

library(gRain)
junction <- compile(as.grain(bn))

# Consulta 1. P(C|W=Positivo, X = "Sí")
querygrain(junction, nodes="C")$C
jcos <- setEvidence(junction, node="W",states="Positivo")
querygrain(jcos, nodes="C")$C
jcos <- setEvidence(jcos, node="X",states="Sí")
querygrain(jcos, nodes="C")$C


cpquery(bn, event = (C == "Sí"),
        evidence = (W == "Positivo" & X == "Sí"))
cpquery(bn, event = (C == "Sí"),
        evidence = list(W = "Positivo", X= "Sí"),
        method = "lw")


dsep(bn, x = "C", y="X", z="W")


# Consulta 2. P(C|A="No", T = "Arcilloso")
querygrain(junction, nodes="C")$C
jcos <- setEvidence(junction, node="A",states="No")
querygrain(jcos, nodes="C")$C
jcos <- setEvidence(jcos, node="T",states="Arcilloso")
querygrain(jcos, nodes="C")$C


cpquery(bn, event = (C == "Sí"),
        evidence = (T == "Arcilloso" & A == "No"))
cpquery(bn, event = (C == "Sí"),
        evidence = list(A = "No", T= "Arcilloso"),
        method = "lw")


dsep(bn, x = "C", y="A", z="T")

#  Consulta 3. P(A|C="No", X="Sí")

querygrain(junction, nodes="A")$A
jab <- setEvidence(junction, node="C",states="No")
querygrain(jab, nodes="A")$A
jab <- setEvidence(jab, node="X",states="Sí")
querygrain(jab, nodes="A")$A


cpquery(bn, event = (A == "Sí"),
        evidence = (C == "No" & X == "Sí"))
cpquery(bn, event = (A == "Sí"),
        evidence = list(C = "No", X= "Sí"),
        method = "lw")


dsep(bn, x = "A", y="X", z="C")
