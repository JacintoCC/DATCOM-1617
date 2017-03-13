// Bayesian Network
//   Elvira format 

bnet  "Entrega1" { 

// Network Properties

kindofgraph = "directed";
visualprecision = "0.00";
version = 1.0;
default node states = (presente , ausente);

// Variables 

node Terreno(finite-states) {
title = "Terreno";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =417;
pos_y =181;
relevance = 7.0;
purpose = "";
num-states = 3;
states = ("Duro" "Arcilloso" "Arenoso");
}

node Plaga(finite-states) {
title = "Plaga";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =755;
pos_y =406;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("si" "no");
}

node Abono(finite-states) {
title = "Abono";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =468;
pos_y =378;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("si" "no");
}

node Clima(finite-states) {
title = "Clima";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =145;
pos_y =229;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("positivo" "negativo");
}

node Variedad(finite-states) {
title = "Variedad";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =612;
pos_y =182;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("Variedad A" "Variedad B");
}

node Plaguicida(finite-states) {
title = "Plaguicida";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =924;
pos_y =178;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("si" "no");
}

node Cosecha(finite-states) {
title = "Cosecha";
kind-of-node = chance;
type-of-variable = finite-states;
pos_x =556;
pos_y =629;
relevance = 7.0;
purpose = "";
num-states = 2;
states = ("positivo" "negativo");
}

// Links of the associated graph:

link Abono Cosecha;

link Clima Abono;

link Clima Cosecha;

link Plaga Cosecha;

link Plaguicida Plaga;

link Terreno Abono;

link Variedad Cosecha;

link Variedad Plaga;

//Network Relationships: 

relation Terreno { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.3 0.6 0.1 );
}

relation Clima { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.7 0.3 );
}

relation Variedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.7 0.3 );
}

relation Plaguicida { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.7 0.3 );
}

relation Abono Clima Terreno { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.65 0.4 0.5 0.85 0.6 0.7 0.35 0.6 0.5 0.15 0.4 0.3 );
}

relation Plaga Plaguicida Variedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.2 0.3 0.6 0.9 0.8 0.7 0.4 0.1 );
}

relation Cosecha Abono Clima Plaga Variedad { 
comment = "";
kind-of-relation = potential;
deterministic=false;
values= table (0.4 0.5 0.95 0.9 0.2 0.25 0.35 0.4 0.35 0.25 0.8 0.7 0.05 0.07 0.3 0.2 0.6 0.5 0.05 0.1 0.8 0.75 0.65 0.6 0.65 0.75 0.2 0.3 0.95 0.93 0.7 0.8 );
}

}
