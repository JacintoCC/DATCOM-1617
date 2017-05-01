%% Implementaci�n del algoritmo EM para MoG (mezcla de gaussianas)
%
%  El objetivo de esta pr�ctica es implementar el algoritmo de Maximizaci�n
% de la Esperanza (EM) para una mezcla de dos Gaussianas.
%
%  Deber�s completar el c�digo necesario para estimar
% - lambda1 y lambda2  (pesos que suman uno)
% - mu1 y mu2          (medias de las dos Gaussianas)
% - Sigma1, Sigma2     (covarianzas de las dos Gaussianas)
%
% � Juan Gabriel Serra P�rez

%% Comenzaremos limpiando el espacio de trabajo y cerrando todas las figuras
clear
clc
close all


%% PASO 1 - Generaci�n del conjunto de muestras
% En este bloque vamos a generar los datos. No debes modificar nada.
% Cuando hayas implementado el algoritmo EM te recomendamos que cambies los
% pesos, medias y covarianzas y veas como se comporta el algoritmo.

rng(1)

%  Vamos a suponer que nuestros datos provienen de una mezcla de 2
% gaussianas 2D.

% medias
mu1 = [5;9];
mu2 = [-3;0];

%  matrices de covarianza
S1 = [1 0;0 9];
S2 = [5 -3;-3 5]/4;

%  pesos de las gaussianas
lambda1 = 0.3;
lambda2 = 1-lambda1;

%  n�mero de muestras a generar
N = 200;

%  Observa esta forma, un poco tramposa, de generar los datos y comp�rala
% con la que hemos visto en clase

N1 = lambda1*N;
N2 = lambda2*N;

%  generaci�n aleatoria de muestras
x1 = mvnrnd(mu1,S1,N1);
x2 = mvnrnd(mu2,S2,N2);


x = [x1;x2];

%  El algoritmo EM solo tendr� acceso a los datos, es decir a x.
%  La matriz x tendr� tama�o Nx2; las N1 primeras filas provienen de la
% primera Gaussiana y las N2=N-N1 de la segunda. Observa que al algoritmo
% EM no le decimos qu� Gaussiana ha generado cada muestra.


%% Paso 2- Dibujar los valores observados con su clase responsable

%  calculamos la responsabilidades de cada clase en cada una de las
% observaciones. Obviamente este dato no se lo daremos al algoritmo EM

rik = [ones(N1,1) zeros(N1,1);zeros(N2,1) ones(N2,1)];

%  Dibujamos los datos. Es muy importante que entiendas el funcionamiento de
% la funci�n plotEM_MoG y el formato de sus par�metros de entrada

figure(1), clf
subplot(3,6,2), plotEM_MoG(x,[lambda1 lambda2],[mu1 mu2],cat(3,S1,S2),rik)
title('Distribuci�n original')


%% PASO 3 - Definimos par�metros de convergencia del algoritmo

%  par�metros del algoritmo

%  n�mero m�ximo de iteraciones
maxiter = 1000; 

%  tolerancia (diferencia m�xima permitida entre los 
% par�metros de una iteraci�n y la siguiente)
tol = 1e-6;    


%% Paso 4 - Inicializaci�n de las medias y covarianzas para el algoritmo EM

%  Estas inicializaciones no debes modificarlas. Cuando hayas implementado
% el algoritmo EM ser�a bueno que las cambiases y vieses si mejora o
% empeora su convergencia.                            

% Inicializamos las medias
mu = [-1 -3;-2 -1];    %mu contiene la primera media en la primera columna,
% seguida de la segunda en la segunda columna
                                                               
% Inicializamos las matrices de covarianza                  
% Observa que Sigma(:,:,1) contiene la covarianza de la primera Gaussiana
% Observa que Sigma(:,:,2) contiene la covarianza de la primera Gaussiana
% Ambas se han inicializado a la identidad

Sigma = cat(3,eye(2),eye(2));    %                              
                                                               
%  Inicializamos los pesos                                   
                                                               
lambda = [0.5 0.5];                                            
                                                               
%  Observa que no hemos proporcionado informaci�n sobre la responsabilidad

%  Vamos a dibujar ahora los datos de partida (la inicializaci�n).
% Aprovecharemos la misma funci�n para dibujar, pero dado que no conocemos
% las responsabilidades en este punto (se calculan en el algoritmo EM),
% les daremos a todas un valor 0.5. Este valor no tiene un significado real
% en este punto m�s all� de expresar la incertidumbre sobre la
% responsabilidad de cada gaussiana sobre cada una de las muestras.
subplot(3,6,1), plotEM_MoG(x,lambda,mu,Sigma,0.5*ones(N,2))
title('Inicializaci�n')


%% Paso 5 - Dibujo de la estimaci�n inicial de la mezcla de Gaussianas                                                              
 
% Nota: Lo que tienes que implementar ser� m�s f�cil si entiendes como funciona
% la funci�n mvnpdf

tx = -10:0.05:10;
ty = -5:0.05:20;
[xx,yy] = meshgrid(tx,ty);
X = [xx(:) yy(:)];
zz = lambda(1)*mvnpdf(X,mu(:,1)',Sigma(:,:,1))+lambda(2)*mvnpdf(X,mu(:,2)',Sigma(:,:,2));
zz = reshape(zz,size(xx));
subplot(3,6,4), surf(tx,ty,zz), colormap hot, shading interp, hold on
title('fdp estimada inicial')


%% Paso 6 - Corte de la fdp estimada (sobre la gr�fica anterior)

lx = -7:0.05:10;
ly = lx+2; % ecuaci�n del plano (y = x + 2)
lz = lambda(1)*mvnpdf([lx' ly'],mu(:,1)',Sigma(:,:,1))+lambda(2)*mvnpdf([lx' ly'],mu(:,2)',Sigma(:,:,2));
% esto dibuja el corte en 3D sobre la fdp estimada completa
plot3(lx,ly,lz','r'), view(0,90)

%% Paso 7 - Dibujo del corte de la fdp estimada vs fdp original

% ahora dibujamos solo el corte en 2D para una m�s f�cil comparaci�n visual
% con la fdp original (en el mismo corte, obviamente)
likelihood = lambda1*mvnpdf([lx' ly'],mu1',S1)+lambda2*mvnpdf([lx' ly'],mu2',S2);
subplot(3,6,5), plot(lx,likelihood), hold on, plot(lx,lz,'r'), grid
xlabel('y = x+2'), xlim([lx(1) lx(end)])
legend({'original','estimada'}), title('estimada vs original (corte)')


%% Paso 8 - Algoritmo EM

pause
i=0;
lik = zeros(N,1);
arg  = zeros(1,maxiter+1);
for k=1:2
    lik = lik + lambda(k)*mvnpdf(x,mu(:,k)',Sigma(:,:,k));
end
arg(1) = sum(log(lik));
incr = 1+tol;
while incr>tol && i<maxiter
    disp(num2str(i))
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Paso E actualizar las responsabilidades
    % INCLUIR EL C�DIGO PARA LA ACTUALIZACI�N DE LAS RESPONSABILIDADES

    samples =  [mvnpdf(x, mu(:,1)', Sigma(:,:,1)) mvnpdf(x, mu(:,2)', Sigma(:,:,2))];

    rik = lambda .* samples ./ repmat(sum(lambda .* samples,2), 1, 2);
    
     
    % FIN DE C�DIGO A INCLUIR PARA ACTUALIZAR LAS RESPONSABILIDADES
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    subplot(3,6,2*mod(i,6)+7), plotEM_MoG(x,lambda,mu,Sigma,rik)
    title(['Paso E, i = ' num2str(i+1)])
    pause
    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    % Paso M actualizar los pesos, medias y covarianzas de las 
    % distribuciones
    % INCLUIR C�DIGO PARA ACTUALIZAR PESOS, MEDIAS Y MATRICES DE COVARIANZA
                                                             
    % Actualizo los pesos                                      
    lambda = sum(rik) / sum(sum(rik));
                                                               
    % Actualizo las medias                                     

    for k=1:2
      mu(:,k) = sum(rik(:,k) .* x) ./ sum(rik(:,k));
    end
    
    % Actualizo las matrices de covarianza                     

    for k=1:2
      Sigma(:,:,k) = zeros(2,2);
      for j=1:N
	Sigma(:,:,k) = Sigma(:,:,k) + rik(j,k) * ((x(j,:)' - mu(:,k)) * (x(j,:) - mu(:,k)'));
      end 
      Sigma(:,:,k) = Sigma(:,:,k) / sum(rik(:,k));
    end

    % FIN DE C�DIGO A INCLUIR PARA ACTUALIZAR PESOS, MEDIAS Y 
    % RESPONSABILIDADES
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    % Dibujamos los resultados obtenidos
    
    subplot(3,6,2*mod(i,6)+8), plotEM_MoG(x,lambda,mu,Sigma,rik)
    title(['Paso M, i = ' num2str(i+1)])
    
    zz = lambda(1)*mvnpdf(X,mu(:,1)',Sigma(:,:,1))+lambda(2)*mvnpdf(X,mu(:,2)',Sigma(:,:,2));
    zz = reshape(zz,size(xx));
    subplot(3,6,4), cla, surf(tx,ty,zz), colormap hot, shading interp, hold on
    title('fdp estimada')
    lz = lambda(1)*mvnpdf([lx' ly'],mu(:,1)',Sigma(:,:,1))+lambda(2)*mvnpdf([lx' ly'],mu(:,2)',Sigma(:,:,2));
    plot3(lx,ly,lz','r'), view(0,90)
    
    subplot(3,6,5), hold off, plot(lx,likelihood), hold on, plot(lx,lz,'r'), grid on
    xlabel('y = x+2'), xlim([lx(1) lx(end)])
    legend({'original','estimada'}), title('estimada vs original (corte)')
  
    
    % incrementamos contador de iteraci�n y comprobamos la convergencia
    
    %  El criterio sobre la convergencia debe ser la convergencia de la
    % funci�n, es decir, la log verosimilitud.
    
    i = i+1;
    lik = 0*lik;
    for k=1:2                                                  
        lik = lik + lambda(k)*mvnpdf(x,mu(:,k)',Sigma(:,:,k));  
    end
    arg(i+1) = sum(log(lik));
    incr = (arg(i+1)-arg(i))^2/(arg(i))^2;
    
    subplot(3,6,6), plot(arg(1:i+1))
    title('log verosimilitud'), grid on
    pause
end

%% Paso 9 - Dibujo del resultado final


subplot(3,6,3), plotEM_MoG(x,lambda,mu,Sigma,rik)
title(['Resultado final, i = ' num2str(i)])



%% Paso 10 - Mostramos los resultados obtenidos



disp('Lambdas:')
disp([' - originales: ' num2str(lambda1) ', ' num2str(lambda2)])
disp([' - estimadas:  ' num2str(lambda(1)) ', ' num2str(lambda(2))])
disp(' ')
disp('Medias:')
disp([' - originales: mu1 = [' num2str(mu1') '], mu2 = [' num2str(mu2') ']'])
disp([' - estimadas : mu1 = [' num2str(mu(:,1)') '], mu2 = [' num2str(mu(:,2)') ']'])
disp(' ')
disp('Matrices de covarianza:')
disp(' - originales:')
S1
S2
disp(' - estimadas:')
Sigma(:,:,1)
Sigma(:,:,2)


%% Paso 11 - Si has implementado correctamente los pasos .....

%  El algoritmo habr� convergido tras 6 iteraciones y los resultados del
% paso anterior deben ser
%
%
% lambda = [0.3 0.7]
% mu1 = [4.8927 9.0947]', mu2 = [-2.9413 -0.039482]
% Sigma1 =    [ 0.9910   -0.0561
%              -0.0561    8.3043]
% Sigma2 =      1.1973   -0.7562
%              -0.7562    1.1391]

% Si cambias la inicializaci�n, f�cilmente podr�a darse el caso de que los
% resultados salgan cambiados de orden




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
