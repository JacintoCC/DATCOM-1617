%% Machine Learning Online Class
%  Reducción de la Dimensionalidad| PCA 
%
%  Instrucciones
%  ------------
%
%  Recuerda que debes cambiar ApellidosNombre en el nombre del fichero 
%  a tus apellidos y tu nombre.

%  Este fichero contiene código que te ayudará a completar la práctica.
%  Necesitarás completar las funciones (cambiando ApellidosNombre por tus 
%  Apellidos y Nombre):
%
%     ApellidosNombrepca.m
%     ApellidosNombreprojectData.m
%     ApellidosNombrerecoverData.m
%     
%
%  En las llamadas en este fichero .m a ApellidosNombrepca.m,
%  ApellidosNombreprojectData.m, ApellidosNombrerecoverData.m deberás 
%  cambiar ApellidosNombre por tus Apellidos y Nombre
%

%% Initialization
clear ; close all; clc

%% ================== Section 1: Load Example Dataset  ===================
%  We start this exercise by using a small dataset that is easily to
%  visualize
%
fprintf('Visualizing example dataset for PCA.\n\n');

%  The following command loads the dataset. You should now have the 
%  variable X in your environment
load ('ERRDdata1.mat');

% TEN CUIDAO, OBSERVA QUE CADA EJEMPLO ESTÁ ALMACENADO COMO UNA FILA
% Y NO COMO UNA COLUMNA COMO HABIAMOS UTILIZADO EN CLASE

%  Visualize the example dataset
plot(X(:, 1), X(:, 2), 'bo');
axis([0.5 6.5 2 8]); axis square;


%% =============== Section 2: Principal Component Analysis ===============
%  You should now implement PCA, a dimension reduction technique. You
%  should complete the code in ApellidosNombrepca.m
%
fprintf('\nRunning PCA on example dataset.\n\n');

%  Before running PCA, it is important to first normalize X
[X_norm, mu, sigma] = featureNormalize(X);

%  Run PCA
[U, S] = CarrascoCastilloJacintopca(X_norm);

%  Compute mu, the mean of the each feature

%  Draw the eigenvectors centered at mean of data. These lines show the
%  directions of maximum variations in the dataset.
hold on;
drawLine(mu, mu + 1.5 * S(1,1) * U(:,1)', '-k', 'LineWidth', 2);
drawLine(mu, mu + 1.5 * S(2,2) * U(:,2)', '-k', 'LineWidth', 2);
hold off;

fprintf('Top eigenvector: \n');
fprintf(' U(:,1) = %f %f \n', U(1,1), U(2,1));
fprintf('\n(you should expect to see -0.707107 -0.707107)\n');




%% =================== Section 3: Dimension Reduction ===================
%  You should now implement the projection step to map the data onto the 
%  first k eigenvectors. The code will then plot the data in this reduced 
%  dimensional space.  This will show you what the data looks like when 
%  using only the corresponding eigenvectors to reconstruct it.
%

%
fprintf('\nDimension reduction on example dataset.\n\n');

%  Plot the normalized dataset (returned from pca)
plot(X_norm(:, 1), X_norm(:, 2), 'bo');
axis([-4 3 -4 3]); axis square

%  You should complete the code in ApellidosNombreprojectData.m
%  Project the data onto K = 1 dimension
K = 1;
Z = CarrascoCastilloJacintoprojectData(X_norm, U, K);
fprintf('Projection of the first example: %f\n', Z(1));
fprintf('\n(this value should be about 1.481274)\n\n');

%  You should complete the code in ApellidosNombrerecoverData.m
%  Project the data onto K = 1 dimension

X_rec  = CarrascoCastilloJacintorecoverData(Z, U, K);
fprintf('Approximation of the first example: %f %f\n', X_rec(1, 1), X_rec(1, 2));
fprintf('\n(this value should be about  -1.047419 -1.047419)\n\n');

%  Draw lines connecting the projected points to the original points
hold on;
plot(X_rec(:, 1), X_rec(:, 2), 'ro');
for i = 1:size(X_norm, 1)
    drawLine(X_norm(i,:), X_rec(i,:), '--k', 'LineWidth', 1);
end
hold off


%% =============== Sección 4: Loading and Visualizing Face Data =============
%  We start the exercise by first loading and visualizing the dataset.
%  The following code will load the dataset into your environment
%
fprintf('\nLoading face dataset.\n\n');

%  Load Face dataset
load ('ERRDfaces.mat')

%  Display the first 100 faces in the dataset
displayData(X(1:100, :));


%% =========== Section 5: PCA on Face Data: Eigenfaces  ===================
%  Run PCA and visualize the eigenvectors which are in this case eigenfaces
%  We display the first 36 eigenfaces.
%
fprintf(['\nRunning PCA on face dataset.\n' ...
         '(this mght take a minute or two ...)\n\n']);

%  Before running PCA, it is important to first normalize X by subtracting 
%  the mean value from each feature
[X_norm, mu, sigma] = featureNormalize(X);

%  Run PCA
[U, S] = CarrascoCastilloJacintopca(X_norm);

%  Visualize the top 36 eigenvectors found
displayData(U(:, 1:36)');




%% ============= Part 6: Dimension Reduction for Faces =================
%  Project images to the eigen space using the top k eigenvectors 
%  If you are applying a machine learning algorithm 
fprintf('\nDimension reduction for face dataset.\n\n');

K = 100;
Z = CarrascoCastilloJacintoprojectData(X_norm, U, K);

fprintf('The projected data Z has a size of: (%i,%i) \n', size(Z)(1),size(Z)(2));



%% ==== Part 7: Visualization of Faces after PCA Dimension Reduction ====
%  Project images to the eigen space using the top K eigen vectors and 
%  visualize only using those K dimensions
%  Compare to the original input, which is also displayed

fprintf('\nVisualizing the projected (reduced dimension) faces.\n\n');

K = 100;
X_rec  = CarrascoCastilloJacintorecoverData(Z, U, K);

% Display normalized data
subplot(1, 2, 1);
displayData(X_norm(1:100,:));
title('Caras Originales');
axis square;

% Display reconstructed data from only k eigenfaces
subplot(1, 2, 2);
displayData(X_rec(1:100,:));
title('Caras aproximadas');
axis square;
