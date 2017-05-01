
function plotEM_MoG(x,lambdak,muk,Sk,rk)

% x:        datos
% lambda_k: pesos de las gaussianas
% mu_k:     medias
% S_k:      matrices de covarianza
% r_k:      responsabilidades
%
% © Juan Gabriel Serra Pérez

if size(x,2)>2
    error('La función solo es válida para datos 2D.')
end

cd = [0 0 1;1 0 0];
colors = lines(length(lambdak));
cla
hold on
for i=1:size(x,1)
    plot(x(i,1),x(i,2),'+','Color',rk(i,:)*cd)
end

for k=1:size(Sk,3)
    [v,d]=eig(Sk(:,:,k));
    t=(-1:0.01:1)*d(1); y=d(4)*sqrt(1-(t/d(1)).^2);
    tx=[t fliplr(t); y fliplr(-y)]; TX=v*tx+muk(:,k)*ones(1,2*length(t));
    plot(TX(1,:),TX(2,:),'Color',colors(k,:),'lineWidth',3*lambdak(k))
end
hold off
grid on