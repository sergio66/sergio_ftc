function [C,coeff,thelist] = optimize_predictor2(il,waterOD,stemp,ptemp,gas_1,gas_3)

warning off
thelist = 1 : 5;

xwaterOD = waterOD(:,il);
xtemp    = ptemp(:,il);
xgas1    = gas_1(:,il)/1e20;
xgas3    = gas_3(:,il)/1e20;
xones    = ones(size(xgas1));
xstemp   = stemp';

matr = [xones xstemp xtemp xgas1 xgas3];
coeff1 = matr\xwaterOD;
coeff2 = inv(matr' * matr) * matr' * xwaterOD;

newOD1 = matr * coeff1;
newOD2 = matr * coeff2;

array = [il*ones(1,5)' coeff1 coeff2 coeff1./coeff2];
fprintf(1,'%3i %8.6e %8.6e %8.6e \n',array');

plot(1:length(xwaterOD),newOD1-xwaterOD,1:length(xwaterOD),newOD2-xwaterOD,...
     1:length(xwaterOD),xwaterOD)

C     = newOD2;
coeff = coeff2;

warning on