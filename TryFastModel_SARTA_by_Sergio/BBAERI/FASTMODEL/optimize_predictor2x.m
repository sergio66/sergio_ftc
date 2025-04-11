function [outputODN,coeffN,thelistN] = optimize_predictor2x(il,inputOD,family)

warning off

clear coeffN outputODN thelistN bestN
coeffN   = ones(1,length(family.names)) * -9999;
thelistN = ones(1,length(family.names)) * -9999;

xinputOD = inputOD(:,il);
goodOD = find(xinputOD >= 0); badOD = find(xinputOD < 0); 

chisqr0 = sum(inputOD(goodOD).*inputOD(goodOD));

if length(goodOD) < 1
  disp('oops all ODs < 0   gotta buzz out')
  outputODN = zeros(size(xinputOD))';
  return
end

xinputOD = max(xinputOD,0);
for ii = 1 : length(family.names)
  x(ii,:) = family.vars(ii,:,il);
end

%% this is FIRST TERM
N = 1;
clear chisqr
for ii = 1 : length(family.names)
  matr = x(ii,:);
  coeff = matr(goodOD)'\xinputOD(goodOD);
  newOD = matr * coeff;  newOD = max(newOD,0); newOD(badOD) = 0;
  chisqr(ii) = real(nansum((xinputOD(goodOD)' - newOD(goodOD)).^2));
end
zz = find(chisqr == min(chisqr)); bestN(N) = zz(1);  chisqrN(N) = chisqr(bestN(N));
semilogy(1:length(chisqr),chisqr,bestN(N),chisqr(bestN(N)),'ro'); 
outputODN = newOD;
coeffN(N)   = coeff;
thelistN(N) = bestN; 

%% this is SECOND TERM
clear chisqr
N = N + 1;
matrN = x(bestN,:);
iaList = 1 : length(family.names); iaList = setdiff(iaList,bestN);
for ii = 1 : length(iaList)
  jj = iaList(ii);
  matr = [matrN; x(jj,:)];
  coeff = matr(:,goodOD)'\xinputOD(goodOD);
  newOD = matr' * coeff;  newOD = max(newOD,0); newOD(badOD) = 0;
  chisqr(ii) = real(nansum((xinputOD(goodOD) - newOD(goodOD)).^2));
end
zz = find(chisqr == min(chisqr)); bestN(N) = iaList(zz(1));  chisqrN(N) = chisqr(zz(1));
semilogy(1:length(chisqr),chisqr,zz(1),chisqrN(N),'ro'); 
outputODN = newOD;
thelistN(1:N) = bestN; 
coeffN(1:N)   = coeff;

fprintf(1,'N = %3i  chisqr0 = %8.6e chisqrN = %8.6e \n',N,chisqr0,chisqrN(N));

iContinue = +1;
while ((chisqrN(N-1)+eps)/(chisqrN(N)+eps) > 1.01) & ((chisqrN(N-1)+eps)/(chisqr0+eps) > 1e-4) & ...
       N <= 9 & iContinue > 0
  %% this is Nth TERM
  clear chisqr
  N = N + 1;
  matrN = x(bestN,:);
  iaList = 1 : length(family.names); iaList = setdiff(iaList,bestN);
  for ii = 1 : length(iaList)
    jj = iaList(ii);
    matr = [matrN; x(jj,:)];
    coeff = matr(:,goodOD)'\xinputOD(goodOD);
    newOD = matr' * coeff;  newOD = max(newOD,0);  newOD(badOD) = 0;
    chisqr(ii) = real(nansum((xinputOD(goodOD) - newOD(goodOD)).^2));
  end
  zz = find(chisqr == min(chisqr)); 
  if chisqr(zz(1)) < chisqrN(N-1)   %% yes things have improved
    bestN(N) = iaList(zz(1));  chisqrN(N) = chisqr(zz(1));
    plot(1:length(chisqr),chisqr,zz(1),chisqrN(N),'ro'); 
    thelistN(1:N) = bestN; 
    coeffN(1:N) = coeff;
    outputODN = newOD;
    fprintf(1,'N = %3i  chisqr0 = %8.6e chisqrN = %8.6e x2(N)/x2(N-1) = %8.6f \n',N,chisqr0,chisqrN(N),chisqrN(N-1)/chisqr(zz(1)));
  else
    disp('no improvement! stoppping!');
    iContinue = -1;
    break
  end
end

clear coeff newOD matr chisqr
%% this is simplest stuff
matr = [x(1,:); x(2,:); x(3,:); x(4,:); x(5,:)]; %% const stemp ptemp gas1 gas3
coeff = matr(:,goodOD)'\xinputOD(goodOD);
newOD = matr' * coeff;  newOD = max(newOD,0);  newOD(badOD) = 0;
chisqr = real(nansum((xinputOD(goodOD) - newOD(goodOD)).^2));

bwah = 1:length(xinputOD);
plot(bwah(goodOD),outputODN(goodOD)-xinputOD(goodOD),'bo-',...
     bwah(goodOD),newOD(goodOD)-xinputOD(goodOD),'g',...
     bwah(goodOD),xinputOD(goodOD),'r')


warning on

