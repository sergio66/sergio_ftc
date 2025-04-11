clear coeffN outputODN thelistN bestN

xinputOD = inputOD(:,il);
for ii = 1 : length(family.names)
  x(ii,:) = family.vars(ii,:,il);
end

%% this is FIRST TERM
N = 1;
for ii = 1 : length(family.names)
  matr = x(ii,:);
  coeff = matr'\xinputOD;
  newOD = matr * coeff;
  chisqr(ii) = real(sum((xinputOD' - newOD).^2));
end
zz = find(chisqr == min(chisqr)); bestN(N) = zz(1);  chisqrN(N) = chisqr(bestN(N));
semilogy(1:length(chisqr),chisqr,bestN(N),chisqr(bestN(N)),'ro'); 
outputODN = newOD;
coeffN   = coeff;
thelistN = bestN; 

%% this is SECOND TERM
clear chisqr
N = N + 1;
matrN = x(bestN,:);
iaList = 1 : length(family.names); iaList = setdiff(iaList,bestN);
for ii = 1 : length(iaList)
  jj = iaList(ii);
  matr = [matrN; x(jj,:)];
  coeff = matr'\xinputOD;
  newOD = matr' * coeff;
  chisqr(ii) = real(sum((xinputOD - newOD).^2));
end
zz = find(chisqr == min(chisqr)); bestN(N) = iaList(zz(1));  chisqrN(N) = chisqr(zz(1));
semilogy(1:length(chisqr),chisqr,zz(1),chisqrN(N),'ro'); 
thelistN = bestN; 
coeffN = coeff;
outputODN = newOD;

whos thelistN coeffN outputODN
fprintf(1,'N = %3i  chisqrN = %8.6f \n',N,chisqrN(N));

iContinue = +1;
while (chisqrN(N) < chisqrN(N-1)) & N <= 9 & iContinue > 0
  %% this is Nth TERM
  clear chisqr
  N = N + 1;
  matrN = x(bestN,:);
  iaList = 1 : length(family.names); iaList = setdiff(iaList,bestN);
  for ii = 1 : length(iaList)
    jj = iaList(ii);
    matr = [matrN; x(jj,:)];
    coeff = matr'\xinputOD;
    newOD = matr' * coeff;
    chisqr(ii) = real(sum((xinputOD - newOD).^2));
  end
  zz = find(chisqr == min(chisqr)); 
  if chisqr(zz(1)) < chisqrN(N-1)   %% yes things have improved
    bestN(N) = iaList(zz(1));  chisqrN(N) = chisqr(zz(1));
    plot(1:length(chisqr),chisqr,zz(1),chisqrN(N),'ro'); 
    thelistN = bestN; 
    coeffN = coeff;
    outputODN = newOD;
    fprintf(1,'N = %3i  chisqrN = %8.6f \n',N,chisqrN(N));
  else
    disp('no improvement! stoppping!');
    iContinue = -1;
    break
  end
end

clear coeff newOD matr chisqr
%% this is simplest stuff
matr = [x(1,:); x(2,:); x(3,:); x(4,:); x(5,:)]; %% const stemp ptemp gas1 gas3
coeff = matr'\xinputOD;
newOD = matr' * coeff;
chisqr = real(sum((xinputOD - newOD).^2));

plot(1:length(xinputOD),outputODN-xinputOD,1:length(xinputOD),newOD-xinputOD,...
     1:length(xinputOD),xinputOD)
