function zhat = KernelRegressorFastNd(matrOut,matrTrain,zTrain,h)

%% see simple_gkr_demo_code_Nd.m

%% matrTrain should be eg 6x500 where 6=number of preds, 500 = training (a,b,c,d...) points
%% zTrain    should be eg 1x500 where                    500 = training     Z        points
%%
%% matrOut should be eg 6x1 where 6=number of preds, 1 = where we want output

[NumOfPred,  SizeOfTrainingSet]    = size(matrTrain);   %% should be eg 6x500
[xzTrain,    lenzTrain] = size(zTrain);                 %% should be eg 1x500
[oNumOfPred,oSizeOfTrainingSet]    = size(matrOut);     %% should be eg 6x1

if SizeOfTrainingSet ~= lenzTrain
  error('SizeOfTrainingSet ~= lenzTrain')
end
if xzTrain ~= 1
  error('xzTrain ~= 1')
end
if oSizeOfTrainingSet ~= 1
  error('oSizeOfTrainingSet ~= 1')
end  

for ii = 1 : NumOfPred
  str = ['A(ii,:) = matrTrain(ii,:);'];
  eval(str);
  str = ['Xtemp = matrTrain(ii,:);'];
  eval(str);

  str = ['ARepeated(ii,:) = repmat(matrOut(ii),1,SizeOfTrainingSet);'];
  eval(str);
end

KX = 0;
for ii = 1 : NumOfPred
  KX = KX + ((ARepeated(ii,:) - A(ii,:)).^2) / h(ii);
end
KX = exp( -KX);
zhat = sum(KX.*zTrain)/sum(KX);

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
