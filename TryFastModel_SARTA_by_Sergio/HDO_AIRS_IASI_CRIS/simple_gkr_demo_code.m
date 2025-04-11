%% see Amir-MatlabTutorial_look_pg_19_29.pdf

% Number of samples
n = 1000;
% Noise variance
sigma = 1;

% Bandwidth
h = 0.05;
disp('Gauss Bandwidth : large is bad (too much data used in fitting');
disp('                  small is bad (too little data used so fits individual wiggles)');
h = input('Enter Gauss Bandwidth (0.05 is good number) : ');

xSamples = (rand(1,n)-0.5)*8;
xGrid = linspace(-4,4,2000);
ySamples = 2*sin(2*xSamples) + 0.5*cos(5*xSamples) + sigma^2 * randn(1,n);
yTrue = 2*sin(2*xGrid) + 0.5*cos(5*xGrid);

%% slow
tic
% Evaluating the regressor on a set of test points
for m=1:length(xGrid)
  yTest1(m) = KernelRegressorSlow(xGrid(:,m),xSamples,ySamples,h);
end
plot(xSamples,ySamples,'.',xGrid,yTrue,'r',xGrid,yTest1,'g','Linewidth',2);
toc

%% fast
tic
for m=1:length(xGrid)
  yTest2(m) = KernelRegressorFast(xGrid(:,m),xSamples,ySamples,h);
end
plot(xSamples,ySamples,'.',xGrid,yTrue,'r',xGrid,yTest1,'g.',xGrid,yTest2,'k','Linewidth',2);
toc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function yhat = KernelRegressorSlow(xQuery,XTrain,YTrain,h)

SizeOfTrainingSet = size(XTrain,2);
yhat = 0;
Normalizer = 0;

for m=1:SizeOfTrainingSet
  Kxi = exp( -(xQuery - XTrain(:,m))^2/h );
  yhat = yhat + Kxi*YTrain(m);
  Normalizer = Normalizer + Kxi;
end
yhat = yhat/Normalizer;

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function yhat = KernelRegressorFast(xQuery,XTrain0,YTrain0,h)

SizeOfTrainingSet = size(XTrain0,2);

%disp('reducing training size by 1/N')
N = 5;
N = 1;
XTrain = XTrain0(1:N:SizeOfTrainingSet);
YTrain = YTrain0(1:N:SizeOfTrainingSet);
SizeOfTrainingSet = size(XTrain,2);

xQueryRepeated = repmat(xQuery,1,SizeOfTrainingSet);
KX = exp( -((xQueryRepeated - XTrain).^2) / h);
yhat = sum(KX.*YTrain)/sum(KX);

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
