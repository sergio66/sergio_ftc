%% see Amir-MatlabTutorial_look_pg_19_29.pdf

% Number of samples
n = 1000;
% Noise variance
sigma = 1;
sigma = 0.25;
sigma = 0.05;
sigma = 0.0001;

% Bandwidth
h = [0.05 0.05];
disp('Gauss Bandwidth : large is bad (too much data used in fitting');
disp('                  small is bad (too little data used so fits individual wiggles)');
h = input('Enter Gauss Bandwidth ([0.005 0.005] is good number) : ');

xGrid = linspace(-4,4,2000);
yGrid = linspace(-4,4,2000);

nSamplesFac = 4;   %% cool
nSamplesFac = 10;  %% cool
nSamplesFac = 0.5; %% too few points to sample well!

xSamples = (rand(1,nSamplesFac*n)-0.5)*8;
ySamples = (rand(1,nSamplesFac*n)-0.5)*8;
%xSamples = (randn(1,n)-0.5);
%ySamples = (randn(1,n)-0.5);

zSamples = 2*sin(2*xSamples).*cos(4*ySamples) + 0.5*cos(5*xSamples).*(ySamples) + sigma^2 * randn(1,nSamplesFac*n);
zTrue = 2*sin(2*xGrid).*cos(4*yGrid) + 0.5*cos(5*xGrid).*(yGrid);

%zSamples = 2*sin(2*xSamples + 4*ySamples) + 0.5*cos(5*xSamples + ySamples) + sigma^2 * randn(1,nSamplesFac*n);
%zTrue = 2*sin(2*xGrid + 4*yGrid) + 0.5*cos(5*xGrid + yGrid);

%% slow
tic
% Evaluating the regressor on a set of test points
for m=1:length(xGrid)
  zTest1(m) = KernelRegressorSlow(xGrid(:,m),yGrid(:,m),xSamples,ySamples,zSamples,h);
end
plot(xSamples,zSamples,'.',xGrid,zTrue,'r',xGrid,zTest1,'g','Linewidth',2);
toc

%% fast
tic
for m=1:length(xGrid)
  zTest2(m) = KernelRegressorFast(xGrid(:,m),yGrid(:,m),xSamples,ySamples,zSamples,h);
end
plot(xSamples,zSamples,'.',xGrid,zTrue,'r',xGrid,zTest1,'g.',xGrid,zTest2,'k','Linewidth',2);
toc

%% debug
tic
for m=1:length(xGrid)
  matrIn = [xGrid(:,m); yGrid(:,m)];
  matrSamples = [xSamples; ySamples];
  zTest3(m) = KernelRegressorFastNd(matrIn,matrSamples,zSamples,h);
end
plot(xSamples,zSamples,'.',xGrid,zTrue,'r',xGrid,zTest1,'g.',xGrid,zTest2,'k',xGrid,zTest3,'co-','Linewidth',2);
toc

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function zhat = KernelRegressorSlow(xQuerz,yQuerz,XTrain,YTrain,ZTrain,h)

SizeOfTrainingSet = size(XTrain,2);
zhat = 0;
Normalizer = 0;

for m=1:SizeOfTrainingSet
  Kxi = (xQuerz - XTrain(:,m))^2/h(1) + (yQuerz - YTrain(:,m))^2/h(2);
  Kxi = exp(-Kxi);
  zhat = zhat + Kxi*ZTrain(m);
  Normalizer = Normalizer + Kxi;
end
zhat = zhat/Normalizer;

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

function zhat = KernelRegressorFast(xQuerz,yQuerz,XTrain0,YTrain0,ZTrain0,h)

SizeOfTrainingSet = size(XTrain0,2);

%disp('reducing training size by 1/N')
N = 5;
N = 1;
XTrain = XTrain0(1:N:SizeOfTrainingSet);
YTrain = YTrain0(1:N:SizeOfTrainingSet);
ZTrain = ZTrain0(1:N:SizeOfTrainingSet);
SizeOfTrainingSet = size(XTrain,2);

xQuerzRepeated = repmat(xQuerz,1,SizeOfTrainingSet);
yQuerzRepeated = repmat(yQuerz,1,SizeOfTrainingSet);

KX = ((xQuerzRepeated - XTrain).^2) / h(1) + ((yQuerzRepeated - YTrain).^2) / h(2);
%whos xQuerzRepeated XTrain xQuerz XTrain0 ZTrain0
KX = exp( -KX);
zhat = sum(KX.*ZTrain)/sum(KX);

%whos KX ZTrain
%error('ksjg')

end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
