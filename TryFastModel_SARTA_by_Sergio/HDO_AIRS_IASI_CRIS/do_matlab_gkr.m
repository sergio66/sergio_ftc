iVers = 3; %% hmm
iVers = 2; %% hmm
iVers = 0; %% simple
iVers = 1;

usedata = booOD;

step = 2;
step = 200;
step = 100;
step = 150;
step = 10;
step = 2; 
odds  = 1 : step : length(booOD);
evens = 2 : step : length(booOD);

good = find(isfinite(usedata(odds)));
odds = odds(good);
good = find(isfinite(usedata(evens)));
evensgood = evens(good);

if iVers == 0
  gprMdl = fitrgp(matr(odds,:),usedata(odds)','Standardize',true,...
      	          'verbose',1);
elseif iVers == 1
   gprMdl = fitrgp(matr(odds,:),usedata(odds)','KernelFunction','squaredexponential','sigma',0.2,'verbose',1,...
                  'Standardize',true);
elseif iVers == 2
   gprMdl = fitrgp(Xtrain,ytrain,'Basis','constant','FitMethod','exact',...
                 'PredictMethod','exact','KernelFunction','ardsquaredexponential',...
                 'KernelParameters',[sigmaM0 sigmaF0],'Sigma',sigma0,'Standardize',1,...
                 'verbose',1);
elseif iVers == 3
  rng default
  gprMdl = fitrgp(Xtrain,ytrain,'Basis','constant','FitMethod','exact',...
                  'PredictMethod','exact','KernelFunction','ardsquaredexponential',...
                  'KernelParameters',[sigmaM0 sigmaF0],'Sigma',sigma0,'Standardize',1,...
                  'KernelFunction','squaredexponential',...
	          'OptimizeHyperparameters','auto','HyperparameterOptimizationOptions',...
                  struct('AcquisitionFunctionName','expected-improvement-plus'),'verbose',1);
end

%if iVers ~= 0
%  [gpred_odds,~,yerr_est] = resubPredict(gprMdl,matr(odds,:));
%  [gpred_evens,~,zerr_est] = resubPredict(gprMdl,matr(evens,:));
%  [gpred_evensgood,~,zerr_estgood] = resubPredict(gprMdl,matr(evensgood,:));
%else
%  [gpred_odds] = resubPredict(gprMdl,matr(odds,:));
%  [gpred_evens] = resubPredict(gprMdl,matr(evens,:));
%  [gpred_evensgood] = resubPredict(gprMdl,matr(evensgood,:));
%end
if iVers ~= 0
  [gpred_odds] = resubPredict(gprMdl);
else
  [gpred_odds] = resubPredict(gprMdl);
end

figure(2); plot(booOD,predict,'.',usedata(odds),gpred_odds,'r.'); legend('linear','Matlab GKR','location','best')
  xlabel('log10(OD)'); ylabel('log10(fitOD)'); hl = legend('linear','Matlab GKR','location','best'); set(hl,'fontsize',10);
  grid
figure(3); plot(booOD,booOD-predict,'.',usedata(odds),usedata(odds)-gpred_odds,'r.'); legend('linear','Matlab GKR','location','best')
  xlabel('log10(OD)'); ylabel('log10(orig)-log10(fitted)'); hl = legend('linear','Matlab GKR','location','best'); set(hl,'fontsize',10)
  grid
