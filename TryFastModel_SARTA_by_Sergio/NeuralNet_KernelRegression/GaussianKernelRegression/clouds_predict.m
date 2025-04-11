matr = [landfrac; rlat; rlon; stemp; btcalc]';

thetype = input('Enter (1) iceOD (2) icetop (3) icesize (4) waterOD (5) watertop : ');

if thetype == 1
  usedata = iceOD;
elseif thetype == 2  
  usedata = icetop;
elseif thetype == 3
  usedata = icesze;
elseif thetype == 4
  usedata = waterOD;
elseif thetype == 5  
  usedata = watertop;
end

if thetype == 1 | thetype == 4
  deltaOD = -5 : 0.1 : 5;
elseif thetype == 2 | thetype == 5
  deltaOD = -200 : 10 : 200;
elseif thetype == 3
  deltaOD = -20 : 1 : 20;
end  

step = 2;
step = 200;
step = 100;
step = 150;
odds  = 1 : step : length(iceOD);
evens = 2 : step : length(iceOD);

good = find(isfinite(usedata(odds)));
odds = odds(good);
good = find(isfinite(usedata(evens)));
evensgood = evens(good);

coefs = matr(odds,:) \ usedata(odds)';
pred_odds = matr(odds,:) * coefs;
pred_evens = matr(evens,:) * coefs;
pred_evensgood = matr(evensgood,:) * coefs;
figure(1); plot(usedata(odds),pred_odds,'b.',usedata(evens),pred_evens,'r.');
  hl = legend('training','new data'); set(hl,'fontsize',10)
figure(2); plot(usedata(odds),usedata(odds)-pred_odds','b.',usedata(evens),usedata(evens)-pred_evens','r.');
  hl = legend('training','new data'); set(hl,'fontsize',10)
figure(3);
  plot(deltaOD,histc(usedata(odds)-pred_odds',deltaOD),'b',deltaOD,histc(usedata(evens)-pred_evens',deltaOD),'mo-',...
       deltaOD,histc(usedata(evensgood)-pred_evensgood',deltaOD),'r')
  hl = legend('training','new data '); set(hl,'fontsize',10)
  
ytrain = usedata(odds)';
Xtrain = matr(odds,:);
sigma0 = std(ytrain);
sigmaF0 = sigma0;
d = size(Xtrain,2);
sigmaM0 = 10*ones(d,1);
sigmaM0 = std(Xtrain);

iVers = 3; %% hmm
iVers = 2; %% hmm
iVers = 0; %% simple

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
            struct('AcquisitionFunctionName','expected-improvement-plus'),...
	    'verbose',1);	   
end

if iVers ~= 0
  [gpred_odds,~,yerr_est] = predict(gprMdl,matr(odds,:));
  [gpred_evens,~,zerr_est] = predict(gprMdl,matr(evens,:));
  [gpred_evensgood,~,zerr_estgood] = predict(gprMdl,matr(evensgood,:));
else
  [gpred_odds] = predict(gprMdl,matr(odds,:));
  [gpred_evens] = predict(gprMdl,matr(evens,:));
  [gpred_evensgood] = predict(gprMdl,matr(evensgood,:));
end

figure(1); plot(usedata(odds),gpred_odds,'r.',usedata(odds),pred_odds,'b.',usedata(odds),usedata(odds),'k.');
  title('training');
  hl = legend('GPR','LLS','location','best'); set(hl,'fontsize',10)

figure(2); plot(usedata(odds),pred_odds,'go',usedata(odds),gpred_odds,'ko',...
               usedata(evens),gpred_evens,'m.',usedata(evensgood),gpred_evensgood,'r.',...
	       usedata(evens),pred_evens,'c.',usedata(evensgood),pred_evensgood,'b.',...
               usedata(evens),usedata(evens),'k')
  title('new data')
  hl = legend('LLS training','GPR training','GPR test','GPR besttest','LLS test','LLS besttest','location','best'); set(hl,'fontsize',10)

figure(3);
  plot(deltaOD,histc(usedata(odds)-pred_odds',deltaOD),'go-',deltaOD,histc(usedata(odds)-gpred_odds',deltaOD),'k',...
       deltaOD,histc(usedata(evens)-gpred_evens',deltaOD),'mx-',deltaOD,histc(usedata(evensgood)-gpred_evensgood',deltaOD),'r',...
       deltaOD,histc(usedata(evens)-pred_evens',deltaOD),'cx-',deltaOD,histc(usedata(evensgood)-pred_evensgood',deltaOD),'b','linewidth',2)
  hl = legend('LLS training','GPR training','GPR test','GPR besttest','LLS test','LLS besttest','location','best'); set(hl,'fontsize',10)       
