function gkrpred = loop_do_gkr(matr,booOD,tolerance)

%% input
%%   matr      NxM  where N = number of training points eg 100000, M=number of predicts eg 4
%%   booOD     Nx1  where N = number of training points
%%
%%   predict   Nx1  linear prediction result, for plotting purposes

addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/demo/ch06
addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/chapter06
addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/common

midpt = min(booOD) + (max(booOD) - min(booOD))/2;

if nargin < 2 | nargin > 3
  error('calling is loop_do_gkr(matr,booOD,[tolerance])')
end

if nargin == 2
  tolerance = -2:0.5:+2;
  tolerance = 10.^tolerance;
end

lala = randn(size(booOD));
lala = find(lala > 0 & isfinite(booOD));

trainingsubsetinds = 1 : 4 : length(booOD);
for ii = 1 : length(tolerance)
  thetol = tolerance(ii);
  fprintf(1,'%2i %8.6f \n',ii,thetol);

  [gkrpred2,chisqr2] = do_simplegkr_subset(thetol,matr,booOD,trainingsubsetinds);
  thechisq.fitGKR2(ii) = chisqr2.fitGKR;
  thechisq.testGKR2(ii) = chisqr2.testGKR;

  [gkrpred,chisqr] = do_gkr_subset(thetol,matr,booOD,trainingsubsetinds);  
  thechisq.fitlinear(ii) = chisqr.fitlinear;
  thechisq.testlinear(ii) = chisqr.testlinear;
  thechisq.fitGKR(ii) = chisqr.fitGKR;
  thechisq.testGKR(ii) = chisqr.testGKR;

  figure(4); semilogx(tolerance(1:ii),thechisq.fitGKR,'bx-',tolerance(1:ii),thechisq.testGKR,'b--',...
                      tolerance(1:ii),thechisq.fitGKR2,'rx-',tolerance(1:ii),thechisq.testGKR2,'r--',...  
                      tolerance(1:ii),thechisq.fitlinear,'kx-',tolerance(1:ii),thechisq.testlinear,'k--',...
		      'linewidth',2);
  hl = legend('fitGKR','testGKR','fitGKR2','testGKR2','fitLinear','testLinear','location','best');
  xlabel('tolerance'); ylabel('chisqr');
  pause(0.1)
end

