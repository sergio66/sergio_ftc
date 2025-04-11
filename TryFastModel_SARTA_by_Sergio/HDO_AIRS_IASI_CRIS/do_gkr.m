function gkrpred = do_gkr(tolerance,matr,booOD,predict)

%% input
%%   tolerance for the GKR
%%   matr      NxM  where N = number of training points eg 100000, M=number of predicts eg 4
%%   booOD     Nx1  where N = number of training points
%%
%%   predict   Nx1  linear prediction result, for plotting purposes

addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/demo/ch06
addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/chapter06
addpath /home/sergio/MATLABCODE/TryFastModel_SARTA_by_Sergio/NeuralNet_KernelRegression/BooksandPapers/Bishop_PatternLearning_with_MatlabCodeExamples/PRMLT/common

midpt = min(booOD) + (max(booOD) - min(booOD))/2;

lala = randn(size(booOD));
lala = find(lala > 0 & isfinite(booOD));

themean = nanmean(booOD(lala));

model = knReg(matr(lala,:)',booOD(lala)'-themean,tolerance,@knGauss);
%model = knReg(matr(lala,:)',booOD(lala)'-themean,tolerance*1000,@knLin);   %%% haha should be same as Predict! Linear Kernel!!!
[gkrpred,s] = knRegPred(model,matr(lala,:)');

gkrpred = gkrpred + themean;

figure(1); clf; plot(booOD(lala),predict(lala),'b.',booOD(lala),gkrpred,'r.',booOD,booOD,'k.')
xlabel('log10(OD)'); ylabel('log10(fitOD)'); hl = legend('linear','GKR','location','best'); set(hl,'fontsize',10)
grid

figure(2); clf; plot(booOD(lala),booOD(lala)-predict(lala),'b.',booOD(lala),booOD(lala)-gkrpred','r.')
xlabel('log10(OD)'); ylabel('log10(orig)-log10(fitted)'); hl = legend('linear','GKR','location','best'); set(hl,'fontsize',10)
grid

figure(3); clf; 
pcerror1 = (booOD(lala)-predict(lala))./booOD(lala)*100; %scatter(booOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
pcerror2 = (booOD(lala)-gkrpred')./booOD(lala)*100;         %scatter(booOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
derror = -100 : 1: +100;
dn1 = histc(pcerror1,derror);
dn2 = histc(pcerror2,derror);
plot(derror,dn1/length(pcerror1),'b',derror,dn2/length(pcerror1),'r'); title('percent error');
hl = legend('linear','GKR','location','best'); set(hl,'fontsize',10); grid