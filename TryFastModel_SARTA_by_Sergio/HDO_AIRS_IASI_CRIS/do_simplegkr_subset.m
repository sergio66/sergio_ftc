function [gkrpred,chisqr] = do_simplegkr_subset(tolerance,matr,booOD,trainingsubsetinds)

%% input
%%   tolerance for the GKR
%%   matr        NxM  where N = total number of training points eg 100000, M=number of predicts eg 4
%%   booOD       Nx1  where N = total number of training points
%%
%%   subsetinds  Kx1  subset of numbers to use for training, by default rest are used for testiing
%%
%% output
%%   gkrpred     Nx1  output where obviously gkrpred(trainingsubsetinds) come from training
%%                                           gkrpred(~trainingsubsetinds) come from testing

lala = find(isfinite(booOD) & imag(booOD) < eps);
lala = intersect(lala,trainingsubsetinds);  %% use these for training
notlala = setdiff(1:length(booOD),lala);    %% use these for testing

themean = nanmean(booOD(lala));

%% linear stuff, from Scott
A = matr(lala,:) \ (booOD(lala)-themean);
predict1 = matr(lala,:) * A;
predict2 = matr(notlala,:) * A;
predict1 = predict1 + themean;
predict2 = predict2 + themean;

%% GKR
[mm,nn] = size(matr);
for ii = 1 : length(lala)
  [gkrpred1(ii)] = KernelRegressorFastNd(matr(lala(ii),:)',matr(lala,:)',booOD(lala)'-themean,tolerance*ones(1,nn));
end
for ii = 1 : length(notlala)
  [gkrpred2(ii)] = KernelRegressorFastNd(matr(notlala(ii),:)',matr(lala,:)',booOD(lala)'-themean,tolerance*ones(1,nn));
end

gkrpred1 = gkrpred1 + themean;
gkrpred2 = gkrpred2 + themean;
gkrpred = [gkrpred1 gkrpred2];

figure(1); clf;
plot(booOD(lala),predict1,'b.',booOD(lala),gkrpred1,'r.',booOD(notlala),predict2,'c.',booOD(notlala),gkrpred2,'m.',booOD,booOD,'k.')
xlabel('log10(OD)'); ylabel('log10(fitOD)');
hl = legend('linearFit','GKRFit','linearTest','GKRTest','location','best'); set(hl,'fontsize',10);
grid

figure(2); clf;
plot(booOD(lala),booOD(lala)-predict1,'b.',booOD(lala),booOD(lala)-gkrpred1','r.',...
     booOD(notlala),booOD(notlala)-predict2,'c.',booOD(notlala),booOD(notlala)-gkrpred2','m.')
xlabel('log10(OD)'); ylabel('log10(orig)-log10(fitted)');
hl = legend('linearFit','GKRFit','linearTest','GKRTest','location','best'); set(hl,'fontsize',10);
grid

chisqr.fitlinear  = nansum((booOD(lala)-predict1).*(booOD(lala)-predict1))/length(lala);
chisqr.testlinear = nansum((booOD(notlala)-predict2).*(booOD(notlala)-predict2))/length(notlala);
chisqr.fitGKR  = nansum((booOD(lala)-gkrpred1').*(booOD(lala)-gkrpred1'))/length(lala);
chisqr.testGKR = nansum((booOD(notlala)-gkrpred2').*(booOD(notlala)-gkrpred2'))/length(notlala);
chisqr.fitind = lala;
chisqr.testind = notlala;

figure(3); clf; 
pcerror1a = (booOD(lala)-predict1)./booOD(lala)*100; 
pcerror1b = (booOD(lala)-gkrpred1')./booOD(lala)*100;     
pcerror2a = (booOD(notlala)-predict2)./booOD(notlala)*100; 
pcerror2b = (booOD(notlala)-gkrpred2')./booOD(notlala)*100;     
derror = -100 : 1: +100;
dn1a = histc(pcerror1a,derror);
dn1b = histc(pcerror1b,derror);
dn2a = histc(pcerror2a,derror);
dn2b = histc(pcerror2b,derror);
plot(derror,dn1a/length(pcerror1a),'b',derror,dn1b/length(pcerror1b),'r',...
     derror,dn2a/length(pcerror1a),'c',derror,dn2b/length(pcerror1b),'m','linewidth',2);
title('percent error');
hl = legend('linearFit','GKRFit','linearTest','GKRTest','location','best'); set(hl,'fontsize',10);
grid

pause(0.1)