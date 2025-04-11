%% see https://www.mathworks.com/help/stats/fitrgp.html#buyp_k9-1
tbl = readtable('abalone.data','Filetype','text',...
     'ReadVariableNames',false);
tbl.Properties.VariableNames = {'Sex','Length','Diameter','Height',...
    'WWeight','SWeight','VWeight','ShWeight','NoShellRings'};
tbl(1:7,:)

gprMdl = fitrgp(tbl,'NoShellRings','KernelFunction','ardsquaredexponential',...
      'FitMethod','sr','PredictMethod','fic','Standardize',1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ypred = resubPredict(gprMdl);

figure(1);
plot(tbl.NoShellRings,'r.');
hold on
plot(ypred,'b');
xlabel('x');
ylabel('y');
legend({'data','predictions'},'Location','Best');
axis([0 4300 0 30]);
hold off;

L = resubLoss(gprMdl)


