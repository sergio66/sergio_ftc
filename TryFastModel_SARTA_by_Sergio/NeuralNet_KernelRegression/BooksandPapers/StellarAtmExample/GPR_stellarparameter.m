% This code is based on the more general code by  
%  Rasmussen and his coauthors. 
covfunc = {@covMaterniso, 5}; %Covariance function
 
  ell = 1/4; sf=1;  hyp.cov = log([ell; sf]); % the hyperparameter of covariance function

 h=40 % 40 PCs
 
meanfunc = {@meanLinear}; % mean function
 
hyp.mean=ones(h,1); %the hyperparameter of mean function
 
 ;
 
 
 
   
  
 likfunc = @likGauss; %lieklihood function 
 
  sn = 0.1;
 
 sn=0.1; hyp.lik = log(sn); %hyperparameter of likelihood function 
 

 T1=train_pca; %traing data
 T2=test_pca;%test data
 trainlabel=train_label3; %train label 
testlabel=test_label3; %test label 
[mm,nn]=size(testlabel);
 
  testlabel_predicted = gp(hyp, @infLaplace,meanfunc, covfunc, likfunc,T1(:,1:h), trainlabel,T2(:,1:h),testlabel); %using the initial hyperparameter to run GPR and  get the predicted atmoapherica
                                                                                                    % parameters corresponding to the test data. 
 
  
   
 
hyp1 = minimize(hyp, @gp, -1,@infLaplace,meanfunc, covfunc, likfunc,T1(:,1:h),trainlabel); % find the optimal hyperparameters


 testlabel_optimal = gp(hyp1, @infLaplace,meanfunc, covfunc, likfunc,T1(:,1:h), trainlabel,T2(:,1:h),testlabel); %run GPR using the optimal hyperparameters and get the optimal predicted 
                                                                                                                 % atmoapheric parameter corresponding to test data.
toc
  med_sdss(sss)=median(testlabel_optimal-testlabel); %compute the median of the difference
   std_sdss(sss)=std(testlabel_optimal-testlabel); %compute the std of the difference
    
