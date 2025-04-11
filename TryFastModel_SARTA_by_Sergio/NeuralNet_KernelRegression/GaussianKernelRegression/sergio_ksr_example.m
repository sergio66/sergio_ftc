https://stats.stackexchange.com/questions/242984/is-kernel-regression-similar-to-gaussian-process-regression

https://towardsdatascience.com/a-visual-comparison-of-gaussian-process-regression-kernels-8d47f2c9f63c
A Gaussian process regression is an application of a multivariate Gaussian distribution as a powerful predictive tool for data that is highly non-linear or not easily modeled using linear or multivariate regression. Gaussian processes can be expressed entirely by #1. a vector of mean values (defined by the data at input variables x1,x2…xn), and #2. a covariance matrix across (x1,x1), (x1,x2)… (xi,xj).

http://katbailey.github.io/post/from-both-sides-now-the-math-of-linear-regression/
http://katbailey.github.io/post/gaussian-processes-for-dummies/

http://scikit-learn.org/stable/auto_examples/gaussian_process/plot_gpr_noisy_targets.html

https://www.mathworks.com/help/stats/fitckernel.html
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear all;

iTest = 2;

if iTest == 1
  %% http://people.revoledu.com/kardi/tutorial/Regression/kernelregression/KernelRegression.htm
  xi = [1  1.2 3.2 4  5.1];
  yi = [23 17 12   27 8 ];
  x = 0:0.1:10;
elseif iTest == 2
  %% see ksr.m
  xi = 1:100;
  yi = sin(xi/10)+(xi/50).^2;
  yi = yi + 0.2*randn(1,100);
  x = 0:0.1:100;
end

%% see ksr.m
    n = length(xi);
    % optimal bandwidth suggested by Bowman and Azzalini (1997) p.31
    hx=median(abs(xi-median(xi)))/0.6745*(4/3/n)^0.2;
    hy=median(abs(yi-median(yi)))/0.6745*(4/3/n)^0.2;
    h=sqrt(hy*hx);
    fprintf(1,'optimal width hx,hy,h = %8.6e %8.6e %8.6e \n',hx,hy,h);
    
alpha = input('enter alpha : ');

for ii = 1 : length(xi)
  wah = (x-xi(ii))/(sqrt(2)*alpha);
  K(ii,:) = exp(-wah.^2);
  yK(ii,:) = yi(ii) * K(ii,:);
end

y = sum(yK) ./ sum(K);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
B = xi;
f = yi;
gprMdl = fitrgp(B',f','Standardize',true,'verbose',1);
gprMdl = fitrgp(B',f','KernelFunction','SquaredExponential','verbose',1);
[gpred,~,yerr_est] = predict(gprMdl,x');
bcalc = gpred;

figure(1); plot(xi,yi,'b-',xi,yi,'co',x,y,'k',x,bcalc,'r','linewidth',2)
legend('true','data','sergio','fitrgp','location','northwest');
%% http://www.cs.toronto.edu/~duvenaud/cookbook/index.html
grid

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

r=ksr(xi,yi);
r1=ksrlin(xi,yi);
figure(2); plot(xi,yi,'b-',xi,yi,'co',r.x,r.f,'g--',r1.x,r1.f,'r--','linewidth',2)
legend('true','data','GK','GK linear','location','northwest');
title('Gaussian kernel regression')
grid

figure(2); ax = axis; figure(1); axis(ax);