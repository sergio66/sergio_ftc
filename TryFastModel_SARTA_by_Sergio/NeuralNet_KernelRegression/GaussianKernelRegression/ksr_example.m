http://mccormickml.com/2014/02/26/kernel-regression/


% Example 1: smooth curve with noise
x = 1:100;
y = sin(x/10)+(x/50).^2;
yn = y + 0.2*randn(1,100);
r=ksr(x,yn);
plot(x,y,'b-',x,yn,'co',r.x,r.f,'r--','linewidth',2)
legend('true','data','regression','location','northwest');
title('Gaussian kernel regression')
disp('ret to continue'); pause

% Example 2: with missing data
x = sort(rand(1,100)*99)+1;
y = sin(x/10)+(x/50).^2;
y(round(rand(1,20)*100)) = NaN;
yn = y + 0.2*randn(1,100);
r=ksr(x,yn);
plot(x,y,'b-',x,yn,'co',r.x,r.f,'r--','linewidth',2)
legend('true','data','regression','location','northwest');
title('Gaussian kernel regression with 20% missing data')
disp('ret to continue'); pause

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Example 1: smooth curve with noise
x = 1:100;
y = sin(x/10)+(x/50).^2;
yn = y + 0.2*randn(1,100);
r=ksrlin(x,yn);
r1=ksr(x,yn); % downloadable from FEX ID 19195
plot(x,y,'b-',x,yn,'co',r.x,r.f,'r--',r1.x,r1.f,'m-.','linewidth',2)
legend('true','data','local linear','local constant','location','northwest');
title('Gaussian kernel regression')
disp('ret to continue'); pause

% Example 2: with missing data
x = sort(rand(1,100)*99)+1;
y = sin(x/10)+(x/50).^2;
y(round(rand(1,20)*100)) = NaN;
yn = y + 0.2*randn(1,100);
r=ksrlin(x,yn);
r1=ksr(x,yn); % downloadable from FEX ID 19195
plot(x,y,'b-',x,yn,'co',r.x,r.f,'r--',r1.x,r1.f,'m-.','linewidth',2)
legend('true','data','local linear','local constant','location','northwest');
title('Gaussian kernel regression with 20% missing data')
disp('ret to continue'); pause
