% demos for ch06 -- Sergio
addpath ../../chapter06
addpath ../../common

%% Kernel regression with gaussian kernel
clear; 
n = 100;
x1 = linspace(0,2*pi,n);        % test data
x2 = linspace(0,4*pi,n)+pi/2;   % test data
t = sin(x1).*cos(x2)+rand(size(x1))/2;

x1 = linspace(-1,+1,n);  % test data
x2 = linspace(-2,+2,n);  % test data
t = (x1-0.5).^4 + (x2+0.5).^2;
t = exp(-t/2) + rand(size(x1))/2;
toll = 1e-4;

x1 = linspace(0,2*pi,n); % test data
x2 = linspace(-2,+2,n);  % test data
t = sin(x1.*x1).*exp(-x2.^2)+rand(size(x1))/2;
toll = 1e-8; %% overfit
toll = 1e+8; %% underfit
toll = 1e-2; %% sounds fine

z = [x1.^2; x2];
whos z t
model = knReg(z,t,toll,@knGauss);
[y,s] = knRegPred(model,z);

figure(1);
plot3(x1,x2,y,'r'); hold on
plot3(x1,x2,t,'bo')
hold off; xlabel('x1'); ylabel('x2'); zlabel('z')

figure(2); subplot(211); plot(x1,y,'r',x1,t,'bo'); xlabel('x1'); grid
figure(2); subplot(212); plot(x2,y,'r',x2,t,'bo'); xlabel('x2'); grid
