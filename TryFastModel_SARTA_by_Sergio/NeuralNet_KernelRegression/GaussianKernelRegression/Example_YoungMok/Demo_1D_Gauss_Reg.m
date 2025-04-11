%% Initialization
clear;clc;

for ii = 1 : 4
  figure(ii); clf;
end

iGen = 2; %% sergio
iGen = 1; %% orig

%% Dataset Generation
if iGen == 1
  x = rand(1,100)-0.5;
  y = (x-0.5).^2 + 0.1*rand(1,100);
  y = 1*(x-0.5).^2 + 3*x + 0.1*rand(1,100);

  x = 1 : 100;
  y = sin(x/10) + (x/50).^2 + 0.1*rand(1,100);
  
else
  x = rand(1,100)-0.5;
  x = x*5;
  y = (x-0.5).^2;
  y = exp(-y) + 0.1*rand(1,100);  
end

%% Gaussian Kernel Bandwidth Setting
if iGen == 1
  h=[0.1];
  h = 0.01;
  h = 5;
  
else
  h = 0.1;
end

%% gaussian pred
for j=1:length(x)
  xs = x(j);
  ypredGK(j) = gaussian_kern_reg(xs,x,y,h); % Prediction
end

%% linear pred
xx = [x; ones(1,100)];
A = xx'\y';
ypredlinear = A'*xx;

figure(1);
plot(x,y,'k.',x,ypredGK,'rx',x,ypredlinear,'bo')
hl = legend('orig','GK','LLS');

figure(2);
plot(1:100,y-ypredGK,'rx',1:100,y-ypredlinear,'bo')
hl = legend('y0-GK','y0-LLS');
[mean(y-ypredGK) std(y-ypredGK) mean(y-ypredlinear) std(y-ypredlinear)]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prediction point
if iGen == 1
  [xx1] = -1:0.01:1;
  xx1 = -50:0.025:150;
else
  [xx1] = -1:0.01:1;
  xx1 = xx1 * 10;
end

for j=1:length(xx1)
  xs = xx1(j);
  ys(j)=gaussian_kern_reg(xs,x,y,h); % Prediction
  xs = [xs; 1];
  yslinear(j) = A' *xs;
end

%% Result Plot
figure(3);colormap jet; hold on; 
plot(x,y,'b.')    % training dataset plot
plot(xx1,ys)      % GK prediction point plot
legend('Training data','GK Predicted Surface')

%% Result Plot
figure(4);colormap jet; hold on;
plot(x,y,'b.')     % training dataset plot
plot(xx1,yslinear) % LLS prediction point plot
legend('Training data','LLS Predicted Surface')
hold off

figure(5); plot(xx1,ys);       title('GK pred');       % GK     prediction point plot
figure(6); plot(xx1,yslinear); title('LLS pred');      % linear prediction point plot

for ii = 1 : 4
  figure(ii); grid on;
end
