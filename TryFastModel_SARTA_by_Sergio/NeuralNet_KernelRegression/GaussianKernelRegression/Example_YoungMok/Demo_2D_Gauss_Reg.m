%% Initialization
clear;clc;

for ii = 1 : 4
  figure(ii); clf;
end

iGen = 1; %% orig
iGen = 2; %% sergio

%% Dataset Generation
if iGen == 1
  x = rand(2,100)-0.5;
  y = (x(1,:)-0.5).^2+x(2,:)+0.1*rand(1,100);
else
  x = rand(2,100)-0.5;
  x = x*5;
  y = (x(1,:)-0.5).^2 + (x(2,:)-0.75).^2;
  y = exp(-y) + 0.1*rand(1,100);  
end

%% Gaussian Kernel Bandwidth Setting
h=[0.1;0.1]; 

%% gaussian pred
for i=1:size(x,1)
    for j=1:size(x,2)
        xs=[x(1,j);x(2,j)];
        ypredGK(j)=gaussian_kern_reg(xs,x,y,h); % Prediction
    end
end

%% linear pred
xx = [x; ones(1,100)];
A = xx'\y';
ypredlinear = A'*xx;

figure(1);
plot3(x(1,:),x(2,:),y,'k.',x(1,:),x(2,:),ypredGK,'rx',x(1,:),x(2,:),ypredlinear,'bo')
hl = legend('orig','GK','LLS');

figure(2);
plot(1:100,y-ypredGK,'rx',1:100,y-ypredlinear,'bo')
hl = legend('y0-GK','y0-LLS');
[mean(y-ypredGK) std(y-ypredGK) mean(y-ypredlinear) std(y-ypredlinear)]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Prediction point
if iGen == 1
  [xx1 xx2] = meshgrid(-1:0.1:1,-1:0.1:1);
else
  [xx1 xx2] = meshgrid(-1:0.1:1,-1:0.1:1);
  xx1 = xx1 * 10;
  xx2 = xx2 * 10;
end

for i=1:size(xx1,1)
    for j=1:size(xx1,2)
        xs=[xx1(i,j);xx2(i,j)];
        ys(i,j)=gaussian_kern_reg(xs,x,y,h); % Prediction
	
	xs = [xs; 1];
	yslinear(i,j) = A' *xs;
    end
end

%% Result Plot
figure(3);colormap jet; hold on; 
plot3(x(1,:),x(2,:),y,'b.')  % training dataset plot
mesh(xx1,xx2,ys)             % GK prediction point plot
legend('Training data','GK Predicted Surface')
grid ; view(3);
colorbar
hold off

%% Result Plot
figure(4);colormap jet; hold on; 
plot3(x(1,:),x(2,:),y,'b.')  % training dataset plot
mesh(xx1,xx2,yslinear)       % linear prediction point plot
legend('Training data','LLS Predicted Surface')
grid ; view(3);
colorbar
hold off

figure(5); contour(xx1,xx2,ys,100);       colormap jet; colorbar; title('GK pred');       % GK     prediction point plot
figure(6); contour(xx1,xx2,yslinear,100); colormap jet; colorbar; title('LLS pred');      % linear prediction point plot

for ii = 1 : 4
  figure(ii); grid on;
end
