function model = knReg(X, t, lambda, kn)
% Gaussian process (kernel) regression
% Input:
%   X: d x n data
%   t: 1 x n response
%   lambda: regularization parameter
% Output:
%   model: trained model structure
% Written by Mo Chen (sth4nth@gmail.com).
if nargin < 4
    kn = @knGauss;
end
if nargin < 3
    lambda = 1e-2;
end
K = knCenter(kn,X);
tbar = mean(t);

%% new
gnu = K+lambda*eye(size(X,2));
[V,D] = eig(gnu);
iCnt = 0;
while min(diag(D)) < 0 & iCnt < 20
  iCnt = iCnt + 1;
  if iCnt == 1
    disp('oops not positive definite ... trying to fix ...')
    disp('see https://www.mathworks.com/matlabcentral/answers/6057-repair-non-positive-definite-correlation-matrix')  
    fprintf(1,'  orig min diag = %8.6f \n',min(diag(D)))
  end
  V1 = V(:,1);
  if iCnt < 5
    factor = 1;
  elseif iCnt < 10
    factor = 100;
  elseif iCnt < 15
    factor = 10000;
  elseif iCnt < 20
    factor = 1000000;
  end
  gnu = gnu + V1*V1'*(factor*eps*D(1,1)-D(1,1));
  [V,D] = eig(gnu);
  fprintf(1,'  iCnt = %2i new  min diag = %8.6f \n',iCnt,min(diag(D)))  
end  
%% new

%U = chol(K+lambda*eye(size(X,2)));    % 6.62
U = chol(gnu);                        % 6.62
a = U\(U'\(t(:)-tbar));               % 6.68

model.kn = kn;
model.a = a;
model.X = X;
model.tbar = tbar;
%% for probability prediction
y = a'*K+tbar;
beta = 1/mean((t-y).^2);              % 3.21
alpha = lambda*beta;           % lambda=a/b P.153 3.55
model.alpha = alpha;
model.beta = beta;
model.U = U;