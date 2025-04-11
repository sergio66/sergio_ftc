load x
load spec
load /home/sergio/MATLABCODE/BBAERI/yuval_07_24_2013_RH55_Tc23.mat
plot(wAERI,odAERIall)

%{
Hi Sergio,

I know you are very busy, so, only if you have time, attached AERI’s data (spec.mat) and + X axis (x.mat).

During measurements the IR source was at 47 c, ambient temp was 23.3 c,  RH= 55%

Distance between AERI and IR source is ~46 cm.

When AERI is “facing” the target (on the table), the overall path is ~68 cm.

Thanks,

Yuval


%}

addpath /asl/matlib/aslutil/

%% 46 cm path length
tau = interp1(wAERI,odAERIall*46,x,[],'extrap');
rSource = ttorad(x,47.0+273.15);
rAmb    = ttorad(x,23.3+273.15);

%% changing source 
rSource = ttorad(x,39.3+273.15);
rAmb    = ttorad(x,23.3+273.15);

eEmis = 0.9*ones(size(x));
rSource = eEmis .* ttorad(x,47+273.15);

rNew = rSource .* exp(-tau) + rAmb .* (1 - exp(-tau));

plot(x,spec,x,rNew)
axis([500 1800 0 200]); grid