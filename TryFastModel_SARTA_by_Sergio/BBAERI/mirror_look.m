xstartup

[od,w] = readkcstd('OD.dat');

stemp = 272.2;   % for MLW
sair  = 272.2;   % assume air is at same temp

stemp = input('Enter stemp (272K) : ');
sair  = input('Enter sair (272K) : ');

plot(w,od)

plevs = [1013.95 986.067];   %% AIRS pressure levels
hgts = p2h(plevs);
layerthick = abs(diff(hgts));

%% assume 30 cm from BBAERI to reflecting mirror/surface
newOD = od * 0.3/layerthick;
figure(1); plot(w,exp(-od),w,exp(-newOD),'r')
hl = legend('250 m air layer','30 cm air layer');
set(hl,'fontsize',10); grid on;
xlabel('Wavenumber cm-1'); ylabel('Transmission');


%% if emis == 1 and sair = stemp then RT becomes 
%% emis ttorad(w,stemp) T + ttorad(w,stemp) (1 - T)  where T = transmission
%% == (emis-1) T ttorad(w,stemp) + ttorad(w,stemp) = ttorad(w,stemp);
%% 
%% if emis ~= 1 and stemp ~ siar
%% Rad = [emis ttorad(w,stemp) - ttorad(w,sair)]*T + ttorad(w,sair)
%% 
emis = ones(size(w))*0.9;       %% assume this surface emis
r0 = emis .* ttorad(w,stemp);   %% this is emitted by surface
r = r0.*exp(-newOD) + ttorad(w,sair).*(1-exp(-newOD));

figure(2); plot(w,ttorad(w,stemp),'k',w,r0,'bo-',w,r,'rx');
hl = legend('Planck at stemp','emission from surface','at BBAERI');
set(hl,'fontsize',10); grid on;
xlabel('Wavenumber cm-1'); ylabel('Radiance');
arrow([670 100],[1000 100],'linewidth',2);
  text(1050,100,'CO2 667 cm-1 line')
arrow([1400 30],[1800 30],'linewidth',2);
arrow([1800 30],[1400 30],'linewidth',2);
  text(1450,40,'WV 6.67 um lines')
%shade(fig,X0,Y0,W,H,color,transperancy)
shade(2,0600,0,200,120,'red',0.1);
shade(2,1400,0,400,120,'red',0.1);

figure(3); plot(w,ones(size(w))*stemp,'k',w,rad2bt(w,r0),'bo-',w,rad2bt(w,r),'rx');
hl = legend('Planck at stemp','emission from surface','at BBAERI');
set(hl,'fontsize',10); grid on;
xlabel('Wavenumber cm-1'); ylabel('BT');

% printfig(1,'transmission','png');
% printfig(2,'at_bbaeri','png');
