function [absc,abscS,abscF] = contcalc(prof, freq, ip, copt)

%   absc = contcalc(prof, freq, copt)
%
% INPUTS
%   prof   - kcmix format profile data
%   freq   - desired output frequency grid
%   copt   - optional function parameters
% 
% OUTPUTS
%   absc   - npt by nlev arr
%   freq   - n-vector of radiance frequencies
% 
% OPTIONS (valid copt fields) include
%
%   cdir   - location of tabulated continuum data, default 
%            '/asl/data/kcarta/KCARTADATA/General/CKDieee_le'
%   cvers  - continuum version, default '24'
%   cswt   - self continuum adjustment weight, default 1
%   cfwt   - foreign continuum adjustment weight, default 1
%

% physcial constants
kAvog = 6.022045e26;
kPlanck2=1.4387863;

cvers = copt.cvers;
cdir  = copt.cdir;
cswt  = copt.cswt;  %% self component weight
cfwt  = copt.cfwt;  %% forn component weight

% build self and foreign continuum files 
sfile = [cdir, '/CKDSelf', cvers, '.bin'];
ffile = [cdir, '/CKDFor',  cvers, '.bin'];

% read the continuum data, ks=self, kf=foreign
[ks, junk, ts] = contread(sfile);
[kf, fcoarse, tf] = contread(ffile);

semilogy(junk,ks,'b',junk,kf,'r')

if (length(junk) ~= length(fcoarse))
   sfile
   ffile
   error('Mismatched length of self and foreign continuum files');
end
if (abs(fcoarse-junk) > 0.01)
   sfile
   ffile
   error('Mismatched frequency points in self and foreign continuum files');
end
nts = length(ts);

% initialize output
nfreq = length(freq);
nlays = length(prof.mpres(:,ip));
absc = zeros(nfreq, nlays);

%-------------------------------
% set up frequency interpolation
%-------------------------------
df = diff(fcoarse);   % frequency step sizes
n = length(df);       % number of frequency steps
fmid = fcoarse(1:n) + df./2;                 % midpoints of step intervals
imid = interp1(fmid, 1:n, freq, 'nearest');  % indices of midpoints
ineed = unique([imid, imid(length(imid)) + 1]);
fw1 = (fcoarse(imid+1) - freq) ./ df(imid);  % weight for lower coarse point
fw2 = (freq - fcoarse(imid)) ./ df(imid);    % weight for upper coarse point
% Note: kfine = fw1 .* ks(imid) + fw2 * ks(imid+1);

% Declare work arrays
np1 = n + 1;
cscoarse = zeros(1,np1);
cfcoarse = zeros(1,np1);
odcoarse = zeros(1,np1);

indh2o = find(prof.glist(ip,:) == 1);

% loop on layers
for iL = 100-prof.nlays(ip)+1 : 100

  % Current layer mean temperature {Kelvin}
  tL = prof.mtemp(iL,ip);

  % ------------------------------------------
  % temperature interpolate the self-continuum
  % ------------------------------------------
  % table index of greatest lower bound temp
  i1 = max([find(ts <= tL); 1]);
  % table index of least upper bound temp
  i2 = min([find(tL <= ts); nts]);
  % get temperature interpolation weights
  if ts(i2) ~= ts(i1)
    tw2 = (tL - ts(i1)) / (ts(i2) - ts(i1));
    tw1 = 1 - tw2;
  else
    tw2 = 1;
    tw1 = 0;
  end
  %fprintf(1,'%3i %3i %3i %8.6f %8.6f \n',iL,i1,i2,tw1,tw2);

  cscoarse(ineed) = ( (tw1.*ks(i1,ineed)) + ...
                      (tw2.*ks(i2,ineed)) ).*cswt;

  %%%
  cfcoarse(ineed) = kf(1,ineed);
  %%%

  % -----------------------------------
  % combine self and foreign components
  % -----------------------------------
  % scalar values for layer iL
  pL  = prof.mpres(iL,ip)/1013.25;               % layer pressure, atms
  ppL = prof.gpart(iL,ip)/1013.25;               % water partial pressure, atms
  qL  = prof.gamnt(iL,ip)/6.023e26;              % water gas amount
  a1  = qL * kAvog * 296.0 / tL;
  a2  = kPlanck2 / (2 * tL);
  %

  %%% TOTAL
  odcoarse(ineed) = (cscoarse(ineed).*ppL + cfcoarse(ineed).*(pL - ppL)) ...
     .* fcoarse(ineed) .* tanh(a2.*fcoarse(ineed)) .* a1;
  % Interpolate from coarse to output freqs
  absc(:, iL) = (odcoarse(imid).*fw1 + odcoarse(imid+1).*fw2); %

  %% SELF
  cs_coarse(ineed) = (cscoarse(ineed).*ppL) ...
     .* fcoarse(ineed) .* tanh(a2.*fcoarse(ineed)) .* a1;
  % Interpolate from coarse to output freqs
  abscS(:, iL) = (cs_coarse(imid).*fw1 + cs_coarse(imid+1).*fw2); %

  %% FORN
  cf_coarse(ineed) = (cfcoarse(ineed).*(pL - ppL)) ...
     .* fcoarse(ineed) .* tanh(a2.*fcoarse(ineed)) .* a1;
  % Interpolate from coarse to output freqs
  abscF(:, iL) = (cf_coarse(imid).*fw1 + cf_coarse(imid+1).*fw2); %

end % loop on layers

%plot(freq,absc-(abscS+abscF))
