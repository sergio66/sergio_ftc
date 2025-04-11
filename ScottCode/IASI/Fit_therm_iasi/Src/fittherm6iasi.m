% Program fittherm6iasi
%
% Reads in reflected thermal regression data and fits for the
% F-factor coefficients.  This 6 term version uses an "rdown"
% term calculationed using the fast model. This rdown should
% be calculated for the same profiles, angles, surface pressure,
% water continuum, and tuning as was used for the rtherm calc.
% (Note: stemp and emis have no effect on rdown).

% Created: 25 April 2007, Scott Hannon - based on xfitthermband2.m for AIRS
% Update: 09 Jan 2008, S.Hannon - minor mods for Jan08 IASI production
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Number of IASI channels
%nchan = 8861;
nchan = 8461;

% Max number of layers in atmos
maxlay = 100;

% Max number of terms to use in fit
maxterm = 6;

% Brightness temperatures used to calc radmin (min rad to consider)
btref = 220;
btpert = 0.005;

% Min radtherm ratio (radtherm[only]/radnotherm) to consider.  Since
% the radiances come from real*4 values with 7 or 8 significant
% digits, and radtherm is the difference of two such radiances,
% then radtherm is untrustworthy for radrat < 1E-7 or even 1E-6.
minradrat = 1E-6;

% Min number of profiles
nprofmin = 12;

% Min layer-to-space transmittance to consider
taumin = 1.0E-3;

% Assign weight min & max
% Warning: do not weight too heavily; a small reflected thermal
% radiance may still be a significant fraction of the total
% radiance if the atmos is cold.
wmax = 2;
wmin = 1;
% Min and max for weight2 (based on thermal dBT for 250K radiance)
wmax2 = 2;
wmin2 = 0.5;

% Filename for input rdown file name
inname_rdown = '../Data/rdown_iasi_notuning';

% Filename for input refl therm data file name
inname = '../Data/thermdata_iasi';

% Name for output F-factor coefficient file name
outname = 'iasi_therm6';


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need to be modified
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Load the rdown data
eval(['load ' inname_rdown]);

rdown = rdown/1000; % convert mW to W

% Load the rtherm data
eval(['load ' inname]);

radtherm = radwiththerm - radnotherm;
% Note: this gets rid of the surface emission, so surface temp does not matter

rho = (1 - emis)/pi;

radrat = radtherm./radnotherm;


% Declare arrays for fit results
coefall = zeros(nchan,maxterm);
rmsall = -9999*ones(nchan,1);
rmsall2 = -9999*ones(nchan,1);
%
bcoefall = zeros(nchan,maxterm);
brmsall = -9999*ones(nchan,1);
brmsall2 = -9999*ones(nchan,1);



% Calc radmin
bt = btref*ones(nchan,1);
btp = bt + btpert*ones(nchan,1);
rref = 0.001*ttorad(freq,bt);
rrefpert = 0.001*ttorad(freq,btp);
radmin = rrefpert - rref;


[ii,nprofang] = size(tprof);
if (ii ~= nlay)
   ii
   nlay
   error('Unexpected number of layers in tprof');
end
ii = nprof*nang;
if (ii ~= nprofang)
   ii
   nprofang
   error('Unexpected number of profiles*angles in tprof');
end
fall = zeros(nchan,nprofang);
dbt250=zeros(nchan,nprofang);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the channels
for ic = 1:nchan

   % Determine indices of good data for the current channel
   ind = find( tauz(ic,:) > taumin & radtherm(ic,:) > radmin(ic)  & ...
      radrat(ic,:) > minradrat & secang < 2.01);
   npts = length(ind);

disp(['channel ' int2str(ic) ', points ' int2str(npts)])

   % If there are enough good data points, fit for the F-factor coefficient
   if (npts > nprofmin)

      tauzi = tauz(ic,ind);
      kzi = log(tauzi);
      freqi = freq(ic)*ones(1,npts);
      seci = secang(ind);
      nsec = length( unique(seci) );

      rd = radtherm(ic,ind);
      rdowni = rdown(ic,ind);
      xrd = rd./(rho*pi*rdowni.*tauzi);

      rad250 = ttorad(freq(ic),250);
      bt250 = 250*ones(npts,1);
      bt250t = radtot(freqi',rad250 + 1000*rd');

      % Define weight
      rmax = max(rd);
      rmin = min(rd);
      m = (wmax - wmin)/(rmax - rmin);
      b = wmax - m*rmax;
      weight = m*rd + b;

      % Define weight2
      dbt = bt250t - bt250;
      rmax = max(dbt);
      rmin = min(dbt);
      m = (wmax2 - wmin2)/(rmax - rmin);
      b = wmax2 - m*rmax;
      weight2 = m*dbt' + b; %'

      % Assign predictors for matrix A
      term1 = weight2;
      term2 = weight2./seci;
      term3 = weight2.*tauzi;
      term4 = weight2.*tauzi.^2;
      term5 = weight2.*tauzi./seci;
      term6 = weight2.*tauzi./rdowni;
%%%
%      term1 = weight2;
%      term2 = weight2./seci;
%      term3 = weight2.*kzi;
%      term4 = weight2.*kzi./seci;
%      term5 = weight2./(kzi.*seci);
%      term6 = weight2.*rdowni./seci;
%%%

      % Assign predictors for matrix B
      term1b = weight2;
      term2b = weight2./seci;
      term3b = weight2.*tauzi;
      term4b = weight2.*tauzi.^2;
      term5b = weight2.*tauzi./seci;
      term6b = weight2./(kzi.*seci);
 
      % Assign A & B matrices
      nterm = 1;
      A = [term1'];  %'
      B = [term1b']; %'
      %
      if (npts > nprofmin+6 & nsec > 1)
         nterm = 2;
         A = [A, term2'];  %'
         B = [B, term2b']; %'
         %
         if (npts > nprofmin+12)
            nterm = 3;
            A = [A, term3'];  %'
            B = [B, term3b']; %'
            %
            if (npts > nprofmin+18)
               nterm = 4;
               A = [A, term4'];  %'
               B = [B, term4b']; %'
               %
               if (npts > nprofmin+24 & nsec > 2)
                  nterm = 5;
                  A = [A, term5'];  %'
                  B = [B, term5b']; %'
                  %
                  if (npts > nprofmin+30 & nsec > 3)
                     nterm = 6;
                     A = [A, term6'];  %'
                     B = [B, term6b']; %'
                  end
               end
            end
         end
      end

      % Do regression for matrix A
      f = weight2.*xrd;
      coef = A\f'; %'
      coefall(ic,1:nterm) = coef;
      calc = A*coef;
      radthermcalc = rho*pi.*(calc./weight2').*rdowni'.*tauzi'; %'
      bt250tcalc = radtot(freqi',rad250 + 1000*radthermcalc');

      % Do regression for matrix B
      fb = weight2.*xrd;
      bcoef = B\fb'; %'
      bcoefall(ic,1:nterm) = bcoef;
      bcalc = B*bcoef;
      radthermcalcb = rho*pi.*(bcalc./weight2').*rdowni'.*tauzi'; %'
      bt250tcalcb = radtot(freqi',rad250 + 1000*radthermcalcb');

      % Calc F-factor percent error statistics
      wmean = mean(weight);
      dif = weight'.*(calc./f' - 1)./wmean;
      rmsall(ic) = 100 * sqrt(mean(dif.^2));
      dif = weight'.*(bcalc./fb' - 1)./wmean;
      brmsall(ic) = 100 * sqrt(mean(dif.^2));

      % Calc BT error statistics using bt250
      dif = bt250tcalc - bt250t;
      rmsall2(ic) = sqrt(mean(dif.^2));
      dif = bt250tcalcb - bt250t;
      brmsall2(ic) = sqrt(mean(dif.^2));

      % F-Factor
      fall(ic,ind) = xrd;

      dbt250(ic,ind)=bt250t - bt250;

   end % end of if enough npts

end % end of loop over channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write an output file
eval(['save ' outname ' coefall rmsall rmsall2 freq idchan ' ...
   ' dbt250 fall'])
%    ' bcoefall brmsall brmsall2']);

plot(fall,'.')

%%% end of program %%%
