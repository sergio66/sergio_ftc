%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of HDO for all channels in the input file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the input 'datafile' is a concatenated and height re-ordered
% set of L2S predictors for the breakouts calculated uisng kCARTA
% on the regression atmospheric profiles.
%
% Jan 2019: draft: using FW'O/FW'' breakouts where W' is water without HDO
% and W'' water with adjusted HDO (TBC)
%

%


addpath /home/chepplew/gitLib/ftc_dev/fit_hdo

% Select sensor, regression set and sub-spectral region
csens  = 'IASI';
regset = 'r49';
band   = 'all';

% Which sensors
allsens = {'CRIS_LR','CRIS_MR','CrIS_HR','AIRS_L1B','AIRS_L1C','IASI'};
csens = 'CRIS_HR';
if( ~ismember(csens,{'CRIS_LR','CRIS_MR','CRIS_HR','AIRS_L1B','AIRS_L1C','IASI'}) )
   error('Wrong instrument. Options are: cris_lr, cris_mr, cris_hr, airs_l1b, airs_l1c, iasi')
   return
end

% check choice of regression profile set
regset = upper(regset);
if( ~ismember(regset,{'R49','SAF704'}) )
  error('Incorrection option for regression profiles. Options are R49, SAF704');
  return
end

% hardwire the kCARTA run_date
run_date = 'dec2018';

% hardware the fitting production version 
run_prod = 'prod_2019';

% source and destination directories:
srcdr = ['/home/chepplew/data/sarta/' run_prod '/' lower(csens) '/' ...
         run_date '/HDO/'];
outdr = ['/home/chepplew/data/sarta/' run_prod '/' lower(csens) '/' ...
         run_date '/fitc/' lower(regset) '/'];

if(strcmp(regset,'R49'))
  switch csens
    case 'CRIS_HR'
      band     = 'all';
      datafile = [srcdr 'merged_hdo_data_r49_' band '.mat'];
      csname   = ['CRIS_HR_' regset ];
      fnout    = [outdr csname '_allcoef_hdo_' band];
    case 'AIRS_L1C'
      datafile = '';
    case 'IASI'
      datafile = [srcdr 'merged_hdo_data_r49_' band '.mat'];
      csname   = ['IASI_' regset ];
      fnout    = [outdr csname '_allcoef_hdo_' band];
  end
end

% Get the reference profile
x=load('refprof_hdo.mat');
tref = x.tref;
wref = x.wref;

% Min number of profiles
nprofmin=6;

% Min layer-to-space transmittance to consider
taumin=5E-4;

% Assign weight min & max
wmax=2;
wmin=1;

%%%%%%%
% Load in the data
load(datafile);

% Clip tauz>tauzD  (tauzD: pHDO. tauz: FW'O)
iclip = find(tauz > tauzD); numel(iclip)
tauzD(iclip)=tauz(iclip);

% QA Clip transmittance>1
iclip = find(tauzD > 1.0);  numel(iclip)
tauzD(iclip) = 1.0;
tauz(iclip)  = 1.0;
iclip = find(tauz > 1.0);   numel(iclip)
tauz(iclip) = 1.0;

% QA Clip transmittance<0
tauclip = 1E-7;
iclip = find(tauz < tauclip); numel(iclip)
tauz(iclip)  = tauclip;
tauzD(iclip) = tauclip;
iclip = find(tauzD < tauclip); numel(iclip)
tauzD(iclip) = tauclip;


% Calc total nominal and offset layer-to-space optical depths
% tauz:  path: all gases have weight 1.0 except H2O, HDO, O3 = 0.0
% tauzD: path: all gases have weight 1.0 except H2O, O3 = 0.0 and HDO = 1.0 (TBC)
% Note: tauzD = tauz except for offset HDO
zodtot = -log( tauz );                    % total layer-to-space optical depth
% Note: the offset HDO and nominal HDO amount.  
% Thus the delta optical depth will be nagative. For fitting purposes it is
% necessary to work with positive values.
% zodhdo = -log( tauzD ) - zodtot;       % correct, but is a negative value
zodhdo = log( tauzD ) + zodtot;          % multiplied by -1 to make it positive
clear tauz tauzD;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define the weighting equations:
% coef ranges...
x=ones(7,1);
x(1)=5.0E-3;
x(2)=4.0E-2;
x(3)=3.0E-1;
x(4)=1.0E+0;
x(5)=1.7E+0;
x(6)=3.0E+0;
x(7)=5.0E+0;
% ...weightings,...
y=x;
y(1)=2.0;
y(2)=3.0;
y(3)=5.0;
y(4)=7.0;
y(5)=5.0;
y(6)=3.0;
y(7)=1.0;
% ...and the linear equations connecting the above points
a=ones(6,1);
b=ones(6,1);
for I = 1:6
   a(I)=( y(I+1)-y(I) )/( x(I+1)-x(I) );
   b(I)=y(I)-a(I)*x(I);
end
clear I

%wz=cumsum(h2o);
%wzref=cumsum(wref)*ones(1,nprofang);

% Assign output coef array
allcoef = zeros(nchan,nlay,7);     % use 4 predictors for view angles 
maxdoo  = zeros(nchan,1);
rmsperr = zeros(nchan,nlay);
lkmean  = zeros(nchan,nlay);

%%%%%%%%%%%%%%%%%%%%
% Loop over channels
for ic=1:nchan
   disp(['Doing channel ' int2str(ichan(ic)) ' = ' num2str(fchan(ic)) ' cm-1'])

   %%%%%%%%%%%%%%%%%%%%%%
   % Loop over the layers
   for il = 1:100
      
      ilm1 = il - 1;
      % Find good/useable data points 
      % CLH: comment out this IF block to match fit_so2all_iasi.m
      if (il == 1)
         lk = squeeze( zodhdo(ic,il,:) );
      else
         lk = squeeze( zodhdo(ic,il,:) ) - squeeze( zodhdo(ic,ilm1,:) );
      end
      lkz = squeeze( zodtot(ic,il,:) );

      % These limits are yet to be determined
      % lk  = squeeze( zodhdo(ic,il,:) );
      % ind = find(lk > 1E-6 & lkz < 17);
      ind  = find(lk > 2E-5 & lkz < 6.7);      % this one used last
      % ind = find(abs(lk) > 2E-5 & lkz < 6.7);
      npts = length(ind);
      na   = length(unique( angles(ind) ) );

      %%%
%      if (npts > 10 & na > 2)   % less good fit for test
      if (npts > 64 & na > 3)
         lk=lk(ind);
         lkz=lkz(ind);

         %%%%%%%%%%%%%%
         % Make up variables with just this layers data
         %
         lang=angles(ind);
         ltr=temp(il,ind)./tref(il);
%         lwzr=wz(il,ind)./wzref(il);
         %
         min_lkz = min(lkz);
         max_lkz = max(lkz);
         min_lk  = min(lk);
         max_lk  = max(lk);
         %
         junk=max(lk./lang'); %'
         if (junk > maxhdo(ic))
            maxhdo(ic)=junk;
         end

         %%%%%%%%%%%%%
         % Determine the scale/weight factor for each point of the layer data
         % First scale for abs coef in current layer
         indl=find( lk <= x(1) );
         ind1=find( lk > x(1) & lk <= x(2) );
         ind2=find( lk > x(2) & lk <= x(3) );
         ind3=find( lk > x(3) & lk <= x(4) );
         ind4=find( lk > x(4) & lk <= x(5) );
         ind5=find( lk > x(5) & lk <= x(6) );
         ind6=find( lk > x(6) & lk <= x(7) );
         indh=find( lk > x(7) );
         %
         wkl=lk;
         wkl(indl)=y(1)*ones(length(indl),1);
         wkl(ind1)=a(1)*lk(ind1) + b(1);
         wkl(ind2)=a(2)*lk(ind2) + b(2);
         wkl(ind3)=a(3)*lk(ind3) + b(3);
         wkl(ind4)=a(4)*lk(ind4) + b(4);
         wkl(ind5)=a(5)*lk(ind5) + b(5);
         wkl(ind6)=a(6)*lk(ind6) + b(6);
         wkl(indh)=y(7)*ones(length(indh),1);
         %
         % Now scale for abs coef above current layer
         indl=find( lkz <= x(1) );
         ind1=find( lkz > x(1) & lkz <= x(2) );
         ind2=find( lkz > x(2) & lkz <= x(3) );
         ind3=find( lkz > x(3) & lkz <= x(4) );
         ind4=find( lkz > x(4) & lkz <= x(5) );
         ind5=find( lkz > x(5) & lkz <= x(6) );
         ind6=find( lkz > x(6) & lkz <= x(7) );
         indh=find( lkz > x(7) );
         %
         wkz=lkz;
         wkz(indl)=y(1)*ones(length(indl),1);
         wkz(ind1)=a(1)*lkz(ind1) + b(1);
         wkz(ind2)=a(2)*lkz(ind2) + b(2);
         wkz(ind3)=a(3)*lkz(ind3) + b(3);
         wkz(ind4)=a(4)*lkz(ind4) + b(4);
         wkz(ind5)=a(5)*lkz(ind5) + b(5);
         wkz(ind6)=a(6)*lkz(ind6) + b(6);
         wkz(indh)=y(7)*ones(length(indh),1);
         %
         wk=wkl.*wkz;
         weight=(wk./lk)'; %'

         % Reduce the weight of datapoints at overly large angles
         i2 = find(lang > 2.0);
         wk(i2) = 0.5*wk(i2);
         weight(i2) = 0.5*weight(i2);

         %%%%%%%%%%%%%%%%%%%
         % Assign predictors
         % Note: these are the same predictors I use for other trace gases
         term1=lang;
         term2=ltr;
         term3=lang.*ltr;
         term4=lang.*ltr.^2;
%        Extra predictors below - hopefully not needed TBD
         term5=lang.^2;
         term6=ones(1,npts);
         term7=sqrt(lang);

         % Assign Predictor matrix
	 % CLH comment out IF block for 'na' value. Force ncoef=4.
         if (na > 5)
            ncoef = 7;
            A=[ (weight.*term1); (weight.*term2); (weight.*term3); ...
                (weight.*term4); ...
		(weight.*term5); (weight.*term6); (weight.*term7)]'; %'
         else
            if (na > 4)
               ncoef = 6;
               A=[ (weight.*term1); (weight.*term2); (weight.*term3); ...
                   (weight.*term4); ...
		   (weight.*term5); (weight.*term6)]'; %'
            else
               ncoef = 4;
               A=[ (weight.*term1); (weight.*term2); (weight.*term3); ...
                   (weight.*term4)]'; %'
            end
         end

         %%%%%%%%%%%%
         % Do the fit
         %
         coef=A\wk;
         calc=A*coef;
         %
         allcoef(ic,il,1:ncoef)=coef;

         % Do statistics only on non-solar angles
         iok = find(lang < 2.0);
         if (length(iok) > 5)
            perr = 100*( (lk(iok) - calc(iok)./weight(iok)')./lk(iok) ); %'
            rmsperr(ic,il) = sqrt(mean( perr.^2 ) );
            lkmean(ic,il) = mean(lk(iok));
         else
            perr = 100*( (lk - calc./weight')./lk ); %'
            rmsperr(ic,il) = sqrt(mean( perr.^2 ) );
            lkmean(ic,il) = mean(lk);
         end
      end       % end if (npts > 64 & na > 3)
   end          % end for layers
end             % end for channel

save(fnout, 'ichan', 'fchan', 'maxhdo', 'allcoef', 'hdo_mult', 'rmsperr', 'lkmean')

% Option to write the coefficient file for use with fast model here

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fnout = [fnout '.dat']
fid     = fopen(fnout,'w','ieee-be');
% Record size for binary fortran file NB depends on ncoef.
ifm=4*(1 + 1 + 7*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);
iok=ones(nchan,1);
% write out data to fortran file
for ic=1:nchan
      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,allcoef(ic,il,1:7),'real*4');
      end
      fwrite(fid,ifm,'integer*4');
      %
      % update list file index array
      ncount=ncount + 1;
      ind(ncount)=ic;
end

% Close fortran file
fclose(fid);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
