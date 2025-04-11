%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of offset CH4 for all channels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load refprof_ch4.mat

datafile=input('Enter name of CH4 data file: ','s');
bandpt=input('Enter band (eg long_pt2): ','s');

% Min number of profiles
nprofmin=6;

% Min layer-to-space transmittance to consider
taumin=5E-4;


% Assign weight min & max
wmax=2;
wmin=1;

%%%%%%%
% Load in the data
eval(['load ' datafile]);


% Clip tauz<tauz6  (tauz6 has 5% more CH4 than tauz)
iclip = find(tauz < tauz6);
tauz6(iclip)=tauz(iclip);

% Clip transmittance>1
iclip = find(tauz6 > 1.0);
tauz6(iclip) = 1.0;
tauz(iclip) = 1.0;
iclip = find(tauz > 1.0);
tauz(iclip) = 1.0;

% Clip transmittance<0
tauclip = 1E-7;
iclip = find(tauz < tauclip);
tauz(iclip) = tauclip;
tauz6(iclip) = tauclip;
iclip = find(tauz6 < tauclip);
tauz6(iclip) = tauclip;


% Calc total nominal and offset CH4 layer-to-space optical depths
% Note: tauz6 = tauz except for offset CH4
zodtot = -log( tauz );  % total layer-to-space optical depth
clear tauz
% Note: the offset CH4 is 1.05 times the nominal CH4 amount.  Thus the
% delta optical depth will be positive.
zodch4 = -log( tauz6 ) - zodtot;
clear tauz6


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


%%%
% Assign output coef array
allcoef=zeros(nchan,nlay,7);
maxch4=zeros(nchan,1);
rmsperr=zeros(nchan,nlay);
lkmean=zeros(nchan,nlay);

%%%%%%%%%%%%%%%%%%%%
% Loop over channels
for ic=1:nchan

   disp(['Doing channel ' int2str(ichan(ic)) ' = ' num2str(fchan(ic)) ' cm-1'])

   %%%%%%%%%%%%%%%%%%%%%%
   % Loop over the layers
   for il = 1:100

      ilm1 = il - 1;
      % Find good/useable data points
      if (il == 1)
         lk = squeeze( zodch4(ic,il,:) );
      else
         lk = squeeze( zodch4(ic,il,:) ) - squeeze( zodch4(ic,ilm1,:) );
      end
      lkz = squeeze( zodtot(ic,il,:) );
%      ind = find(lk > 1E-6 & lkz < 17);
      ind = find(lk > 2E-5 & lkz < 6.7);
      npts = length(ind);
      na = length(unique( angles(ind) ) );

      %%%
%      if (npts > 10 & na > 2)
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
         min_lkz=min(lkz);
         max_lkz=max(lkz);
         min_lk=min(lk);
         max_lk=max(lk);
         %
         junk=max(lk./lang'); %'
         if (junk > maxch4(ic))
            maxch4(ic)=junk;
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
         % Note: these are the same predictors I use for CO2
         term1=lang;
         term2=ltr;
         term3=lang.*ltr;
         term4=lang.*ltr.^2;
         term5=lang.^2;
         term6=ones(1,npts);
         term7=sqrt(lang);

         % Assign Predictor matrix
         if (na > 5)
            ncoef = 7;
            A=[ (weight.*term1); (weight.*term2); (weight.*term3); ...
                (weight.*term4); (weight.*term5); (weight.*term6); ...
                (weight.*term7)]'; %'
         else
            if (na > 4)
               ncoef = 6;
               A=[ (weight.*term1); (weight.*term2); (weight.*term3); ...
                   (weight.*term4); (weight.*term5); (weight.*term6)]'; %'
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
      end
   end
end

eval(['save allcoef_ch4_' bandpt ' ichan fchan maxch4 allcoef ch4_mult rmsperr lkmean'])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
