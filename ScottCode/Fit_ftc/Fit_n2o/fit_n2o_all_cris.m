%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of CrIS N2O for all channels in the input file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load refprof_n2o.mat

datafile=input('Enter name of CrIS N2O data file: ','s');
band=input('Enter band (long or short): ','s');
name=input('Enter band of data {eg cris, crishiresg4} : ','s');

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


% Clip tauz>tauz4  (tauz4 has 25% *less* N2O than tauz)
iclip = find(tauz > tauz4);
tauz4(iclip)=tauz(iclip);

% Clip transmittance>1
iclip = find(tauz4 > 1.0);
tauz4(iclip) = 1.0;
tauz(iclip) = 1.0;
iclip = find(tauz > 1.0);
tauz(iclip) = 1.0;

% Clip transmittance<0
tauclip = 1E-7;
iclip = find(tauz < tauclip);
tauz(iclip) = tauclip;
tauz4(iclip) = tauclip;
iclip = find(tauz4 < tauclip);
tauz4(iclip) = tauclip;


% Calc total nominal and offset N2O layer-to-space optical depths
% Note: tauz4 = tauz except for offset N2O
zodtot = -log( tauz );  % total layer-to-space optical depth
clear tauz
% Note: the offset N2O is 0.75 times the nominal N2O amount.  Thus the
% delta optical depth will be nagative. For fitting purposes it is
% easier to work with positive values.
% zodn2o = -log( tauz4 ) - zodtot; % correct, but is a negavite value
zodn2o = log( tauz4 ) + zodtot; % multiplied by -1 to make it positive
clear tauz4


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
maxn2o=zeros(nchan,1);
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
         lk = squeeze( zodn2o(ic,il,:) );
      else
         lk = squeeze( zodn2o(ic,il,:) ) - squeeze( zodn2o(ic,ilm1,:) );
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
         if (junk > maxn2o(ic))
            maxn2o(ic)=junk;
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
%        Extra (non-CO2) predictors below
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

outname=[name '_allcoef_n2o_' band]
eval(['save ' outname ' ichan fchan maxn2o allcoef n2o_mult rmsperr lkmean'])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
