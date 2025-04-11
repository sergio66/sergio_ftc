%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of offset SO2 for a single channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

load refprof

datafile=input('Enter name of data file : ','s');

% Min number of profiles
nprofmin=6;

% Min layer-to-space transmittance to consider
taumin=1.5E-5;


% Assign weight min & max
wmax=2;
wmin=1;

%%%%%%%
% Load in the data
eval(['load ' datafile]);

% Calc offset SO2 layer optical depth
% Note: tauz9 = tauz except for offset SO2 in layer L only
odso2=-log( tauz9 ) + log( tauz );
clear tauz
zodso2=-log( tauz9 );
clear tauz9


%%%%%%%
% Select channel
iok=0;
clf
ichan=1;
junk=squeeze( odso2(:,97,6) );
while (iok == 0)
   subplot(2,1,1), plot(1:nchan,fchan, 'bo'),grid
   axis([1 nchan min(fchan) max(fchan)])
   subplot(2,1,2), plot(1:nchan,junk,'bo', ichan,junk(ichan),'ro'),grid
   axis([ 1 nchan min(min(junk)) max(max(junk)) ])
   disp(['current ichan=' int2str(ichan)])
   ii=input('Enter channel number or -1 to end selection : ');
   if (ii >= 1 & ii <= nchan)
      ichan=ii;
   else
      iok=1;
   end
end


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


wz=cumsum(h2o);
wzref=cumsum(wref)*ones(1,nprofang);


coefall = zeros(100,4);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the layers
for il = 1:100
   disp(['layer=' int2str(il)])

   %%%%%%
   % Find good/useable data points
   lk=squeeze( odso2(ichan,il,:) );
   lkz=squeeze( zodso2(ichan,il,:) );
%   ind=find(lk > 1E-5 & lkz < 17);
   ind=find(lk > 1.5E-5 & lkz < 6.8);
   npts=length(ind);
   na=length(unique( angles(ind) ) );

   %%%
%   if (npts > 10 & na > 2)
   if (npts > 64 & na > 3)
      disp(['npts=' int2str(npts) ', nuang=' int2str(na)])
      lk=lk(ind);
      lkz=lkz(ind);


      %%%%%%%%%%%%%%
      % Make up variables with just this layers data
      %
      lang=angles(ind);
      ltr=temp(il,ind)./tref(il);
      lwzr=wz(il,ind)./wzref(il);
      %
      min_lkz=min(lkz);
      max_lkz=max(lkz);
      min_lk=min(lk);
      max_lk=max(lk);
      disp(['min_lkz=' num2str(min_lkz) ', max_lkz=' num2str(max_lkz)]);
      disp(['min_lk =' num2str(min_lk ) ', max_lk =' num2str(max_lk)]);


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


      %%%%%%%%%%%%%%%%
      % Load up A & B arrays with the predictors
      term1=lang;
      term2=ltr;
      term3=lang.*ltr;
      term4=lang.*ltr.^2;


      %%%%%%%%%%
      A=[(weight.*term1);(weight.*term2);(weight.*term3);(weight.*term4)]';%'


      %%%%%%%%%%%%%%%%%%
      % Do the fit
      %
      coef=A\wk;
      calc=A*coef;
      disp(['coef=' num2str(coef')]) %'
      coefall(il,:) = coef'; %'
      %
      rcoef=coef.*(0.9999+0.0001*rand(length(coef),1));
      rcalc=A*rcoef;


      % Plot the results
      figure(1)
      subplot(2,1,1)
      plot(lang,lk,'b+'),grid,hold
      plot(lang,calc./weight','ro',lang,rcalc./weight','go')
      hold
      title([ 'k: layer' eval([' int2str(il) ']) ]);
      subplot(2,1,2),plot(lang,lkz,'b+'),grid
      %
      figure(2)
      subplot(2,1,1)
      plot(lang,100*(wk-calc)./wk,'ro'),grid
      title([ 'Fit % error: layer' eval([' int2str(il) ']) ]);
      subplot(2,1,2)
      plot(coefall)
      pause

   %%%%%%%%%%%%%%%%%%%
   else
      disp(['Insufficent data: npts=' int2str(npts) ', nangs=', int2str(na)])
   end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
