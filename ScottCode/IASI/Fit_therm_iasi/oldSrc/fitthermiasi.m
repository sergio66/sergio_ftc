% Program fitthermiasi
%
% Reads in reflected thermal regression data and fits for the
% F-factor coefficients.
%

% 15 September 2003 Scott Hannon - IASI version of "fitthermband.m"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit the following variables as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Max number of layers in atmos
maxlay=100;


% Max number of terms to use in fit
maxterm=5;


% Brightness temperatures used to calc radmin (min rad to consider)
btref=220;
btpert=0.005;

% Min radtherm ratio (radtherm[only]/radnotherm) to consider.  Since
% the radiances come from real*4 values with 7 or 8 significant
% digits, and radtherm is the difference of two such radiances,
% then radtherm is untrustworthy for radrat < 1E-7 or even 1E-6.
minradrat=1E-6;


% Min number of profiles required for F-factor fit
nprofmin=12;


% Min layer-to-space transmittance to consider
taumin=1.0E-3;


% Assign limits to test best layer to use (number up from bottom)
lmin=2;
lmax=30;


% Assign weight min & max
% Warning: don't weight too heavily; a small reflected thermal
% radiance may still be a significant fraction of the total
% radiance if the atmos is cold.
wmax=2;
wmin=1;


% Input refl therm data file name
inname='../Data/thermdata_iasi';


% Output F-factor coefficient file name
outname='therm_fcoef';


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not need to be modified
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Load in the convolved data
eval(['load ' inname]);


% Assign reflectivity for thermal
rho=(1 - emis)/pi;


radtherm=radwiththerm - radnotherm;
% Note: this gets rid of the surface emission, so surface temp doesn't matter

radrat=radtherm./radnotherm;


% Declare arrays for fit results
coefall=zeros(nchan,maxterm);
rmsall=1.0E+16*ones(nchan,1);
laybestall=zeros(nchan,1);
%
bcoefall=zeros(nchan,maxterm);
brmsall=1.0E+16*ones(nchan,1);
blaybestall=zeros(nchan,1);


% Calc radmin
bt=btref*ones(nchan,1);
btp=bt + btpert*ones(nchan,1);
rref=0.001*ttorad(freq,bt);
rrefpert=0.001*ttorad(freq,btp);
radmin=rrefpert-rref;


[ii,nprofang]=size(tprof);
if (ii ~= nlay)
   ii
   nlay
   error('Unexpected number of layers in tprof');
end
ii=nprof*nang;
if (ii ~= nprofang)
   ii
   nprofang
   error('Unexpected number of profiles*angles in tprof');
end
fall=zeros(nchan,nprofang);


%%%%%%%%%%%%%%%%%%%%%%%%
% Loop over the channels
for ic=1:nchan

   % Determine indices of good data for the current channel
   ind=find( tauz(ic,:) > taumin & radtherm(ic,:) > radmin(ic) & ...
      radrat(ic,:) > minradrat );
   npts=length(ind);


disp(['channel ' int2str(ic) ', points ' int2str(npts)])

   % If there are enough good data points, fit for the F-factor coefficients
   if npts > nprofmin

      % Pull out the good data points
      tauzi=tauz(ic,ind);
      freqi=freq(ic)*ones(1,npts);
      seci=secang(ind);
      nsec=length( unique(seci) );
      %
      rd=radtherm(ic,ind);
      xrd=rd./(rho*pi*tauzi.*(1 - tauzi));
      %
      btr=zeros(maxlay,npts);
      for i=1:lmax
         btr(i,:)=0.001*ttorad(freqi,tprof(i,ind))';
      end

      %%%%%
      % Define weight
      rmax=max(rd);
      rmin=min(rd);
      m=(wmax - wmin)/(rmax - rmin);
      b=wmax - m*rmax;
      weight=m*rd + b;


      % Loop over the layers
      laybestall(ic)=1;
      for laybtx=lmin:lmax

         % Assign predictors
         btrat=btr(1,:)./btr(laybtx,:);
         term1=weight;
         term2=weight./seci;
         term3=weight.*btr(laybtx,:);
         term4=weight.*btr(laybtx,:)./seci;
         term5=weight.*btrat;
         term6=weight.*tauzi./exp( 1.67*log(tauzi) );

         % Assign A & B matrices
         nterm=1;
         A=[term1'];
         B=[term6'];
         %
         if (npts > nprofmin+6 & nsec > 1)
           A=[A, term2'];
           B=[B, term1'];
           nterm=2;
           %
           if (npts > nprofmin+12)
             A=[A, term3'];
             B=[B, term2'];
             nterm=3;
             %
             if (npts > nprofmin+18)
               A=[A, term4'];
               B=[B, term3'];
               nterm=4;
               %
               if (npts > nprofmin+24)
                 A=[A, term5'];
                 B=[B, term4'];
                 nterm=5;
               end
             end
           end
         end

         % Calc F-factor to be fit
         f=weight.*xrd./btr(laybtx,:);

         % Do the regression for the F-factor coefficients
         coef=A\f';
         calc=A*coef;
         %
         bcoef=B\f';
         bcalc=B*bcoef;
         %
         wmean=mean(weight);
         dif=weight'.*(calc./f' - 1)./wmean;
         rms=sqrt(mean(dif.^2));
         %
         if (rms < rmsall(ic))
            laybestall(ic)=laybtx;
            rmsall(ic)=rms;
            coefall(ic,1:nterm)=coef';
            %
            fall(ic,ind)=f./weight;
            %
            notused=maxterm - nterm;
            if (notused > 0)
              coefall(ic,nterm+1:maxterm)=zeros(1,notused);
            end
         end
         %
         dif=weight'.*(bcalc./f' - 1)./wmean;
         brms=sqrt(mean(dif.^2));
         %
         if (brms < brmsall(ic))
            blaybestall(ic)=laybtx;
            brmsall(ic)=brms;
            bcoefall(ic,1:nterm)=coef';
            %
            notused=maxterm - nterm;
            if (notused > 0)
              bcoefall(ic,nterm+1:maxterm)=zeros(1,notused);
            end
         end
%[laybtx,rms,brms]

      end % End loop over layers


   end % End of if enough data points

end % End loop over channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Write an output file
layabove=laybestall - 1;
blayabove=blaybestall - 1;

eval(['save ' outname ' coefall rmsall layabove freq idchan'])
%    ' bcoefall brmsall blayabove']);

plot(fall,'.')
