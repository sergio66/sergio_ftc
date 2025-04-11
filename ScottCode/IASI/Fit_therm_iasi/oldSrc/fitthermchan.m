%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of reflected thermal F function for a single channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

band=input('Enter band : ','s');

% Brightness temperatures used to calc radmin (min rad to consider)
btref=220;
btpert=0.005;
bandminrad=1E-6;

% Min number of profiles
nprofmin=6;

% Min layer-to-space transmittance to consider
%taumin=1.0E-3;
taumin=5E-4;

% Assign limit on layers to test
lmax=30;

% Assign weight min & max
% Warning: don't weight too heavily; a small reflected thermal
% radiance may still be a significant fraction of the total
% radiance if the atmos is cold.
wmax=2;
wmin=1;

%%%%%%%
% Load in the convolved data
eval(['load ../Data/thermdata_b' band]);
%
radtherm=radwiththerm - radnotherm;
% Note: this gets rid of the surface emission, so surface temp doesn't matter

rho=(1 - emis)/pi;

whos


%%%%%%%
% Pull out indices of angles nearest to nominal ref therm angle sec=1.68
%ind168=find(secang > 1.67 & secang < 1.69);

coefall=zeros(nchan,5);
rmsall=1.0E+16*ones(nchan,1);
laybestall=zeros(nchan,1);

% Calc radmin
bt=btref*ones(nchan,1);
btp=bt + btpert*ones(nchan,1);
rref=ttorad(freq,bt);
rrefpert=ttorad(freq,btp);
radmin=rrefpert-rref;

[nlay,nprofang]=size(tprof);
tjunk=cumsum(tprof)./cumsum( ones(nlay,nprofang) );


% Select channel
subplot(2,1,1),plot(1:nchan,freq, 'bo'),grid
axis([1 nchan min(freq) max(freq)])
ind=1:nang:nprof*nang;
subplot(2,1,2),plot(1:nchan,tauz(:,ind)),grid
axis([ 1 nchan min(min(tauz(:,ind))) max(max(tauz(:,ind))) ])
ichan=input('Enter channel number: ');
clf


   ichan
   %
   % Pull out the data for this channel
   ind=find( tauz(ichan,:) > taumin & radtherm(ichan,:) > radmin(ichan)  & ...
      radtherm(ichan,:) > bandminrad);
   npts=length(ind)
   %
   if npts > nprofmin
      tauzi=tauz(ichan,ind);
      freqi=freq(ichan)*ones(1,npts);
      seci=secang(ind);
      %
      % Pull out tauz data for angles secant = 1.68 (optimal F therm angle)
%      tauz168i=tauz(ichan,ind168(ind));
      %
      freq(ichan)
      x=1:npts;
      %
      rd=radtherm(ichan,ind);
      xrd=rd./(rho*pi*tauzi.*(1 - tauzi));
      %
      btr=zeros(lmax,npts);
      bjtr=zeros(lmax,npts);
      bref=zeros(lmax);
      for i=1:lmax
         btr(i,:)=ttorad(freqi',tprof(i,ind)')';
         bjtr(i,:)=ttorad(freqi',tjunk(i,ind)')';
         bref(i)=ttorad(freq(ichan),tref(i));
      end
      %
      %%%%%
      % Define weight
      rmax=max(rd);
      rmin=min(rd);
      m=(wmax - wmin)/(rmax - rmin);
      b=wmax - m*rmax;
      weight=m*rd + b;
      %
      %%%%%
      % Loop over the layers
      laybestall(ichan)=1;
      %
      for laybtx=1:lmax
         %
         % Calc some layer dependent predictors
         btrat=btr(1,:)./btr(laybtx,:);
         %
         term1=weight;
         term2=weight./seci;
         term3=weight.*btr(laybtx,:);
         term3b=weight.*bjtr(laybtx,:);
         term4=weight.*btr(laybtx,:)./seci;
         term4b=weight.*bjtr(laybtx,:)./seci;
         term5=weight.*btrat;
%         term5b=weight.*(tjunk(laybtx,:) - trefjunk(laybtx));
%         term5b=weight.*(bjtr(laybtx,:) - bref(laybtx)).*bjtr(laybtx,:);
%         term5b=weight.*bjtr(1,:)./bjtr(laybtx,:);
         term6=weight.*tauzi./exp( 1.67*log(tauzi) );
%         term7=weight.*tauzi./tauz168i );
         %
         nterm=1;
         A=[term1'];
%         B=[term6'];
         B=[term3b'];
         %
         if (npts > 12)
           A=[A, term2'];
           B=[B, term1'];
           nterm=2;
           %
           if (npts > 18)
             A=[A, term3'];
             B=[B, term2'];
             nterm=3;
             %
             if (npts > 24)
               A=[A, term4'];
%               B=[B, term3b'];
               B=[B, term3'];
               nterm=4;
               %
               if (npts > 30)
                 A=[A, term5'];
%                 B=[B, term4b'];
                 B=[B, term4'];
                 nterm=5;
                 %
%                 if (npts > 36)
%                   A=[A, term6'];
%                   B=[B, term5'];
%                   nterm=6;
%                 end
               end
             end
           end
         end
         %
         f=weight.*xrd./btr(laybtx,:);
         coef=A\f'
         calc=A*coef;
         %
%         fb=weight.*xrd./bjtr(laybtx,:);
%         bcoef=B\fb'
         bcoef=B\f'
         bcalc=B*bcoef;
         %
         plot(x,100*(calc-f')./f','ro'),grid,hold
         plot(x,100*(bcalc-f')./f','cx')
         title(['% error, laybtx=' int2str(laybtx)]);
         %
         wmean=mean(weight);
         dif=weight'.*(calc./f' - 1)./wmean;
         rms=sqrt(mean(dif.^2))
         %
         bdif=weight'.*(bcalc./f' - 1)./wmean;
         brms=sqrt(mean(bdif.^2))
         %
         plot(x,100*rms*ones(1,npts),'m--',x,100*brms*ones(1,npts),'b--')
         plot(x,-100*rms*ones(1,npts),'m--',x,-100*brms*ones(1,npts),'b--')
         %
         hold
         laybtx
         pause
         %
      end
      %%%%%
      % End loop over layers
   end
   %
