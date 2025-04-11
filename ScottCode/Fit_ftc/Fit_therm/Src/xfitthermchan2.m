%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do a fit of reflected thermal F function for a single channel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% version2 expects input rdown was NOT multipled by tauz

band=input('Enter band : ','s');
%band='long'

load ../Data/rdown2
if (strcmp(band,'short'))
   rdown = rdown(1865:2378,:);
end
rdown = rdown/1000; % undo wrtrtp_sarta.f multiply by 1000

% Brightness temperatures used to calc radmin (min rad to consider)
btref=220;
btpert=0.005;
bandminrad=1E-6;

% Min number of profiles
nprofmin=12;

% Min layer-to-space transmittance to consider
taumin=1.0E-3;
%taumin=5E-4;

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
eval(['load ../Data/thermdata_' band]);
%
radtherm=radwiththerm - radnotherm;
% Note: this gets rid of the surface emission, so surface temp doesn't matter

rho=(1 - emis)/pi;

whos


%%%%%%%
coefall=zeros(nchan,5);
rmsall=1.0E+16*ones(nchan,1);
laybestall=zeros(nchan,1);

% Calc radmin
bt=btref*ones(nchan,1);
btp=bt + btpert*ones(nchan,1);
rref=0.001*ttorad(freq,bt);
rrefpert=0.001*ttorad(freq,btp);
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
%   ind=find( tauz(ichan,:) > taumin & radtherm(ichan,:) > radmin(ichan)  & ...
%      radtherm(ichan,:) > bandminrad);
   ind=find( tauz(ichan,:) > taumin & radtherm(ichan,:) > radmin(ichan)  & ...
      radtherm(ichan,:) > bandminrad & secang < 2.01);
   npts=length(ind)
   %
   if npts > nprofmin
      tauzi=tauz(ichan,ind);
      freqi=freq(ichan)*ones(1,npts);
      seci=secang(ind);
      radthermi=radtherm(ichan,ind);
      %
      % Pull out tauz data for angles secant = 1.68 (optimal F therm angle)
%      tauz168i=tauz(ichan,ind168(ind));
      %
      freq(ichan)
      x=1:npts;
      %
      rad250=ttorad(freq(ichan),250);
      bt250=250*ones(npts,1);
      bt250t=radtot(freqi',rad250 + 1000*radthermi');
      %
      rd=radtherm(ichan,ind);
      xrd=rd./(rho*pi*tauzi.*(1 - tauzi));
      %
      xrdb=rd./(rho*pi*rdown(ichan,ind).*tauzi);
      rdowni=rdown(ichan,ind);
      %
      btr=zeros(lmax,npts);
      for i=1:lmax
         btr(i,:)=0.001*ttorad(freqi',tprof(i,ind)')';
      end
      %
      %%%%%
      % Define weight
      rmax=max(rd);
      rmin=min(rd);
      m=(wmax - wmin)/(rmax - rmin);
      b=wmax - m*rmax;
      weight=m*rd + b;
      %%%
      dbt = bt250t - bt250;
      rmax = max(dbt);
      rmin = min(dbt);
      m = (wmax - wmin)/(rmax - rmin);
      b = wmax - m*rmax;
      weight2 = m*dbt' + b;
      %
      %%%%%
      % Loop over the layers
      laybestall(ichan)=1;
      %
      for laybtx=1:lmax
%      for laybtx=2:2
         %
         % Calc some layer dependent predictors
         btrat=btr(1,:)./btr(laybtx,:);
         %
         kzi=log(tauzi);
         %
         term1=weight;
         term2=weight./seci;
         term3=weight.*btr(laybtx,:);
         term4=weight.*btr(laybtx,:)./seci;
         term5=weight.*btrat;
         %
%%%
         term1b=weight2;
         term2b=weight2./seci;
         term3b=weight2.*tauzi;
         term4b=weight2.*tauzi.^2;
         term5b=weight2.*tauzi./seci;
         term6b=weight2./(kzi.*seci);
%%%
%         term1b=weight2;
%         term2b=weight2./seci;
%         term3b=weight2.*kzi;
%         term4b=weight2.*kzi./seci;
%         term5b=weight2./(kzi.*seci);
%         term6b=weight2.*rdowni./seci;
%%%
         %
%%%
%         term1c=weight2;
%         term2c=weight2./seci;
%         term3c=weight2.*kzi;
%         term4c=weight2.*kzi./seci;
%         term5c=weight2./(kzi.*seci);
%         term6c=weight2.*rdowni./seci;
%%%
         term1c=weight2;
         term2c=weight2./seci;
         term3c=weight2.*tauzi;
         term4c=weight2.*tauzi.^2;
         term5c=weight2.*tauzi./seci;
         term6c=weight2.*tauzi./rdowni;
%%         term6c=weight2./sqrt(1-tauzi);
%%         term6c=weight2./sqrt(-seci.*kzi);
%%         term6c=weight2./(kzi.*seci);
%%%
         %
         nterm=1;
         A=[term1'];
         B=[term1b'];
         C=[term1c'];
         %
         if (npts > 12)
           A=[A, term2'];
           B=[B, term2b'];
           C=[C, term2c'];
           nterm=2;
           %
           if (npts > 18)
             A=[A, term3'];
             B=[B, term3b'];
             C=[C, term3c'];
             nterm=3;
             %
             if (npts > 24)
               A=[A, term4'];
               B=[B, term4b'];
               C=[C, term4c'];
               nterm=4;
               %
               if (npts > 30)
                 A=[A, term5'];
                 B=[B, term5b'];
                 C=[C, term5c'];
                 nterm=5;
                 if (npts > 36)
                   B=[B, term6b'];
                   C=[C, term6c'];
                   nterm=6;
                 end
               end
             end
           end
         end
         %
         f=weight.*xrd./btr(laybtx,:);
         coef=A\f'
         calc=A*coef;
         radthermcalc=rho*pi.*(calc'./weight).*btr(laybtx,:).*tauzi.*(1-tauzi);
         bt250tcalc=radtot(freqi',rad250 + 1000*radthermcalc);
         %
%%%
%junk = xrdb;
%ilo = find(junk < 0.5);
%ihi = find(junk > 2.0);
%junk(ilo) = 0.5;
%junk(ihi) = 2.0;
%xrdb = junk;
%%%
         fb=weight2.*xrdb;
         bcoef=B\fb'
         bcalc=B*bcoef;
         radthermcalcb=rho*pi.*(bcalc./weight2').*rdown(ichan,ind)'.*tauzi';
         bt250tcalcb=radtot(freqi',rad250 + 1000*radthermcalcb');
         %
         fc=weight2.*xrdb;
         ccoef=C\fc'
         ccalc=C*ccoef;
         radthermcalcc=rho*pi.*(ccalc./weight2').*rdown(ichan,ind)'.*tauzi';
         bt250tcalcc=radtot(freqi',rad250 + 1000*radthermcalcc');
         %
         clf
         subplot(311)
         hold
         plot(x,100*(calc-f')./f','bo'),grid
         plot(x,100*(bcalc-fb')./fb','rx')
         plot(x,100*(ccalc-fb')./fb','c+')
         title(['F factor % error, laybtx=' int2str(laybtx)]);
         %
         wmean=mean(weight);
         dif=weight'.*(calc./f' - 1)./wmean;
         rms=sqrt(mean(dif.^2))
         %
         wmean=mean(weight2);
         bdif=weight2'.*(bcalc./fb' - 1)./wmean;
         brms=sqrt(mean(bdif.^2))
         %
         cdif=weight2'.*(ccalc./fb' - 1)./wmean;
         crms=sqrt(mean(cdif.^2))
         %
         plot(x,100*rms*ones(1,npts),'b--',x,100*brms*ones(1,npts),'r--')
         plot(x,-100*rms*ones(1,npts),'b--',x,-100*brms*ones(1,npts),'r--')
         hold
         %
         %
         subplot(312)
         plot(x,bt250t,'bo'),grid
         title('Thermal BT contribution at 250K and emis=0.85')
         %
         subplot(313)
         plot(x,bt250tcalc-bt250t,'bo', ...
            x,bt250tcalcb-bt250t,'rx',x,bt250tcalcc-bt250t,'c+'),grid
         title('BT error')
         %
         laybtx
         pause
      end
      %%%%%
      % End loop over layers
   end
   %
