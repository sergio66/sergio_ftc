load fc_605_2490.mat
emis = 1.0;
space = 2.73;

addpath /home/sergio/SPECTRA
%% need continuum!

copt.cvers = '6';
copt.cdir  = '/asl/data/kcarta/KCARTADATA/General/CKDieee_le/';
copt.cswt  = 1;
copt.cfwt  = 1;

xprof = prof2xprof(profN);

for ip = 1 : NP
  temps = ptemp(ip,:);
  
  absc = contcalc2_S_F(xprof,fc,ip,copt);

  xtryFWO = emis * ttorad(fc(iDo),space);
  booW  = real(squeeze(xlayodW(iDo,:,ip)));  booW(booW < 0) = 0.0;
  booF  = real(squeeze(xlayodF(iDo,:,ip)));  booF(booF < 0) = 0.0;
  booOz = real(squeeze(xlayodOz(iDo,:,ip))); booOz(booOz < 0) = 0.0;
  odX = booW + booF + booOz + absc;

  odX(isnan(odX)) = 0.0;

  nlays = profN.nlevs(ip) - 1;
  nstoplays = 100 - nlays + 1;
  for il = 100 : -1 : nstoplays
    layOD = odX(:,il);
    if temps(il) > 100
      xtryFWO = xtryFWO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));
    else
      fprintf(1,'bad radtrans temp : profile %3i lay %3i temp(lay) %3i \n',ip,il,temps(il))
    end
    %if il < 15
    %  plot(fc,xtryFWO); 
    %  title([num2str(ip) ' ' num2str(il) ' ' num2str(temps(il))]);
    %  pause(0.1)
    %end
  end
  raaxTryFWO(ip,:)  = xtryFWO;
end
