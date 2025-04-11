
emis = 1.0;
space = 2.73;
for ip = 1 : NP
  rtryW   = emis * ttorad(fc(iDo),space);
  rtryF   = emis * ttorad(fc(iDo),space);
  rtryO   = emis * ttorad(fc(iDo),space);
  rtryFW  = emis * ttorad(fc(iDo),space);
  rtryFWO = emis * ttorad(fc(iDo),space);

  temps = ptemp(ip,:);
  odFWO = squeeze(CW(iDo,:,ip)) + squeeze(CF(iDo,:,ip)) + squeeze(COz(iDo,:,ip));
  odFW  = squeeze(CW(iDo,:,ip)) + squeeze(CF(iDo,:,ip));
  odW  = squeeze(CW(iDo,:,ip));
  odF  = squeeze(CF(iDo,:,ip));
  odO  = squeeze(COz(iDo,:,ip));

  xtryFWO = emis * ttorad(fc(iDo),space);
  booW  = real(squeeze(xlayodW(iDo,:,ip)));  booW(booW < 0) = 0.0;
  booF  = real(squeeze(xlayodF(iDo,:,ip)));  booF(booF < 0) = 0.0;
  booOz = real(squeeze(xlayodOz(iDo,:,ip))); booOz(booOz < 0) = 0.0;
  odX = booW + booF + booOz;

  for il = 100 : -1 : 1
    layOD = odFWO(:,il);
    rtryFWO = rtryFWO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odFW(:,il);
    rtryFW = rtryFW .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odW(:,il);
    rtryW = rtryW .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odO(:,il);
    rtryO = rtryO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odF(:,il);
    rtryF = rtryF .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

    layOD = odX(:,il);
    xtryFWO = xtryFWO .* exp(-layOD) + ttorad(fc(iDo),temps(il)) .* (1 - exp(-layOD));

  end
  raaTryO(ip,:)  = rtryO;
  raaTryW(ip,:)  = rtryW;
  raaTryF(ip,:)  = rtryF;
  raaTryFW(ip,:)  = rtryFW;
  raaTryFWO(ip,:) = rtryFWO;

  raaxTryFWO(ip,:)  = xtryFWO;
end
