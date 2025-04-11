function xprof = prof2xprof(profN);

%% takes the input profiles and produces something for CONTINUUM calc

playsN = profN.plevs(1:100,:) - profN.plevs(2:101,:);
playsD = log(profN.plevs(1:100,:)./profN.plevs(2:101,:));

thick = abs(profN.palts(1:100,:)-profN.palts(2:101,:));    %% in m

xprof.glist = ones(length(profN.stemp),1) * 1;

xprof.thick = thick;                %% in m
xprof.mpres = playsN./playsD;       %% in mb or N/m2/100
xprof.mtemp = profN.ptemp;          %% in K
xprof.gamnt(:,:,1) = profN.gas_1;   %% in kmol/cm2

qAll = thick .* (xprof.mpres * 100) / 8.31 ./xprof.mtemp(1:100,:); %% mol/m2
qAll = qAll * 1e-4 * 6.023e23;             %% mol/m2 -> mol/cm2 -> molecules/cm2
xprof.gpart(:,:,1) = (profN.gas_1(1:100,:) ./ qAll) .* xprof.mpres;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

xprof.gamnt = xprof.gamnt(1:100,:);
xprof.mtemp = xprof.mtemp(1:100,:);

xprof.thick = flipud(xprof.thick);
xprof.mpres = flipud(xprof.mpres);
xprof.mtemp = flipud(xprof.mtemp);
xprof.gamnt = flipud(xprof.gamnt);
xprof.gpart = flipud(xprof.gpart);
xprof.nlays = profN.nlevs-1;
