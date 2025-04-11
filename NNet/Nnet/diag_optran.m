%load fixed_net_optran_chan1099_v1_fitallchans.mat
%load fixed_net_optran_chan1099_v1_fitallchans_jan21_test
%load net_optran_chan1099_v1_fitallchans.mat

regr49 = true;

if regr49
   load optran_chan1099_v1_regr49_jan23test
   load Inputs/profiles_49reg
   nprof = 49*6;
else
   load optran_chan1099_v1_jan23test
   load Inputs/profiles_704
   nprof = 704*6;
end

load_fcris_hr
chi = 1099;
freq = fcris_hr;

ptemp = repmat(ptemp,1,6);
pwater = repmat(pwater,1,6);
pozone = repmat(pozone,1,6);
stemp = repmat(stemp,1,6);

secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
nsecang = length(secang);
s = repmat(secang,704,1);
s = reshape(s,1,704*6);

istart = 8;
iend = 287;

kcall = nan(300,nprof);
koall = nan(300,nprof);
for i=1:300
   if size(netfinal(i).net) > 0
      kcall(i,:) = netfinal(i).calc;
      koall(i,:) = netfinal(i).obs;
   end      
%    kcall(i,netfinal(i).k) = netfinal(i).calc;
%    koall(i,netfinal(i).k) = netfinal(i).obs;
end

k1 = ~isnan(prof_optran_index);
k2 = ~isnan(koall);
kx = and(k1,k2);

for i=1:nprof
   tk(:,i) = interp1(prof_optran_index(kx(:,i),i),koall(kx(:,i),i),1:100,'linear');
   ctk(:,i) = interp1(prof_optran_index(kx(:,i),i),kcall(kx(:,i),i),1:100,'linear');
end

tk(isnan(tk)) = 0;
ctk(isnan(ctk)) = 0;
ctk(ctk < 0 ) = 0;
tl = exp(-tk);
ctl = exp(-ctk);
tl(101,:) = 1;
ctl(101,:) = 1;
for i=1:101;
   l2s(i,:)  = prod(tl(i:101,:));
   l2sc(i,:) = prod(ctl(i:101,:));
end

for i=1:nprof
   tsurf = stemp(i);
   arad(i) = 0;
   carad(i) = 0;
   sfcemit(i) = bt2rad(freq(chi),tsurf)*l2s(1,i);
   csfcemit(i) = bt2rad(freq(chi),tsurf)*l2sc(1,i);
   for l = 1:100
      arad(i) =  arad(i)  + bt2rad(freq(chi),ptemp(l,i)) .* (l2s(l+1,i) - l2s(l,i));
      carad(i) = carad(i) + bt2rad(freq(chi),ptemp(l,i)) .* (l2sc(l+1,i) - l2sc(l,i));
   end
   rad = sfcemit + arad;
   crad = csfcemit + carad;
end

% Convert to observed BT
btobs = rad2bt(freq(chi),rad);
btcal = rad2bt(freq(chi),crad);

% Get latitudes that go with the 704*6 profile set
load Inputs/lat_4224set

if regr49
   ig = netfit_kselect;
else
   ig = netfinal(40).kselect
end

big = setdiff(1:nprof,ig);
figure;
[b,i] = sort(ig);
plot(b,btobs(i)-btcal(i),'+-')
hold on;
plot(big,btobs(big)-btcal(big),'-o')
hl = legend('Dependent','Independent')

nanstd(btobs-btcal)
nanstd(btobs(ig)-btcal(ig))
nanstd(btobs(big)-btcal(big))

