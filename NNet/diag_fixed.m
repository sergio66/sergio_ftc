% diag_fixed.m

addpath /asl/matlib/aslutil
addpath /asl/matlib/h4tools

load fit_k_by_layer_chan49_mod_temp_ratio_newcode_sameig_alllayers

% Set these for type of data you fit.  Usually doing_k = true, rest false
doing_l2s = false;
doing_l = false;
doing_k = true;

load Inputs/profiles_704

load_fcris_hr
chi = 49;
freq = fcris_hr;

nprof = 4224;
%nprof = 2000;

ptemp = repmat(ptemp,1,6);
pwater = repmat(pwater,1,6);
pozone = repmat(pozone,1,6);
stemp = repmat(stemp,1,6);

secang = [1.00  1.19  1.41  1.68  1.99  2.37] ;
nsecang = length(secang);
s = repmat(secang,704,1);
s = reshape(s,1,704*6);


if doing_l2s
   l2s = targets(:,1:nprof);
   l2sc = ctargets(:,1:nprof);
   l2s(101,:) = 1;
   l2sc(101,:) = 1;
end

% Needed for l2s calcs
if doing_l
   tl(101,:) = 1;
   ctl(101,:) = 1;
   for i=1:101;
      l2s(i,:)  = prod(tl(i:101,1:nprof));
      l2sc(i,:) = prod(ctl(i:101,1:nprof));
   end
end

if doing_k
   for i=1:100
      if length(netfinal(i).obs) > 10
         tk(i,:) = netfinal(i).obs;
         ctk(i,:) = netfinal(i).calc;
      else
         tk(i,:) = zeros(1,4224);
         ctk(i,:) = zeros(1,4224);
      end
   end
   tl = exp(-tk);
   ctl = exp(-ctk);
   tl(101,:) = ones(1,4224);
   ctl(101,:) = ones(1,4224);
   for i=1:101;
      l2s(i,:)  = prod(tl(i:101,1:nprof));
      l2sc(i,:) = prod(ctl(i:101,1:nprof));
   end
end

if doing_k 
   l2s(isnan(l2s)) = 0;
   l2sc(isnan(l2sc)) = 0;
end
if doing_l2s
   kbad = isnan(l2s);
   l2s(kbad) = 0;
   l2sc(kbad) = 0;
end

% Now do baby RTA for testing
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

%[h,ha,p,pa]=rtpread('/asl/s1/strow/Data/Work/Rta/ftc_dev/SAF/save_SAF_704_profiles_29-Apr-2016_1100mb.op.rtp');
%lat = repmat(p.rlat,1,6);

% scatter(lat,btobs-btcal,30,s,'o')
% plot(btobs-btcal);
