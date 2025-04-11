% Program compare_set1234567o.m
%
% Read in sarta_xx and useconv spectr and compare
%

% Created: 21 Apr 2008, S.Hannon - based on comapre_set12.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

addpath /asl/matlab/h4tools
addpath /asl/matlab/rtptools

csname=input('Enter channel set name (m130x, m150, etc) : ','s');

% All channels including fake; index=ID
iset = zeros(2834,1);
id = (1:2834)'; %'
f = zeros(2834,1);
btc = zeros(2834,288);
btu = zeros(2834,288);


% Load in data for fow_long (sets 1 & 2)
load ../../List/list_set1
load ../../List/list_set2
id1 = round( list_set1(:,1) );
id2 = round( list_set2(:,1) );
clear list_set1 list_set2
iset(id1)=1;
iset(id2)=2;
%
% sarta data
hfile = [csname '_rad12.rtp'];
[head, hattr, prof, pattr] = rtpread(hfile);
nchan = head.nchan;
bt = radtot(head.vchan,prof.rcalc);
[junk,ind1,junk2] = intersect(round(head.ichan),id1);
[junk,ind2,junk2] = intersect(round(head.ichan),id2);
btc(id1,:) = bt(ind1,:);
btc(id2,:) = bt(ind2,:);
f(id1) = head.vchan(ind1);
f(id2) = head.vchan(ind2);
%
% useconv data
x=load(['../../Output_useconv/rad_' csname '_fow_long_3']);
bt = reshape(x(:,3:8),nchan,288);
btu(id1,:) = bt(ind1,:);
btu(id2,:) = bt(ind2,:);
clear x



% Load in data for fmw (set 3)
load ../../List/list_set3
id3 = round( list_set3(:,1) );
clear list_set3
iset(id3)=3;
%
% sarta data
hfile = [csname '_rad3.rtp'];
[head, hattr, prof, pattr] = rtpread(hfile);
nchan = head.nchan;
bt = radtot(head.vchan,prof.rcalc);
[junk,ind3,junk2] = intersect(round(head.ichan),id3);
btc(id3,:) = bt(ind3,:);
f(id3) = head.vchan(ind3);
%
% useconv data
x=load(['../../Output_useconv/rad_' csname '_fmw_3']);
bt = reshape(x(:,3:8),nchan,288);
btu(id3,:) = bt(ind3,:);
clear x


% Update set1 & set3 for optran
load ../../List/list_optran
ido = round( list_optran(:,1) );
iset(ido) = -iset(ido);


% Load in data for fcow (set 4)
load ../../List/list_set4
id4 = round( list_set4(:,1) );
clear list_set4
iset(id4)=4;
%
% sarta data
hfile = [csname '_rad4.rtp'];
[head, hattr, prof, pattr] = rtpread(hfile);
nchan = head.nchan;
bt = radtot(head.vchan,prof.rcalc);
[junk,ind4,junk2] = intersect(round(head.ichan),id4);
btc(id4,:) = bt(ind4,:);
f(id4) = head.vchan(ind4);
%
% useconv data
x=load(['../../Output_useconv/rad_' csname '_fcow_4']);
bt = reshape(x(:,3:8),nchan,288);
btu(id4,:) = bt(ind4,:);
clear x


% Load in data for fow_long (sets 5,6,7)
load ../../List/list_set5
load ../../List/list_set6
load ../../List/list_set7
id5 = round( list_set5(:,1) );
id6 = round( list_set6(:,1) );
id7 = round( list_set7(:,1) );
clear list_set5 list_set6 list_set7
iset(id5)=5;
iset(id6)=6;
iset(id7)=7;
%
% sarta data
hfile = [csname '_rad567.rtp'];
[head, hattr, prof, pattr] = rtpread(hfile);
nchan = head.nchan;
bt = radtot(head.vchan,prof.rcalc);
[junk,ind5,junk2] = intersect(round(head.ichan),id5);
[junk,ind6,junk2] = intersect(round(head.ichan),id6);
[junk,ind7,junk2] = intersect(round(head.ichan),id7);
btc(id5,:) = bt(ind5,:);
btc(id6,:) = bt(ind6,:);
btc(id7,:) = bt(ind7,:);
f(id5) = head.vchan(ind5);
f(id6) = head.vchan(ind6);
f(id7) = head.vchan(ind7);
%
% useconv data
x=load(['../../Output_useconv/rad_' csname '_fow_short_3']);
bt = reshape(x(:,3:8),nchan,288);
btu(id5,:) = bt(ind5,:);
btu(id6,:) = bt(ind6,:);
btu(id7,:) = bt(ind7,:);
clear x


% Clean up
clear ind1 ind2 ind3 ind4 ind5 ind6 ind7 head hattr prof pattr nchan bt


%%%
%% Subset angles to exclude angles over 60 degrees (secant=2)
%iok = find(prof.scanang < 2);
%btc = btc(:,iok);
%btu = btu(:,iok);
%clear iok
%%%

% Difference sarta BT from useconv BT
dbt = btu - btc;


% RMS error
rms = sqrt( mean(dbt.^2 ,2) );

% Mean BT spectra
btmean = mean(btu,2);

% Plot results
clf
subplot(211),plot(f,btmean,'b.'),grid
subplot(212)
ido1=intersect(ido,id1);
ido3=intersect(ido,id3);
plot(f(id1),rms(id1),'b.', f(id2),rms(id2),'g.', ...
   f(id3),rms(id3),'r.', f(id4),rms(id4),'c.', f(id5),rms(id5),'m.', ...
   f(id6),rms(id6),'y.', f(id7),rms(id7),'k.', ...
   f(ido1),rms(ido1),'bo', f(ido3),rms(ido3),'ro'),grid

junk = [id, f, btmean, rms, iset];
outname = [csname '_id_f_bt_rms_iset.txt'];
eval(['save ' outname ' junk -ascii']);

%%% end of program %%%
