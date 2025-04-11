% Read in rdown RTP and pull out and re-order data for use
% in the Rtherm regression.  The data needs to end up in
% order:  prof1_ang1 prof1_ang2 ... prod48_ang5 prof48_ang6

% Created: 22 April 2008, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear

nprof = 48;
nang = 6;
comment='downward thermal emission radiance from fast model (wcon tuning only)';

hfile = input('Enter name of rdown RTP file : ','s');
[head, hattr, prof, pattr] = rtpread(hfile);


% Check scanang 1:nprof is constant
if (length(unique(prof.scanang(1:nprof))) ~= 1)
   error('Unexpected order of RTP profile/angle data')
end


% Create re-ordering indices
ind = zeros(1,nang*nprof);
indp = 1:nprof;
indpx = 0:nang:round((nang*nprof)-1); % exact integers
for ii=1:nang
   ip = indp + round((ii-1)*nprof);
   ipx = indpx + ii;
   ind(ipx) = ip;
end

% Pull out rdown and secang
rdown = prof.rcalc(:,ind);
secang = prof.scanang(ind);

fout=input('Enter name of rdown mat file to create : ','s');
eval(['save ' fout ' rdown secang comment']);

%%% end of program %%%
