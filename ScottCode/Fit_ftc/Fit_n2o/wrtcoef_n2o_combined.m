% program wrtcoef_n2o_combined
%
% Reads in a matlab file of offset SO2 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 21 Dec 2007, Scott Hannon - created from wrtcoef_so2.m
% Update: 18 Apr 2008, S.Hannon - "combined" variant created
% Update: 24 Apr 2008, S.Hannon - add channel list if/else block
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Channels forced to output
idforce = [];
%idforce = [1256:1259, 1287, 1290, 1291, 2190, 2191, 2159:2161];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in n2o_long data
fnamel=input('Enter name of n2o_long coef file to read (no .mat): ','s');
eval(['load ' fnamel])
fchanl = fchan;
ichanl = round( ichan ); % exact integer
coefl = allcoef;
maxn2ol = maxn2o;
%
[nchanl, nlay, ncoef]=size(coefl);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 7)
   error('Unexpected number of coefficients in coef data')
end


% Read in n2o_short data
fnames=input('Enter name of n2o_short coef file to read (no .mat): ','s');
eval(['load ' fnames])
fchans = fchan;
ichans = round( ichan ); % exact integer
coefs = allcoef;
maxn2os = maxn2o;
clear allcoef fchan ichan maxn2o
%
[nchans, nlay, ncoef]=size(coefs);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 7)
   error('Unexpected number of coefficients in coef data')
end


% Combine long and short data sorted by channel ID
ichan = union(ichanl, ichans);
nchan = length(ichan);
fchan  = zeros(nchan, 1);
maxn2o = zeros(nchan, 1);
coef = zeros(nchan, nlay, ncoef);
[junk, indl, junk2] = intersect(ichan, ichanl);
[junk, inds, junk2] = intersect(ichan, ichans);

fchan(indl) = fchanl;
fchan(inds) = fchans;
maxn2o(indl) = maxn2ol;
maxn2o(inds) = maxn2os;
coef(indl,:,:) = coefl;
coef(inds,:,:) = coefs;
clear nchanl nchans fchanl fchans ichanl ichans maxn2ol maxn2os coefl coefs


ldone = input('Is there already a channel cut list for N2O? (0=no,1=yes): ');
if (ldone == 1)
   fname = input('Enter name of N2O list to read : ','s');
   listn2o = load(fname);
   idok = round( listn2o(:,1) );
   idok = union(idok, idforce);
   [junk, iok, junk2]  = intersect(ichan,idok);
   nchan = length(iok);
   if (nchan < length(idok))
      error('Channel list includes channels not found in combined data files')
   end
else
   clf
   plot(fchan,maxn2o,'b.'),grid
   while (ldone ~= 1)
      minn2o = input('Enter min n2o : ');
      iok = find(maxn2o > minn2o);
      idok = union(ichan(iok), idforce);
      [junk, iok, junk2]  = intersect(ichan,idok);
      nchan = length(iok);
      clf
      plot(fchan,maxn2o,'b.',fchan(iok),maxn2o(iok),'r.'),grid
      title(['minn2o=' num2str(minn2o) ', nchan=' int2str(nchan)])
      blowup
      ldone = input('Enter 0=try again or 1=finished : ');
   end
end
ichan = ichan(iok);
fchan = fchan(iok);
maxn2o = maxn2o(iok);
coef = coef(iok,:,:);

fname_out = input('Enter name of output file to create (no .dat): ','s');
outname=[fname_out '.dat']
listname=[fname_out '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 7*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);

% write out data to fortran file
for ic=1:nchan

      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,coef(ic,il,1:7),'real*4');
      end
      fwrite(fid,ifm,'integer*4');
      %
      % update list file index array
      ncount=ncount + 1;
      ind(ncount)=ic;

end

% Close fortran file
fclose(fid);

% Write list text file
junk=[ichan, fchan, maxn2o];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%
