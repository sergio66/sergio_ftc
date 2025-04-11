% program wrtcoef_so2_combined
%
% Reads in a matlab file of offset SO2 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 25 April 2003, Scott Hannon
% Update: 18 Apr 2008, S.Hannon - "combined" variant created
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in so2_long coef data
fnamel=input('Enter name of so2_long coef file to read (no .mat): ','s');
eval(['load ' fnamel])
fchanl = fchan;
ichanl = round( ichan ); % exact integer
coefl = allcoef;
maxso2l = maxso2;
%
[nchanl, nlay, ncoef]=size(coefl);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 4)
   error('Unexpected number of coefficients in coef data')
end


% Read in so2_short coef data
fnames=input('Enter name of so2_short coef file to read (no .mat): ','s');
eval(['load ' fnames])
fchans = fchan;
ichans = round( ichan ); % exact integer
coefs = allcoef;
maxso2s = maxso2;
clear fchan ichan allcoef
%
[nchans, nlay, ncoef]=size(coefs);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 4)
   error('Unexpected number of coefficients in coef data')
end


% Combine long and short data sorted by channel ID
ichan = union(ichanl, ichans);
nchan = length(ichan);
fchan  = zeros(nchan, 1);
maxso2 = zeros(nchan, 1);
coef = zeros(nchan, nlay, ncoef);
[junk, indl, junk2] = intersect(ichan, ichanl);
[junk, inds, junk2] = intersect(ichan, ichans);

fchan(indl) = fchanl;
fchan(inds) = fchans;
maxso2(indl) = maxso2l;
maxso2(inds) = maxso2s;
coef(indl,:,:) = coefl;
coef(inds,:,:) = coefs;
clear nchanl nchans fchanl fchans ichanl ichans maxso2l maxso2s coefl coefs

clf
plot(fchan,maxso2,'b.'),grid
ldone = 0;
while (ldone ~= 1)
   minso2 = input('Enter min so2 : ');
   iok = find(maxso2 > minso2);
   nchan = length(iok);
   clf
   plot(fchan,maxso2,'b.',fchan(iok),maxso2(iok),'r.'),grid
   title(['minso2=' num2str(minso2) ', nchan=' int2str(nchan)])
   blowup
   ldone = input('Enter 0=try again or 1=finished : ');
end
ichan = ichan(iok);
fchan = fchan(iok);
maxso2 = maxso2(iok);
coef = coef(iok,:,:);

fname_out = input('Enter name of output file to create (no .dat): ','s');
outname=[fname_out '.dat']
listname=[fname_out '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 4*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

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
         fwrite(fid,coef(ic,il,1:4),'real*4');
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
junk=[ichan, fchan, maxso2];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%
