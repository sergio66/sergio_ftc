% program wrtcoef_hno3
%
% Reads in a matlab file of offset HNO3 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 21 Dec 2007, Scott Hannon - created from wrtcoef_so2.m
% Update: 25 Apr 2008, S.Hannon - add if/else block for channel list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% minhno3: minimum allowed maxhno3 (skip channel if maxhno3 < minhno3)
%minhno3=2E-4;
minhno3=1.5E-5;

% Channels forced to output
%idforce = [];
idforce = [378 379];

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

csname = input('Enter channel set name (m130x, m150, etc) : ','s');

% Read in matlab data
fname = [csname '_allcoef_hno3_long.mat'];
load(fname);

[nchan, nlay, ncoef]=size(allcoef);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 4)
   error('Unexpected number of coefficients in coef data')
end


outname=['hno3_' csname '.dat']
listname=['list_hno3_' csname];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 4*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each


% Decide which channels to write to binary file
lyn = input('Cut channels using a channel list? (0=no,1=yes) : ');
if (lyn == 1)
   fname = input('Enter name of list file to read : ','s');
   listhno3 = load(fname);
   idw = round( listhno3(:,1) ); % exact integer
   idw = union(idw,idforce);
   [junk, iw, junk2] = intersect(ichan, idw);
else
   iw = find(maxhno3 > minhno3);
   idw = union( ichan(iw), idforce);
   [junk, iw, junk2] = intersect(ichan, idw);
end
nchan = length(iw);
fchan = fchan(iw);
ichan = ichan(iw);
maxhno3 = maxhno3(iw);
allcoef = allcoef(iw,:,:);


% write out data to fortran file
for ic=1:nchan

   % Write to binary fortran file
   fwrite(fid,ifm,'integer*4');
   fwrite(fid,ichan(ic),'integer*4');
   fwrite(fid,fchan(ic),'real*4');
   for il=1:100
      fwrite(fid,allcoef(ic,il,1:4),'real*4');
   end
   fwrite(fid,ifm,'integer*4');

end


% Close fortran file
fclose(fid);

% Write list text file
junk=[ichan, fchan, maxhno3];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%
