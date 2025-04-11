% program wrtcoef_hno3
%
% Reads in a matlab file of offset SO2 coeffiencents and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Created: 21 Dec 2007, Scott Hannon - created from wrtcoef_so2.m
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% minhno3: minimum allowed maxhno3 (skip channel if maxhno3 < minhno3)
%minhno3=2E-4;
minhno3=1.5E-5;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Read in matlab data
fname=input('Enter name of coef file to read (no .mat): ','s');
eval(['load ' fname])

[nchan, nlay, ncoef]=size(allcoef);
if (nlay ~= 100)
   error('Unexpected number of layers in coef data')
end
if (ncoef ~= 4)
   error('Unexpected number of coefficients in coef data')
end


outname=[fname '.dat']
listname=[fname '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 4*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);


%%%
%iok=ones(nchan,1);
%%%
iok=zeros(nchan,1);
ii=find( ichan > 4 & ichan < 11 );
iok(ii)=1;
ii=find(ichan > 466 & ichan < 476);
iok(ii)=1;
ii=find(ichan > 817 & ichan < 1110);
iok(ii)=1;
ii=find(ichan > 2517 & ichan < 2840);
iok(ii)=1;
ii=find(ichan > 4105 & ichan < 5396);
iok(ii)=1;
%%%


% write out data to fortran file
for ic=1:nchan

%   if (maxhno3(ic) > minhno3)
   if (maxhno3(ic) > minhno3 & iok(ic) == 1)
      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,allcoef(ic,il,1:4),'real*4');
      end
      fwrite(fid,ifm,'integer*4');
      %
      % update list file index array
      ncount=ncount + 1;
      ind(ncount)=ic;
   end

end

% Close fortran file
fclose(fid);

% Write list text file
junk=[ichan(ind(1:ncount)), fchan(ind(1:ncount)), maxhno3(ind(1:ncount))];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%
