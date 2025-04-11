% program wrtcoef_n2o
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

% minn2o: minimum allowed maxn2o (skip channel if maxn2o < minn2o)
%minn2o=2E-4;
minn2o=1.5E-5;


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
if (ncoef ~= 7)
   error('Unexpected number of coefficients in coef data')
end


outname=[fname '.dat']
listname=[fname '_list.txt'];

% Open fortran binary output file
fid=fopen(outname,'w','ieee-be');

% Record size for binary fortran file
ifm=4*(1 + 1 + 7*100); % (ichan=1, fchan=1, coefs=4x100) at 4 bytes each

% Initialize index array for list file
ncount=0;
ind=zeros(nchan,1);


%%%
iok=ones(nchan,1);
%%%
%iok=zeros(nchan,1);
%
%ii=find( ichan > 1926 & ichan < 2263 );
%iok(ii)=1;
%ii=find(ichan > 2322 & ichan < 2751);
%iok(ii)=1;
%
%ii=find(ichan > 4935 & ichan < 4946);
%iok(ii)=1;
%ii=find(ichan > 4960 & ichan < 4967);
%iok(ii)=1;
%
%ii=find(ichan > 5935 & ichan < 6457);
%iok(ii)=1;
%ii=find(ichan > 7094 & ichan < 7143);
%iok(ii)=1;
%%%


% write out data to fortran file
for ic=1:nchan

%   if (maxn2o(ic) > minn2o)
   if (maxn2o(ic) > minn2o & iok(ic) == 1)
      % Write to binary fortran file
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ichan(ic),'integer*4');
      fwrite(fid,fchan(ic),'real*4');
      for il=1:100
         fwrite(fid,allcoef(ic,il,1:7),'real*4');
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
junk=[ichan(ind(1:ncount)), fchan(ind(1:ncount)), maxn2o(ind(1:ncount))];
eval(['save ' listname ' junk -ascii'])


%%% end of program %%%
