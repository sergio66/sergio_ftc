% program wrt_therm
%
% Reads in matlab refl thermal F-factor fit results and
% writes out a fortran binary data file for use with the
% fast model program.
%

% Update: 16 April 2002, Scott Hannon - updated for
%    the April 2002 productioncode package.
% Update: 15 Aug 2005, S.Hannon - modified for for MODIS; expects channels
%    should are sorted in decending order; pads with dummy zero data so
%    output file spans chans 1:maxid
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Edit this section as needed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% List of bands in order of ascending fast model channel IDs
bands={'long', 'short'};
bandpads=[ 1,      19];

% Name of input matlab data file to read
inname='modis_therm_fcoef_';


% Name of output binary fortran data file to create
outname='modis_therm_pad.dat';


% Max channel ID
maxid=36;


% Total number of channels expected
totchan=16;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The code below should not require modifications
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Open output file
fid=fopen(outname,'w','ieee-be');

% Loop over the bands
count=0;
idsofar=[];
ifm=4*(1 + 1 + 1 + 5); % 4 bytes each * (idchan, freq, layabove, & 5 coefs)
for ib=1:length(bands)
   band=char(bands(ib));
   npad=bandpads(ib);
   disp(['doing band ' band])

   % Read in the matlab data
   eval(['load ' inname band]);
   nchan=length(freq);
   count=count + nchan;

   % Make sure channel IDs are unique and sorted in descending order
   [idsort,ind]=sort(idchan,1,'descend');
   if (length(unique(idchan)) ~= nchan)
      band
      error('Some channel IDs are repeated in input matlab data file')
   end
   ibad=intersect(idchan,idsofar);
   if (length(ibad) > 0)
      ibad
       error('Some channel IDs were repeated in other bands')
   end
   idsofar=[idsofar, idchan'];

   % write out data to fortran file
   for ii=1:nchan
      ic=ind(ii);

      fwrite(fid,ifm,'integer*4');
      fwrite(fid,idchan(ic),'integer*4');
      fwrite(fid,freq(ic),'real*4');

%     Chris Barnet/Goddard version expects 0 flag for no data
%      fwrite(fid,layabove(ic),'integer*4');
      la=layabove(ic);
      if (la < 0)
         la=0;
      end
      fwrite(fid,la,'integer*4');

      fwrite(fid,coefall(ic,:),'real*4');
      fwrite(fid,ifm,'integer*4');
   end

   % Write dummy pad data to binary fortran file
   ic = idchan(ic);
   for ii=1:npad
      ic=ic - 1;
      fwrite(fid,ifm,'integer*4');
      fwrite(fid,ic,'integer*4');
      fwrite(fid,0,'real*4');
      fwrite(fid,0,'integer*4');
      fwrite(fid,zeros(1,5),'real*4');
      fwrite(fid,ifm,'integer*4');
   end


end % end of loop over bands

fclose(fid);

if (count ~= totchan)
   count
   totchan
   error('Unexpected number of channels. Check & redo')
end

disp(['finished writing data to file ' outname ])

%%% end of program %%%
