% Program check_coef.m
%
% Read in coef data, look for channels with suspicious data, plot
% the data for the channel, and ask if the data for the suspicious layer
% should be zeroed out.  When done it writes out a new coef data file.
%

% Created: 02 January 2008, Scott Hannon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fname = input('Enter filename: ','s');
d = dir(fname);
if (length(d) ~= 1)
   error(['unable to read file: ' fname])
else
   outname = ['checked_' d.name];
end

disp('WARNING! may zero some high altitude CO2 channels inappropriately (eg 667.25 wn')

set = input('Enter set number (1-7, 8=CO2/SO2/HNO3, 9=optran, 10=N2O): ');
lmerged = input('Merged data? (set1-7 con or set9 avgprd) 0=no,1=yes : ');

if (set == 9)
   disp('Warning: this routine gives questionable results with OPTRAN')
end

% Read in the coef data
[ichan, fchan, coef, info] = rdcoef(set, lmerged, fname);
checkedcoef = coef;

% Open figures for plotting
figure(1)
clf
figure(2)
clf


% Loop over the breakouts
for ib=1:info.nbreakout
   gasidstr = int2str(info.gasid(ib));
   disp(['breakout ' int2str(ib) ' of ' int2str(info.nbreakout) ...
      ': gasid=' gasidstr])
   % indices of current breakout
   indc = info.startind(ib):(info.startind(ib) + info.ncoefeach(ib) - 1);

   maxcoef = squeeze(max(coef(:,:,indc),[],3));
   prevmaxcoef = zeros(info.nchan,info.nlay);
   nextmaxcoef = zeros(info.nchan,info.nlay);
   prevmaxcoef(:,2:info.nlay) = maxcoef(:,1:(info.nlay-1));
   nextmaxcoef(:,1:(info.nlay-1)) = maxcoef(:,2:info.nlay);

   % Loop over the channels
   for ic=1:info.nchan
      % Find suspicious starting coefs
      ilay = find( prevmaxcoef(ic,:) == 0 & maxcoef(ic,:) > 0.05 & ...
         nextmaxcoef(ic,:)./maxcoef(ic,:) < 0.15);
      if (length(ilay) > 0)
         chancoef = squeeze( coef(ic,:,indc) );
         figure(1)
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
%         pause(1)
%         scheck = input('Zero out indicated coefs? n or y: ','s');
%         if (strcmp(scheck,'y'))
            checkedcoef(ic,ilay,indc) = 0;
%         end
         figure(2)
         chancoef(ilay,:) = 0;
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
         pause(2)
      end
      % Find suspicious ending coefs
      ilay = find( nextmaxcoef(ic,:) == 0 & maxcoef(ic,:) > 0.05 & ...
         maxcoef(ic,:)./prevmaxcoef(ic,:) > 6.67);
      if (length(ilay) > 0)
         ilay=ilay(1);
         chancoef = squeeze( coef(ic,:,indc) );
         figure(1)
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
%         pause(1)
%         scheck = input('Zero out indicated coefs? n or y: ','s');
%         if (strcmp(scheck,'y'))
            checkedcoef(ic,ilay,indc) = 0;
%         end
         figure(2)
         chancoef(ilay,:) = 0;
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
         pause(2)
      end
   end
end

% Write out coef data to a new file
[iok] = wrtcoef(ichan, fchan, checkedcoef, info, outname);

%%% end of program %%%
