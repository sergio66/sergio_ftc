% Program check_coef.m
%
% Read in coef data, look for channels with suspicious data, plot
% the data for the channel, and ask if the data for the suspicious layer
% should be zeroed out.  When done it writes out a new coef data file.
%

% Created: 02 January 2008, Scott Hannon
% Update: 07 Jan 2008, S.Hannon - plot corrected as well as uncorrected data
% Update: 08 Jan 2008, S.Hannon - repeat coefs 3rd from last layer for last
%    2 layers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fname = input('Enter filename: ','s');
d = dir(fname);
if (length(d) ~= 1)
   error(['unable to read file: ' fname])
else
   outname = ['check2_' d.name];
end

set = input('Enter set number (1-7, 8=CO2, 9=optran): ');
lmerged = input('Merged data? (set1-7 con or set9 avgprd) 0=no,1=yes : ');
iaskplot = input('Plot changes? (0=no, 1=yes): ');

% Read in the coef data
[ichan, fchan, coef, info] = rdcoef(set, lmerged, fname);
checkedcoef = coef;

nchan = info.nchan;
nlay = info.nlay;

if (iaskplot == 1)
   % Open figures for plotting
   figure(1)
   clf
   figure(2)
   clf
end

% Loop over the breakouts
for ib=1:info.nbreakout
   gasidstr = int2str(info.gasid(ib));
   disp(['breakout ' int2str(ib) ' of ' int2str(info.nbreakout) ...
      ': gasid=' gasidstr])
   % indices of current breakout
   indc = info.startind(ib):(info.startind(ib) + info.ncoefeach(ib) - 1);

   maxcoef = squeeze(max(coef(:,:,indc),[],3));
   prevmaxcoef = zeros(nchan,nlay);
   nextmaxcoef = zeros(nchan,nlay);
   prevmaxcoef(:,2:nlay) = maxcoef(:,1:(nlay-1));
   nextmaxcoef(:,1:(nlay-1)) = maxcoef(:,2:nlay);

   % Loop over the channels
   for ic=1:info.nchan

      % Find suspicious starting coefs
%%%
%      ilay = find( prevmaxcoef(ic,:) == 0 & maxcoef(ic,:) > 0.05 & ...
%         nextmaxcoef(ic,:)./maxcoef(ic,:) < 0.15);
%%%
      ilay = find( prevmaxcoef(ic,:) == 0 & maxcoef(ic,:) > 0.08 & ...
         nextmaxcoef(ic,:)./maxcoef(ic,:) < 0.1);
      if (length(ilay) > 0)
         chancoef = squeeze( coef(ic,:,indc) );

if (iaskplot == 1)
         figure(1)
         plot( 1:nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	 ',lay=' int2str(ilay)]);
         figure(2)
         chancoef(ilay,:) = 0;
         plot( 1:nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
         pause(1)
end
%%%
%         scheck = input('Zero out indicated coefs? n or y: ','s');
%         if (strcmp(scheck,'y') | strcmp(scheck,'Y'))
            checkedcoef(ic,ilay,indc) = 0;
%         else
%	    disp('NOT corrected')
%            pause(1)
%         end
%%%
      end

      % Find suspicious ending coefs
%%%
%      ilay = find( nextmaxcoef(ic,:) == 0 & maxcoef(ic,:) > 0.05 & ...
%         maxcoef(ic,:)./prevmaxcoef(ic,:) > 6.67);
%      if (length(ilay) > 0)
%         ilay=ilay(1);
%         chancoef = squeeze( coef(ic,:,indc) );
%         figure(1)
%         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
%         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
%	 ',lay=' int2str(ilay)]);
%         figure(2)
%         chancoef(ilay,:) = 0;
%         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
%         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
%	    ',lay=' int2str(ilay)]);
%         pause(1)
%         scheck = input('Zero out indicated coefs? n or y: ','s');
%         if (strcmp(scheck,'y') | strcmp(scheck,'Y'))
%            checkedcoef(ic,ilay,indc) = 0;
%         else
%	    disp('NOT corrected')
%            pause(1)
%         end
%      end
%%%
      % Find coefs that do not reach nlay
      ilaylast = max( find(maxcoef(ic,:) > 0) );
      if (ilaylast < nlay & ilaylast > 5)
         chancoef = squeeze( coef(ic,:,indc) );
         iguess = ilaylast - 2;
         if (set == 9)
            ilast = min([ilaylast+4,nlay]);
         else
	    ilast = ilaylast+1;
         end
         for ilay=(iguess+1):ilast
            checkedcoef(ic,ilay,indc)=checkedcoef(ic,iguess,indc);
         end
if (iaskplot == 1)
         figure(1)
         plot( 1:nlay,chancoef )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ' before'])
         figure(2)
         chancoef = squeeze( checkedcoef(ic,:,indc) );
         plot( 1:nlay,chancoef)
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ' after'])
         pause(1)
      end
end
%%%

   end
end

% Write out coef data to a new file
[iok] = wrtcoef(ichan, fchan, checkedcoef, info, outname);

%%% end of program %%%
