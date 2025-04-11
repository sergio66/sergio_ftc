% Program check_coef2.m
%
% Read in coef data, look for channels with suspicious data, plot
% the data for the channel, and ask if the data for the suspicious layer
% should be zeroed out.  When done it writes out a new coef data file.
%

% Created: 02 January 2008, Scott Hannon
% Update: 07 Jan 2008, S.Hannon - plot corrected as well as uncorrected data
% Update: 15 Apr 2008, S.Hannon - version2 created with repeat option
% Update: 08 Jun 2011, S.Hannon - add set 10 for 4 coefs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fname = input('Enter coef filename: ','s');
d = dir(fname);
if (length(d) ~= 1)
   error(['unable to read file: ' fname])
else
   outname = ['checked_' d.name];
end

set = input('Enter set number (1-7, 8=CO2, 9=optran, 10=pert4coef) : ');
lmerged = input('Merged data? (set1-7 con or set9 avgprd) 0=no,1=yes : ');

% Read in the coef data
if (set < 10)
   [ichan, fchan, coef, info] = rdcoef(set, lmerged, fname);
else
   [ichan, fchan, coef] = rdcoef_pert4(fname);
   info.set = 10;
   info.nbreakout = 1;
   info.gasid = 11; % NH3
   info.ncoef = 4;
   info.ncoefeach = 4;
   info.startind = 1;
   info.nlay = 100;
   info.nchan = length(ichan);
end
% Copy input coef to output coef
checkedcoef = coef;


% Open figure for plotting
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

disp('looking for suspicioius coefs')

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
         figure(2)
%         chancoef(ilay,:) = 0;
         chancoef(ilay,:) = chancoef(ilay+1,:);
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
         pause(1)
         scheck = input('Action: n=none, 0=zero, r=repeat(next): ','s');
         if (strcmp(scheck,'0'))
            checkedcoef(ic,ilay,indc) = 0;
         else
            if (strcmp(scheck,'r') & ilay > 1 & ilay < nlay)
               checkedcoef(ic,ilay,indc) = checkedcoef(ic,ilay+1,indc);
            else
               disp('NOT corrected')
               pause(1)
            end
         end
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
         figure(2)
%         chancoef(ilay,:) = 0;
         chancoef(ilay,:) = chancoef(ilay-1,:);
         plot( 1:info.nlay,chancoef,ilay,0,'k*' )
         title(['gasid=' gasidstr ', ichan=' int2str(ichan(ic)) ...
	    ',lay=' int2str(ilay)]);
         pause(1)
         scheck = input('Action: n=none, 0=zero, r=repeat(prev): ','s');
         if (strcmp(scheck,'0'))
            checkedcoef(ic,ilay,indc) = 0;
         else
            if (strcmp(scheck,'r') & ilay > 1)
               checkedcoef(ic,ilay,indc) = checkedcoef(ic,ilay-1,indc);
            else
               disp('NOT corrected')
               pause(1)
            end
         end
      end
   end
end

% Write out coef data to a new file
[iok] = wrtcoef(ichan, fchan, checkedcoef, info, outname);

%%% end of program %%%
