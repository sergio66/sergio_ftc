xstartup
iDo = find(iaExist > 0);

fprintf(1,'reconstruncting ODs')
for ix = 1 : length(iDo)
  iChan = iDo(ix);
  
  if mod(ix,500) == 0
    fprintf(1,'o')
  elseif mod(ix,100) == 0
    fprintf(1,'.')
  end

  %% water
  thecoeff = squeeze(coeffW(iChan,:,:));
  thelist  = squeeze(listW(iChan,:,:));

  for iL = 1 : 100
    laylist = thelist(iL,:);     
    laycoeffs = thecoeff(iL,:);  
    laypredictors = squeeze(vars(:,:,iL));

    xlaylistA = find(laylist > 0);
    xlaylist = laylist(xlaylistA);
    xlayodW(iChan,iL,:) = nansum((laycoeffs(xlaylistA)' * ones(1,NP)) .* laypredictors(xlaylist,:));
  end

  %% fixed
  thecoeff = squeeze(coeffF(iChan,:,:));
  thelist  = squeeze(listF(iChan,:,:));

  for iL = 1 : 100
    laylist = thelist(iL,:);     
    laycoeffs = thecoeff(iL,:);  
    laypredictors = squeeze(vars(:,:,iL));

    xlaylistA = find(laylist > 0);
    xlaylist = laylist(xlaylistA);
    xlayodF(iChan,iL,:) = nansum((laycoeffs(xlaylistA)' * ones(1,NP)) .* laypredictors(xlaylist,:));
  end

  %% ozone
  thecoeff = squeeze(coeffOz(iChan,:,:));
  thelist  = squeeze(listOz(iChan,:,:));

  for iL = 1 : 100
    laylist = thelist(iL,:);     
    laycoeffs = thecoeff(iL,:);  
    laypredictors = squeeze(vars(:,:,iL));

    xlaylistA = find(laylist > 0);
    xlaylist = laylist(xlaylistA);
    xlayodOz(iChan,iL,:) = nansum((laycoeffs(xlaylistA)' * ones(1,NP)) .* laypredictors(xlaylist,:));
  end

end