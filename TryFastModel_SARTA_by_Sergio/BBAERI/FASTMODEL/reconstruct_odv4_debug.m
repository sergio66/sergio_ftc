iChan = find(fc >= 1225,1)

  %% water
  thecoeff = squeeze(coeffW(iChan,:,:));
  thelist  = squeeze(listW(iChan,:,:));

  for iL = 1 : 1
    laylist = thelist(iL,:);     
    laycoeffs = thecoeff(iL,:);  
    laypredictors = squeeze(vars(:,:,iL));

    disp('use this list(index) of  predictors : ')
    xlaylistA = find(laylist > 0);
    xlaylist = laylist(xlaylistA)

    disp('for profile 1, these are the profile predictors corresponding to above : ')
    laypredictors(xlaylist,1)

    disp('these are the coeffs : ')
    laycoeffs(xlaylistA)

    xdebuglayodW = nansum((laycoeffs(xlaylistA)' * ones(1,NP)) .* laypredictors(xlaylist,:));
  end


