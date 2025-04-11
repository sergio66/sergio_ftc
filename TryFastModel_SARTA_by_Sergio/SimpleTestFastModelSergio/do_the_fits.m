clear fit fitall
fitall.obsOD = [];
fitall.predOD = [];
fitall.coef = [];

%% 1 = TOA, 100 = GND

if iBreak <= 32
  for iLay = 1 : 100
    fitter = ['fit = ' lower(strBreak) '(secants,secang,pStd,hall,pall,iLay,iChan,data);'];
    fprintf(1,'fitter = %s \n',fitter)
    eval(fitter);
    fitall.coef(iLay,:) = fit.coef;
    if iLay == 1
      fitall.inds{iLay} = 1:length(fit.obsOD);
    else
      fitall.inds{iLay} = (1:length(fit.obsOD)) + max(fitall.inds{iLay-1});
    end
    fitall.obsOD  = [fitall.obsOD  fit.obsOD];
    fitall.predOD = [fitall.predOD fit.predOD];
  end
elseif iBreak == 33
  for iLay = 99 : 100
    fitter = ['fit = ' lower(xstrBreak) '(secants,secang,pStd,hall,pall,iLay,iChan,data,data2);'];
    fprintf(1,'fitter = %s \n',fitter)
    eval(fitter);
    fitall.coef(iLay,:) = fit.coef;
    if iLay == 1
      fitall.inds{iLay} = 1:length(fit.obsOD);
    else
      fitall.inds{iLay} = (1:length(fit.obsOD)) + max(fitall.inds{iLay-1});
    end
    fitall.obsOD  = [fitall.obsOD  fit.obsOD];
    fitall.predOD = [fitall.predOD fit.predOD];    
  end
end  

plot(fitall.obsOD,fitall.predOD,'b.',fitall.obsOD,fitall.obsOD,'k');