iCnt = 0;
for ii = 1 : 2490
  if mod(ii,500) == 0
    fprintf(1,'o')
  elseif mod(ii,100) == 0
    fprintf(1,'.')
  end

  y1str = ['COEFFS/605/aeriSRFv2_water605_chan' num2str(ii) '.mat'];
  y2str = ['COEFFS/605/aeriSRFv2_fixed605_chan' num2str(ii) '.mat'];
  y3str = ['COEFFS/605/aeriSRFv2_ozone605_chan' num2str(ii) '.mat'];

  iaExist(ii) = 0;
  if exist(y1str) > 0 &  exist(y2str) > 0 &  exist(y3str) > 0
    iCnt = iCnt + 1;
    iaExist(ii) = 1;
    ystr = ['COEFFS/605/aeriSRFv2_water605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    CW(ii,:,:) = y.xCW;
    coeffW(ii,:,:) = y.xcoeffW;
    listW(ii,:,:) = y.xlistW;

    ystr = ['COEFFS/605/aeriSRFv2_fixed605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    CF(ii,:,:) = y.xCF;
    coeffF(ii,:,:) = y.xcoeffF;
    listF(ii,:,:) = y.xlistF;

    ystr = ['COEFFS/605/aeriSRFv2_ozone605_chan' num2str(ii) '.mat'];
    loader = ['y = load(''' ystr ''');'];
    eval(loader);
    COz(ii,:,:) = y.xCOz;
    coeffOz(ii,:,:) = y.xcoeffOz;
    listOz(ii,:,:) = y.xlistOz;

  end
end

fprintf(1,'loaded in  %4i out of 2490 F W O files ...\n',iCnt)
iSave = input('save the predictors??? (-1/+1) : ')
if iSave > 0
  commentstr1 = 'from running do_make_predictors_v4';
  commentstr2 = 'CW,CF,COz are the coeffs that come out from the fitting of the 49 regr profs';
  commentstr3 = 'list* and coeff* are wat you want';
  save v4Predictors_605.mat CW CF COz coeffW listW coeffF listF coeffOz listOz iCnt iaExist commentstr*
end
