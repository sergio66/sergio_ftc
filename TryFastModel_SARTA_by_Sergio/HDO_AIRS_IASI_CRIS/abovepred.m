abovefitQ = zeros(size(fitQ));;
abovefitT = zeros(size(fitT));;
for ii = 100 : -1 : 1
  numerator = 0;
  denominator = 0;
  for jj = 1 : ii
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    
    denom = log10(gas_1(jj,49,:));  denom = repmat(denom,[1 48 1]); denom = denom*wah;
    numer = fitQ(jj,:,:);                                           numer = numer*wah;

    numerator = numerator + numer;
    denominator = denominator + denom;
  end
  pah = squeeze(numerator ./ denominator);
  abovefitQ(ii,:,:) = pah;

  numerator = 0;
  denominator = 0;
  for jj = 2 : ii
    wah = plays(jj)*(plays(jj)-plays(jj+1));
    
    denom = tempr(jj,49,:);   denom = repmat(denom,[1 48 1]);
    numer = tempr(jj,1:48,:);                                

    numerator = numerator + wah * numer./denom/5e5;
  end
  pah = squeeze(numerator);
  abovefitT(ii,:,:) = pah;

end

