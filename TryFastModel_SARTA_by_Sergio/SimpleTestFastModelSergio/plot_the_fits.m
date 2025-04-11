sgn = sign(fitall.coef);

[mm,nn] = size(fitall.obsOD);

theinds = [];
for ii = 1 : nn
  theinds = [theinds fitall.inds{ii}];
end

figure(1); plot(1:nn,log10(abs(fitall.coef))); title('log10(abs(coef))'); grid
figure(2); plot(fitall.obsOD,fitall.predOD,'b.',fitall.obsOD,fitall.obsOD,'k'); grid
figure(3); loglog(fitall.obsOD,fitall.predOD,'b.',fitall.obsOD,fitall.obsOD,'k'); grid
figure(4); scatter(log10(fitall.obsOD(:)),log10(fitall.predOD(:)),10,theinds/mm); colorbar; title('layer 1=TOA 100=GND')

for ii = 2:4; xlabel('OD'); ylabel('predOD'); grid on; end