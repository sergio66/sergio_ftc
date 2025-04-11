clear boo*

%%boox  = ones(size(booQ))*0.1;  %% don;t really need this, just need to scale for this deltaDeplete
booOD = log10(fitdeltaOD1(:));  %%% fit this deltaOD
booT  = fitT(:);
booQ  = fitQ(:);
booS  = fitS(:);
abooT = abovefitT(:);
abooQ = abovefitQ(:);
boo1  = ones(size(booQ));
lpa  = layer_profile_angle(:,1:48,:);  lpa = lpa(:);
lpaL = layer_profile_angleL(:,1:48,:); lpaL = lpaL(:);
lpaP = layer_profile_angleP(:,1:48,:); lpaP = lpaP(:);
lpaA = layer_profile_angleA(:,1:48,:); lpaA = lpaA(:);
mmx  = mmwater_all(:,1:48,:);          mmx = mmx(:);
st   = stemp_all(:,1:48,:);            st  = st(:);

ok    = find(abs(imag(booOD)) < eps);
ok    = find(fitdeltaOD1 > 0);
%ok    = find(fitdeltaOD1 > 1e-3);
booOD = booOD(ok);
booT = booT(ok);
booQ = booQ(ok)/18;
booS = booS(ok);
abooT = abooT(ok);
abooQ = abooQ(ok);
boo1  = ones(size(booQ));
lpa = lpa(ok);
lpaP = lpaP(ok);
lpaA = lpaA(ok);
lpaL = lpaL(ok);
mmx  = mmx(ok);
st   = st(ok);

matr = [booT booQ.*booQ booS boo1];
matr = [booT booQ booS boo1];
matr = [booS booT booQ booS.*booT booS.*booQ boo1];
%matr = [booS booT booQ booS.*booT booS.*booQ booS.*booT booS.*booQ  boo1];
%matr = [booS booT booQ booS.*booT booS.*booQ booS.*booT.^3 booS.*booQ boo1];
matr = [booS booT booQ abooT abooQ boo1];
matr = [booS booT booQ abooQ abooT.^(1/3) boo1];
matr = [booS booT booQ booS.*abooT booS.*abooQ boo1];
fitc = matr\booOD;

predict = matr * fitc;
therror = booOD-predict;

figure(1); plot(booOD,predict,'.',booOD,booOD,'r.'); xlabel('log10(deltaOD)'); ylabel('predicted delta OD')
figure(2); plot(booOD,therror,'.')
figure(3); scatter(booOD,therror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); pcerror = (booOD-predict)./booOD*100; scatter(booOD,pcerror,10,lpaP,'filled'); colorbar; colormap jet
figure(3); plot(lpaP,pcerror,'.'); xlabel('profile'); ylabel('percent error')
figure(3); scatter(lpaP,pcerror,10,mmx,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = mmw')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 
figure(3); scatter(lpaP,pcerror,10,st,'filled'); xlabel('profile'); ylabel('percent error'); colorbar; title('colorbar = stemp')
  jett = jet(64); ; jett = jett(1:48,:); colormap(jett); 

