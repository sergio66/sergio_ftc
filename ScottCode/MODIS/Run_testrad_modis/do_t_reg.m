clear
clf

load radall_fow_short.mat

[nchan, nang, nprof] = size(btall);
nprofang = nang*nprof;

ind1_30 = 10:29;
ind30_100 = 30:44;
ind100_400 = 45:70;
ind400_1000 = 71:97;
temp_all2D = reshape(temp_all,100,nprofang);
btall2D = reshape(btall,nchan,nprofang);
bttransall2D = reshape(bttransall,nchan,nprofang);
t1_30 = mean(temp_all2D(ind1_30,:));
t30_100 = mean(temp_all2D(ind30_100,:));
t100_400 = mean(temp_all2D(ind100_400,:));
t400_1000 = mean(temp_all2D(ind400_1000,:));

term1 = ones(1,288);
term2 = 0.005 * t1_30;
term3 = 0.005 * t30_100;
term4 = 0.005 * t100_400;
term5 = 0.005 * t400_1000;

A = [term1; term2; term3; term4; term5]';
dbt = btall2D - bttransall2D;
for ii = 1:nchan
   B = dbt(ii,:)';
   X = (A\B)
   bcalc = A*X;
   plot(1:nprofang,B,'bo',1:nprofang,bcalc,'rx'),grid
   pause
end

%%% end of program %%%
