function [predsets,pred] = make_sarta_line_predictors(angles);

pred = make_sarta_preds(angles);

predsets.F.set123_1 = pred.a;
predsets.F.set123_2 = pred.a .* pred.a;
predsets.F.set123_3 = pred.a .* pred.Tr;
predsets.F.set123_4 = pred.a .* pred.Tr .* pred.Tr;
predsets.F.set123_5 = pred.Tr;
predsets.F.set123_6 = pred.Tr .* pred.Tr;
predsets.F.set123_7 = pred.a .* pred.Tz;
predsets.F.set123_8 = pred.a .* pred.Tz ./ pred.Tr;

predsets.F.set4_1 = pred.a;
predsets.F.set4_2 = pred.a .* pred.a;
predsets.F.set4_3 = pred.a .* pred.Tr;
predsets.F.set4_4 = pred.a .* pred.Tr .* pred.Tr;
predsets.F.set4_5 = pred.Tr;
predsets.F.set4_6 = pred.Tr .* pred.Tr;
predsets.F.set4_7 = pred.a .* pred.Tz;
predsets.F.set4_8 = pred.a .* pred.a .* pred.Tz;
predsets.F.set4_9 = pred.a .* pred.a .* pred.Tr;
predsets.F.set4_10 = pred.a .* pred.a .* pred.a;
predsets.F.set4_11 = sqrt(pred.a);

predsets.F.set5_1 = pred.a;
predsets.F.set5_2 = pred.a .* pred.a;
predsets.F.set5_3 = pred.a .* pred.Tr;
predsets.F.set5_4 = pred.a .* pred.Tr .* pred.Tr;
predsets.F.set5_5 = pred.Tr;
predsets.F.set5_6 = pred.Tr .* pred.Tr;
predsets.F.set5_7 = pred.a .* pred.Tz;
predsets.F.set5_8 = pred.a .* pred.Tz ./ pred.Tr;
predsets.F.set5_9 = pred.a .* pred.a .* pred.Tr;
predsets.F.set5_10 = sqrt(pred.a);
predsets.F.set5_11 = pred.Tz;

predsets.F.set67_1 = pred.a;
predsets.F.set67_2 = pred.a .* pred.a;
predsets.F.set67_3 = pred.a .* pred.Tr;
predsets.F.set67_4 = pred.a .* pred.Tr .* pred.Tr;
predsets.F.set67_5 = pred.Tr;
predsets.F.set67_6 = pred.Tr .* pred.Tr;
predsets.F.set67_7 = pred.a .* pred.Tz;
predsets.F.set67_8 = sqrt(pred.a);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

predsets.W.set1_1  = pred.a .* pred.W;
predsets.W.set1_2  = sqrt(pred.a .* pred.W);
predsets.W.set1_3  = pred.a .* pred.W .* pred.W ./ pred.Wz;
predsets.W.set1_4  = pred.a .* pred.W .* pred.dT;
predsets.W.set1_5  = (pred.a .* pred.W).^2;
predsets.W.set1_6  = sqrt(pred.a .* pred.W) .* pred.dT;
predsets.W.set1_7  = (pred.a .* pred.W).^(0.25);
predsets.W.set1_8  = sqrt(pred.a .* pred.W) .* pred.W ./ pred.Wz;
predsets.W.set1_9  = (pred.a .* pred.W) .^ 3;
predsets.W.set1_10 = pred.W;
predsets.W.set1_11 = pred.a .* pred.W .* pred.dT .* abs(pred.dT);

predsets.W.set2_1  = pred.a .* pred.W;
predsets.W.set2_2  = sqrt(pred.a .* pred.W);
predsets.W.set2_3  = pred.a .* pred.W .* pred.dT;
predsets.W.set2_4  = pred.a .* pred.W .* (pred.a .* pred.Ox);
predsets.W.set2_5  = (pred.a .* pred.W).^2;
predsets.W.set2_6  = (pred.a .* pred.W).^(0.25);
predsets.W.set2_7  = sqrt(pred.a .* pred.W) .* pred.dT;
predsets.W.set2_8  = (pred.a .* pred.W) .* pred.W ./ pred.Wz;
predsets.W.set2_9  = (pred.a .* pred.W) .^ 3;
predsets.W.set2_10 = pred.a .* pred.W .* (pred.a .* pred.Ox).^2;
predsets.W.set2_11  = sqrt(pred.a .* pred.W) .* pred.W ./ pred.Wz;

predsets.W.set3_1  = pred.a .* pred.W;
predsets.W.set3_2  = sqrt(pred.a .* pred.W);
predsets.W.set3_3  = pred.a .* pred.W .* pred.W ./ pred.Wz;
predsets.W.set3_4  = pred.a .* pred.W .* pred.dT;
predsets.W.set3_5  = (pred.a .* pred.W).^2;
predsets.W.set3_6  = sqrt(pred.a .* pred.W) .* pred.dT;
predsets.W.set3_7  = (pred.a .* pred.W).^(0.25);
predsets.W.set3_8  = (pred.a .* pred.W) .^ 3;
predsets.W.set3_9  = pred.W;
predsets.W.set3_10  = sqrt(pred.a .* pred.W) .* pred.W ./ pred.Wz;
predsets.W.set3_11 = pred.a .* pred.W .* (pred.a .* pred.Mz);


