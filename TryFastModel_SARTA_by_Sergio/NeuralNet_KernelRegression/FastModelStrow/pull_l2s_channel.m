% pull_l2s_channel.m

% Pull in a single channel layer-to-space set of files

cd /home/sergio/MATLABCODE/REGR_PROFILES/RUN_KCARTA/SAF704/F/

for i=1:704
   fnpre = 'convolved_kcarta_F_'
   fn = [fnpre sprintf('%d',i) '.mat'];
   load(fn,'rcris_all');
   f(i,:,:,:) = rcris_all(:,51,:);
   i
end
save ~/cris_ch51_f f

% cd /home/sergio/MATLABCODE/REGR_PROFILES/RUN_KCARTA/SAF704/FO/
% for i=1:704
%    fnpre = 'convolved_kcarta_FO_'
%    fn = [fnpre sprintf('%d',i) '.mat'];
%    load(fn,'rcris_all');
%    fo(i,:,:,:) = rcris_all(:,51,:);
%    i
% end
% 
% % % chan 1099
% for i=1:704
% fnpre = 'convolved_kcarta_FWO_'
% fn = [fnpre sprintf('%d',i) '.mat'];
% load(fn,'rcris_all');
% fwo(i,:,:,:) = rcris_all(:,1099,:);
% i
% end
% 
% cd /home/sergio/MATLABCODE/REGR_PROFILES/RUN_KCARTA/SAF704/FO/
% 
% for i=1:704
% fnpre = 'convolved_kcarta_FO_'
% fn = [fnpre sprintf('%d',i) '.mat'];
% load(fn,'rcris_all');
% fo(i,:,:,:) = rcris_all(:,1099,:);
% i
% end
% 
% 
% save ~/cris_ch1099_fwo_fo fwo fo
