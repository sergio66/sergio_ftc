addpath /home/sergio/MATLABCODE

chans9 = [54 359 1055 1249 1282 1291 1475 1614 2333];
chans38 = [        41          54         181         273         317         359         445         449 ...
                  532         758         903         904        1000        1020        1034        1055 ...
                 1075        1103        1249        1282        1291        1447        1475        1557 ...
                 1604        1614        1618        1660        1790        1866        1867        1868 ...
                 1878        1888        2112        2140        2321        2333];
chans41 = [chans38 2325        2339        2353]; chans41 = sort(chans41);
fairs = instr_chans;
lw1 = find(fairs >= 700 & fairs <= 962);
lw2 = find(fairs >= 1060 & fairs <= 1232);
lw = [lw1; lw2];
[lw,iX,iUse] = intersect(lw,chans41);
whos iUse

%{
see /home/sergio/MATLABCODE/CRODGERS_FAST_CLOUD /clust_stats_results_random_allfovV1.m

thedir = ['/asl/oem_retrieval/rtp_airibrad_v5/' num2str(yy) '/SergioRandomRetriveNight/'];
if iWhich == -2
  thefile = ['era_airibrad_day' num2str(doy,'%03d') '_random_Start_with_AIRSL3geo.z5sergio_cloud_retrieve_allfov.mat'];
elseif iWhich == +1
  thefile = ['era_airibrad_day' num2str(doy,'%03d') '_random_Start_with_ERAgeo.z5sergio_cloud_retrieve_allfov.mat'];
end

%}

iWhich = +1;

dirout = '/asl/ftp/pub/sergio/EMADDY/CLOUD_RCALC/';
for yy = 2003
  daysINmonth = [31 28 31 30 31 30 31 31 30 31 30 31];
  if mod(yy,4) == 0
    daysINmonth(2) = 29;
  end
  for mm = 1 : 12
    for dd = 1 : daysINmonth(mm)
      clear data
      doy = sum(daysINmonth(1:mm-1)) + dd;
      thedir = ['/asl/oem_retrieval/rtp_airibrad_v5/' num2str(yy) '/SergioRandomRetriveNight/'];
      if iWhich == -2
        thefile = ['era_airibrad_day' num2str(doy,'%03d') '_random_Start_with_AIRSL3geo.z5sergio_cloud_retrieve_allfov.mat'];
      elseif iWhich == +1
        thefile = ['era_airibrad_day' num2str(doy,'%03d') '_random_Start_with_ERAgeo.z5sergio_cloud_retrieve_allfov.mat'];
      end
      fin = [thedir thefile];
      if exist(fin)
        outfile = [dirout '/stats_' num2str(doy,'%03d') '_' num2str(yy,'%04d')];
        saver = ['save -struct ' outfile ' data fuse ichans'];
        saver = ['save ' outfile ' data fuse ichans'];	
        outfile2 = [dirout '/stats_' num2str(doy,'%03d') '_' num2str(yy,'%04d') '.hdf5'];
        saver2 = ['save -v7.3 -struct ' outfile2 ' data fuse ichans'];
        saver2 = ['save -v7.3 ' outfile2 ' data fuse ichans'];	
	
        loader = ['a = load(''' fin ''');'];
	eval(loader);
	poemNew = a.poemNew;
	poemNew = convert_rtp_to_cloudOD(poemNew);
        data.rlat = poemNew.rlat;
        data.rlon = poemNew.rlon;
        data.solzen = poemNew.solzen;
        data.satzen = poemNew.satzen;
	data.iceOD  = poemNew.iceOD;
	data.icetop = poemNew.icetop;
	data.icesze = poemNew.icesze;
	data.icefrac = poemNew.icefrac;
	data.waterOD  = poemNew.waterOD;
	data.watertop = poemNew.watertop;
	data.watersze = poemNew.watersze;
	data.waterfrac = poemNew.waterfrac;
	data.stemp = poemNew.stemp;
	data.landfrac = poemNew.landfrac;
	data.rcalc = poemNew.rcalc(chans41(iUse),:);
        fuse = fairs(chans41(iUse));
	ichans = chans41(iUse);
	
        eval(saver);
        eval(saver2);	
	
      end    %% file exists?
    end      %% loop over dd
  end        %% loop over mm
end          %% loop over yy
