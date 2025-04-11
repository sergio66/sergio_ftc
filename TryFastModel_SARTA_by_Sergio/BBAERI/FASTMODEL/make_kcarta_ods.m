iaBand = [500 605 1805 2830];

kcartaexec = '/home/sergio/KCARTA/BIN/bkcarta.x';

xstartup
for iBand = 1 : 3
  f1 = iaBand(iBand);
  f2 = iaBand(iBand+1);
  [h,ha,p,pa] = oldrtpread('regr48.op.rtp');

  h.vcmin = f1;
  h.vcmax = f2;
  fout = ['output' num2str(f1) '.op.rtp'];
  oldrtpwrite(fout,h,ha,p,pa);

  for ip = 1 : 49
    for ix = 1 : 4
      fprintf(1,'Band = %4i Prof = %2i  iWV/CO2/O3/XYZ = %3i \n\n',f1,ip,ix);

      if ix == 1
        outnml = ['wv_prof_' num2str(ip) '_B_' num2str(f1) '.nml'];
        sedder = ['!sed -e "s/FF1/' num2str(f1) '/g"  -e "s/FF2/' num2str(f2) '/g" '];
        sedder = [sedder ' -e "s/PP/'    num2str(ip) '/g"'];
        sedder = [sedder ' template_dumpOD_WV.nml  > '  outnml];
        outfile = ['/asl/s1/sergio/BBAERI/WV/prof_' num2str(ip) '_B_' num2str(f1) '.dat'];
        kcartaer = ['!' kcartaexec ' ' outnml ' ' outfile];

      elseif ix == 2
        outnml = ['co2_prof_' num2str(ip) '_B_' num2str(f1) '.nml'];
        sedder = ['!sed -e "s/FF1/' num2str(f1) '/g"  -e "s/FF2/' num2str(f2) '/g" '];
        sedder = [sedder ' -e "s/PP/'    num2str(ip) '/g"'];
        sedder = [sedder ' template_dumpOD_CO2.nml  > '  outnml];
        outfile = ['/asl/s1/sergio/BBAERI/CO2/prof_' num2str(ip) '_B_' num2str(f1) '.dat'];
        kcartaer = ['!' kcartaexec ' ' outnml ' ' outfile];

      elseif ix == 3
        outnml = ['o3_prof_' num2str(ip) '_B_' num2str(f1) '.nml'];
        sedder = ['!sed -e "s/FF1/' num2str(f1) '/g"  -e "s/FF2/' num2str(f2) '/g" '];
        sedder = [sedder ' -e "s/PP/'    num2str(ip) '/g"'];
        sedder = [sedder ' template_dumpOD_O3.nml  > '  outnml];
        outfile = ['/asl/s1/sergio/BBAERI/O3/prof_' num2str(ip) '_B_' num2str(f1) '.dat'];
        kcartaer = ['!' kcartaexec ' ' outnml ' ' outfile];

      elseif ix == 4
        outnml = ['others_prof_' num2str(ip) '_B_' num2str(f1) '.nml'];
        sedder = ['!sed -e "s/FF1/' num2str(f1) '/g"  -e "s/FF2/' num2str(f2) '/g" '];
        sedder = [sedder ' -e "s/PP/'    num2str(ip) '/g"'];
        sedder = [sedder ' template_dumpOD_OTHERS.nml  > '  outnml];
        outfile = ['/asl/s1/sergio/BBAERI/OTHERS/prof_' num2str(ip) '_B_' num2str(f1) '.dat'];
        kcartaer = ['!' kcartaexec ' ' outnml ' ' outfile];

      end       %% if ix == 1,2,3,4

      eval(sedder)
      eval(kcartaer)
      rmer = ['!/bin/rm ' outnml]; eval(rmer);

    end         %% for ix = 1 : 4
  end           %% for ip = 1:49
end             %% for iBand = 1,2,3

rmer = ['!/bin/rm co2*nml others*.nml o3*.nml wv*.nml output*.rtp'];
eval(rmer)

%% ls -lt   /asl/s1/sergio/BBAERI/*/*.dat