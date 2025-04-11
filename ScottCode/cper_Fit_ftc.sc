ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Bin
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_ch4
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_hno3
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_n2o
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_nh3
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_so2
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Fit_therm
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Src_fitftc
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Src_fitftc
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Src_merge_cut
ls -lt /asl/s2/hannon/AIRS_prod08/Fit_ftc/Src_useconv

ls -lt /asl/s2/hannon/AIRS_prod08/AIRS/
ls -lt /asl/s2/hannon/AIRS_prod08/CrIS
ls -lt /asl/s2/hannon/AIRS_prod08/IASI
ls -lt /asl/s2/hannon/AIRS_prod08/MODIS

read -p "Press [Enter] key to start copying..."

rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/Fit_ftc   /home/sergio/FastModelDevelopment/ScottCode
rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/AIRS      /home/sergio/FastModelDevelopment/ScottCode
rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/CrIS      /home/sergio/FastModelDevelopment/ScottCode
rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/IASI      /home/sergio/FastModelDevelopment/ScottCode
rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/MODIS     /home/sergio/FastModelDevelopment/ScottCode

rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/DATA      /home/sergio/FastModelDevelopment/ScottCode
rsync -av --exclude="*.dat" --exclude="*.mat" /asl/s2/hannon/AIRS_prod08/Prof385   /home/sergio/FastModelDevelopment/ScottCode
