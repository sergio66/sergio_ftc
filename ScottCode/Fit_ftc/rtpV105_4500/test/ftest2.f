c
c Demonstration of RTP read/modify/write loop
c
c this demo is a prototype for programs that read,
c modify and write RTP files.  It reads the RTP file 
c 'test4.hdf' and writes an RTP file 'ftest2.hdf'
c 
        program ftest
        implicit none
c
c RTP declarations
c
        include 'rtpdefs.f'
        integer rtpopen, rtpread, rtpwrite, rtpclose
        record /RTPHEAD/ head
        record /RTPPROF/ prof
        record /RTPATTR/ hatt(MAXNATTR), patt(MAXNATTR)
c 
c other variables
c
        integer i
        integer status
        integer chan1, chan2
        character*32 fin, fout, mode

c
c initialize header and profile fields with sensible values
c 
        call rtpinit(head, prof)

c
c open the input file for reading
c
        mode = 'r'
        fin = 'test4.hdf'
        status = rtpopen(fin, mode, head, hatt, patt, chan1)
        print *, 'read open status = ', status
c
c add an example attribute
c
        i = 1
        do while (hatt(i).fname(1:1) .ne. char(0) .and. i .lt. MAXNATTR)
          print *, 'attribute ', i
          print *, 'field name    : ', hatt(i).fname
          print *, 'attribute name: ', hatt(i).aname
          print *, 'attribute text: ', hatt(i).atext
          i = i + 1
        end do
        hatt(i).fname = 'header'//char(0)
        hatt(i).aname = 'ftest2 comment'//char(0)
        hatt(i).atext = 'modified by ftest1'//char(0)
        hatt(i+1).fname = char(0)

c
c create the output file
c
        mode = 'c'
        fout  = 'ftest2.hdf'
        status = rtpopen(fout, mode, head, hatt, patt, chan2)
        print *, 'create open status = ', status
c
c loop on profiles, until EOF
c
        i = 0
        do while (.true.)
          status = rtpread(chan1, prof)
          if (status .eq. -1) goto 22
          i = i + 1
c
c modifications of the profile data go here
c
c         prof.spres = 800
c         prof.stemp = 290
c
c write out the updated profile
c

          status = rtpwrite(chan2, prof)
          print *, 'write status = ', status, i

        end do

 22     continue

        status = rtpclose(chan1)
        print *, 'read close status = ', status

        status = rtpclose(chan2)
        print *, 'write close status = ', status

        stop
        end

