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
        integer i, j
        integer status
        integer chan1, chan2
        character*64 fin, fout, mode
c
c open the input file for reading
c
        mode = 'r'
c       fin = 'test4.hdf'//char(0)
        fin = 'bugdat/stdpro44g.hdf'//char(0)
        status = rtpopen(fin, mode, head, hatt, patt, chan1)
        print *, 'read open status = ', status
c
c add an example attribute
c
        i = 1
        do while (hatt(i).fname(1:1) .ne. char(0) .and. i .lt. MAXNATTR)
          print *, i, hatt(i).fname
          print *, i, hatt(i).aname
          print *, i, hatt(i).atext
          i = i + 1
        end do
        hatt(i).fname = 'header'//char(0)
        hatt(i).aname = 'ftest3 comment'//char(0)
        hatt(i).atext = 'modified by ftest3'//char(0)
        hatt(i+1).fname = char(0)
c
c create the output file
c
        mode = 'c'
        fout  = 'ftest3.hdf'
        status = rtpopen(fout, mode, head, hatt, patt, chan2)
        print *, 'create open status = ', status

c
c update the header data
c
c       flag type as profiles + IR calc rad
        head.ptype = 3

c       add some fake channels
        head.nchan = MAXCHAN
        do j = 1, MAXCHAN
          head.ichan(j) = j
          head.vchan(j) = j + 10000
        end do

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

c       add some fake radiance data
        do j = 1, MAXCHAN
          prof.rcalc(j) = 1.2345
        end do

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

