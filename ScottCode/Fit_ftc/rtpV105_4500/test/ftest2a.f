c
c more RTP read and write tests
c
c reads and writes demo fitting profile set in "test4.hdf"
c
c note: this version saves all the profiles in a structure array, 
c which takes up a lot of space.  See ftest2.f for a read/write loop 
c with a single profile and header structure buffer
c
        program ftest1
        implicit none
c
c RTP declarations
c
        include 'rtpdefs.f'
        integer rtpopen, rtpread, rtpwrite, rtpclose
        record /RTPHEAD/ head
        record /RTPPROF/ prof(48)
        record /RTPATTR/ hatt(MAXNATTR), patt(MAXNATTR)
c 
c other variables
c
        integer i
        integer status
        integer rchan
        character*32 fname, mode
c
c RTP read demo
c
        mode = 'r'
        fname = 'test4.hdf'

        status = rtpopen(fname, mode, head, hatt, patt, rchan)
        print *, 'read open status = ', status
c
c print out the HDF attributes
c
        i = 1
        do while (ichar(hatt(i).fname) <> 0 .and. i <= MAXNATTR)
           print *, 'header attribute number ', i
           print *, hatt(i).fname
           print *, hatt(i).aname
           print *, hatt(i).atext
           i = i + 1
        end do
 
        i = 1
        do while (ichar(patt(i).fname) <> 0 .and. i <= MAXNATTR)
           print *, 'profile attribute number ', i
           print *, patt(i).fname
           print *, patt(i).aname
           print *, patt(i).atext
           i = i + 1
        end do
 
        do i = 1, 48
          status = rtpread(rchan, prof(i))
c         print *, 'read status = ', status, i
        end do

c an extra read, to test EOF error
        status = rtpread(rchan, prof(48))
        print *, 'EOF read status = ', status

        status = rtpclose(rchan)
        print *, 'read close status = ', status
c
c print out some sample data values
c
        print *, 'nlevs = ', prof(2).nlevs

        do i = 1, prof(2).nlevs
          print *, prof(2).gamnt(i,1), prof(2).gamnt(i,2), prof(2).gamnt(i,3)
        end do
c
c RTP write demo
c
        mode = 'c'
        fname = 'ftest.hdf'

        status = rtpopen(fname, mode, head, hatt, patt, rchan)
        print *, 'write open status = ', status

        do i = 1, 48
          status = rtpwrite(rchan, prof(i))
c         print *, 'write status = ', status, i
        end do

        status = rtpclose(rchan)
        print *, 'write close status = ', status

        stop
        end

