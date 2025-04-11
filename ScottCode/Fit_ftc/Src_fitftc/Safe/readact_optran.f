C***********************************************************************
C
C      Subroutine READaCT : read a single freq of convolved trans with
C                           angles
C
C      Arguements
C         IOIN   aI i : unit numbers of the input files
C         TAUX   aR o : whatever gas transmittance
C         TAUXW  aR o : whatever & water transmittance
C         FREQ    R o : frequency (in cm-1)
C         MORDAT  L o : true/false more data to come
C
C***********************************************************************
C
       SUBROUTINE READaCT(IOIN, TAUX, TAUXW, IDCHAN, FREQ, MORDAT)
C
       INCLUDE 'farray.f'
C
C      Input arguments
       INTEGER   IOIN(MAXPRO)
C
C      Output arguments
       REAL   TAUX(MAXPRO,MAXLAY,MAXANG)
       REAL  TAUXW(MAXPRO,MAXLAY,MAXANG)
       INTEGER IDCHAN
       REAL   FREQ
       LOGICAL MORDAT
C
C      Local variables
       INTEGER   IANG
       INTEGER   ILAY
       INTEGER   IPRO
C
       REAL    RES
       REAL RNFWHM
       REAL TAULTS(MAXANG,MAXLAY,NUMSET)

C-----------------------------------------------------------------------
C
       MORDAT=.TRUE.
C
       DO IPRO=1,MAXPRO
C
          READ(IOIN(IPRO),END=90) FREQ, IDCHAN, RES, RNFWHM,
     $       (((TAULTS(IANG,ILAY,IGAS),IANG=1,MAXANG),ILAY=1,MAXLAY),
     $       IGAS=1,NUMSET)
c      write(6,*) freq, idchan, res, rnfwhm
C
          DO IANG=1,MAXANG
             DO ILAY=1,MAXLAY
                TAUX(IPRO,ILAY,IANG)=TAULTS(IANG,ILAY,IBOKX)
                TAUXW(IPRO,ILAY,IANG)=TAULTS(IANG,ILAY,IBOKW)
             ENDDO
          ENDDO
C
       ENDDO
C
       GOTO 99
C
C      The following block is only done if at end of input files
 90    WRITE(6,1000)
 1000  FORMAT('Finished reading data...closing files')
       DO IPRO=1,MAXPRO
          CLOSE(IOIN(IPRO))
       ENDDO
       MORDAT=.FALSE.
C
 99    RETURN
       END
