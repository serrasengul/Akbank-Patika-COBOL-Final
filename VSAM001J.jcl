//VSAM001J JOB ' ',CLASS=A,MSGLEVEL=(1,1),MSGCLASS=X,NOTIFY=Z95622
//DELET500 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSIN    DD *
    DELETE Z95622.VSAM.DD CLUSTER PURGE
    IF LASTCC LE 08 THEN SET MAXCC = 00
    DEF CL ( NAME(Z95622.VSAM.DD)         -
         FREESPACE( 20 20 )                -
         SHR( 2,3 )                        -
         KEYS(5 0)                         -
         INDEXED SPEED                     -
         RECSZ(47 47)                      -
         TRK (10 10)                       -
         LOG(NONE)                         -
         VOLUME(VPWRKB)                    -
         UNIQUE )                          -
    DATA ( NAME(Z95622.VSAM.DD.DATA))     -
    INDEX ( NAME(Z95622.VSAM.DD.INDEX))

//REPRO600 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//INN001   DD DSN=Z95622.QSAM.SB,DISP=SHR
//OUT001   DD DSN=Z95622.VSAM.DD,DISP=SHR
//SYSIN    DD *
   REPRO INFILE(INN001) OUTFILE(OUT001)
