'program untuk capture daftar genre di winamp

sub TLSTRF inline '(filehandler%, string$)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H0C, &H8B, &H54, &H02, &HC5, &H76, &H0A, &H8B, &H1C
 $inline &H8E, &HD8, &HB4, &H40, &HCD, &H21, &H1F, &H5D
End Sub

sub BKFIL inline '(namafile$, akses, filehandler%) 'akses 0read1write2readwrite
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H3D
 $inline &HC5, &H76, &H0A, &HAC, &HC5, &H76, &H0E, &H8B, &H54, &H02
 $inline &H8E, &HDB, &HCD, &H21, &H72, &H08, &HB4, &H00, &HC4, &H7E
 $inline &H06, &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB
 $inline &H1F, &H5D
End Sub

sub BTFIL inline '(NAMFIL$, atribut%, hand%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H0C, &HC5, &H76, &H0E, &H8B, &H54, &H02, &H8E
 $inline &HDB, &HB4, &H3C, &HCD, &H21, &H72, &H06, &HC4, &H7E, &H06
 $inline &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB, &H1F
 $inline &H5D
End Sub

sub TPFIL inline '(handler%)
 $inline &H55, &H8B, &HEC, &H1E, &HC5, &H76, &H06, &H8B, &H1C, &HB4
 $inline &H3E, &HCD, &H21, &H1F, &H5D
End Sub

sub PDPTR inline '(hand%,cx,dx,mode)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0E, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H14, &HC5
 $inline &H76, &H06, &HAC, &HB4, &H42, &HCD, &H21, &H1F, &H5D
End Sub

sub ptrmove(lvfileno%, lvbanyak&)
 call lng2int(lvbanyak&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,1)
end sub

sub LNG2INT inline '(asf!,hi%,lo%)
 $inline &H55,&H89,&HE5,&H1E,&HC5,&H76,&HE,&H66,&H8B,&H14
 $inline &H89,&HD0,&HC4,&H7E,&H6,&HAB,&HB1,&H10,&H66,&HD3
 $inline &HEA,&H89,&HD0,&HC4,&H7E,&HA,&HAB,&H1F,&H5D
end sub

sub BCSTRF inline (handler,string$)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H0C, &H8B, &H54, &H02, &HC5, &H76, &H0A, &H8B, &H1C
 $inline &H8E, &HD8, &HB4, &H3F, &HCD, &H21, &H1F, &H5D
End Sub

'=========
'alamat target = 235704
screen 0
dim stringku$(255)

namadll$ = "h:\winamp\plugins\in_mp3.dll"
call btfil("e:\coba.txt"+chr$(0),0,fh1%)
call bkfil(namadll$+chr$(0), 0, fh%)
 call ptrmove(fh%, 235704)
 indexku% = 0
 huruf$ = chr$(0)

 do
  ctr% = ctr% + 1
  temp$ = ""
  do
   call bcstrf(fh%, huruf$)
  loop until huruf$ <> chr$(0)
  do
   temp$ = temp$ + huruf$
   call bcstrf(fh%, huruf$)
  loop until huruf$ = chr$(0)
  
  stringku$(ctr%-1) = temp$
 loop until temp$ = "TAG"
call tpfil(fh%)
for a% = ctr%-3 to 0 step -1
 call tlstrf(fh1%,stringku$(a%))
 call tlstrf(fh1%, chr$(13) + chr$(10))
next
call tpfil(fh1%)
