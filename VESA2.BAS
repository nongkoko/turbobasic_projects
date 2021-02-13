screen 1
sub PALFILE inline '(384buffer%(0), namafile)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H54, &H02, &H8E, &HD8, &HB8, &H00, &H3D, &HCD, &H21
 $inline &H1E, &H52, &H50, &H89, &HC3, &HB4, &H3F, &HC5, &H56, &H0A
 $inline &HB9, &H00, &H03, &HCD, &H21, &H89, &HD6, &HBA, &HC8, &H03
 $inline &HB0, &H00, &HEE, &HBA, &HC9, &H03, &HAC, &HEE, &HE0, &HFC
 $inline &H5B, &H5A, &H1F, &HB4, &H3E, &HCD, &H21, &H1F, &H5D
End sub
sub onekey inline '(buttno, codeno) '(kodetombol%, ascii%)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
End sub
sub PXL512 inline '(seg,x,y,w)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8E, &H04, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HC5, &H76, &H0E, &HAD, &H01, &HC7, &HC5, &H76
 $inline &H06, &HA4, &H1F, &H5D
End sub
reg 1,&H4F02
reg 2,&H134
call interrupt &H10

sub titik512(segmen%,x%,y%,warna%)
 shared state%
 local temp%
 temp% = int(y%/128)
 if temp% <> state% then
  state% = temp%
  reg 1,&H4F05
  reg 2,0
  reg 4,temp%
  call interrupt &h10
 end if
' y2% = y% mod 128
 call pxl512(segmen%,x%,y%,warna%)
end sub

for y% = 0 to 384
 for x% = 0 to 511
  call titik512(&HA000,x%,y%,y% mod 256)
 next x%
next y%
dim dynamic pal%(384)
call palfile(pal%(0), "e:\pst\coba.pal"+chr$(0))

? "mode 134h; 512 x 384"
? "3 bank; 128 line per bank"
? "leaf fun"
? "leave_fun_for_me@yahoo.com"
call onekey(dum%,dum%)
end
