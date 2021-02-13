'program untuk memperlihatkan 255 karakter ascii
screen 0
sub DIRPRINT inline '(segs%,y,x,teks$,attr%)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H50, &HC5, &H76
 $inline &H12, &H8B, &H1C, &H89, &HD8, &HBB, &H05, &H00, &HF7, &HE3
 $inline &HD1, &HE0, &HD1, &HE0, &HD1, &HE0, &HD1, &HE0, &HD1, &HE0
 $inline &HC5, &H76, &H0E, &H8B, &H0C, &HD1, &HE1, &H89, &HC7, &H01
 $inline &HCF, &HC5, &H76, &H16, &H8E, &H04, &HC5, &H76, &H0A, &H8B
 $inline &H0C, &H8B, &H5C, &H02, &H53, &HC5, &H76, &H06, &HAC, &H5E
 $inline &H1F, &HA4, &HAA, &HE2, &HFC, &H1F, &H5D
End sub
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
sub onekey inline '(buttno, codeno) '(kodetombol%, ascii%)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
End sub
def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def

jum% = 15

for a% = 0 to 128
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9,fnkarakter$(a%),15)
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9+5,chr$(a%),15)
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9+7,"|",10)
next a%
call dirprint(&hb800,24,0,"Leaf Fun. Tekan apa saja untuk melanjutkan",15)
call onekey(dum%,dum%)
cls
for a% = 0 to 126
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9,fnkarakter$(a%+129),15)
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9+5,chr$(a%+129),15)
 call dirprint(&Hb800,a% mod jum%,(a%\jum%)*9+7,"|",10)
next a%
call dirprint(&hb800,24,0,"Leaf Fun. Tekan apa saja untuk keluar",15)
call onekey(dum%,dum%)
