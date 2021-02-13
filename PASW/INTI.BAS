'leaf fun paswot por dos
'lupa paswot kagak jalan
samplehuruf$ = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890~!@#$%^&*()-=_+{}[];':" + chr$(34) + "/\,.<>?|"
dim dynamic ph$(6)
ph$(1) = "i|"
ph$(2) = "l;':,."
ph$(3) = "abcdefghjknpqrstuxyzI1!(){}[]"+chr$(34)
ph$(4) = "vABCDEFGHJKLPQRSUXZ2345679@-=_/\<>?"
ph$(5) = "mowMNOTVWY80#$%^&*+"
ph$(6) = "~"
indexed$ = "gjpqy"
dim dynamic datahuruf%(172)
restore huruf
call bacadatakearrayn(datahuruf%(),0,173,dum%)

sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
sub STR2NUM inline '(string$, offstr%, varnum%, offset%, banyak%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H16, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &HC5, &H76, &H0A, &HAD, &HC4, &H7E, &H0E
 $inline &H01, &HC7, &HC5, &H76, &H12, &HAD, &HC5, &H76, &H16, &H8B
 $inline &H74, &H02, &H01, &HC6, &H8E, &HDA, &HF3, &HA4, &H1F, &H5D
End Sub
sub FBSTR inline '(str$, char%, start%, bancari%, pos%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H0C, &HC5, &H76, &H12, &H8A, &H24, &HC5, &H76
 $inline &H0E, &H8B, &H14, &HC5, &H76, &H16, &H8B, &H74, &H02, &H01
 $inline &HD6, &H8E, &HDB, &H31, &HDB, &HE3, &H09, &HAC, &H38, &HC4
 $inline &H74, &H0D, &H49, &H43, &HEB, &HF5, &HB8, &HFF, &HFF, &HC4
 $inline &H7E, &H06, &HAB, &HEB, &H08, &H01, &HD3, &HC4, &H7E, &H06
 $inline &H26, &H89, &H1D, &H1F, &H5D
End Sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub BS2PXY inline '(segsum, offsum, segtuj, x, y, warna, banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0E, &H8B, &H3C, &HAD
 $inline &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HC5, &H76
 $inline &H12, &HAD, &H01, &HC7, &H57, &HC5, &H76, &H16, &H8E, &H04
 $inline &HC5, &H76, &H0A, &H8A, &H14, &HC5, &H76, &H06, &H8A, &H34
 $inline &HC5, &H76, &H1A, &HAD, &HC5, &H76, &H1E, &H8E, &H1C, &H89
 $inline &HC6, &H88, &HD4, &H80, &HFE, &H00, &H74, &H23, &HAC, &H88
 $inline &HC3, &HB2, &H01, &HB1, &H08, &H20, &HD0, &H75, &H13, &H88
 $inline &HD8, &H81, &HC7, &H40, &H01, &HD0, &HE2, &HFE, &HC9, &H75
 $inline &HF0, &HFE, &HCE, &H5F, &H47, &H57, &HEB, &HDD, &H26, &H88
 $inline &H25, &HEB, &HE8, &H5F, &H1F, &H5D
End sub
sub PSIMG inline '(NAMAFILE,SEGTUJ,OFFTUJ,NEXTOFF)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H50, &HC5, &H76
 $inline &H12, &H8B, &H54, &H02, &H1F, &H1E, &H52, &HB8, &H00, &H3D
 $inline &HCD, &H21, &H89, &HC3, &HB8, &H02, &H42, &H31, &HC9, &H31
 $inline &HD2, &HCD, &H21, &H50, &HB8, &H00, &H42, &HCD, &H21, &H59
 $inline &HB4, &H3F, &HC5, &H76, &H0A, &H8B, &H14, &HC5, &H76, &H0E
 $inline &H8E, &H1C, &HCD, &H21, &HC4, &H7E, &H06, &H01, &HD0, &HAB
 $inline &HB4, &H3E, &H5A, &H1F, &HCD, &H21, &H1F, &H5D
End sub

def fnpanhur%(ascii%)
 local posi%, a%
 shared ph$()
 if ascii% = 32 then
  fnpanhur% = 2
  exit def
 end if
 posi% = -1
 for a% = 1 to ubound(ph$(1))
  call panstr(ph$(a%),pan%)
  call fbstr(ph$(a%),ascii%,0,pan%,posi%)
  if posi% <> -1 then
   fnpanhur% = a%
   exit def
  end if
 next a%
end def
def fnpankal%(strin$)
local inti%
 call panstr(strin$,temp%)
 for temp2% = 0 to temp%-1
  call str2num(strin$,temp2%,ascii%,0,1)
  inti% = inti% + fnpanhur%(ascii%) + 1
 next temp2%
 fnpankal% = inti%
end def
def fnpospon%(ascii%)
 local a%, temp%, posi%, b%
 shared samplehuruf$
 call fbstr(samplehuruf$,ascii%,0,93,posi%)
 for a% = 0 to posi%-1
  call str2num(samplehuruf$,a%,b%,0,1)
  temp% = temp% + fnpanhur%(b%)
 next a%
 fnpospon% = temp%
end def

sub bacadatakearrayn(ArAy%(0),datake%,banyakpembacaan,keselanjutnya%)
 for A = datake% to datake%+banyakpembacaan-1
  read n
  aray%(A) = n
 next a
 keselanjutnya% = datake% + banyakpembacaan
end sub

sub vhuruf(tek$,segmen%,x%,y%,warna%)
local a%, poshur%
shared datahuruf%()
 call pointer(datahuruf%(0),seghur%)
 call panstr(tek$,pan%)
 for a% = 0 to pan%-1
  call str2num(tek$,a%,thehur%,0,1)
  if thehur% = 32 then
   poshur% = poshur% + 3
  else
   panjanghuruf% = fnpanhur%(thehur%)
   call bs2pxy(seghur%,fnpospon(thehur%),segmen%,x%+poshur%,y%+fnposy%(thehur%),warna%,panjanghuruf%)
   poshur% = poshur% + panjanghuruf% + 1
  end if
 next a%
end sub

def fnposy%(ascii%)
 shared indexed$
 temp% = -1
 call panstr(indexed$,pan%)
 call fbstr(indexed$,ascii%,0,pan%,temp%)
 if temp% <> -1 then
  fnposy% = 3
 else
  fnposy% = 0
 end if
end def

huruf:
data &HA848, &HFFF0, &H7088, &H8870, &H7050, &HFF88, &H9478, &H0848, &H0AFC, &H914E
data &HFF7F, &HF008, &H40FA, &H7D80, &H20FF, &H01D8, &HF8FF, &HF008, &HF008, &H08F8
data &H70F0, &H8888, &H7088, &H11FF, &H0E0E, &HFF11, &H10F8, &H9008, &H48A8, &H7C08
data &H7888, &HF880, &H6018, &HF880, &HC078, &HC020, &HD878, &HD820, &H904F, &HC87F
data &H98A8, &H11FE, &HFE11, &H91FF, &H6E91, &H817E, &H6681, &H81FF, &H7E81, &H91FF
data &H8191, &H11FF, &H0111, &H817E, &HE691, &H10FF, &HFF10, &HFF81, &H6181, &H8181
data &HFF7F, &H2418, &HFFC3, &H8080, &HFF80, &H300E, &HFF0E, &H06FF, &H6018, &H7EFF
data &H8181, &H7E81, &H11FF, &H0E11, &HA17E, &HFEC1, &H11FF, &HCE31, &H8966, &H6691
data &H0103, &H01FF, &H7F03, &H8080, &H0F7F, &H8070, &H0F70, &H60FF, &H601C, &HE7FF
data &H1818, &H07E7, &HF008, &H0708, &H91E3, &HC789, &HFF82, &HC680, &H91A1, &H668E
data &H9981, &H3866, &HFF24, &H6620, &H8985, &H7E71, &H8989, &H0372, &H7181, &H660F
data &H9199, &H6699, &H8946, &H7E91, &HA17E, &H8599, &H047E, &H0202, &H0404, &H0602
data &H06BF, &H897E, &H4E95, &HFF24, &HFF24, &H2424, &HFF4A, &H2452, &H25C2, &HA852
data &H0447, &H0102, &H0402, &H916E, &H46A9, &H24A0, &H7E18, &H2418, &H423C, &H8181
data &H3C42, &H1010, &H1010, &H2828, &H2828, &H8080, &H8080, &H1010, &H107C, &H0810
data &H8176, &H7681, &HFF08, &H8181, &H8181, &H16FF, &H0236, &H3606, &H0636, &H0600
data &H30C0, &H030C, &H0C03, &HC030, &HC040, &HC0C0, &H2810, &H8244, &H4482, &H1028
data &HA106, &H0E11, &H0EF7

def fninput3$(segmen%,x%,y%,warna%,tipe$,segback%)
local bank$
bank$ = ""
do
 kode% = 0
 call rdkey(kode%,tpt%)
 if kode% = 8 then
  call panstr(bank$,temp%)
  if temp% = 0 then exit if
  bank$ = left$(bank$,temp%-1)
  if tipe$ = "psw" then
   tempposx% = x%+fnpankal(string$(temp%-1,"*"))
   call mvblok(segback%,tempposx%,y%,segmen%,tempposx%,y%,6,11)
  else
   tempposx% = x%+fnpankal(bank$)
   call mvblok(segback%,tempposx%,y%,segmen%,tempposx%,y%,6,11)
  end if
  call vhuruf(string$(temp%,"*"),segmen%,x%,y%,warna%)
 elseif kode% = 13 then
 elseif kode% = 27 then
 elseif kode% <> 0 then
  bank$ = bank$ + chr$(kode%)
  if tipe$ = "psw" then
   call panstr(bank$,temp%)
   call vhuruf(string$(temp%,"*"),segmen%,x%,y%,warna%)
  else
   call vhuruf(bank$,segmen%,x%,y%,warna%)
  end if
 end if
loop until kode% = 27 or kode% = 13
fninput3$ = bank$
end def
sub Rdkey(KODE%,TPE%)
 V1$=inkey$
 if V1$<>"" then
  kode% = asc(right$(V1$,1))
  call panstr(v1$,tpe%)
 end if
end sub

sub BAR2 inline '(panjang, lebar, warna, segmen, x, y)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H06, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01
 $inline &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &HD1, &HE7, &HC5, &H76, &H0A, &HAD, &H01, &HC7, &HC5
 $inline &H76, &H16, &H8B, &H1C, &HC5, &H76, &H1A, &H8B, &H0C, &HC5
 $inline &H76, &H12, &HAC, &H89, &HCA, &HF3, &HAA, &H4B, &H74, &H0A
 $inline &H81, &HC7, &H40, &H01, &H29, &HD7, &H89, &HD1, &HEB, &HF1
 $inline &H1F, &H5D
End sub
sub MVBLOK inline '(segsum, x, y,segtuj, x, y,panjang,lebar)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H14, &HC5
 $inline &H76, &H0A, &H8B, &H0C, &HC5, &H76, &H16, &H8E, &H04, &HC5
 $inline &H76, &H0E, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01
 $inline &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &HD1, &HE7, &HC5, &H76, &H12, &HAD, &H01, &HC7, &HC5
 $inline &H76, &H1A, &HAD, &H89, &HC3, &HD1, &HE3, &HD1, &HE3, &H01
 $inline &HC3, &HD1, &HE3, &HD1, &HE3, &HD1, &HE3, &HD1, &HE3, &HD1
 $inline &HE3, &HD1, &HE3, &HC5, &H76, &H1E, &HAD, &H01, &HC3, &HC5
 $inline &H76, &H22, &HAD, &H8E, &HD8, &H89, &HDE, &H89, &HCB, &HF3
 $inline &HA4, &H4A, &H74, &H10, &H89, &HD9, &H81, &HC7, &H40, &H01
 $inline &H29, &HCF, &H81, &HC6, &H40, &H01, &H29, &HDE, &HEB, &HEB
 $inline &H1F, &H5D
End sub
'============================
sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub
def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
sub ADA inline '(pathname$+chr$[0], yesno%)
 $inline &H55, &H8B, &HEC, &H1E, &H8B, &H1E, &H00, &H00, &HB9, &H16
 $inline &H00, &H50, &H49, &H75, &HFC, &H16, &H1F, &H8B, &HD4, &HB4
 $inline &H1A, &HCD, &H21, &HC5, &H76, &H0A, &H8B, &H54, &H02, &H8E
 $inline &HDB, &HB4, &H4E, &HB9, &H17, &H00, &HCD, &H21, &H72, &H09
 $inline &HB8, &HFF, &HFF, &HC4, &H7E, &H06, &HAB, &HEB, &H07, &HC4
 $inline &H7E, &H06, &HB8, &H00, &H00, &HAB, &HB9, &H16, &H00, &H58
 $inline &H49, &H75, &HFC, &H1F, &H5D
end Sub
sub VP2 inline '(nama,segm,x,y) non transparent non buff
 $inline &H55, &H8B, &HEC, &H1E, &H6A, &H00, &H6A, &H00, &H8B, &H1E
 $inline &H00, &H00, &HC5, &H76, &H0E, &H8E, &H04, &HC5, &H76, &H06
 $inline &HAD, &H8B, &HF8, &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &HD1
 $inline &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &HC5, &H76, &H0A, &HAD, &H01, &HC7, &HC5, &H76, &H12
 $inline &H8B, &H54, &H02, &H8E, &HDB, &HB8, &H00, &H3D, &HCD, &H21
 $inline &H8B, &HD8, &HB4, &H3F, &HB9, &H04, &H00, &H16, &H1F, &H8B
 $inline &HD4, &HCD, &H21, &H8B, &HD7, &HB4, &H3F, &H06, &H1F, &H36
 $inline &H8B, &H4E, &HFA, &HCD, &H21, &H81, &HC2, &H40, &H01, &H36
 $inline &HFF, &H4E, &HFC, &H75, &HEC, &HB4, &H3E, &HCD, &H21, &H58
 $inline &H58, &H1F, &H5D
End Sub
sub PALFILE inline '(384buffer%(0), namafile)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H54, &H02, &H8E, &HD8, &HB8, &H00, &H3D, &HCD, &H21
 $inline &H1E, &H52, &H50, &H89, &HC3, &HB4, &H3F, &HC5, &H56, &H0A
 $inline &HB9, &H00, &H03, &HCD, &H21, &H89, &HD6, &HBA, &HC8, &H03
 $inline &HB0, &H00, &HEE, &HBA, &HC9, &H03, &HAC, &HEE, &HE0, &HFC
 $inline &H5B, &H5A, &H1F, &HB4, &H3E, &HCD, &H21, &H1F, &H5D
End sub
sub MOVE inline 'Call MOVE(ssum,osum,stuj,otuj,bany)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H16, &H8B, &H14, &H8E
 $inline &HDA, &H89, &HDE, &HF3, &HA5, &H1F, &H5D
End sub

index% = 1
screen 1
dim dynamic temp%(384)
dim dynamic back%(32766)
call pointer(back%(0),bgr%)
scr% = &HA000

? "mhn tngg"
do
 nam$ = "back\back"+fnkarakter$(index%)+".pst"
 call ada(nam$+chr$(0),ya%)
 if not ya% then exit loop
 index% = index% + 1
loop until instat
index% = index% - 1
? "selesai"
call layar19
if index% > 1 then timer on
on timer(5) gosub ganti
ke% = 0
gosub ganti

tanya:

b$ = fninput3$(scr%,80,0,12,"psw",bgr%)
call move(bgr%,0,scr%,0,&H8000)
call vhuruf("Masukkan Password: ",scr%,0,0,12)
if b$ <> "cmon" then
 call vhuruf("Password yang anda masukkan salah",scr%,0,188,12)
 goto tanya
end if

erase temp%
end
ganti:
 ke% = ke% + 1
 if ke% > index% then ke% = 1
 nam$ = "back\back"+fnkarakter$(ke%)
 call vp2(nam$+".pst"+chr$(0),bgr%,0,0)
 call move(bgr%,0,scr%,0,&H8000)
 call vhuruf("Masukkan Password: ",scr%,0,0,12)
 call palfile(temp%(0),nam$+".pal"+chr$(0))
 return