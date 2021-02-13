sub dblclick

end sub

sub mbkey1down

end sub

sub mbkey2down

end sub

sub mbkey3down

end sub

sub mbkeyup

end sub

sub GTMOSPST inline '(xm%, ym%, tm%)
 $inline &H55, &H89, &HE5, &H1E, &HB8, &H03, &H00, &HCD, &H33, &HC5
 $inline &H76, &H06, &H89, &H1C, &HC5, &H76, &H0A, &H89, &H14, &HC5
 $inline &H76, &H0E, &H89, &H0C, &H1F, &H5D
End Sub
sub advmouse(lvmode%, lvxm%, lvym%)
 shared previousmb%, lvmovement%
 if lvmode% = 1 then 'muncul
  reg(1),1
  call interrupt &H33
 elseif lvmode% = 2 then 'hide
  reg(1),2
  call interrupt &H33
 elseif lvmode% = 0 then 'mati
  reg(1),0
  call interrupt &H33
  exit sub
 end if
 call gtmospst(lvxm%, lvym%, lvmb%)
 if previousmb% <> lvmb% then 'tombol di klik atau di lepas
  if lvmb% = 1 then 'tombolkiriditekan
   if kounter% <> 0 then
    call dblclick
   else
    kounter% = 2500
   end if
   call mbkey1down
  elseif lvmb% = 2 then 'tombolkananditekan
   call mbkey2down
  elseif lvmb% = 3 then 'tombolkanankiriditekan
   call mbkey3down
  elseif lvmb% = 0 then 'tomboldilepas
   call mbkeyup
  end if
  previousmb% = lvmb%
 else
  if lvmb% = 1 then
   'call drag1
  elseif lvmb% = 2 then
   'call drag2
  elseif lvmb% = 3 then
   'call drag3
  end if
 end if
 if previousx%<>lvxm% or previousy%<>lvym% then
  if previousx%<lvxm% and previousy%<lvym% then 'kanan bawah
   lvmovement% = 5
  elseif previousx%<lvxm% and previousy%>lvym% then 'kanan atas
   lvmovement% = 1
  elseif previousx%>lvxm% and previousy%>lvym% then 'kiri atas
   lvmovement% = 11
  elseif previousx%>lvxm% and previousy%<lvym% then 'kiri atas
   lvmovement% = 7
  elseif previousx%<lvxm% then 'kanan
   lvmovement%=3
  elseif previousx%>lvxm% then 'kiri
   lvmovement%=9
  elseif previousy%<lvym% then 'bawah
   lvmovement%=6
  elseif previousy%>lvym% then 'atas
   lvmovement%=12
  end if
  previousx%=lvxm% : previousy%=lvym%
 else
  lvmovement% = 0
 end if
 If kounter% >0 then decr kounter%
end sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub

sub INSTALfont inline '(memorihuruf%[setengahukuranfile], namafont$, erno%)
 $inline &H55, &H8B, &HEC, &H1E, &H8B, &H1E, &H00, &H00, &HB9, &H16
 $inline &H00, &H50, &H49, &H75, &HFC, &HB4, &H1A, &H16, &H1F, &H8B
 $inline &HD4, &HCD, &H21, &HB4, &H4E, &HB9, &H17, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H54, &H02, &H8E, &HDB, &HCD, &H21, &H72, &H02
 $inline &HEB, &H09, &HC4, &H7E, &H06, &HB8, &H02, &H00, &HAB, &HEB
 $inline &H1A, &H8B, &HF4, &H83, &HC6, &H1A, &H36, &H8B, &H0C, &HB8
 $inline &H00, &H3D, &HCD, &H21, &H8B, &HD8, &HB4, &H3F, &HC5, &H56
 $inline &H0E, &HCD, &H21, &HB4, &H3E, &HCD, &H21, &HB9, &H16, &H00
 $inline &H58, &H49, &H75, &HFC, &H1F, &H5D
End Sub

sub VHURUF inline '(fontmemori%[0], segmen%, x, y, teks$, warna)
 $inline &HE9, &H4D, &H00, &H57, &H36, &H8A, &H4E, &HFB, &H80, &HF9
 $inline &H00, &H74, &H08, &H81, &HC7, &H40, &H01, &HFE, &HC9, &H75
 $inline &HF3, &H1E, &HC5, &H76, &H06, &H8A, &H24, &H1F, &H36, &H8A
 $inline &H76, &HFA, &H36, &H8B, &H76, &HFC, &H80, &HFE, &H00, &H74
 $inline &H25, &H57, &HFE, &HCE, &HAC, &H8A, &HD8, &HB2, &H01, &HB1
 $inline &H08, &H20, &HD0, &H75, &H11, &H8A, &HC3, &H81, &HC7, &H40
 $inline &H01, &HD0, &HE2, &HFE, &HC9, &H75, &HF0, &H5F, &H47, &HE9
 $inline &HDC, &HFF, &H26, &H88, &H25, &HE9, &HE9, &HFF, &H5F, &HC3
 $inline &H55, &H8B, &HEC, &H1E, &H50, &H50, &H8B, &H1E, &H00, &H00
 $inline &HC5, &H76, &H0E, &HAD, &H8B, &HF8, &HD1, &HE7, &HD1, &HE7
 $inline &H01, &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HC5, &H76, &H12, &H03, &H3C, &HC5
 $inline &H76, &H16, &H8E, &H04, &HC5, &H76, &H0A, &H8B, &H0C, &H8B
 $inline &H74, &H02, &H8E, &HDB, &HB7, &H00, &H83, &HF9, &H00, &H74
 $inline &H6C, &H49, &HAC, &H1E, &H56, &H51, &HC5, &H76, &H1A, &H8A
 $inline &H24, &H8A, &HDC, &H46, &HB7, &H00, &H38, &H04, &H74, &H13
 $inline &HFE, &HC7, &HFE, &HCB, &H74, &H04, &H46, &HE9, &HF2, &HFF
 $inline &H83, &HC7, &H03, &H59, &H5E, &H1F, &HE9, &HD5, &HFF, &H8A
 $inline &HD7, &HB6, &H00, &H8B, &HC8, &H86, &HCD, &HB5, &H00, &H8B
 $inline &HF2, &H01, &HCE, &H8A, &H5C, &H01, &H36, &H88, &H5E, &HFA
 $inline &H01, &HCE, &H8A, &H5C, &H01, &H36, &H88, &H5E, &HFB, &H01
 $inline &HCE, &H01, &HD6, &H8B, &H54, &H01, &H8B, &HC1, &HD1, &HE0
 $inline &HD1, &HE0, &H01, &HC8, &H01, &HD0, &H40, &H36, &H89, &H46
 $inline &HFC, &HE8, &H19, &HFF, &H36, &H8B, &H4E, &HFA, &HB5, &H00
 $inline &H01, &HCF, &H47, &H59, &H5E, &H1F, &HE9, &H8F, &HFF, &H58
 $inline &H58, &H1F, &H5D
End Sub

sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
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
'=============
sub VERLINE inline '(seg%, x%, y1%, y2%, w%)
 $inline &H55, &H8B, &HEC, &H1E, &HC5, &H76, &H16, &H8E, &H04, &HC5
 $inline &H76, &H0A, &H8B, &H14, &HC5, &H76, &H0E, &H8B, &H0C, &H39
 $inline &HD1, &H7C, &H02, &H87, &HD1, &H29, &HCA, &H42, &H8B, &HF9
 $inline &HD1, &HE7, &HD1, &HE7, &H01, &HCF, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HC5, &H76
 $inline &H12, &HAD, &H01, &HC7, &HC5, &H76, &H06, &HAC, &HAA, &H4A
 $inline &H74, &H06, &H81, &HC7, &H3F, &H01, &HEB, &HF6, &H1F, &H5D
End Sub
sub HORLINE inline '(seg%, y%, x1%, x2%, w%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H16, &H8E, &H04, &HC5
 $inline &H76, &H0A, &HAD, &HC5, &H76, &H0E, &H8B, &H0C, &H39, &HC8
 $inline &H78, &H02, &H87, &HC1, &H29, &HC1, &H41, &H50, &HC5, &H76
 $inline &H12, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01, &HC7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &H58, &H01, &HC7, &HC5, &H76, &H06, &HAC, &HF3
 $inline &HAA, &H1F, &H5D
End sub
sub G2 inline '(segmen,x1,y1,x2,y2,warna)
 $inline &H55, &H8B, &HEC, &H1E, &H6A, &H00, &H6A, &H00, &H6A, &H00
 $inline &HC5, &H76, &H1A, &H8E, &H04, &HC5, &H76, &H16, &H8B, &H04
 $inline &HC5, &H76, &H0E, &H8B, &H1C, &H39, &HD8, &H74, &H10, &HC5
 $inline &H76, &H12, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H14, &H39
 $inline &HD1, &H74, &H3A, &HEB, &H61, &HC5, &H76, &H12, &H8B, &H14
 $inline &HC5, &H76, &H0A, &H8B, &H0C, &H39, &HD1, &H7F, &H02, &H87
 $inline &HD1, &H8B, &HFA, &HD1, &HE7, &HD1, &HE7, &H01, &HD7, &HD1
 $inline &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &H01, &HC7, &H29, &HD1, &H41, &HC5, &H76, &H06, &HAC
 $inline &HAA, &H81, &HC7, &H3F, &H01, &H49, &H75, &HF8, &HE9, &H36
 $inline &H01, &H39, &HD8, &H7C, &H01, &H93, &H8B, &HF9, &HD1, &HE7
 $inline &HD1, &HE7, &H01, &HCF, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &H8B, &HCB
 $inline &H29, &HC1, &H41, &HC5, &H76, &H06, &HAC, &HF2, &HAA, &HE9
 $inline &H0D, &H01, &H36, &H89, &H56, &HFA, &H29, &HCA, &H83, &HFA
 $inline &H00, &H7F, &H03, &HF7, &HD2, &H42, &H36, &H89, &H5E, &HFC
 $inline &H29, &HC3, &H83, &HFB, &H00, &H7F, &H03, &HF7, &HD3, &H43
 $inline &H39, &HDA, &H7F, &H76, &H36, &H3B, &H46, &HFC, &H7C, &H08
 $inline &H36, &H87, &H46, &HFC, &H36, &H87, &H4E, &HFA, &H36, &H8B
 $inline &H56, &HFA, &H29, &HCA, &H83, &HFA, &H00, &H7F, &H0B, &H36
 $inline &HC7, &H46, &HF8, &HC0, &HFE, &HF7, &HD2, &H42, &HEB, &H06
 $inline &H36, &HC7, &H46, &HF8, &H40, &H01, &H8B, &HF9, &HD1, &HE7
 $inline &HD1, &HE7, &H01, &HCF, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &H8B, &HCB
 $inline &H8B, &HDA, &H29, &HCB, &HD1, &HE3, &HD1, &HE2, &H8B, &HC2
 $inline &H29, &HC8, &H41, &H36, &H89, &H46, &HFC, &HC5, &H76, &H06
 $inline &HAC, &HAA, &H36, &H83, &H7E, &HFC, &H00, &H7C, &H0E, &H36
 $inline &H01, &H5E, &HFC, &H36, &H03, &H7E, &HF8, &H49, &H75, &HED
 $inline &HE9, &H80, &H00, &H36, &H01, &H56, &HFC, &H49, &H75, &HE3
 $inline &HEB, &H77, &H36, &H3B, &H4E, &HFA, &H7C, &H08, &H36, &H87
 $inline &H4E, &HFA, &H36, &H87, &H46, &HFC, &H36, &H8B, &H5E, &HFC
 $inline &H29, &HC3, &H83, &HFB, &H00, &H7F, &H0B, &HF7, &HD3, &H43
 $inline &H36, &HC7, &H46, &HF8, &HFF, &HFF, &HEB, &H06, &H36, &HC7
 $inline &H46, &HF8, &H01, &H00, &H8B, &HF9, &HD1, &HE7, &HD1, &HE7
 $inline &H01, &HCF, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &H8B, &HCA, &H8B, &HC3
 $inline &HD1, &HE0, &H8B, &HD0, &H29, &HC8, &H36, &H89, &H46, &HFC
 $inline &H29, &HCB, &HD1, &HE3, &H41, &HC5, &H76, &H06, &HAC, &HAA
 $inline &H81, &HC7, &H3F, &H01, &H36, &H83, &H7E, &HFC, &H00, &H7C
 $inline &H0D, &H36, &H01, &H5E, &HFC, &H36, &H03, &H7E, &HF8, &H49
 $inline &H75, &HE9, &HEB, &H07, &H36, &H01, &H56, &HFC, &H49, &H75
 $inline &HE0, &H58, &H58, &H58, &H1F, &H5D
End Sub

sub u(x,y,z,vd,xmon%,ymon%)
 xmon% =   x / (vd+z) * vd
 ymon% = -(y / (vd+z) * vd)
end sub

sub px(st%,sp%,drj)
shared y(),z()
 for a% = st% to sp%
   call putar(y(a%),z(a%),drj,t1,t2)
   y(a%) = t1
   z(a%) = t2
 next a%
end sub

sub pz(st%,sp%,drj)
shared x(),y()
 for a% = st% to sp%
   call putar(x(a%),y(a%),drj,t1,t2)
   x(a%) = t1
   y(a%) = t2
 next a%
end sub

sub py(st%,sp%,drj)
shared x(),z()
 for a% = st% to sp%
   call putar(x(a%),z(a%),drj,t1,t2)
   x(a%) = t1
   z(a%) = t2
 next a%
end sub

sub putar(length,height,degrees,NEWLENGTH,NEWHEIGHT)
 bintang = 180/(22/7)
 newlength =  length * cos(degrees/bintang) + height * sin(degrees/bintang)
 newheight = -length * sin(degrees/bintang) + height * cos(degrees/bintang)
end sub

sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub
sub PXL inline '(seg,x%,y%,w)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8E, &H04, &HC5
 $inline &H76, &H0A, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01
 $inline &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &HD1, &HE7, &HC5, &H76, &H0E, &HAD, &H01, &HC7, &HC5
 $inline &H76, &H06, &HA4, &H1F, &H5D
End sub
sub MOVE inline 'Call MOVE(ssum,osum,stuj,otuj,bany)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H16, &H8B, &H14, &H8E
 $inline &HDA, &H89, &HDE, &HF3, &HA5, &H1F, &H5D
End sub
def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
'=========
screen 0
dim dynamic back%(32766)
dim dynamic canv%(32766)
dim dynamic hurufku%(819)

call instalFont(hurufku%(0), "e:\turbob~1\projek\livan.lff" + chr$(0), jeah%)

layar% = &HA000
call pointer(back%(0),bgr%)
call pointer(canv%(0),can%)

call layar19
'                y
'                |
'                |
'                ------x
'              /
'            / z

data -10,0,0
data -10,10,0
data 10,10,0
data 10,0,0
data -10,0,0
data -10,0,10
data -10,10,10
data -10,10,0
data -10,10,10
data 10,10,10

dim x(20), y(20), z(20)
jt% = 10 'jumlah titik
vd = 200 'view distancenya
xpus% = 160 'titik pusat x
ypus% = 100 ' titik pusat y

for t% = 1 to jt%
 read tx%,ty%,tz%
 x(t%) = tx%
 y(t%) = ty%
 z(t%) = tz%
next t%

randomize timer

butuhRender = true

plus% = 2
do
  call advmouse(1, myX%, myY%)
  if lvmovement% = 3 then
   call py(1, jt%, 10)
   butuhRender = true
  elseif lvmovement% = 9 then
   call py(1, jt%, -10)
   butuhRender = true
  end if

  if lvmovement% = 12 then
   call px(1,jt%,10)
   butuhRender = true
  elseif lvmovement% = 6 then
   call px(1, jt%, -10)
   butuhRender = true
  end if

  if butuhRender = true then
  butuhRender = false
  call move(bgr%,0,can%,0,&H8000)


  'call pz(1,jt%,1)

  call vhuruf(hurufku%(0), can%, 0,0, "i-gorithm:: leave_fun_for_me@yahoo.com", 14)
  call vhuruf(hurufku%(0), can%, 0,180, "press any key to continue :: tekan apa saja untuk melanjutkan", 14)
  call vhuruf(hurufku%(0), can%, 0,24, "this is a real 3D.. move the mouse please", 14)

  if (rnd*10) mod 2 = 0 then plus% = -plus%
  for a% = 1 to jt%-1
   call u(x(a%),y(a%),z(a%),vd,x1%,y1%)
   call u(x(a%+1),y(a%+1),z(a%+1),vd,x2%,y2%)
   call g2(can%,x1%+xpus%,y1%+ypus%,x2%+xpus%,cint(y2%+ypus%),10)
  next a%

  call pxl(can%,xpus%,ypus%,12)
  call move(can%,0,layar%,0,&H8000) 'kalo di VB itu BitBlt
  end if
loop until instat


end