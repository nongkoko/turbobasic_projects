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
sub Rdkey(KODE%,TPE%)
 V1$=inkey$
 if V1$<>"" then
  kode% = asc(right$(V1$,1))
  call panstr(v1$,tpe%)
 end if
end sub
sub PPIMG2 inline '(segsum,offsum,segtuj,x,y)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &HAD, &H89, &HC7
 $inline &HD1, &HE7, &HD1, &HE7, &H01, &HC7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HC5, &H76
 $inline &H0A, &HAD, &H01, &HC7, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &HAD, &HC5, &H76, &H16, &H8E, &H1C, &H89, &HC6
 $inline &HAD, &H89, &HC3, &HAD, &H89, &HC1, &H89, &HDA, &HAC, &H3C
 $inline &H00, &H74, &H02, &HAA, &H4F, &H47, &H4A, &H74, &H02, &HEB
 $inline &HF3, &H49, &H74, &H0A, &H89, &HDA, &H29, &HDF, &H81, &HC7
 $inline &H40, &H01, &HEB, &HE6, &H1F, &H5D
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
sub MOVE inline 'Call MOVE(ssum,osum,stuj,otuj,bany)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H16, &H8B, &H14, &H8E
 $inline &HDA, &H89, &HDE, &HF3, &HA5, &H1F, &H5D
End sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub getnamafiledta(dump%(0), nam$)
 tmp$ = string$(12,0)
 call namadta(dump%(0),tmp$)
 nam$ = fnunascz$(tmp$)
end sub
def fnunascz$(ascz$)
 call STROASCZ(ascz$,tru%)
 fnunascz$ = left$(ascz$,tru%)
end def
sub STROASCZ inline '(string$,Lennya)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H31, &HDB, &HC5
 $inline &H76, &H0A, &H8B, &H0C, &H8B, &H74, &H02, &H8E, &HD8, &HE3
 $inline &H09, &HAC, &H3C, &H00, &H74, &H04, &H43, &H49, &HEB, &HF5
 $inline &H89, &HD8, &HC4, &H7E, &H06, &HAB, &H1F, &H5D
End Sub
sub NAMADTA inline '(dta%[22],nama$[12])
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H7C, &H02, &HC5, &H76, &H0A, &H83, &HC6, &H1E, &H8E
 $inline &HC0, &HB9, &H0C, &H00, &HF3, &HA4, &H1F, &H5D
End Sub
sub ATTRDTA inline '(dta%[22], attribut%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &H83, &HC6, &H15, &HA4, &H1F, &H5D
End Sub
sub CARI inline '(mask$,dumpdta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H56
 $inline &H0A, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0E, &H8B, &H54
 $inline &H02, &HB9, &H17, &H00, &HB8, &H00, &H4E, &H8E, &HDB, &HCD
 $inline &H21, &HB4, &H00, &HC4, &H7E, &H06, &HAB, &H1F, &H5D
End Sub
sub CARILAGI inline '(dta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &HB4, &H1A, &HC5, &H56, &H0A, &HCD
 $inline &H21, &HB8, &H00, &H4F, &HCD, &H21, &H30, &HE4, &HC4, &H7E
 $inline &H06, &HAB, &H1F, &H5D
End Sub
sub UKFIL2 inline '(namafile$+chr$[0], ukuranfile&)
 $inline &H55, &H8B, &HEC, &H1E, &HB9, &H16, &H00, &H50, &H49, &H75
 $inline &HFC, &H8B, &H1E, &H00, &H00, &H8B, &HD4, &H16, &H1F, &H1E
 $inline &H52, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0A, &H8B, &H54
 $inline &H02, &HB9, &H17, &H00, &H8E, &HDB, &HB4, &H4E, &HCD, &H21
 $inline &H5E, &H1F, &H83, &HC6, &H1A, &HC4, &H7E, &H06, &HA5, &HA5
 $inline &HB9, &H16, &H00, &H58, &H49, &H75, &HFC, &H1F, &H5D
End Sub
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
sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub
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
sub PANKAL inline '(memofont%[0], teks$, panjangpixel)
 $inline &H55, &H8B, &HEC, &H1E, &H6A, &H00, &H8B, &H1E, &H00, &H00
 $inline &HC5, &H76, &H0A, &H8B, &H0C, &H83, &HF9, &H00, &H74, &H40
 $inline &H8B, &H74, &H02, &H8E, &HDB, &H83, &HF9, &H00, &H74, &H4B
 $inline &H49, &H8A, &H3C, &H46, &H1E, &H56, &H51, &HC5, &H76, &H0E
 $inline &H8A, &H24, &HB6, &H00, &H8A, &HD4, &H46, &H31, &HC9, &HAC
 $inline &H38, &HF8, &H74, &H07, &H41, &H4A, &H74, &H24, &HE9, &HF4
 $inline &HFF, &H86, &HC4, &HB4, &H00, &H41, &H01, &HC8, &H8B, &HF0
 $inline &HAC, &HB4, &H00, &H40, &H36, &H01, &H46, &HFC, &H59, &H5E
 $inline &H1F, &HE9, &HC5, &HFF, &HC4, &H7E, &H06, &H26, &HC7, &H05
 $inline &H00, &H00, &HEB, &H14, &H36, &H83, &H46, &HFC, &H03, &H59
 $inline &H5E, &H1F, &HE9, &HB0, &HFF, &HC4, &H7E, &H06, &H36, &H8B
 $inline &H46, &HFC, &H48, &HAB, &H58, &H1F, &H5D
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
   call drag1
  elseif lvmb% = 2 then
   call drag2
  elseif lvmb% = 3 then
   call drag3
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
  elseif previousx%<lvxm% then
   lvmovement%=3
  elseif previousx%>lvxm% then
   lvmovement%=9
  elseif previousy%<lvym% then
   lvmovement%=6
  elseif previousy%>lvym% then
   lvmovement%=12
  end if
  previousx%=lvxm% : previousy%=lvym%
 else
  lvmovement% = 0
 end if
 If kounter% >0 then decr kounter%
end sub
'==================================================
sub dblclick
end sub
sub mbkey1down
 shared menu1.left%, menu1.top%, menu1.width%, menu1.height%, xm%, ym%
 xmlocal% = xm% / 2
 if fnarea(menu1.left%,menu1.top%,menu1.left%+menu1.width%,menu1.top%+menu1.height%,xmlocal%,ym%) then
  call menu1.onclick
 end if
end sub
sub mbkey2down
end sub
sub mbkey3down
end sub
sub mbkeyup
end sub
sub drag1
 shared lvmovement%, req%, canv%, font%(), bgrmoved%, back%, startindex%
 if lvmovement% = 6 then 'dragged and toward to down
  startindex% = startindex% + 1
 elseif lvmovement% = 12 then
  if startindex% > 0 then
   startindex% = startindex% - 1
  end if
 end if
 gosub viewfile
' gosub b2c
 req% = -1
end sub
sub drag2
end sub
sub drag3
end sub
sub GTMOSPST inline '(xm%, ym%, tm%)
 $inline &H55, &H89, &HE5, &H1E, &HB8, &H03, &H00, &HCD, &H33, &HC5
 $inline &H76, &H06, &H89, &H1C, &HC5, &H76, &H0A, &H89, &H14, &HC5
 $inline &H76, &H0E, &H89, &H0C, &H1F, &H5D
End Sub
def fnarea(x1%,y1%,x2%,y2%,xm%,ym%)
 if x1%<=xm% and xm%<=x2% and y1%<=ym% and ym%<=y2% then fnarea = -1
end def
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
def fnkarakter$(angka%)
local tmp$, tmp2$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
'=============== sub mouse button
sub menu1.onclick
 shared font%(), canv%
 call vhuruf(font%(0),canv%,0,180,"Test",15)
end sub

'=============== install font
call ukfil2("livan.lff"+chr$(0),ukuran&)
dim dynamic font%(ceil(ukuran&/2))
call instalfont(font%(0),"livan.lff"+chr$(0),erno%)
if erno% <> 0 then
 ? "Tak dapat menginstal font"
 end
end if

'=============== main prog
 screen 1
 call layar19
 dim dynamic filelist$(255)
 dim dynamic filesize&(255)
 dim dynamic memori%(32766)

 dim dynamic background%(32766)
 dim dynamic canvas%(32766)

 call pointer(background%(0),back%)
 call pointer(canvas%(0),canv%)
 call pointer(memori%(0),memo%)


'==== define menu
 menu1.caption$ = "File"
 menu1.left% = 0
 menu1.top% = 0
 menu1.height% = 8
 call pankal(font%(0),menu1.caption$,menu1.width%)
 menu2.caption$ = "Seting"
 menu2.left% = 30
 menu2.top% = 0
 menu2.height% = 8
 call pankal(font%(0),menu2.caption$,menu2.width%)
 filelist.left% = 0
 filelist.top% = 12
 filelist.width% = 150
 filelist.height% = 180

'======= install cursor
 call psimg("cursor.pst"+chr$(0),memo%,cursor%,nxt%)

'============ viewing
 call bar2(320,12,1,back%,0,0)
 call bar2(320,8,1,back%,0,192)
 call vhuruf(font%(0),back%,menu1.left%,menu1.top%,menu1.caption$,15)
 call vhuruf(font%(0),back%,menu2.left%,menu2.top%,menu2.caption$,15)
 gosub carifile

 gosub viewfile

 do
  kode% = 0
  call advmouse(2,xm%,ym%)
  call rdkey(kode%,tpt%)
  xm% = xm%/2
  onfilelist% = fnarea(filelist.left%,filelist.top%,filelist.left%+filelist.width%-1,filelist.top%+filelist.height%-1,xm%,ym%)

  if onfilelist% and lvmovement% <> 0 then
   baris% = ceil((ym%-filelist.top%+1)/12)
   gosub b2c
   top% = baris%*12
   call g2(canv%,0,top%-2,filelist.left%+filelist.width%-1,top%-2,15)
   call g2(canv%,0,top%+10,filelist.left%+filelist.width%-1,top%+10,15)
   req% = -1
  end if

  if lvmovement% <> 0 then
   gosub b2c
   call ppimg2(memo%,cursor%,canv%,xm%,ym%)
   call vhuruf(font%(0),canv%,200,0,fnkarakter$(xm%)+","+fnkarakter$(ym%),15)
   req% = -1
  end if

  if kode% <> 0 then
   gosub b2c
   call vhuruf(font%(0),canv%,200,180,fnkarakter$(kode%)+";"+fnkarakter$(tpt%),15)
   req% = -1
  end if

  if req% then
   call move(canv%,0,&Ha000,0,&H7D00)
   bgrmoved% = 0
  end if
 loop until kode% = 27
 end
end

carifile:
 erno% = 0
 fileindex% = 0
 viewfile% = -1
 dim dynamic dta%(22)
  call cari("*.*"+chr$(0), dta%(0), erno%)
  do
   if (erno% = 2) or (erno% = 18) then
    exit loop
   else
    call attrdta(dta%(0), attribut%)
    if (attribut% and &B10000) = &B10000 then
     call getnamafiledta(dta%(),temp$)
     filelist$(fileindex%) = temp$ +"\"
     filesize&(fileindex%) = 0
    else
     call getnamafiledta(dta%(),temp$)
     filelist$(fileindex%) = temp$
     call ukfil2(temp$+chr$(0),tempukfil&)
     filesize&(fileindex%) = tempukfil&
    end if
    incr fileindex%
   end if
   call carilagi(dta%(0),erno%)
  loop
 erase dta%
return

b2c:
 if not bgrmoved% then
  call move(back%,0,canv%,0,&H7D00)
  bgrmoved% = -1
 end if
 return

viewfile:
 call bar2(filelist.width%,filelist.height%,17,back%,filelist.left%,filelist.top%)
 for a% = startindex% to startindex%+14
  tempy% = (a%-startindex%)*12+12
  call vhuruf(font%(0),back%,0,tempy%,filelist$(a%),12)
  call vhuruf(font%(0),back%,80,tempy%,fnkarakter$(filesize&(a%)),12)
 next a%
 gosub b2c
 return