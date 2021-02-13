sub st inline
 $inline "e:\aabin\stmsarea.bin"
end sub
sub PSIMG inline '(NAMAFILE,SEGTUJ,OFFTUJ,NEXTOFF)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H50, &HC5, &H76
 $inline &H12, &H8B, &H54, &H02, &H1F, &H1E, &H52, &HB8, &H00, &H3D
 $inline &HCD, &H21, &H89, &HC3, &HB8, &H02, &H42, &H31, &HC9, &H31
 $inline &HD2, &HCD, &H21, &H50, &HB8, &H00, &H42, &HCD, &H21, &H59
 $inline &HB4, &H3F, &HC5, &H76, &H0A, &H8B, &H14, &HC5, &H76, &H0E
 $inline &H8E, &H1C, &HCD, &H21, &HC4, &H7E, &H06, &H01, &HD0, &HAB
 $inline &HB4, &H3E, &H5A, &H1F, &HCD, &H21, &H1F, &H5D
End sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub Rdkey(KODE%,TPE%)
 V1$=inkey$
 if V1$<>"" then
  kode% = asc(right$(V1$,1))
  call panstr(v1$,tpe%)
 end if
end sub
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub
sub advmouse (lvmode$, lvxm%, lvym%, lvmb%, lvmovement$)
 lvmode$ = ucase$(lvmode$)
 aaa = instr(lvmode$,"APPEAR")
 bbb = instr(lvmode$,"HIDE")
 ccc = instr(lvmode$,"OFF")
 if aaa <> 0 then reg(1),1 : call interrupt &H33
 if bbb <> 0 then reg(1),2 : call interrupt &H33
 if ccc <> 0 then reg(1),0 : call interrupt &H33
 if aaa <> 0 or bbb <> 0 then
  call gtmospst (lvxm%, lvym%, lvmb%)
  if previousx<lvxm% then lvmovement$="RIGHT"
  if previousx>lvxm% then lvmovement$="LEFT"
  if previousy<lvym% then lvmovement$="DOWN"
  if previousy>lvym% then lvmovement$="UP"
  previousx=lvxm% : previousy=lvym%
 end if
end sub
sub GTMOSPST inline '(xm%, ym%, tm%)
 $inline &H55, &H89, &HE5, &H1E, &HB8, &H03, &H00, &HCD, &H33, &HC5
 $inline &H76, &H06, &H89, &H1C, &HC5, &H76, &H0A, &H89, &H14, &HC5
 $inline &H76, &H0E, &H89, &H0C, &H1F, &H5D
End Sub
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
sub MOVE inline 'Call MOVE(ssum,osum,stuj,otuj,bany)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H16, &H8B, &H14, &H8E
 $inline &HDA, &H89, &HDE, &HF3, &HA5, &H1F, &H5D
End sub
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
sub besarpst inline '(namafile$,  x%,   y%)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H0E
 $inline &H8B, &H54, &H02, &H8E, &HD8, &H1E, &H52, &HB8, &H00, &H3D
 $inline &HCD, &H21, &H89, &HC3, &HC5, &H56, &H0A, &HB9, &H02, &H00
 $inline &HB4, &H3F, &HCD, &H21, &HC5, &H56, &H06, &HB4, &H3F, &HCD
 $inline &H21, &HB4, &H3E, &H5A, &H1F, &HCD, &H21, &H1F, &H5D
End Sub

def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def

dim dynamic backg%(32766)
dim dynamic canvas%(32766)
dim dynamic memofont%(500)
dim dynamic gudang%(1000)

dim dynamic xpeluru%(1000)
dim dynamic ypeluru%(1000)


call pointer(gudang%(0),gdg%)
call pointer(backg%(0),bgr%)
call pointer(canvas%(0),can%)

call psimg("e:\pst\kapal.pst"+chr$(0),gdg%,kapal%,laba%)
call psimg("e:\pst\laba.pst"+chr$(0),gdg%,laba%,pluru%)
call psimg("e:\pst\pl.pst"+chr$(0),gdg%,pluru%,n%)
call besarpst("e:\pst\kapal.pst"+chr$(0),pkap%,lkap%)
call besarpst("e:\pst\pl.pst"+chr$(0),pkap%,lkap%)

call instalfont(memofont%(0), "livan.lff"+chr$(0),erno%)

screen 1
call layar19

do
 kode% = 0 : mv$ = "" : tm% = 0 : bgrtocan% = 0 : cantoscr% = 0
 reqcantoscr% = 0
 call rdkey(kode%,tpt%)
 call advmouse("HIDE",xm%,ym%,tm%,mv$)
 call st(0,0,640-pkap%*2,200-lkap%)
 xm% = xm% / 2

 if tm% = 1 then
  jumpel% = jumpel% + 1
  xpeluru%(jumpel%) = xm%
 end if

 for a% = 1 to jumpel%
  gosub bgrtocan
  ypeluru%(a%) = ypeluru%(a%) - 1
  call ppimg2(gdg%,pluru%,can%,xpeluru%(a%),ypeluru%(a%))
  reqcantoscr% = -1
 next a%

 if mv$ <> "" then
  gosub bgrtocan
  call ppimg2(gdg%,kapal%,can%,xm%,ym%)
  reqcantoscr% = -1
 end if

 if reqcantoscr% then gosub cantoscr
loop until kode% = 27
end

bgrtocan:
 if not bgrtocan% then
  call move(bgr%,0,can%,0,&H8000)
  bgrtocan% = -1
 end if
 return

cantoscr:
 if not cantoscr% then
  call move(can%,0,&Ha000,0,&H8000)
  cantoscr% = -1
 end if
 return