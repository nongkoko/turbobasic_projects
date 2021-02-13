'program cari string di file
screen 0
sub UKFIL2 inline '(namafile$+chr$[0], ukuranfile&)
 $inline &H55, &H8B, &HEC, &H1E, &HB9, &H16, &H00, &H50, &H49, &H75
 $inline &HFC, &H8B, &H1E, &H00, &H00, &H8B, &HD4, &H16, &H1F, &H1E
 $inline &H52, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0A, &H8B, &H54
 $inline &H02, &HB9, &H17, &H00, &H8E, &HDB, &HB4, &H4E, &HCD, &H21
 $inline &H5E, &H1F, &H83, &HC6, &H1A, &HC4, &H7E, &H06, &HA5, &HA5
 $inline &HB9, &H16, &H00, &H58, &H49, &H75, &HFC, &H1F, &H5D
End Sub
def fnjumlahpage%(ukuranfile&, part&)
 fnjumlahpage% = ceil(ukuranfile& / part&)
end def
def fnbytedipage&(nopage%, ukuranfile&, part&)
local temp&
 temp& = ukuranfile& - (nopage%-1) * part&
 if temp& < part& then
  fnbytedipage& = temp&
 else
  fnbytedipage& = part&
 end if
end def
sub BKFIL inline '(namafile$, akses, filehandler%) 'akses 0read1write2readwrite
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H3D
 $inline &HC5, &H76, &H0A, &HAC, &HC5, &H76, &H0E, &H8B, &H54, &H02
 $inline &H8E, &HDB, &HCD, &H21, &H72, &H08, &HB4, &H00, &HC4, &H7E
 $inline &H06, &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB
 $inline &H1F, &H5D
End Sub
def fnkatake$(kk,m$,p$)
 local starthu, stophu,pan%
 m$ = m$ + p$
 if kk = 1 then
  starthu = 1
  stophu  = fnjedake(1,m$,p$)-1
 else
  starthu = fnjedake(kk-1,m$,p$)+1
  stophu  = fnjedake(kk,m$,p$)-1
  call panstr(m$,pan%)
  if stophu <= -1 then stophu = pan%
 end if
 fnkatake$ = fnkalimat$(m$,starthu,stophu)
end def
def fnkalimat$(master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub
def fnjedake(brp%,m$,p$)
local start%
 i.hrfke = brp%
 start% = 1
 do
  if i.hrfke <=0 then quitjedake
  start% = instr(start%,m$,p$) + 1
  i.hrfke = i.hrfke - 1
 loop
 quitjedake:
  fnjedake = start% - 1
end def
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub BCNUMF inline '(handler%, num, banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H1C, &HC5, &H56, &H0A, &HB4, &H3F, &HCD
 $inline &H21, &H1F, &H5D
End Sub
sub TPFIL inline '(handler%)
 $inline &H55, &H8B, &HEC, &H1E, &HC5, &H76, &H06, &H8B, &H1C, &HB4
 $inline &H3E, &HCD, &H21, &H1F, &H5D
End Sub
sub FBNUM inline '(start%, numersumber%, ygdicari%, banyakpencarian%, posisi%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8A, &H24, &HC5, &H76, &H16, &H8B, &H14, &HC5
 $inline &H76, &H12, &H01, &HD6, &H31, &HDB, &HE3, &H11, &HAC, &H38
 $inline &HC4, &H74, &H04, &H43, &H49, &HEB, &HF5, &HC4, &H7E, &H06
 $inline &H01, &HD3, &H26, &H89, &H1D, &H1F, &H5D
End Sub
sub STR2NUMF inline '(string$, offstr%, varnum%, banyak%)
 $inline &H55, &H8B, &HEC, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &HC4, &H7E, &H0A, &HC5, &H76, &H0E, &HAD
 $inline &HC5, &H76, &H12, &H8B, &H74, &H02, &H01, &HC6, &H8E, &HDB
 $inline &HF3, &HA4, &H1F, &H5D
End Sub
sub NUM2STRF inline '(varnum%, offset%, string$, banyak%)
 $inline &H55, &H8B, &HEC, &H1E, &H8E, &H06, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H7C, &H02, &HC5
 $inline &H76, &H0E, &HAD, &HC5, &H76, &H12, &H01, &HC6, &HF3, &HA4
 $inline &H1F, &H5D
End Sub
sub ADA inline '(pathname$+chr$[0], yesno%)
 $inline &H55, &H8B, &HEC, &H1E, &H8B, &H1E, &H00, &H00, &HB9, &H16
 $inline &H00, &H50, &H49, &H75, &HFC, &H16, &H1F, &H8B, &HD4, &HB4
 $inline &H1A, &HCD, &H21, &HC5, &H76, &H0A, &H8B, &H54, &H02, &H8E
 $inline &HDB, &HB4, &H4E, &HB9, &H17, &H00, &HCD, &H21, &H72, &H09
 $inline &HB8, &HFF, &HFF, &HC4, &H7E, &H06, &HAB, &HEB, &H07, &HC4
 $inline &H7E, &H06, &HB8, &H00, &H00, &HAB, &HB9, &H16, &H00, &H58
 $inline &H49, &H75, &HFC, &H1F, &H5D
end Sub

'========================
perintah$ = command$

if perintah$ = "" then
 ? "gunain namafile$ searchstring$"
 end
 'perintah$ = "g:\astab___.ttf ew"
end if

namafile$ = fnkatake$(1, perintah$, " ")
searchstring$ = fnkalimat$(perintah$, fnjedake(1,perintah$," ")+1,len(perintah$))
'? "namafile: "; namafile$;";"
call ada(namafile$+chr$(0), fileada%)
if not fileada% then
 end
end if

call ukfil2(namafile$+chr$(0), ukuranfile&)
dim dynamic memory%(16383) : call pointer(memory%(0), bank%)

call bkfil(namafile$+chr$(0),0,filehandler%)
pointercari% = 0
temp$ = searchstring$
'? "mencari: "; searchstring$

for a% = 1 to fnjumlahpage(ukuranfile&, 32766)
 untukdibaca% = fnbytedipage(a%, ukuranfile&, 32766)
 call bcnumf(filehandler%, memory%(0), untukdibaca%)
 call str2numf(searchstring$, 0, char%, 1)
 hasil%=-1
 do
  call fbnum(hasil%+1, memory%(0), char%, 32766-hasil%, hasil%)
  if hasilx% = hasil% then
   exit loop
  else
   hasilx% = hasil%
  end if
  call num2strf(memory%(0), hasil%, temp$, len(temp$))
  if ucase$(temp$) = ucase$(searchstring$) then
   print "file ";namafile$;" mau dibunuh ? ";
   input "",k$
   if instr("Yy",k$) <> 0 then
    kill namafile$
   end if
  end if
 loop
next a%
call tpfil(filehandler%)
end