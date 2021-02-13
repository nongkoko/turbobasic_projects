'leave fun software
'fileindex adalah jumlahfile yang ditemukan
'filelist$ isi dari file-file itu, mulai dari 0 sampai jumlahfile - 1
screen 0
pathasal$ = fncurdir$ + "\"
if pathasal$ = "" then pathasal$ = "e:\myexe" + "\"

perintah$ = command$
if perintah$ = "" then
 perintah$ = "e:\"
end if

pathku$ = fnpathke(1, perintah$)
driveku$ = ucase$(fndrivefile(perintah$))
call str2num(driveku$, 0, nomordrive%, 0, 1)
call chdrv(nomordrive% - 65)
call chdirs(pathku$+chr$(0),erno%)

if fnada%(pathasal$ + "bmptool.exe") = 0 then
 print "file bmptool.exe tidak ada. pastikan ada pada folder yang sama "
 print "dengan program ini. program berhenti. tekan apa saja untuk selesai"
 call onekey(dum%, dum%)
 end
end if
sub STR2NUM inline '(string$, offstr%, varnum%, offset%, banyak%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H16, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &HC5, &H76, &H0A, &HAD, &HC4, &H7E, &H0E
 $inline &H01, &HC7, &HC5, &H76, &H12, &HAD, &HC5, &H76, &H16, &H8B
 $inline &H74, &H02, &H01, &HC6, &H8E, &HDA, &HF3, &HA4, &H1F, &H5D
End Sub
sub CHDIRs inline'(dir$,erno%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H54, &H02, &HB8, &H00, &H3B, &H8E, &HDB, &HCD
 $inline &H21, &H73, &H06, &HC4, &H7E, &H06, &H88, &HE0, &HAA, &H1F
 $inline &H5D
End sub
sub pokenum inline '(segmen, offset, varnum, jbyteygakanditransf)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H3C, &HC5, &H76, &H12, &H8E, &H04, &HC5
 $inline &H76, &H0A, &HF3, &HA4, &H1F, &H5D
End sub
sub PALMEM inline '(segmen%, offset%)
 $inline &H55, &H89, &HE5, &H1E, &HB0, &H00, &HBA, &HC8, &H03, &HEE
 $inline &HC5, &H76, &H0A, &HAD, &HC5, &H76, &H06, &H8B, &H34, &H8E
 $inline &HD8, &HB9, &H00, &H01, &HBB, &H03, &H00, &HAC, &HBA, &HC9
 $inline &H03, &HEE, &H4B, &H75, &HF8, &H49, &H74, &H02, &HEB, &HF0
 $inline &H1F, &H5D
End sub
sub onekey inline '(buttno, codeno) '(kodetombol%, ascii%)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
end sub

sub peeknum inline '(segmen, offset, varnum, banyakbyte)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H14, &HC5, &H76, &H12, &H8E, &H1C, &H89
 $inline &HD6, &HC4, &H7E, &H0A, &HF3, &HA4, &H1F, &H5D
End sub
sub ADA inline '(pathname$+chr$[0], dumpdta%[22], yesno%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H56
 $inline &H0A, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0E, &H8B, &H54
 $inline &H02, &H8E, &HDB, &HB9, &H17, &H00, &HB8, &H00, &H4E, &HCD
 $inline &H21, &H72, &H09, &HB8, &HFF, &HFF, &HC4, &H7E, &H06, &HAB
 $inline &HEB, &H07, &HB8, &H00, &H00, &HC4, &H7E, &H06, &HAB, &H1F
 $inline &H5D
End Sub
sub SIZEODTA inline '(dta%[22],ukufil&)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H83, &HC6, &H1A, &HA5, &HA5, &H1F, &H5D
End Sub
sub PXL inline '(seg,x,y,w)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8E, &H04, &HC5
 $inline &H76, &H0A, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01
 $inline &HC7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1
 $inline &HE7, &HD1, &HE7, &HC5, &H76, &H0E, &HAD, &H01, &HC7, &HC5
 $inline &H76, &H06, &HA4, &H1F, &H5D
End sub
sub SETPAL inline '(COLORNUMBER,R,G,B)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8A, &H04, &HBA
 $inline &HC8, &H03, &HEE, &HBA, &HC9, &H03, &HC5, &H76, &H0E, &H8A
 $inline &H04, &HEE, &HC5, &H76, &H0A, &H8A, &H04, &HEE, &HC5, &H76
 $inline &H06, &H8A, &H04, &HEE, &H1F, &H5D
End sub
sub MOVE inline 'Call MOVE(ssum,osum,stuj,otuj,bany)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0A, &H8B, &H3C, &HC5, &H76, &H0E, &H8E, &H04, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H16, &H8B, &H14, &H8E
 $inline &HDA, &H89, &HDE, &HF3, &HA5, &H1F, &H5D
End sub
sub PEEKS inline '(Segmen, Offset, StringTujuan$)
 $inline &H55, &H89, &HE5, &H1E, &H8E, &H06, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &H8B, &H7C, &H02, &HC5, &H76, &H0E, &H8B
 $inline &H04, &HC5, &H76, &H0A, &H8B, &H34, &H8E, &HD8, &HF3, &HA4
 $inline &H1F, &H5D
End sub
sub BC2MEM inline '(no_handler,seg,off,banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0A, &H8B, &H14, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8E, &H1C, &HB8, &H00, &H3F, &HCD, &H21, &H1F
 $inline &H5D
End Sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub QATTR inline 'CALL QATTR(y%,x%,tinggi%,panjang%,warna%)
 $inline &H55, &H8B, &HEC, &H1E, &H06, &HB4, &H0F, &HCD, &H10, &H3C
 $inline &H07, &H75, &H08, &HBB, &H00, &HB0, &HBA, &HBA, &H03, &HEB
 $inline &H06, &HBB, &H00, &HB8, &HBA, &HDA, &H03, &H53, &H07, &HC5
 $inline &H76, &H0E, &H8B, &H0C, &H51, &H52, &H33, &HDB, &H8A, &HDC
 $inline &HC5, &H76, &H16, &H8B, &H04, &H48, &HF7, &HE3, &HD1, &HE0
 $inline &HC5, &H76, &H12, &H8B, &H1C, &H4B, &HD1, &HE3, &H03, &HD8
 $inline &H8B, &HFB, &HC5, &H76, &H06, &H8B, &H1C, &HFC, &HC5, &H76
 $inline &H0A, &H8B, &H34, &H5A, &H57, &HFA, &H8B, &HCE, &H47, &HEC
 $inline &HA8, &H01, &H75, &HFB, &HEC, &HA8, &H01, &H74, &HFB, &H8A
 $inline &HC3, &HAA, &HE2, &HF0, &HFB, &H5F, &H59, &H49, &HE3, &H07
 $inline &H51, &H81, &HC7, &HA0, &H00, &HEB, &HDF, &H07, &H1F, &H5D
End sub
sub PALFILE inline '(384buffer%(0), namafile)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H54, &H02, &H8E, &HD8, &HB8, &H00, &H3D, &HCD, &H21
 $inline &H1E, &H52, &H50, &H89, &HC3, &HB4, &H3F, &HC5, &H56, &H0A
 $inline &HB9, &H00, &H03, &HCD, &H21, &H89, &HD6, &HBA, &HC8, &H03
 $inline &HB0, &H00, &HEE, &HBA, &HC9, &H03, &HAC, &HEE, &HE0, &HFC
 $inline &H5B, &H5A, &H1F, &HB4, &H3E, &HCD, &H21, &H1F, &H5D
End sub
sub GTMOSPST inline '(xm%, ym%, tm%)
 $inline &H55, &H89, &HE5, &H1E, &HB8, &H03, &H00, &HCD, &H33, &HC5
 $inline &H76, &H06, &H89, &H1C, &HC5, &H76, &H0A, &H89, &H14, &HC5
 $inline &H76, &H0E, &H89, &H0C, &H1F, &H5D
End Sub
sub VIEWPST inline '(bUFFERINT,NAMFIL$,SEG,X,Y)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &H50, &HC5, &H76
 $inline &H06, &HAD, &H89, &HC7, &HD1, &HE7, &HD1, &HE7, &H01, &HC7
 $inline &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7, &HD1, &HE7
 $inline &HD1, &HE7, &HC5, &H76, &H0A, &HAD, &H01, &HC7, &HC5, &H76
 $inline &H0E, &H8E, &H04, &HC5, &H76, &H12, &H8B, &H54, &H02, &H1F
 $inline &H1E, &H52, &HB8, &H00, &H3D, &HCD, &H21, &H89, &HC3, &HB8
 $inline &H02, &H42, &H31, &HC9, &H31, &HD2, &HCD, &H21, &H50, &HB8
 $inline &H00, &H42, &HCD, &H21, &H59, &HC5, &H56, &H16, &HB4, &H3F
 $inline &HCD, &H21, &H31, &HF6, &HAD, &H8B, &H0C, &HBE, &H04, &H00
 $inline &H50, &H89, &HC2, &HAC, &H3C, &H00, &H74, &H02, &HAA, &H4F
 $inline &H47, &H4A, &H74, &H02, &HEB, &HF3, &H49, &H58, &H74, &H08
 $inline &H29, &HC7, &H81, &HC7, &H40, &H01, &HEB, &HE4, &HB4, &H3E
 $inline &H5A, &H1F, &HCD, &H21, &H1F, &H5D
End sub
sub LAYAR19 inline
 $inline &HB8, &H13, &H00, &HCD, &H10
End sub
sub BKFIL inline '(namafile$, akses, filehandler%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H3D
 $inline &HC5, &H76, &H0A, &HAC, &HC5, &H76, &H0E, &H8B, &H54, &H02
 $inline &H8E, &HDB, &HCD, &H21, &H72, &H08, &HB4, &H00, &HC4, &H7E
 $inline &H06, &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB
 $inline &H1F, &H5D
End Sub
sub TPFIL inline '(namafile$, handler%)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H0A
 $inline &H8B, &H54, &H02, &HC5, &H76, &H06, &H8B, &H1C, &H8E, &HD8
 $inline &HB8, &H00, &H3E, &HCD, &H21, &H1F, &H5D
End Sub
sub BCFIL inline '(handler%, stringbuffer$)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H54, &H02, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H1C
 $inline &H8E, &HD8, &HB8, &H00, &H3F, &HCD, &H21, &H1F, &H5D
End Sub
sub BCFILSTR inline '(filehandler%, string$, offstr%, banyak%)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H0C, &HC5, &H76, &H0E, &H8B, &H54, &H02, &HC5, &H76
 $inline &H0A, &H8B, &H1C, &H01, &HDA, &HC5, &H76, &H12, &H8B, &H1C
 $inline &H8E, &HD8, &HB4, &H3F, &HCD, &H21, &H1F, &H5D
End Sub
sub BCFILNUM inline '(filehandler%, numvar%, offnum%, banyak%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HC5, &H76, &H0A, &HAD, &HC5, &H56
 $inline &H0E, &H01, &HC2, &HB4, &H3F, &HCD, &H21, &H1F, &H5D
End Sub
sub PDPTR inline '(hand%,cx,dx,mode)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0E, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H14, &HC5
 $inline &H76, &H06, &HAC, &HB4, &H42, &HCD, &H21, &H1F, &H5D
End Sub
sub LNG2INT inline '(asf&,hi%,lo%)
 $inline &H55,&H89,&HE5,&H1E,&HC5,&H76,&HE,&H66,&H8B,&H14
 $inline &H89,&HD0,&HC4,&H7E,&H6,&HAB,&HB1,&H10,&H66,&HD3
 $inline &HEA,&H89,&HD0,&HC4,&H7E,&HA,&HAB,&H1F,&H5D
end sub
sub CHDRV inline '(nomordrive%) 0 - floppy
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8A, &H14, &HB4
 $inline &H0E, &HCD, &H21, &H1F, &H5D
End sub
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
sub CARILAGI inline '(dta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &HB4, &H1A, &HC5, &H56, &H0A, &HCD
 $inline &H21, &HB8, &H00, &H4F, &HCD, &H21, &H30, &HE4, &HC4, &H7E
 $inline &H06, &HAB, &H1F, &H5D
End Sub
sub gtdir inline '(dir$) panjang string secukupnya
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H74, &H02, &HB8, &H00, &H47, &H8E, &HDB, &HBA
 $inline &H00, &H00, &HCD, &H21, &H1F, &H5D
End sub
sub GTDRV inline '(drv$)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H19
 $inline &HCD, &H21, &HC5, &H76, &H06, &H8B, &H7C, &H02, &H8E, &HC3
 $inline &H04, &H41, &HAA, &H1F, &H5D
End sub
sub CARI inline '(mask$,dumpdta%[22],erno%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H56
 $inline &H0A, &HB4, &H1A, &HCD, &H21, &HC5, &H76, &H0E, &H8B, &H54
 $inline &H02, &HB9, &H17, &H00, &HB8, &H00, &H4E, &H8E, &HDB, &HCD
 $inline &H21, &HB4, &H00, &HC4, &H7E, &H06, &HAB, &H1F, &H5D
End Sub
sub ATTRDTA inline '(dta%[22], attribut%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &H83, &HC6, &H15, &HA4, &H1F, &H5D
End Sub

'============================================
def fnada%(lvpathname$)
 dim dynamic dta%(22)
  call cari(lvpathname$+chr$(0), dta%(0), tmp%)
 erase dta%
 if (tmp% = 2) or (tmp% = 18) then
  fnada% = 0
 else
  fnada% = -1
 end if
end def
def fnukuranfile&(namafile$)
 dim dynamic dta%(22)
 local temp&
 call ada(namafile$+chr$(0), dta%(0), ada%)
 if ada% then
  call sizeodta(dta%(0), temp&)
  fnukuranfile& = temp&
 else
  fnukuranfile& = -1
 end if
 erase dta%
end def
def fndrivefile$(any$)
 tmp% = instr(any$,":")
 if tmp% <> 0 then
  fndrivefile$ = left$(any$,tmp%)
 end if
end def
'========================================================
dim filelist$(500)
dim baris%(20)
startindex% = 0
menuklikkanan% = 0
apakahhapus% = 0

gosub carifile
call atasbawahdrive
call viewfilelist(startindex%)
do
 kode% = 0
 tpt% = 0
 tm% = 0
 mov$ = ""
 call rdkey(kode%, tpt%)
 call advmouse("APPEAR", xm%, ym%, tm%, mov$)
 gosub definearea

 if listarea% then gosub padalistarea
 if buttonarea% then gosub padabuttonarea

 if mov$ <> "" then
  call label1("Locate "+fnkarakter$(ym%/8+1)+","+fnkarakter$(xm%/8+1)+"  ")
  if label3kotor% then call bersihlabel3 : label3kotor% = 0
  locate 1, 21 : print "Alamat : ";fncurdir$;"          "
  locate 1, 63 : print "Startindex% = ";fnkarakter$(startindex%);" "
 end if

loop until kode% = 27
end

carifile:
 erno% = 0
 fileindex% = 0
 delay (.2)
 viewfile% = -1
 viewdrive% = 0
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
    else
     call getnamafiledta(dta%(),temp$)
     filelist$(fileindex%) = temp$
    end if
    incr fileindex%
   end if
   call carilagi(dta%(0),erno%)
  loop
 erase dta%
return

caridrive:
 viewfile% = 0
 viewdrive% = -1
 for hitung% = 1 to fnjumlahdrive%
  locate hitung%,5 : print chr$(66+hitung%)+":"
 next hitung%
 return

definearea:
 listarea% = fnarea(32,0,136,152,xm%,ym%)
 buttonarea% = fnarea(144,0,144,152,xm%,ym%)
 upbutton% = fnarea(144,0,144,0,xm%,ym%)
 downbutton% = fnarea(144,8,144,8,xm%,ym%)
 drivebutton% = fnarea(144,16,144,16,xm%,ym%)
 for ke% = 1 to 20
  baris%(ke%) = fnarea(32,(ke%-1)*8,136,(ke%-1)*8,xm%,ym%)
 next ke%
 return

padalistarea:
 for mouseberada% = 1 to 20
  if baris%(mouseberada%) then exit for
 next mouseberada%

 if pstsblm% <> mouseberada% then 	'kalau mouse berpindah
  if not bersih% then
   call label3(space$(80))
   bersih% = -1
  end if
  if menuklikkanan% then
   apakahhapus% = 0
   locate 7,21
   print space$(50)
   menuklikkanan% = 0
  end if
  if not barumulai% then
   call qattr(pstsblm%,5,1,12,warnaasli%)
  end if
  call advmouse("HIDE", dum1%, dum2%, dum3%, dum$)
  warnaasli% = asc(fnwarnalayar$(mouseberada%,5,1))
  call qattr(mouseberada%,5,1,12,&H4F)
  pstsblm% = mouseberada%
  barumulai% = 0
  fileorfolder$ = filelist$(startindex% + mouseberada% - 1)
  extku$ = fnext$(fileorfolder$)
  ukuranfile& = fnukuranfile&(fileorfolder$)
  if not fnsuatufolder%(fileorfolder$) then
   call label2("Ukuran : "+fnkarakter$(ukuranfile&)+space$(8))
  else
   call label2(space$(50))
  end if
 end if	           'akhir dari kalo mouse berpindah

 if viewfile% then		'kalo lagi ngeliatin file trus
  if tm% = 1 or kode% = 13 then		'di klik kiri trus
   if fileorfolder$ = "..\" then	'filenya "..\"
    delay (.2)
    mask$ = fnpathke$(1,fncurdir$) + chr$(0)
    call chdirs(mask$, erno%)
    gosub carifile
    cls
    call viewfilelist(0)
   elseif right$(fileorfolder$,1) = "\" then	'filenya adalah direktori
    delay (.2)
    call chdirs(fileorfolder$+chr$(0), erno%)
    gosub carifile
    cls
    call viewfilelist(0)
   elseif extku$ = "PST" then			'filenya adalah pst
    indexpst% = 1
    dim dynamic temp%(32766)
    lihatpstlagi:
    call layar19
    if paleting% then
     dim dynamic tmp%(384)
     call palfile(tmp%(0), pal$+chr$(0))
     erase tmp%
    end if
    call viewpst(temp%(0), fileorfolder$ + chr$(0), &HA000,0,0)
    delay (.1)
    do
     tm% = 0
     call gtmospst(dum%,dum%,tm%)
     if tm% = 2 then
      checknextfile:
      fileorfolder$ = filelist$(startindex% + mouseberada% - 1 +indexpst%)
      if (startindex% + mouseberada% - 1 +indexpst%) > fileindex% then exit loop
      if fnext$(fileorfolder$) = "PST" then
       indexpst% = indexpst% + 1
       goto lihatpstlagi
      else
       indexpst% = indexpst% + 1
       goto checknextfile
      end if
     end if
   loop until tm% = 1
    delay (.1)
    reg 1, 3 : call interrupt &H10
    erase temp%
    call viewfilelist(startindex%)
   elseif extku$ = "BMP" then			'filenya adalah bmp
    delay (.2)
    indexbmp% = 1
    lihatbmplagi:
    call layar19
    call lihatbmp(fileorfolder$)
    do
     tm% = 0
     call gtmospst(dum4%,dum5%,tm%)
     if tm% = 2 then
      checkbmp:
      if (startindex% + mouseberada% - 1 + indexbmp%) > fileindex% then goto endloop
      fileorfolder$ = filelist$(startindex% + mouseberada% - 1 + indexbmp%)
      if fnext$(fileorfolder$) = "BMP" then
       indexbmp% = indexbmp% + 1
       goto lihatbmplagi
      else
       indexbmp% = indexbmp% + 1
       goto checkbmp
      end if
     end if
    loop until tm% = 1
    endloop:
    delay (.2)
    reg 1, 3 : call interrupt &H10
    call viewfilelist(startindex%)
   beep
  elseif extku$ = "PAL" then			'filenya adalah pal
    paleting% = -1
    pal$ = fnpath(fncurdir$) + fileorfolder$
    locate 20, 21 : print "Palette file : ";pal$;"   "
    delay(.2)
   elseif extku$ = "PCX" then
    indexpcx% = 1
    viewingpcx:
    call lihatpcx(fileorfolder$)
    do
     tm% = 0
     call gtmospst(dum%,dum%,tm%)
     if tm% = 2 then
      delay (.2)
      checkapakahpcx:
      fileorfolder$ = filelist$(startindex% + mouseberada% - 1 + indexpcx%)
      if (startindex% + mouseberada% - 1 + indexpcx%) > fileindex% then exit loop
      if fnext$(fileorfolder$) <> "PCX" then
       indexpcx% = indexpcx% + 1
       goto checkapakahpcx
      else
       indexpcx% = indexpcx% + 1
       goto viewingpcx
      end if
     end if
    loop until tm% = 1
    delay (.2)
    reg 1, 3 : call interrupt &H10
    call viewfilelist(startindex%)
   end if
  elseif tm% = 2 then				'kalo lagi ngeliatin file trus diklik kanan
   locate 7,21
   warnadefault% = 7
   bersih% = 0
   call formtext("$+12$Hapus $+12$Ubah_nama $;$")
   if extku$ = "BMP" then
    call formtext("$+12$Convert_ke_PST Convert_ke_$+12$PAL")
    act% = 1
   elseif extku$ = "PCX" then
    call formtext("$+12$Convert_ke_PST Convert_ke_$+12$PAL")
    act% = 2
   end if
   call label3("Silahkan gunakan keyboard untuk memilih menu")
   menuklikkanan% = -1
  end if
 end if

 if viewdrive% then 				'kalo lagi ngeliatin drive
  if tm% = 1 or kode% = 13 then
   call chdrv(mouseberada%+1)
   gosub carifile
   cls
   call viewfilelist(0)
  end if
 end if

 if menuklikkanan% then
  if instr("Hh",chr$(kode%)) <> 0 then
   apakahhapus% = -1
   locate 22,1 : call formtext("Apakah anda yakin untuk menghapus $10$"+fileorfolder$+"$7$($+12$Y/$+12$T)  ")
   bersih% = 0
  end if
  if instr("Uu",chr$(kode%)) <> 0 then
   call bersihlabel3
   locate 22,1 : namfilbar$ = fnmyinput$("Nama file baru : ")
   shell "ren "+fileorfolder$+" "+namfilbar$
   gosub carifile
   cls
   call viewfilelist(0)
   call bersihlabel3
  end if
  if instr("Cc",chr$(kode%)) <> 0 then
   call bersihlabel3
   locate 22,1 : namfilpst$ = fnmyinput$("Nama file PST : ")
   select case act%
    case 1
     cls
     shell fnpath$(pathasal$)+"bmptool "+fileorfolder$+" b2p "+namfilpst$
    case 2
     shell fnpath$(pathasal$)+"bmptool "+fileorfolder$+" p2p "+namfilpst$
   end select
   gosub carifile
   cls
   call viewfilelist(0)
  end if
  if instr("Pp",chr$(kode%)) <> 0 then
   call bersihlabel3
   locate 22,1 : namfilpal$ = fnmyinput$("Nama file PAL : ")
   select case act%
    case 1
     shell fnpath$(pathasal$)+"bmptool "+fileorfolder$+" b2l "+namfilpal$
    case 2
     shell fnpath$(pathasal$)+"bmptool "+fileorfolder$+" p2l "+namfilpal$
   end select
   gosub carifile
   cls
   call viewfilelist(0)
  end if
 end if
 if apakahhapus% then
  if instr("Yy", chr$(kode%)) <> 0 then
   kill fileorfolder$
   gosub carifile
   cls
   call viewfilelist(0)
   apakahhapus% = 0
  end if
  if instr("Tt", chr$(kode%)) <> 0 then
   cls
   call viewfilelist(0)
   apakahhapus% = 0
  end if
 end if
 return

padabuttonarea:
 if downbutton% then
  if (tm% = 1) and (startindex%+20 < fileindex%) then
   startindex% = startindex% + 1
   call viewfilelist(startindex%)
  end if
  call label3("Gulung ke atas ")
  bersih% = 0
 end if
 if upbutton% then
  if (tm% = 1) and (startindex% > 0) then
   startindex% = startindex% - 1
   call viewfilelist(startindex%)
  end if
  call label3("Gulung ke bawah")
  bersih% = 0
 end if
 if drivebutton% then
  if tm% = 1 then
   cls
   gosub caridrive
  end if
  call label3("Pindah Drive   ")
  bersih% = 0
 end if
 return
'============================================================
sub viewfilelist(startindex%)
 shared filelist$(), fileindex%, barumulai%
 barumulai% = -1
 color 6
 locate 1,19 : print chr$(24)
 locate 2,19 : print chr$(25)
 locate 3,19 : print chr$(08)
 color 7
 baris% = 1
 for hitung% = startindex% to startindex%+19
  if hitung% < fileindex% then
   filenya$ = filelist$(hitung%)
   locate baris%,1
   print using "\  \";fnkarakter$(hitung%);
   extku$ = fnext$(filenya$)
   select case extku$
    case "PST"
     color 12
    case "PAL"
     color 9
    case "BMP"
     color 10
    case "PCX"
     color 6
   end select
   if right$(filenya$,1) = "\" then color 14
   print using "\          \";lcase$(filenya$)
   locate baris%, 20 : color 7
   print "º"
   incr baris%
  end if
 next hitung%
end sub
def fnflagmultiplepcx(byte)
 if (byte AND &HC0) = &HC0 then
  fnflagmultiplepcx = -1
 else
  fnflagmultiplepcx = 0
 end if
end def
def fnisipcx(kode)
 fnisipcx = kode and &H3F
end def
sub lihatpcx(namfil$)
 local x%, y%
 call layar19
 call bkfil(namfil$+chr$(0), 0,nohan%)
  call goptr(nohan%, 4)
  call bcfilnum(nohan%, xmin%, 0,2)
  call bcfilnum(nohan%, ymin%, 0,2)
  call bcfilnum(nohan%, xmax%, 0,2)
  call bcfilnum(nohan%, ymax%, 0,2)
  call goptr(nohan%, 65)
  call bcfilnum(nohan%, nplane%, 0,1)
  call bcfilnum(nohan%, bpl%, 0,2)
  totalbytes% = bpl% * nplane%
  xsize% = xmax% - xmin% + 1
  ysize% = ymax% - ymin% + 1
  call goptr(nohan%, fnukuranfile&(namfil$)-769)
  call bcfilnum(nohan%, kode%, 0,1)
  if kode% = 12 then
   dim temp%(385)
   call bcfilnum(nohan%, temp%(0), 0,768)
   segmn% = varseg(temp%(0))
   def seg = segmn%
   pointer% = varptr(temp%(0))
   for warna% = 0 to 255
    poke pointer%+warna%*3, peek(pointer%+warna%*3)/5
    poke pointer%+warna%*3+1, peek(pointer%+warna%*3+1)/5
    poke pointer%+warna%*3+2, peek(pointer%+warna%*3+2)/5
   next warna%
   call palmem(segmn%, pointer%)
  end if
  erase temp%
  call goptr(nohan%, 128)
  do
   call bcfilnum(nohan%, kode%, 0,1)
   flgmtp% = fnflagmultiplepcx(kode%)
   if flgmtp% then
    call bcfilnum(nohan%, warna%, 0,1)
    for a% = 1 to fnisipcx(kode%)
     call pxl(&HA000, x%, y%, warna%)
     x% = x% + 1
     if x% >= totalbytes% then
      y% = y% + 1
      if y% = ysize% then exit loop
      x% = 0
     end if
    next a%
   else
    call pxl(&HA000, x%, y%, kode%)
    x% = x% + 1
    if x% >= totalbytes% then
     y% = y% + 1
     if y% = ysize% then exit loop
     x% = 0
    end if
   end if
  loop
 call tpfil(namfil$+chr$(0), nohan%)
end sub
def fntekslayar$(y%,x1%,banyak%)
local bank$
 if banyak% = 0 or y% <= 0 or x1% <= 0 then
  fntekslayar$ = ""
  exit def
 end if
 y%  = y% - 1
 x1% = x1% - 1
 def seg   = &HB800
 tinggi%   = y%*160
 for maju% = x1% to x1%+banyak%-1
  startoff% = tinggi% + maju%*2
  bank$ = bank$ + chr$(peek(startoff%))
 next maju%
 fntekslayar$ = bank$
end def
def fnmyinput$(lvcoment$)
 print lvcoment$;
 lvbaris%  = csrlin
 lvkolom%  = pos
 bank$   = ""
 getwarn = -1
 do
  kode% = 0
  tpt%  = 0
  mv$   = ""
  tm%    = 0
  call rdkey(kode%,tpt%)
  call advmouse("APPEAR",xm%,ym%,tm%,mv$)
  if tm% = 0 and grab% = 0 then
   xl% = xm%/8+1
   yl% = ym%/8+1
   panjang% = 1
   tinggi%  = 1
  else 'kalau tm% <> 0 or grab% <> 0
   xl2% = xm%/8+1
   yl2% = ym%/8+1
   panjang% = xl2% - xl%+1
   if panjang% <= 0 then panjang% = 1
   if getwarn = -1 then
    call advmouse("HIDE",xm%,ym%,tm%,mv$)
    warn$ = fnwarnalayar$(yl%,xl%,1)
   end if
   CALL QATTR(yl%,xl%,1,panjang%,&H14)
   call qattr(yl%,xl%+panjang%,1,1,asc(warn$))
   getwarn = 0
   grab% = -1
  end if
  if tm% = 0 and grab% = -1 then
   CALL QATTR(yl%,xl%,1,panjang%,asc(warn$))
   tekscap$ = fntekslayar$(yl%,xl%,panjang%)
   grab%    = 0
   getwarn  = -1
   bank$    = bank$ + tekscap$
   locate lvbaris%, lvkolom% : print bank$+chr$(26)+" ";
  end if
  if (kode% = 13) and (tpt% = 1) then goto finish2
  if (kode% = 27) and (tpt% = 1) then
     bank$ = chr$(27)
     goto finish2
  end if
  if (kode% = 61) and (tpt% = 2) then 'tombol F3
     bank$ = bank$ + banklama$
     locate lvbaris%, lvkolom%
     print bank$+chr$(26)+" ";
     goto toll2
  end if
  if (kode% = 8) and (tpt% = 1) then
     if len(bank$) > 0 then bank$ = left$(bank$,len(bank$)-1)
     locate lvbaris%, lvkolom%
     print bank$+chr$(27)+" ";
     goto toll2
  end if
  if kode% <> 0 then
     bank$ = bank$ + chr$(kode%)
     locate lvbaris%, lvkolom%
     print bank$+chr$(26)+" ";
  end if
  toll2:
 loop
 finish2:
 banklama$ = bank$
 fnmyinput$ = bank$
end def
def fnwarnalayar$(y%,x1%,banyak%)
local bank$
 if banyak% = 0 or y% <= 0 or x1% <= 0 then
  fnwarnalayar$ = ""
  exit def
 end if
 y%  = y% - 1
 x1% = x1% - 1
 def seg   = &HB800
 tinggi%   = y%*160
 for maju% = x1% to x1%+banyak%-1
  startoff% = tinggi% + maju%*2+1
  bank$ = bank$ + chr$(peek(startoff%))
 next maju%
 fnwarnalayar$ = bank$
end def
def fnext$(any$)
 for hurufke% = len(any$) to 1 step -1
  if mid$(any$, hurufke%, 1) = "." then
   exit for
  elseif mid$(any$, hurufke%, 1) = "\" then
   exit def
  end if
 next hurufke%
 fnext$ = mid$(any$,hurufke%+1)
end def
def fnjumlahdrive%
 dim dynamic dta%(22)
  tmp% = 0
  do
   tmp% = tmp% + 1
   call cari(chr$(98+tmp%)+":\"+chr$(0), dta%(0), erno%)
  loop until erno% = 3
  fnjumlahdrive% = tmp% - 1
 erase dta%
end def
def fnbaca$(lvfileno%, lvbanyak%)
 temp$ = string$(lvbanyak%,0)
 call bcfilstr(lvfileno%,temp$,0,lvbanyak%)
 fnbaca$ = temp$
end def
sub goptr (lvfileno%, lvposi&)
 call lng2int(lvposi&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,0)
end sub
sub ptrmove(lvfileno%, lvbanyak&)
 call lng2int(lvbanyak&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,1)
end sub
sub formtext(text$)
 shared warnadefault%
 for hurufke% = 1 to len(text$)
  huruf$ = mid$(text$,hurufke%,1)
  if huruf$ = "$" then
   keyword$ = fnkalimat$(text$,hurufke%+1,instr(hurufke%+1,text$,"$")-1)
   if keyword$ = "" then
    print "$";
   else
    if asc(keyword$) >=48 and asc(keyword$) <= 57 then
     color val(keyword$)
    else
     if keyword$ = ";" then
      lanjut = -1
     else
      if left$(keyword$,1) = "+" then
       color val(right$(keyword$,len(keyword$)-1))
       print mid$(text$,hurufke%+len(keyword$)+1+1,1);
       color warnadefault%
       hurufke% = hurufke% + 1
      end if
     end if
    end if
   end if
   hurufke% = hurufke% + len(keyword$)+1
  else
   print huruf$;
  end if
 next hurufke%
 if not lanjut then print
end sub
def fnpathke$(berapa%, pathname$)
 local jmlkata%, temp$
 jmlkata% = fngeneration%(pathname$+" ")
 for katake% = 1 to jmlkata%-berapa%
  temp$ = temp$ + fnpath$(fnkatake$(katake%,pathname$,"\"))
 next katake%
 fnpathke$ = temp$
end def
def fnpath$ (any$)
 local a%
 if instr(any$, ".") = 0 then
  path$ = any$
  if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
  fnpath$ = path$
  exit def
 end if
 For a% = len(any$) to 1 step - 1
  if mid$(any$, a%,1) = "\" then exit for
 Next a%
 if a% = 0 then
  path$ = any$
 else
  path$ = left$(any$, a%)
 end if
 if (right$(path$,1) <> "\") and (path$ <> "") then path$ = path$ + "\"
 fnpath$ = path$
end def
def fnkatake$ (kk,m$,p$)
 m$ = m$ + p$
 if kk = 1 then
  starthu = 1
  stophu  = fnjedake(1,m$,p$)-1
 else
  starthu = fnjedake(kk-1,m$,p$)+1
  stophu  = fnjedake(kk,m$,p$)-1
  if stophu <= -1 then stophu = len(m$)
 end if
 fnkatake$ = fnkalimat(m$,starthu,stophu)
end def
def fnjedake (brp%,m$,p$)
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
sub getnamafiledta(dump%(0),thestring$)
 local tmp$
 tmp$ = string$(12,0)
 call namadta(dump%(0),tmp$)
 thestring$ = fnunascz$(tmp$)
end sub
def fnkalimat$ (master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
def fngeneration%(pathname$)
 local jmlkata%, temp%, temp$
 if pathname$ <> "" then
  do
   temp% = instr(temp%+1, pathname$,"\")
   temp$ = mid$(pathname$,temp%+1,1)
   if (temp$ <> "\") and (temp$ <> "") then jmlkata% = jmlkata% + 1
  loop until temp% = 0
 end if
 fngeneration% = jmlkata%
end def
def fnkarakter$(angka)
 local tmp$, tmp2$
 if angka < 0 then tmp$ = "-"
 tmp2$ = str$(angka)
 tmp$ = tmp$ + right$(tmp2$, len(tmp2$)-1)
 fnkarakter$ = tmp$
end def
def fnarea(x1%,y1%,x2%,y2%,xm%,ym%)
 if x1%<=xm% and xm%<=x2% and y1%<=ym% and ym%<=y2% then fnarea = -1
end def
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
sub Rdkey(KODE%,TPE%)
 V1$ = inkey$
 if V1$ <> "" then
  kode% = asc(right$(V1$,1))
  tpe% = len(V1$)
 end if
end sub
def fnunascz$(ascz$)
 local tru%
 call STROASCZ(ascz$,tru%)
 fnunascz$ = left$(ascz$,tru%)
end def
def fncurdir$
 livan$ = string$(60,0)
 call gtdir(livan$)
 fncurdir$ = fncurdrive$+"\"+fnunascz$(livan$)
end def
def fncurdrive$
 tmp$ = " "
 call gtdrv(tmp$)
 fncurdrive$ = tmp$+":"
end def
sub atasbawahdrive
 locate 1,19 : print chr$(24)
 locate 2,19 : print chr$(25)
 locate 3,19 : print chr$(08)
end sub
sub label1(teks$)
 locate 23,1 : print teks$
end sub
sub label2(teks$)
 locate 21,21 : print teks$
end sub
sub label3(teks$)
 shared label3kotor%
 label3kotor% = -1
 locate 22, 1 : print teks$
end sub
sub bersihlabel3
 call label3(space$(79))
end sub
def fnsuatufolder%(namfil$)
 if right$(namfil$,1) = "\" then
  fnsuatufolder% = -1
 else
  fnsuatufolder% = 0
 end if
end def

sub lihatbmp(namfil$)
 local nohan%
 call bkfil(namfil$+chr$(0),0,nohan%)
  if nohan% < 0 then beep : goto akhirlihat
  call goptr(nohan%,10)
  call bcfilnum(nohan%,strtgbr&,0,2)
  call goptr(nohan%,18)
  call bcfilnum(nohan%,panjang&,0,4)
  call bcfilnum(nohan%,lebar&,0,4)
  call goptr(nohan%,54)

  dim dynamic pal%(512)
   call bcfilnum(nohan%,pal%(0),0,1024)
   call pointer(pal%(0),tempseg%)
   for warna% = 0 to 255
    tempoff% = warna%*4
    call peeknum(tempseg%,tempoff%,b%,1)
    call peeknum(tempseg%,tempoff%+1,g%,1)
    call peeknum(tempseg%,tempoff%+2,r%,1)
    b% = b%/5
    g% = g%/5
    r% = r%/5
    call setpal(warna%,r%,g%,b%)
   next warna%
  erase pal%
  call goptr(nohan%,strtgbr&)
  rpanjang& = ceil(panjang&/4) * 4
  total& = rpanjang& * lebar&
  dim dynamic buff%(32766)
   call pointer(buff%(0),tempseg%)
   call bc2mem(nohan%,tempseg%,0,total&)
   tempoff& = total&
   panjang% = panjang&/2
   for y% = 1 to lebar&-1
    tempoff& = tempoff& - rpanjang&
    call move(tempseg%,tempoff&,&HA000,320*y%,panjang%)
   next y%
  erase buff%

 akhirlihat:
 call tpfil(namfil$+chr$(0),nohan%)
end sub