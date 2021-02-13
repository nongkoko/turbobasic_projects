'program untuk mengubah beberapa baris menjadi satu baris

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
sub BTFIL inline '(NAMFIL$, atribut%, hand%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H0C, &HC5, &H76, &H0E, &H8B, &H54, &H02, &H8E
 $inline &HDB, &HB4, &H3C, &HCD, &H21, &H72, &H06, &HC4, &H7E, &H06
 $inline &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB, &H1F
 $inline &H5D
End Sub
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
sub BCNUMF inline '(handler%, num, banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H1C, &HC5, &H56, &H0A, &HB4, &H3F, &HCD
 $inline &H21, &H1F, &H5D
End Sub
sub TLNUMF inline '(handler%,num,banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H1C, &HC5, &H56, &H0A, &HB4, &H40, &HCD
 $inline &H21, &H1F, &H5D
End Sub
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
screen 0
namafile$ = command$
if namafile$ = "" then namafile$ = "e:\dokumen\diktat\basic.txt"
ukuran& = fnukuranfile&(namafile$)

'flag
'0 = byte1,2,3 tidak berisi
'1 = byte1 berisi
'2 = byte2 berisi
'3 = byte3 berisi

call bkfil(namafile$+chr$(0),0,file%)
 call btfil("e:\coba.txt"+chr$(0),0,file2%)

 for byteke% = 1 to fnjumlahpage(ukuran&,32000)
  ukuran% = fnbytedipage(byteke%,ukuran&,32000)
  jumbac% = 0
  flag% = 0
  do
   if jumbac% < ukuran% then    'jika masih memungkikan
    call bcnumf(file%,byte1%,1) 'baca satu byte1
    incr jumbac%
    flag% = 1
    if byte1% = 13 then			'kalo byte tsb = 13
     if jumbac% < ukuran% then		'kalo masih memungkinkan
      call bcnumf(file%,byte2%,1)	'baca 1 untuk byte2
      incr jumbac%
      flag% = 2
      if byte2% = 10 then		'kalo byte2 = 10 maka
       if jumbac% < ukuran% then	'kalo masih memungkinkan
        call bcnumf(file%,byte3%,1)
        incr jumbac%
        flag% = 3
        if byte3% = 32 then
         call tlnumf(file2%,&H0A0D,2)
         call tlnumf(file2%,32,1)
         flag% = 0
        elseif byte3% <> 13 then
         call tlnumf(file2%,32,1)
         call tlnumf(file2%,byte3%,1)
         flag% = 0
        else
         call tlnumf(file2%,&H0A0D,2)
         flag% = 0
        end if
       end if
      else				'kalo bukan 10 maka
       call tlnumf(file2%,13,1)
       call tlnumf(file2%,byte2%,1)     'tulis byte1 dan byte2
       flag% = 0			'isi sudah kosong
      end if
     end if
    else				'kalo bukan 13 ya udah
     call tlnumf(file2%,byte1%,1)       'tulis spt biasa
     flag% = 0				'isi kosong
    end if
   end if
  loop until jumbac% = ukuran%		'kalo jumlah pembacaan sudah semua = stop
  if flag% = 1 then
   call tlnumf(file2%,byte1%,1)
  elseif flag% = 2 then
   call tlnumf(file2%,byte1%,1)
   call tlnumf(file2%,byte2%,1)
  elseif flag% = 3 then
   call tlnumf(file2%,byte1%,1)
   call tlnumf(file2%,byte2%,1)
   call tlnumf(file2%,byte3%,1)
  end if
 next byteke%

 call tpfil("e:\coba.txt"+chr$(0),file2%)
call tpfil(namafile$+chr$(0),file%)
? "finish"