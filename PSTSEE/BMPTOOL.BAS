sub BKFIL inline '(namafile$, akses, filehandler%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HB4, &H3D
 $inline &HC5, &H76, &H0A, &HAC, &HC5, &H76, &H0E, &H8B, &H54, &H02
 $inline &H8E, &HDB, &HCD, &H21, &H72, &H08, &HB4, &H00, &HC4, &H7E
 $inline &H06, &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB
 $inline &H1F, &H5D
End Sub
sub ptrmove(lvfileno%, lvbanyak&)
 call lng2int(lvbanyak&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,1)
end sub
sub BTFIL inline '(NAMFIL$, atribut%, hand%)
 $inline &H55, &H89, &HE5, &H1E, &H8B, &H1E, &H00, &H00, &HC5, &H76
 $inline &H0A, &H8B, &H0C, &HC5, &H76, &H0E, &H8B, &H54, &H02, &H8E
 $inline &HDB, &HB4, &H3C, &HCD, &H21, &H72, &H06, &HC4, &H7E, &H06
 $inline &HAB, &HEB, &H06, &HB4, &H80, &HC4, &H7E, &H06, &HAB, &H1F
 $inline &H5D
End Sub
sub BCNUMF inline '(handler%, num, banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H1C, &HC5, &H56, &H0A, &HB4, &H3F, &HCD
 $inline &H21, &H1F, &H5D
End Sub
sub goptr(lvfileno%, lvposi&)
 call lng2int(lvposi&,tmp%,tmp2%)
 call pdptr(lvfileno%,tmp%,tmp2%,0)
end sub
sub LNG2INT inline '(asf&,hi%,lo%)
 $inline &H55,&H89,&HE5,&H1E,&HC5,&H76,&HE,&H66,&H8B,&H14
 $inline &H89,&HD0,&HC4,&H7E,&H6,&HAB,&HB1,&H10,&H66,&HD3
 $inline &HEA,&H89,&HD0,&HC4,&H7E,&HA,&HAB,&H1F,&H5D
end sub
sub PDPTR inline '(hand%,cx,dx,mode)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0E, &H8B, &H0C, &HC5, &H76, &H0A, &H8B, &H14, &HC5
 $inline &H76, &H06, &HAC, &HB4, &H42, &HCD, &H21, &H1F, &H5D
End Sub
sub TLSTRF inline '(filehandler%, string$)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H06
 $inline &H8B, &H0C, &H8B, &H54, &H02, &HC5, &H76, &H0A, &H8B, &H1C
 $inline &H8E, &HD8, &HB4, &H40, &HCD, &H21, &H1F, &H5D
End Sub
sub POINTER inline '(array(), seg%)
 $inline &H55, &H89, &HE5, &H1E, &HC4, &H7E, &H06, &HC5, &H76, &H0A
 $inline &H8C, &HD8, &HAB, &H1F, &H5D
End sub
sub BC2MEM inline '(no_handler,seg,off,banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H12, &H8B, &H1C, &HC5
 $inline &H76, &H0A, &H8B, &H14, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8E, &H1C, &HB8, &H00, &H3F, &HCD, &H21, &H1F
 $inline &H5D
End Sub
sub PEEKS inline '(Segmen, Offset, StringTujuan$)
 $inline &H55, &H89, &HE5, &H1E, &H8E, &H06, &H00, &H00, &HC5, &H76
 $inline &H06, &H8B, &H0C, &H8B, &H7C, &H02, &HC5, &H76, &H0E, &H8B
 $inline &H04, &HC5, &H76, &H0A, &H8B, &H34, &H8E, &HD8, &HF3, &HA4
 $inline &H1F, &H5D
End sub
sub TPFIL inline '(namafile$, handler%)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H0A
 $inline &H8B, &H54, &H02, &HC5, &H76, &H06, &H8B, &H1C, &H8E, &HD8
 $inline &HB8, &H00, &H3E, &HCD, &H21, &H1F, &H5D
End Sub
sub tlfilmem inline '(filehandler, segmen, offset, banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H12, &H8B, &H1C, &HB4, &H40, &HC5, &H76, &H0A, &H8B
 $inline &H14, &HC5, &H76, &H0E, &H8E, &H1C, &HCD, &H21, &H1F, &H5D
End sub
sub TLNUMF inline '(handler%,num,banyak)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H06, &H8B, &H0C, &HC5
 $inline &H76, &H0E, &H8B, &H1C, &HC5, &H56, &H0A, &HB4, &H40, &HCD
 $inline &H21, &H1F, &H5D
End Sub
sub onekey inline '(buttno, codeno) '(kodetombol%, ascii%)
 $inline &H55, &H89, &HE5, &H1E, &H31, &HC0, &HCD, &H16, &HC4, &H7E
 $inline &H06, &HAA, &HC4, &H7E, &H0A, &H88, &HE0, &HAA, &H1F, &H5D
End sub
'=============================
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
sub bit2pal(lvbitmap$, lvpal$)
 dim dynamic livan%(512)
 call pointer(livan%(0), temp%)
 call bkfil(lvbitmap$+chr$(0),0,h1%)
  call bcnumf(h1%, tipe%, 2)
  if tipe% <> 19778 then print "error" : goto ending
  call goptr (h1%,54)
  call bc2mem(h1%,temp%,0,1024)
 call tpfil(lvbitmap$+chr$(0),h1%)
 call btfil(lvpal$+chr$(0),0,h1%)
  def seg = temp%
  for a% = 0 to 255
   b% = peek(a%*4)/5
   g% = peek(a%*4+1)/5
   r% = peek(a%*4+2)/5
   poke a%*3,r%
   poke a%*3+1,g%
   poke a%*3+2,b%
  next a%
  call tlfilmem(h1%, temp%, 0, 768)
  ending:
 call tpfil(lvpal$+chr$(0),h1%)
 erase livan%
end sub
sub bit2pst(lvbitmap$, lvpst$)
 print lvbitmap$
 call bkfil(lvbitmap$+chr$(0),0,satu%)
 call btfil(lvpst$+chr$(0),0,dua%)
  call bcnumf(satu%, tipe%, 2)
  if tipe% <> 19778 then
   print "tak dapat melanjutkan. file bukan file bitmap"
   goto selesai
  end if
  call goptr(satu%,10)
  call bcnumf(satu%,start&,4)
  call goptr(satu%,18)
  call bcnumf(satu%, ex&,4)
  call bcnumf(satu%, ye&,4)
  rex% = ceil(ex&/4)*4
  totimg& = cdbl(rex%) * ye&
  container% = ceil(32000/rex%) * rex%
  jumpage% = fnjumlahpage%(totimg&,container%)
  call tlnumf(dua%, ex&, 2)
  call tlnumf(dua%, ye&, 2)
  dim dynamic temp%(32000)
   call pointer(temp%(0), temp2%)
   call goptr(satu%, start&+totimg&)
   for pageke% = jumpage% to 1 step -1
    bytepage% = fnbytedipage%(pageke%,totimg&,container%)
    call ptrmove(satu%,-bytepage%)
    call bc2mem(satu%,temp2%,0,bytepage%)
    call ptrmove(satu%,-bytepage%)
    offs% = bytepage%
    for baris% = 1 to (bytepage%/rex%)
     offs% = offs% - rex%
     call tlfilmem(dua%, temp2%, offs%, ex&)
    next baris%
   next pageke%
  erase temp%
selesai:
  call tlstrf(dua%, "dari "+lvbitmap$+" oleh TB")
 call tpfil(lvbitmap$+chr$(0),satu%)
 call tpfil(lvpst$+chr$(0),dua%)
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
sub pcx2pst(namfil$,namfil2$)
 local total&
 call bkfil(namfil$+chr$(0), 0,satu%)
 call btfil(namfil2$+chr$(0), 0,dua%)
 call goptr(satu%, 4)
 call bcnumf(satu%, xmin%,2)
 call bcnumf(satu%, ymin%,2)
 call bcnumf(satu%, xmax%,2)
 call bcnumf(satu%, ymax%,2)
 call goptr(satu%, 65)
 call bcnumf(satu%, nplane%, 1)
 call bcnumf(satu%, bpl%, 2)
 totalbytes% = bpl% * nplane%
 xsize% = xmax% - xmin% + 1
 ysize% = ymax% - xmin% + 1
 call tlnumf(dua%, xsize%,2)
 call tlnumf(dua%, ysize%,2)
 call goptr(satu%, 128)
 do
  call bcnumf(satu%, kode%,1)
  flgmtp% = fnflagmultiplepcx(kode%)
  isi% = fnisipcx(kode%)
  if flgmtp% then
   call bcnumf(satu%, warna%,1)
   call tlstrf(dua%, string$(isi%,warna%))
   total& = total& + isi%
   if total& >= totalbytes%*clng(ysize%) then exit loop
  else
   call tlnumf(dua%, kode%, 1)
   total& = total& + 1
   if total& >= totalbytes%*clng(ysize%) then exit loop
  end if
 loop
 call tpfil(namfil$+chr$(0),satu%)
 call tpfil(namfil2$+chr$(0),dua%)
end sub
def fnukuranfile&(pathname$)
 pathname$ = pathname$ + chr$(0)
 call ukfil(pathname$, temp&)
 fnukuranfile& = temp&
end def
sub UKFIL inline '(NAMAFILE$, UKURAN&)
 $inline &H55, &H89, &HE5, &H1E, &HA1, &H00, &H00, &HC5, &H76, &H0A
 $inline &H8B, &H54, &H02, &H52, &H8E, &HD8, &HB8, &H00, &H3D, &HCD
 $inline &H21, &H89, &HC3, &H72, &H10, &HB8, &H02, &H42, &H31, &HC9
 $inline &H31, &HD2, &HCD, &H21, &HC4, &H7E, &H06, &HAB, &H89, &HD0
 $inline &HAB, &HB4, &H3E, &H5A, &HCD, &H21, &H1F, &H5D
End sub
sub pcx2pal(namfil$, namfil2$)
 call bkfil(namfil$+chr$(0), 0,nosatu%)
 call goptr(nosatu%, fnukuranfile(namfil$)-769)
 call bcnumf(nosatu%, kode%,1)
 if kode% = 12 then
  dim temp%(385)
  call bcnumf(nosatu%, temp%(0),768)
  def seg = varseg(temp%(0))
  pointer% = varptr(temp%(0))
  call btfil(namfil2$+chr$(0), 0,nodua%)
  for warna% = 0 to 255
   poke pointer%+warna%*3, peek(pointer%+warna%*3)/5
   poke pointer%+warna%*3+1, peek(pointer%+warna%*3+1)/5
   poke pointer%+warna%*3+2, peek(pointer%+warna%*3+2)/5
  next warna%
  call tlnumf(nodua%, temp%(0), 768)
  call tpfil(namfil2$+chr$(0), nodua%)
 end if
 call tpfil(namfil$+chr$(0), nosatu%)
 erase temp%
end sub
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
def fnkalimat$ (master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
perintah$ = command$ + " "
kata1$ = fnkatake(1,perintah$," ")
kata2$ = fnkatake(2,perintah$," ")
kata3$ = fnkatake(3,perintah$," ")
checkperintah:
if perintah$ = "" or kata1$ = "" or kata2$ = "" or kata3$ = "" then
 cls
 print "leaf fun bmp to pst converter. bmp harus 8 bit. pst akan ditimpa. pal juga"
 print "penggunaan: namaprogram <bmp> <ngapain> <output>"
 print "ngapain: b2p = bmp ke pst"
 print "         b2l = bmp ke pal"
 print "         p2p = pcx ke pst"
 print "         p2l = pcx ke pal"
 print "gunakan dengan huruf kecil. tekan apa saja untuk selesai"
 print "Update: 31-November-2004"
 call onekey(dum%, dum%)
 end
end if
proses$ = lcase$(kata2$)
if proses$ = "b2p" then
 call bit2pst(kata1$, kata3$)
elseif proses$ = "b2l" then
 call bit2pal(kata1$, kata3$)
elseif proses$ = "p2p" then
 call pcx2pst(kata1$, kata3$)
elseif proses$ = "p2l" then
 call pcx2pal(kata1$, kata3$)
else
 perintah$ = ""
 goto checkperintah
'debug
 call onekey(dum%,dum%)
end if