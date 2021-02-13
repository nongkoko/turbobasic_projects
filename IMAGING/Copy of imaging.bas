$include "e:\aamsn\dos\ukfil2.uni"
$include "e:\aamsn\file\pdptr.uni"
$include "e:\aamsn\math\lng2int.uni"
$include "e:\aauni\goptr.uni"
$include "e:\aamsn\imaging\cls.uni"
$include "e:\aamsn\dos\1key.uni"
$include "e:\aamsn\dos\ada.uni"
$include "e:\aamsn\dos\attrdta.uni"
$include "e:\aamsn\dos\cari.uni"
$include "e:\aamsn\dos\carilagi.uni"
$include "e:\aamsn\dos\gtmospst.uni"
$include "e:\aamsn\dos\namadta.uni"
$include "e:\aamsn\fast\bcnumf.uni"
$include "e:\aamsn\fast\tlnumf.uni"
$include "e:\aamsn\fast\tlstrf.uni"
$include "e:\aamsn\file\bkfil.uni"
$include "e:\aamsn\file\btfil.uni"
$include "e:\aamsn\file\tpfil.uni"
$include "e:\aamsn\imaging\320\bar2.uni"
$include "e:\aamsn\imaging\320\g2.uni"
$include "e:\aamsn\imaging\320\layar19.uni"
$include "e:\aamsn\imaging\320\ppimg2.uni"
$include "e:\aamsn\imaging\320\pxl.uni"
$include "e:\aamsn\imaging\320\vhuruf.uni"
$include "e:\aamsn\imaging\320\vp.uni"
$include "e:\aamsn\imaging\gtpal.uni"
$include "e:\aamsn\imaging\instal~1.uni"
$include "e:\aamsn\imaging\palfile.uni"
$include "e:\aamsn\imaging\pankal.uni"
$include "e:\aamsn\imaging\psimg.uni"
$include "e:\aamsn\imaging\savepal.uni"
$include "e:\aamsn\imaging\setpal.uni"
$include "e:\aamsn\variable\fill.uni"
$include "e:\aamsn\variable\move.uni"
$include "e:\aamsn\variable\mvblok.uni"
$include "e:\aamsn\variable\num2num.uni"
$include "e:\aamsn\variable\panstr.uni"
$include "e:\aamsn\variable\pointer.uni"
$include "e:\aamsn\variable\stroascz.uni"
$include "e:\aauni\advmouse.uni"
$include "e:\aauni\input3.uni"
$include "e:\aauni\jaring.uni"
$include "e:\aauni\nmfildta.uni"
$include "e:\aauni\scan.uni"
$include "e:\aauni\unascz.uni"
$include "e:\aamsn\dos\stmsarea.uni"
$include "e:\aamsn\imaging\besarpst.uni"

'leaf fun
'tool = 0 maka pensil
'tool = 1 maka penghapus
'curshow% = 0 maka gak akan ada cursor
'curshow% = -1 maka muncul deh cursor
'subrutin viewing akan memperlihatkan apa yang ada pada matrix pada viewer
'         dan hanya memindahkan dari isi.je ke canvas dan background
'         sehingga untuk melihat hasilnya harus pakai move.the.can% = -1
'subrutin updatejaring edit akan memperlihatkan isi.je pada ckv dan cbv yang
'         dilihat saat itu

screen 1
def fnmyinput$(comment$)
 local bank$
 shared banklama$
 print comment$;
 baris%  = csrlin
 kolom%  = pos
 bank$ = ""
 do
  kode% = 0
  tpt%  = 0
  call rdkey(kode%, tpt%)
  if (kode% = 13) and (tpt% = 1) then goto finish
  if (kode% = 27) and (tpt% = 1) then
   bank$ = chr$(27)
   goto finish
  end if
  if (kode% = 61) and (tpt% = 2) then
   bank$ = banklama$
   locate baris%, kolom%
   print bank$+chr$(26)+" ";
   goto toll
  end if
  if (kode% = 8) and (tpt% = 1) then
   call panstr(bank$, pan%)
   if pan% > 0 then bank$ = left$(bank$, pan%-1)
   locate baris%, kolom%
   print bank$+chr$(27)+" ";
   goto toll
  end if
  tambah:
  if kode% <> 0 then
   bank$ = bank$ + chr$(kode%)
   locate baris%, kolom%
   print bank$+chr$(26)+" ";
  end if
  toll:
 loop
 finish:
 fnmyinput$ = bank$
 banklama$ = bank$
end def
def fnjedake(brp%, m$, p$)
local st%
 i.hrfke = brp%
 st% = 1
 do
  if i.hrfke <=0 then quitjedake
  st% = instr(st%, m$, p$) + 1
  i.hrfke = i.hrfke - 1
 loop
 quitjedake:
  fnjedake = st% - 1
end def
def fnkalimat$(master$, st%, stops%)
 fnkalimat$ = mid$(master$, st%, stops%-st%+1)
end def
def fnkatake$(kk, m$, p$)
 m$ = m$ + p$
 if kk = 1 then
  starthu = 1
  stophu  = fnjedake(1, m$, p$)-1
 else
  starthu = fnjedake(kk-1, m$, p$)+1
  stophu  = fnjedake(kk, m$, p$)-1
  call panstr(m$, pan%)
  if stophu <= -1 then stophu = pan%
 end if
 fnkatake$ = fnkalimat$(m$, starthu, stophu)
end def
def fnarea(x1%, y1%, x2%, y2%, xm%, ym%)
 if x1%<=xm% and xm%<=x2% and y1%<=ym% and ym%<=y2% then fnarea = -1
end def
def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
def fnblok%
 shared bk.x.st%, bk.y.st%, bk.x.sp%, bk.y.sp%
 fnblok% = (bk.x.st% <> bk.x.sp%) and (bk.y.st% <> bk.y.sp%)
end def
'akhir prosedur umum=====================================
sub fol(mask$)
shared can%, scr%, gdg%
shared move.the.can%
shared lvdta$, namafileret$, memofont%()
 erno% = 0
 kolom% = 0
 baris% = 0
 dim dynamic dta%(22)
 call cari(mask$+chr$(0), dta%(0), erno%)
 if erno% = 18 then goto ending4
 call mycls(scr%)
 call getnamafiledta(dta%(), text$)
 call attrdta(dta%(0), attr%)
 if attr% = &H10 then text$ = text$ + "\"
 call vhuruf(memofont%(0), scr%, kolom%, baris%, text$, 10)
 kolom% = kolom% + 80
 dta$ = string$(43, 0)
 do
  call carilagi(dta%(0), erno%)
  if erno% = 18 then exit loop
  call getnamafiledta(dta%(), text$)
  call attrdta(dta%(0), attr%)
  if attr% = &H10 then text$ = text$ + "\"
  call vhuruf(memofont%(0), scr%, kolom%, baris%, text$, 10)
  kolom% = kolom% + 80
  if kolom% >= 320 - 80 + 1 then
   kolom% = 0
   baris% = baris% + 9
  end if
 loop
ending4:
 erase dta%
end sub
sub save(lvpathname$, lvx1%, lvy1%, lvx2%, lvy2%)
 shared je.isi%()
 if lvx2% < lvx1% then swap lvx1%, lvx2%
 if lvy2% < lvy1% then swap lvy1%, lvy2%
 panjang% = lvx2%-lvx1%+1
 lebar% = lvy2%-lvy1%+1
 if panjang% < 1 then goto ending3
 if lebar% < 1 then goto ending3
 if lvpathname$ = "" then goto ending3
 call btfil(lvpathname$+chr$(0), 0, hand%)
 call tlstrf(hand%, mki$(panjang%)+mki$(lebar%))
 for b% = lvy1% to lvy2%
  for a% = lvx1% to lvx2%
   call tlstrf(hand%, chr$(je.isi%(a%, b%)))
  next a%
 next b%
 call tpfil(hand%)
 ending3:
end sub
sub savebmp(nama$, st.x%, st.y%, sp.x%, sp.y%)
 shared je.isi%(), scr%, memofont%()
 local panjang%, lebar%, tempwarna%, ada%, aa%, bb%, cc%, jumlahwarna%
 dim dynamic tablewarna%(256)

 if sp.x% < st.x% then swap st.x%, sp.x%
 if sp.y% < st.y% then swap st.y%, sp.y%
 panjang% = sp.x% - st.x% + 1
 lebar% = sp.y% - st.y% + 1
 if (panjang% and lebar%) = 1 then erase tablewarna% : exit sub

'check for jumlah warna yang benar-benar digunakan
 for aa% = st.y% to sp.y%
  for bb% = st.x% to sp.x%
   tempwarna% = je.isi%(aa%,bb%)
   ada% = 0
   for cc% = 1 to jumlahwarna%
    if tempwarna% = tablewarna%(cc%) then
     ada% = -1
     exit for
    end if
   next cc%
   if not ada% then
    incr jumlahwarna%
    tablewarna%(jumlahwarna%) = je.isi%(aa%,bb%)
   end if
  next bb%
 next aa%
 erase tablewarna%

 dim dynamic temp%(512)
  for tmpa% = 0 to 255
   call gtpal(tmpa%, r%, g%, b%)
   call num2num(b%*4, 0, temp%(0), tmpa%*4, 1)
   call num2num(g%*4, 0, temp%(0), tmpa%*4+1, 1)
   call num2num(r%*4, 0, temp%(0), tmpa%*4+2, 1)
   call num2num(0, 0,temp%(0),tmpa%*4+3,1)
  next tmpa%

 call btfil(nama$+chr$(0),0,hand%)
  call tlstrf(hand%, "BM")
  call tlstrf(hand%, string$(4,0)) 'size of the file
  call tlstrf(hand%, string$(4,0)) 'reserved
  call tlword(hand%, 1078) 'offset of the image
 '========================
  call tlword(hand%, 40) 'size of the header
  call tlword(hand%, cdbl(panjang%)) 'panjang
  call tlword(hand%, cdbl(lebar%)) 'lebar
  call tlnumf(hand%,1,2)
  call tlnumf(hand%,8,2) 'bpp
  call tlword(hand%,0) 'compression
  call tlword(hand%,panjang%*lebar%) 'imagesize
  call tlword(hand%,panjang%*6) 'panjang pels
  call tlword(hand%,lebar%*6) 'panjang pels
  call tlword(hand%,0) 'jumlah warna
  call tlword(hand%,0) 'jumlah warna penting
'========================== palette data mulai dari sini
  call tlnumf(hand%,temp%(0),1024)
'========================== image data
  jumlahblok% = ceil(panjang%/4)
  for tempbar% = sp.y% to st.y% step -1
   for tempa% = 0 to jumlahblok%-1
    for tempb% = 0 to 3
     call num2num(je.isi%(st.x%+tempa%*4+tempb%,tempbar%), 0, blok&, tempb%,1)
    next tempb%
    call tlword(hand%,blok&)
   next tempa%
  next tempbar%
  call tlstrf(hand%,"Livan")
 call tpfil(hand%)
 call ukfil2(nama$+chr$(0),ukfil&)
 call bkfil(nama$+chr$(0),1,hand%)
  call goptr(hand%,2)
  call tlword(hand%,ukfil&)
 call tpfil(hand%)
 erase temp%
end sub
sub load(namafile$, x1%, y1%)
 shared je.isi%(), lvbanyak%, lvfileno%, bacaret$
 if namafile$ = "" then goto ending2
 call bkfil(namafile$+chr$(0), 0, hand%)
 call bcnumf(hand%, panjang%, 2)
 call bcnumf(hand%, lebar%, 2)
 total% = panjang%*lebar%
 for b% = y1% to y1%+lebar%-1
  for a% = x1% to x1%+panjang%-1
   call bcnumf(hand%, datax%, 1)
   if datax% <> 0 then je.isi%(a%, b%) = datax%
  next a%
 next b%
 call tpfil(hand%)
 gosub updatejaringedit
 ending2:
end sub
sub clearlabel(segm%)
shared label.cleared%
 if not label.cleared% then
  call bar2(320,20,0,segm%,0,181)
  label.cleared% = -1
 end if
end sub

sub mbkey1down
 shared je%, klik.x%, je.kolom%, je.baris%, klik.y%
 if je% then
  klik.x% = je.kolom%
  klik.y% = je.baris%
 end if
end sub

sub mbkey2down
end sub

sub mbkey3down
end sub

sub mbkeyup
end sub

sub dblclick
end sub
'############################*******************###########################
dim dynamic memo1%(32766) 'for bekgron
dim dynamic memo2%(32766) 'for canvas
dim dynamic memofont%(500) 'for font
dim dynamic memo3%(587) 'for gudang

call pointer(memo1%(0), bgr%)
call pointer(memo2%(0), can%)
call pointer(memo3%(0), gdg%)
scr% = &HA000

call besarpst("cursor.pst"+chr$(0), cursor.p%, cursor.l%)
call psimg("cursor.pst"+chr$(0), gdg%, 0, pensil%)
call psimg("pslkcl.pst"+chr$(0), gdg%, pensil%, karet%)
call psimg("blank.pst"+chr$(0), gdg%, karet%, neks%)

'jaring maksimal
 mjumkol% = 175
 mjumbar% = 175
 dim dynamic je.isi%(mjumkol%, mjumbar%)

'jaring edit
 je.x% = 0
 je.y% = 0
 je.jumkol% = 60
 je.jumbar% = 60
 je.pkotak% = 2
 je.lkotak% = 2
 je.jarakkotak% = 1
 je.warnadalam% = 0
 je.warnaluar% = 8
 je.pjaring% = (je.pkotak%+je.jarakkotak%) * je.jumkol%
 je.ljaring% = (je.lkotak%+je.jarakkotak%) * je.jumbar%
 je.ckv% = 1
 je.cbv% = 1
 je.mkv% = mjumkol% - je.jumkol% +1
 je.mbv% = mjumbar% - je.jumbar% + 1

'jaring warna
 jw.x% = je.x%+je.pjaring%+1
 jw.y% = je.y%
 jw.pkotak% = 3
 jw.lkotak% = 3
 jw.jumkol% = 16
 jw.jumbar% = 16
 jw.jarakkotak% = 1
 jw.warnaluar% = 8
 jw.pjaring% = (jw.pkotak%+jw.jarakkotak%) * jw.jumkol%
 jw.ljaring% = (jw.lkotak%+jw.jarakkotak%) * jw.jumbar%

'kontrol
 kode% = 0
 tpt%  = 0
 lvmb% = 0
 xm% = 0
 lvym% = 0

'area
 je%  = 0
 jw% = 0

'warna
 wp% = 10
 wp.find% = 0
 wp.jaring% = 0

'pensilbox
 pb.panjang% = 10
 pb.lebar% = 10
 pb.warnaluar% = 8
 pb.x% = jw.x% + jw.pjaring% - pb.panjang%*3 + 1
 pb.y% = jw.y% + jw.ljaring%

'cursorbox
 cursorbox.x% = pb.x%+pb.panjang%+2
 cursorbox.y% = pb.y%
 cursorbox.panjang% = pb.panjang%

'viewer
 v.x% = jw.x%+jw.pjaring% + 1
 v.y% = jw.y%
 v.warnaluar% = 8

'mover
 um.x% = jw.x% + 10
 um.y% = 66
 dm.x% = um.x%
 dm.y% = um.y% + 20
 lm.x% = dm.x% - 10
 lm.y% = um.y% + 10
 rm.x% = um.x% + 10
 rm.y% = lm.y%

'eraser
 eraser.x% = lm.x%
 eraser.y% = dm.y%+10

'pensil2
 pensil2.x% = eraser.x%
 pensil2.y% = eraser.y%+18

'palx
 palx.x% = eraser.x%
 palx.y% = pensil2.y%+13

'blocker
 blocker% = 0
 bk.x.st% = 0
 bk.y.st% = 0
 bk.x.sp% = 0
 bk.y.sp% = 0

'tool
 tool% = 0 '0 pensil; 1 hapus;

'savpal
 savpal.x% = palx.x% + 12
 savpal.y% = palx.y%

call layar19
dim dynamic temp%(162)
call vp(temp%(0), "back.pst"+chr$(0), bgr%, 0, 0)
call jaring(bgr%, je.x%, je.y%, je.jumkol%, je.jumbar%, je.pkotak%, je.lkotak%, je.jarakkotak%, je.warnadalam%, je.warnaluar%)
call jaring(bgr%, jw.x%, jw.y%, jw.jumkol%, jw.jumbar%, jw.pkotak%, jw.lkotak%, jw.jarakkotak%, warnadalam.jw%, jw.warnaluar%)
call bar2(pb.panjang%, pb.lebar%, pb.warnaluar%, bgr%, pb.x%, pb.y%)
call bar2(pb.panjang%, pb.lebar%, pb.warnaluar%, bgr%, pb.x%+1*pb.panjang%, pb.y%)
call bar2(pb.panjang%, pb.lebar%, pb.warnaluar%, bgr%, pb.x%+2*pb.panjang%, pb.y%)
call bar2(pb.panjang%-2, pb.lebar%-2, wp%, bgr%, pb.x%+1, pb.y%+1)
call bar2(pb.panjang%-2, pb.lebar%-2, wp.find%, bgr%, pb.x%+1*pb.panjang%+1, pb.y%+1)
call bar2(pb.panjang%-2, pb.lebar%-2, wp.jaring%, bgr%, pb.x%+2*pb.panjang%+1, pb.y%+1)
call bar2(je.jumkol%+2, je.jumbar%+2, v.warnaluar%, bgr%, v.x%, v.y%)
call bar2(je.jumkol%, je.jumbar%, 0, bgr%, v.x%+1, v.y%+1)
call vp(temp%(0), "aup.pst"+chr$(0), bgr%, um.x%, um.y%)
call vp(temp%(0), "adown.pst"+chr$(0), bgr%, dm.x%, dm.y%)
call vp(temp%(0), "aleft.pst"+chr$(0), bgr%, lm.x%, lm.y%)
call vp(temp%(0), "aright.pst"+chr$(0), bgr%, rm.x%, rm.y%)
call vp(temp%(0), "eraser.pst"+chr$(0), bgr%, eraser.x%, eraser.y%)
call vp(temp%(0), "pensil.pst"+chr$(0), bgr%, pensil2.x%, pensil2.y%)
call vp(temp%(0), "palx.pst"+chr$(0), bgr%, palx.x%, palx.y%)
call vp(temp%(0), "savepal.pst"+chr$(0), bgr%, savpal.x%, savpal.y%)
call vhuruf(memofont%(0), bgr%, palx.x%, savpal.y%+10, "leaf fun", 179)
erase temp%

call instalfont(memofont%(0), "livan.lff"+chr$(0), erno%)
for a% = 1 to jw.jumbar%
 for b% = 1 to jw.jumkol%
  call bar2(jw.pkotak%, jw.lkotak%, ctr%, bgr%, jw.x%+((b%-1)*(jw.pkotak%+jw.jarakkotak%))+1, jw.y%+((a%-1)*(jw.lkotak%+jw.jarakkotak%))+1)
  ctr% = ctr% + 1
 next b%
next a%

do
 gosub mustinit
 call rdkey(kode%, tpt%)
 call advmouse(2, xm%, lvym%, lvmb%, lvmovement$)
 call stmsarea(0,0,640-cursor.p%*2,200-cursor.l%)
 xm% = xm% /2
 gosub definearea

 if lvmovement$ <> "" then
  gosub btoc
  call vhuruf(memofont%(0), can%, 265, 187, "("+fnkarakter$(xm%)+", "+fnkarakter$(lvym%)+")", 10)
  if stoubah% = -1 then stoubah% = 0
  if ptoubah% = -1 then ptoubah% = 0
  move.the.can% = -1
 end if

 if kode% <> 0 then gosub kiybordpenceted
 if je% then gosub injaringedit
 if jw% then gosub injaringwarna
 if um% then gosub inupmover
 if dm% then gosub indownmover
 if lm% then gosub inleftmover
 if rm% then gosub inrightmover
 if er% then gosub ineraser
 if pn% then gosub inpensil
 if jv% then gosub injaringviewer
 if px% then gosub inpalx
 if sp% then gosub insavpal

 if move.the.can% then
  if curshow% then call ppimg2(gdg%, cursor%, can%, xm%, lvym%)
  gosub ctos
 end if

loop until kode% = 27
end

'========================================================
viewing:
 for b% = je.cbv% to je.cbv%+je.jumbar%-1
  for a% = je.ckv% to je.ckv%+je.jumkol%-1
   call pxl(bgr%, a%-je.ckv%+v.x%+1, b%-je.cbv%+v.y%+1, je.isi%(a%, b%))
   call pxl(can%, a%-je.ckv%+v.x%+1, b%-je.cbv%+v.y%+1, je.isi%(a%, b%))
  next a%
 next b%
 if curshow% then call ppimg2(gdg%, cursor%, can%, xm%, lvym%)
 move.the.can% = -1
return

ctos:
 call move(can%, 0, scr%, 0, &H8000)
return

btoc:
 if bgr.moved% = 0 then
  call move(bgr%, 0, can%, 0, &H8000)
  bgr.moved% = -1
 end if
return

updatejaringedit:
 for b% = je.cbv% to je.cbv%+je.jumbar%-1
  for a% = je.ckv% to je.ckv%+je.jumkol%-1
   call bar2(je.pkotak%, je.lkotak%, je.isi%(a%, b%), _
             bgr%, (a%-je.ckv%)*(je.pkotak%+je.jarakkotak%)+1+je.x%, _
             (b%-je.cbv%)*(je.lkotak%+je.jarakkotak%)+1+je.y%)
   call bar2(je.pkotak%, je.lkotak%, je.isi%(a%, b%), _
             can%, (a%-je.ckv%)*(je.pkotak%+je.jarakkotak%)+1+je.x%, _
             (b%-je.cbv%)*(je.lkotak%+je.jarakkotak%)+1+je.y%)
  next a%
 next b%
 if curshow% then call ppimg2(gdg%, cursor%, can%, xm%, lvym%)
 move.the.can% = -1
return

definearea:
 je% = fnarea(je.x%, je.y%, je.x%+je.pjaring%, je.y%+je.ljaring%, xm%, lvym%)
 jw% = fnarea(jw.x%, jw.y%, jw.x%+jw.pjaring%, jw.y%+jw.ljaring%, xm%, lvym%)
 jv% = fnarea(v.x%, v.y%, v.x%+je.jumkol%+1, v.y%+je.jumbar%+1, xm%, lvym%)
 um% = fnarea(um.x%, um.y%, um.x%+8, um.y%+8, xm%, lvym%)
 dm% = fnarea(dm.x%, dm.y%, dm.x%+8, dm.y%+8, xm%, lvym%)
 lm% = fnarea(lm.x%, lm.y%, lm.x%+8, lm.y%+8, xm%, lvym%)
 rm% = fnarea(rm.x%, rm.y%, rm.x%+8, rm.y%+8, xm%, lvym%)
 er% = fnarea(eraser.x%, eraser.y%, eraser.x%+17, eraser.y%+13, xm%, lvym%)
 pn% = fnarea(pensil2.x%, pensil2.y%, pensil2.x%+10, pensil2.y%+10, xm%, lvym%)
 px% = fnarea(palx.x%, palx.y%, palx.x%+10, palx.y%+8, xm%, lvym%)
 sp% = fnarea(savpal.x%, savpal.y%, savpal.x%+12, savpal.y%+8, xm%, lvym%)
 return

kiybordpenceted:
 if instr("Ss", chr$(kode%)) <> 0 and stoubah% and je% then
  teks$ = "Warna diubah = "+fnkarakter$(wp%)+" Warna perubah = "
  call pankal(memofont%(0), teks$, temp%)
  call clearlabel(scr%)
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 10)
  temp$ = fninput3$(scr%, temp%+1, 187, 12, "", bgr%)
  if temp$ = "" then return
  warnanomor% = val(temp$)
  if not fnblok% then
   for aa% = 1 to mjumbar%
    for ab% = 1 to mjumkol%
     if je.isi%(ab%, aa%) = wp% then
      je.isi%(ab%, aa%) = warnanomor%
      move.the.can% = -1
     end if
    next ab%
   next aa%
  else
   for aa% = bk.y.st% to bk.y.sp%
    for ab% = bk.x.st% to bk.x.sp%
     if je.isi%(ab%, aa%) = wp% then
      je.isi%(ab%, aa%) = warnanomor%
      move.the.can% = -1
     end if
    next ab%
   next aa%
  end if
  gosub updatejaringedit
  gosub viewing
 elseif ptoubah% and instr("Pp", chr$(kode%)) <> 0 then
  gosub gantipal
 elseif kode% = 75 and tpt% = 2 then 'tbl kiri
  je.ckv% = je.ckv% - 1
  if je.ckv% <= 0 then je.ckv% = 1
  gosub updatejaringedit
  gosub viewing
 elseif kode% = 77 and tpt% = 2 then 'tbl kanan
  je.ckv% = je.ckv% + 1
  if je.ckv% > je.mkv% then je.ckv% = je.mkv%
  gosub updatejaringedit
  gosub viewing
 elseif kode% = 72 and tpt% = 2 then 'tbl atas
  je.cbv% = je.cbv% - 1
  if je.cbv% <= 0 then je.cbv% = 1
  gosub updatejaringedit
  gosub viewing
 elseif kode% = 80 and tpt% = 2 then 'tbl bawah
  je.cbv% = je.cbv% + 1
  if je.cbv% > je.mbv% then je.cbv% = je.mbv%
  gosub updatejaringedit
  gosub viewing
 elseif kode% = 35 then 'tbl #
  call fol("*.*")
 elseif kode% = 31 and tpt% = 2 then      'alt S
  call fol("*.*")
  teks$ = "tentukan nama file untuk menyimpan gambar"
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 10)
  call pankal(memofont%(0), teks$, temppan%)
  namafile$ = fninput3$(scr%, temppan%, 187, 12, "", bgr%)
  if namafile$ = "" or namafile$ = chr$(27) then exit if
  call savebmp(namafile$, bk.x.st%, bk.y.st%, bk.x.sp%, bk.y.sp%)
  call clearlabel(scr%)
  teks$ = "save sebagai apa pst atau bmp (p/b)"
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 10)
  call pankal(memofont%(0), teks$, temppan%)
  tipe$ = fninput3$(scr%, temppan%+1,187,12,"",can%)
  if instr("Pp",tipe$) <> 0 then
   call save(namafile$, bk.x.st%, bk.y.st%, bk.x.sp%, bk.y.sp%)
  elseif instr("Bb",tipe$) <> 0 then
   call savebmp(namafile$, bk.x.st%, bk.y.st%, bk.x.sp%, bk.y.sp%)
  else
   call clearlabel(scr%)
   call vhuruf(memofont%(0), scr%, 0, 187, "penyimpanan gagal", 12)
  end if
 elseif kode% = 25 and tpt% = 2 then 'alt P
  gosub btoc
  gosub ctos
  teks$ = "Tentukan nama file palette yang akan digunakan."
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 1)
  call pankal(memofont%(0), teks$, temppan%)
  namafile$ = fninput3$(scr%, temppan%, 187, 12, "", bgr%)
  if namafile$ = "" or namafile$ = chr$(27) then goto fastpal
  call ada(namafile$+chr$(0), ada%)
  if not ada% then
   call vhuruf(memofont%(0), can%, 0, 187, "file tidak ada atau direktori tidak ada", 4)
   goto fastpal
  end if
  dim dynamic pale%(384)
  call palfile(pale%(0), namafile$+chr$(0))
  erase pale%
  fastpal:
  move.the.can% = -1
 elseif kode% = 50 and tpt% = 2 and je% then
  gosub btoc
  gosub ctos
  teks$ = "masukkan nama file yang akan diedit"
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 10)
  call pankal(memofont%(0), teks$, temppan%)
  namafile$ = fninput3$(&HA000, temppan%, 187, 12, "", bgr%)
  call ada(namafile$+chr$(0), adakah%)
  if not adakah% then
   call vhuruf(memofont%(0), can%, 0, 187, "direktori tidak ada atau file tak ada", 4)
   move.the.can% = -1
   goto fastload
  end if
  call load(namafile$, je.kolomview%, je.barisview%)
  gosub viewing
  fastload:
 elseif blocker% = 0 and je% and kode% = 13 then
  blocker% = 1
  bk.x.st% = je.kolomview%
  bk.y.st% = je.barisview%
  call clearlabel(can%)
  call vhuruf(memofont%(0), can%, 0, 187, "tentukan pula akhir block", 10)
  move.the.can% = -1
 elseif blocker% = 1 and je% and kode% = 13 then
  blocker% = 0
  bk.x.sp% = je.kolomview%
  bk.y.sp% = je.barisview%
  call clearlabel(can%)
  call vhuruf(memofont%(0), can%, 0, 187, "block berhasil", 10)
  move.the.can% = -1
 else
  call clearlabel(can%)
  call vhuruf(memofont%(0),can%,0,187, "Kode ascii:"+fnkarakter$(kode%)+" Tipe tombol:"+fnkarakter$(tpt%),10)
  move.the.can% = -1
 end if
 return

mustinit:
 kode% = 0
 tpt% = 0
 lvmovement$ = ""
 bgr.moved% = 0
 move.the.can% = 0
 curshow% = -1
 label.cleared% = 0
 return

injaringedit:
 je.kolom% = (xm%-je.x%)\(je.pkotak%+je.jarakkotak%)+1
 je.baris% = (lvym%-je.y%)\(je.lkotak%+je.jarakkotak%)+1
 je.kolomview% = je.kolom% + je.ckv% - 1
 je.barisview% = je.baris% + je.cbv% - 1

 if lvmovement$ <> "" and lvmb%<>1 then
  call bar2(pb.panjang%-2, pb.lebar%-2, je.isi%(je.kolomview%, je.barisview%), can%, pb.x%+2*pb.panjang%+1, pb.y%+1)
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "["+fnkarakter$(je.kolomview%)+", "+fnkarakter$(je.barisview%)+"]="+fnkarakter$(je.isi%(je.kolomview%, je.barisview%)), 10)
  move.the.can% = -1
 end if

 if lvmb% = 1 then
  select case tool%
   case 0
    je.isi%(je.kolomview%, je.barisview%) = wp%
    if je.kolom% > klik.x% then
     panjang% = je.kolom% - klik.x%+1
    else
     panjang% = -(klik.x% - je.kolom%+1)
     if panjang% = -1 then panjang% = 1
    end if
    if je.baris% > klik.y% then
     lebar% = je.baris% - klik.y%+1
    else
     lebar% = -(klik.y% - je.baris%+1)
     if lebar% = -1 then lebar% = 1
    end if
    call clearlabel(can%)
    call vhuruf(memofont%(0),can%,0,187, "Panjang: "+fnkarakter$(panjang%)+" Lebar: "+fnkarakter$(lebar%),12)
   case 1
    curshow% = 0
    for bb% = 0 to 2
     for aa% = 0 to 2
      je.isi%(je.kolomview%+aa%, je.barisview%+bb%) = 0
     next aa%
    next bb%
  end select
  move.the.can% = -1
  gosub viewing
  gosub updatejaringedit
 elseif lvmb% = 2 then
  wp% = je.isi%(je.kolomview%, je.barisview%)
  call clearlabel(can%)
  call vhuruf(memofont%(0), can%, 0, 187, "tekan s untuk ubah semua warna "+fnkarakter$(wp%), 10)
  stoubah% = -1
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, bgr%, pb.x%+1, pb.y%+1)
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, can%, pb.x%+1, pb.y%+1)
  move.the.can% = -1
 elseif lvmb% = 3 then
  call g2(can%, xm%, 0, xm%, je.ljaring%, 3)
  call g2(can%, 0, lvym%, je.pjaring%, lvym%, 3)
  move.the.can% = -1
 end if

 select case tool%
  case 0
   curshow% = -1
  case 1
   curshow% = 0
   call ppimg2(gdg%, karet%, can%, xm%, lvym%)
 end select
 return

injaringwarna:
 kolom.jw% = (xm%-jw.x%)\(jw.pkotak%+jw.jarakkotak%)+1
 baris.jw% = (lvym%-jw.y%)\(jw.pkotak%+jw.jarakkotak%)+1
 wp.find%  = (kolom.jw%-1) + (baris.jw%-1)*16

 if lvmb% = 1 then
  wp% = wp.find%
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, bgr%, pb.x%+1, pb.y%+1)
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, can%, pb.x%+1, pb.y%+1)
  move.the.can% = -1
 elseif lvmb% = 2 then
  call clearlabel(can%)
  call vhuruf(memofont%(0), can%, 0, 187, "Tekan P untuk ubah warna palette", 10)
  ptoubah% = -1
  move.the.can% = -1
 end if
 if lvmovement$ <> "" then
  call bar2(pb.panjang%-2, pb.lebar%-2, wp.find%, can%, pb.x%+pb.panjang%+1, pb.y%+1)
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "Warna : "+fnkarakter$(wp.find%), 10)
  call ppimg2(gdg%, cursor%, can%, xm%, lvym%)
  move.the.can% = -1
 end if
 return

injaringviewer:
 v.kolom% = xm%-v.x%
 v.baris% = lvym%-v.y%
 v.kolomview% = v.kolom% + je.ckv% - 1
 v.barisview% = v.baris% + je.cbv% - 1
 curshow% = 0
 if lvmovement$ <> "" then
  call ppimg2(gdg%, pensil%, can%, xm%, lvym%)
  move.the.can% = -1
 end if
 if lvmb% = 1 then
  je.isi%(v.kolomview%, v.barisview%) = wp%
  gosub viewing
  call ppimg2(gdg%, pensil%, can%, xm%, lvym%)
  move.the.can% = -1
 end if
 if lvmb% = 2 then
  wp% = je.isi%(v.kolomview%, v.barisview%)
  call ppimg2(gdg%, pensil%, can%, xm%, lvym%)
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, bgr%, pb.x%+1, pb.y%+1)
  call bar2(pb.panjang%-2, pb.lebar%-2, wp%, can%, pb.x%+1, pb.y%+1)
  move.the.can% = -1
 end if
 gosub updatejaringedit
 return

inupmover:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "layar ke atas", 10)
 end if
 if lvmb% = 1 then
  je.cbv% = je.cbv% - 1
  if je.cbv% <= 0 then je.cbv% = 1
  gosub updatejaringedit
  gosub viewing
 end if
 return

indownmover:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "layar ke bawah", 10)
 end if
 if lvmb% = 1 then
  je.cbv% = je.cbv% + 1
  if je.cbv% > je.mbv% then je.cbv% = je.mbv%
  gosub updatejaringedit
  gosub viewing
 end if
 return

inleftmover:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "layar ke kiri", 10)
 elseif lvmb% = 1 then
  je.ckv% = je.ckv% - 1
  if je.ckv% <= 0 then je.ckv% = 1
  gosub updatejaringedit
  gosub viewing
 end if
 return

inrightmover:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "layar ke kanan", 10)
 elseif lvmb% = 1 then
  je.ckv% = je.ckv% + 1
  if je.ckv% > je.mkv% then je.ckv% = je.mkv%
  gosub updatejaringedit
  gosub viewing
 end if
 return

ineraser:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "penghapus", 10)
 elseif lvmb% = 1 then
  tool% = 1
 end if
 return

inpensil:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "pensil", 10)
 end if
 if lvmb% = 1 then
  tool% = 0
 end if
 return

inpalx:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "hapus palette", 10)
 elseif lvmb% = 1 then
  call layar19
  gosub ctos
 end if
 return

insavpal:
 if lvmovement$ <> "" then
  call vhuruf(memofont%(0), can%, eraser.x%, 187, "simpan palette", 10)
 elseif lvmb% = 1 then
  gosub savepal
 end if
 return

gantipal:
 call bar2(130, 40, 0, &HA000, 0, 0)
 locate 1, 1
 ? "Warna no: ";fnkarakter$(wp.find%)
 call gtpal(wp.find%, r%, g%, b%)
 ? "Red: "
 ? "Green: "
 ? "Blue: "
 call set(2, 8, 5)
 call set(3, 8, 5)
 call set(4, 8, 5)
 scanstrin$(1) = fnkarakter$(r%)
 scanstrin$(2) = fnkarakter$(g%)
 scanstrin$(3) = fnkarakter$(b%)
 call getem
 gosub ctos
 r% = val(scanstrin$(1))
 g% = val(scanstrin$(2))
 b% = val(scanstrin$(3))
 call setpal(wp.find%, r%, g%, b%)
 return

savepal:
 dim dynamic temp%(384)
  for tmpa% = 0 to 255
   call gtpal(tmpa%, r%, g%, b%)
   call num2num(r%, 0, temp%(0), tmpa%*3, 1)
   call num2num(g%, 0, temp%(0), tmpa%*3+1, 1)
   call num2num(b%, 0, temp%(0), tmpa%*3+2, 1)
  next tmpa%
  call bar2(320, 15, 0, scr%, 0, 182)
  teks$ = "Masukkan nama file untuk pal file anda!"
  call vhuruf(memofont%(0), scr%, 0, 187, teks$, 10)
  call pankal(memofont%(0), teks$, temppan%)
  temp$ = fninput3$(scr%, temppan%+3, 187, 12, "", bgr%)
  gosub ctos
  if temp$ = "" then
   erase temp%
   return
  end if
  call btfil(temp$+chr$(0), 0, hand%)
  call tlnumf(hand%, temp%(0), 768)
  call tpfil(hand%)
 erase temp%
 return

