def fnkarakter$(angka%)
local tmp$
 if angka% < 0 then tmp$ = "-"
 tmp2$ = str$(angka%)
 call panstr(tmp2$, pan%)
 tmp$ = tmp$ + right$(tmp2$, pan%-1)
 fnkarakter$ = tmp$
end def
def fnkalimat$(master$,start%,stops%)
 fnkalimat$ = mid$(master$,start%,stops%-start%+1)
end def
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
sub PANSTR inline '(strin$,panjang%)
 $inline &H55, &H89, &HE5, &H1E, &HC5, &H76, &H0A, &HC4, &H7E, &H06
 $inline &HA5, &H1F, &H5D
End Sub

screen 0
perintah$ = command$
perintah$ = "64,7500,2,e:\coba.txt,30000"
if perintah$ = "" then
 print "jumlah pemain, biaya, jumlah ps, file out, sewaps"
 delay .5
 end
end if
jp% = val(fnkatake$(1, perintah$, ","))
bi% = val(fnkatake$(2, perintah$, ","))
jps% = val(fnkatake$(3, perintah$, ","))
thefile$ = fnkatake$(4, perintah$, ",")
sewaps& = val(fnkatake$(5,perintah$,","))

open thefile$ for output as #1
dim dynamic jrondeput%(100)
dim dynamic jpemput%(100)
dim dynamic jwakput%(100)

jk%= log(jp%) / log(2)
j1&= jk%*2*cdbl(bi%)
j2&= j1&/1.5
j3&= j2&/1.5
sisa& = jp%*cdbl(bi%) - j1& - j2& - j3&
print #1, "Bila jumlah peserta:";jp%
print #1, "Juara 1 perlu menang";jk%;"kali berturut-turut"
print #1, "Dan untuk jadi juara 2 dan 3 cuma butuh menang";jk%-1;"kali berturut-turut"
print #1, "Biaya pendaftaran cuma";bi%
print #1, "Hadiah juara 1:";j1&
print #1, "Hadiah juara 2:";j2&
print #1, "Hadiah juara 3:";j3&
print #1, "Buat bandarnya:";sisa&
putaran% = 0
jpemput%(1) = jp%
do
 incr putaran%
 jrondeput%(putaran%) = ceil(jpemput%(putaran%)/(jps%*2))
 jwakput%(putaran%) = jrondeput%(putaran%)*15
 jpemput%(putaran%+1) = ceil(jpemput%(putaran%)/2)
loop until jpemput%(putaran%+1) <= 1

for putno% = 1 to putaran%
 pemain.no% = 1
 print #1, "Putaran";putno%
 for a% = 1 to jps%
  print #1, "Ps";fnkarakter$(a%);chr$(9);
 next a%
 print #1, ""
 for c% = 1 to jrondeput%(putno%)
  for b% = 1 to jps%
   if pemain.no% <= jpemput%(putno%) then
   print #1, fnkarakter$(pemain.no%);" vs ";fnkarakter$(pemain.no%+1);chr$(9);
   incr pemain.no%, 2
   end if
  next b%
 print #1, "15 menit"
 next c%
 print #1, string$(69, "-");fnkarakter$(jwakput%(putno%));" menit = " '; str$(cdbl(jwakput%(putno%))/60);" jam"
 totalwaktu% = totalwaktu% + jwakput%(putno%)
next putno%
jam% = totalwaktu%\60
menit% = totalwaktu% mod 60
print #1, "Bila kompetisi ini menggunakan";jps%;"console ps"
print #1, "Maka kompetisi memakan waktu";jam%;"jam";menit%;"menit.
print #1, "Waktu ini akan berkurang seiring dengan jumlah ps yang meningkat"
print #1, "Dana yang diperlukan:";jps%*sewaps&;"(";jps%;"ps yang disewa)"
close #1