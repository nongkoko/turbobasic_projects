'path validator
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
def fnunpadboth$(any$,p$)
local pan%, temu%, a%
 call panstr(any$,pan%)
 temu% = 0
 for a% = 1 to pan%
  if mid$(any$,a%,1) <> p$ then
   temu% = -1
   exit for
  end if
 next a%
 if temu% then
  starthu% = a%
  temu% = 0
  for a% = pan% to 1 step -1
   if mid$(any$,a%,1) <> p$ then
    temu% = -1
    exit for
   end if
  next a%
  if temu% then
   fnunpadboth$ = fnkalimat$(any$,starthu%,a%)
  else
   fnunpadboth$ = mid$(any$,starthu%)
  end if
 else
  fnunpadboth$ = ""
 end if
end def
def fnjedake(brp%,m$,p$)
local start%, i.hrfke
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
def fnvaliddir$(th$)
 local a%
 jumlah% = fngeneration(th$)
 bank$ = ""
 for a% = 1 to jumlah%
  if a% <> jumlah% then
   bank$ = bank$ + fnpath$(fnunpadboth$(fnkatake$(a%,th$,"\")," "))
  else
   bank$ = bank$ + fnunpadboth$(fnkatake$(a%,th$,"\")," ")
  end if
 next a%
 fnvaliddir$ = bank$
end def

sumber$ = command$
if sumber$ = "" then
 end
end if

dim dynamic pathfile$(1000)
open sumber$ for input as #1
 while not eof(1)
  incr ctr%
  line input #1, pathfile$(ctr%)
 wend
close #1

open sumber$ for output as #1
 for a% = 1 to ctr%
  if a% <> ctr% then
   print #1, fnvaliddir$(pathfile$(a%))
  else
   print #1, fnvaliddir$(pathfile$(a%));
  end if
 next a%
close #1